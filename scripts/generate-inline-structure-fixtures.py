#!/usr/bin/env python3
"""Generate inline signatures from pinned CommonMark and cmark-gfm XML.

Sources and inputs are shared with generate-block-structure-fixtures.py. Set
COMMONMARK_TEST_SUITE, GFM_TEST_SUITE, and CMARK_GFM to those local inputs when
running this script.
"""

from __future__ import annotations

import argparse
import importlib.util
import os
import re
import sys
import xml.etree.ElementTree as ET
from pathlib import Path
from typing import Any

BLOCK_FIXTURE_PATH = Path(__file__).with_name("generate-block-structure-fixtures.py")
BLOCK_FIXTURE_SPEC = importlib.util.spec_from_file_location(
    "block_structure_fixtures", BLOCK_FIXTURE_PATH
)
if BLOCK_FIXTURE_SPEC is None or BLOCK_FIXTURE_SPEC.loader is None:
    raise RuntimeError(f"cannot load {BLOCK_FIXTURE_PATH}")
block_fixtures = importlib.util.module_from_spec(BLOCK_FIXTURE_SPEC)
sys.modules[BLOCK_FIXTURE_SPEC.name] = block_fixtures
BLOCK_FIXTURE_SPEC.loader.exec_module(block_fixtures)


INLINE_CONTAINERS = {"paragraph", "heading", "table_cell"}

# cmark-gfm 0.29 loses two CommonMark 0.31.2 distinctions in its XML output.
COMMONMARK_SPEC_INLINE_OVERRIDES = {
    "commonmark-0196": [
        [
            [
                [
                    block_fixtures.Symbol(":link"),
                    "/url",
                    "\ntitle\nline1\nline2\n",
                    [[block_fixtures.Symbol(":text"), "foo"]],
                ]
            ]
        ],
        [],
    ],
    "commonmark-0354": [
        [
            [[block_fixtures.Symbol(":text"), "*$*alpha."]],
            [[block_fixtures.Symbol(":text"), "*£*bravo."]],
            [[block_fixtures.Symbol(":text"), "*€*charlie."]],
        ],
        [],
    ]
}


def source_position(value: str, example: block_fixtures.Example) -> tuple[int, int, int, int]:
    match = re.match(r"^(\d+):(\d+)-(\d+):(\d+)$", value)
    if not match:
        raise ValueError(f"{example.identifier}: unusable inline sourcepos {value!r}")
    return tuple(int(group) for group in match.groups())


def inline_footnote_label(
    node: ET.Element,
    lines: list[str],
    example: block_fixtures.Example,
) -> str:
    value = node.get("sourcepos")
    if value is None:
        raise ValueError(f"{example.identifier}: footnote reference has no sourcepos")
    start_line, start_column, end_line, end_column = source_position(value, example)
    if start_line != end_line or not 1 <= start_line <= len(lines):
        raise ValueError(f"{example.identifier}: invalid footnote reference sourcepos {value!r}")
    source = lines[start_line - 1][start_column - 1 : end_column]
    if len(source) < 4 or not source.startswith("[^") or not source.endswith("]"):
        raise ValueError(
            f"{example.identifier}: footnote sourcepos {value!r} does not select [^label]"
        )
    return source[2:-1]


def normalize_sequence(nodes: list[ET.Element], example: block_fixtures.Example,
                       lines: list[str]) -> list[Any]:
    result: list[Any] = []
    for node in nodes:
        signature = inline_signature(node, example, lines)
        if signature[0] == block_fixtures.Symbol(":text") and not signature[1]:
            continue
        if (
            signature[0] == block_fixtures.Symbol(":text")
            and result
            and result[-1][0] == block_fixtures.Symbol(":text")
        ):
            result[-1][1] += signature[1]
        else:
            result.append(signature)
    return result


def inline_signature(
    node: ET.Element,
    example: block_fixtures.Example,
    lines: list[str],
) -> list[Any]:
    name = block_fixtures.local_name(node.tag)
    children = list(node)

    if name == "text":
        return [block_fixtures.Symbol(":text"), node.text or ""]
    if name == "code":
        return [block_fixtures.Symbol(":code"), node.text or ""]
    if name == "html_inline":
        return [block_fixtures.Symbol(":html"), node.text or ""]
    if name == "softbreak":
        return [block_fixtures.Symbol(":soft-break")]
    if name == "linebreak":
        return [block_fixtures.Symbol(":hard-break")]
    if name == "unknown" and not children:
        return [
            block_fixtures.Symbol(":footnote-reference"),
            inline_footnote_label(node, lines, example),
        ]
    if name in {"emph", "strong", "strikethrough"}:
        kind = {
            "emph": ":emphasis",
            "strong": ":strong",
            "strikethrough": ":strikethrough",
        }[name]
        return [block_fixtures.Symbol(kind), normalize_sequence(children, example, lines)]
    if name in {"link", "image"}:
        kind = ":link" if name == "link" else ":image"
        title = node.get("title") or None
        return [
            block_fixtures.Symbol(kind),
            node.get("destination", ""),
            title,
            normalize_sequence(children, example, lines),
        ]

    raise ValueError(f"{example.identifier}: unsupported cmark inline node {name!r}")


def collect_inline_fragments(
    node: ET.Element,
    example: block_fixtures.Example,
    lines: list[str],
    fragments: list[list[Any]],
) -> None:
    name = block_fixtures.local_name(node.tag)
    children = list(node)
    if name in INLINE_CONTAINERS:
        fragments.append(normalize_sequence(children, example, lines))
        return
    if name == "unknown" and children:
        return
    for child in children:
        collect_inline_fragments(child, example, lines, fragments)


def parse_signature(xml: str, example: block_fixtures.Example) -> list[Any]:
    sanitized = block_fixtures.sanitize_cmark_xml(xml)
    root = ET.fromstring(sanitized)
    if block_fixtures.local_name(root.tag) != "document":
        raise ValueError(f"{example.identifier}: XML root is not document")

    lines = block_fixtures.source_lines(example.markdown)
    fragments: list[list[Any]] = []
    footnotes: list[list[Any]] = []
    collect_inline_fragments(root, example, lines, fragments)

    for node in root.iter():
        if block_fixtures.local_name(node.tag) != "unknown" or not list(node):
            continue
        sourcepos = node.get("sourcepos")
        if sourcepos is None:
            raise ValueError(f"{example.identifier}: footnote definition has no sourcepos")
        start_line, _, _, _ = source_position(sourcepos, example)
        label = block_fixtures.footnote_label_from_source(lines, start_line, example)
        content: list[list[Any]] = []
        for child in node:
            collect_inline_fragments(child, example, lines, content)
        footnotes.append([label, content])

    return [fragments, footnotes]


def render_fixture(entries: list[tuple[str, list[Any]]]) -> str:
    lines = [
        "; Generated by scripts/generate-inline-structure-fixtures.py. Do not edit by hand.",
        "(",
    ]
    for identifier, signature in entries:
        lines.append(
            "  "
            + block_fixtures.render_lisp([identifier, signature])
        )
    lines.append(")")
    return "\n".join(lines) + "\n"


def main(argv: list[str] | None = None) -> int:
    script = Path(__file__).resolve()
    repository = script.parent.parent
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--commonmark-spec",
        type=Path,
        default=os.environ.get("COMMONMARK_TEST_SUITE"),
        help="pinned CommonMark 0.31.2 spec.txt (defaults to COMMONMARK_TEST_SUITE)",
    )
    parser.add_argument(
        "--gfm-spec",
        type=Path,
        default=os.environ.get("GFM_TEST_SUITE"),
        help="pinned cmark-gfm extensions.txt (defaults to GFM_TEST_SUITE)",
    )
    parser.add_argument(
        "--cmark",
        default=os.environ.get("CMARK_GFM"),
        help="pinned cmark-gfm binary (defaults to CMARK_GFM, cmark-gfm, or cmark)",
    )
    parser.add_argument(
        "--output",
        type=Path,
        default=repository / "t/data/inline-structure-fixtures.lisp",
        help="fixture destination",
    )
    parser.add_argument("--check", action="store_true", help="fail if the fixture differs")
    args = parser.parse_args(argv)

    try:
        if args.commonmark_spec is None:
            raise ValueError("set COMMONMARK_TEST_SUITE or pass --commonmark-spec")
        if args.gfm_spec is None:
            raise ValueError("set GFM_TEST_SUITE or pass --gfm-spec")
        cmark = block_fixtures.resolve_cmark(args.cmark)
        examples = block_fixtures.read_examples(args.commonmark_spec, "commonmark")
        examples.extend(block_fixtures.read_examples(args.gfm_spec, "gfm"))

        entries = []
        for example in examples:
            xml = block_fixtures.run_cmark(
                cmark, example, example.identifier.startswith("gfm-")
            )
            try:
                signature = parse_signature(xml, example)
                signature = COMMONMARK_SPEC_INLINE_OVERRIDES.get(
                    example.identifier, signature
                )
                entries.append((example.identifier, signature))
            except ET.ParseError as error:
                raise ValueError(f"{example.identifier}: invalid cmark XML: {error}") from error

        rendered = render_fixture(entries)
        if args.check:
            if not args.output.exists():
                raise ValueError(f"fixture does not exist: {args.output}")
            current = args.output.read_text(encoding="utf-8")
            if current != rendered:
                raise ValueError(f"fixture differs from generated output: {args.output}")
        else:
            args.output.write_text(rendered, encoding="utf-8")

        print(f"{len(entries)} examples generated; output {args.output}")
        return 0
    except (OSError, ET.ParseError, RuntimeError, TypeError, ValueError) as error:
        print(f"generate-inline-structure-fixtures.py: {error}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
