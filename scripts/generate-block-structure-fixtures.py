#!/usr/bin/env python3
"""Generate block signatures from the pinned CommonMark and cmark-gfm suites.

Sources:
  https://github.com/commonmark/commonmark-spec/tree/0.31.2
  https://github.com/github/cmark-gfm/tree/0.29.0.gfm.13

Retrieval: read the spec.txt and test/extensions.txt files from the source trees
pinned in flake.lock, then parse each example with the pinned cmark-gfm binary
and its XML renderer. Set COMMONMARK_TEST_SUITE, GFM_TEST_SUITE, and CMARK_GFM
to those local inputs when running this script.
"""

from __future__ import annotations

import argparse
import os
import re
import shutil
import subprocess
import sys
import xml.etree.ElementTree as ET
from dataclasses import dataclass
from pathlib import Path
from typing import Any


EXPECTED_COUNTS = {"commonmark": 652, "gfm": 30}
CMARK_VERSION = "0.29.0.gfm.13"
GFM_EXTENSIONS = (
    "table",
    "strikethrough",
    "autolink",
    "tagfilter",
    "tasklist",
    "footnotes",
)
@dataclass(frozen=True)
class Example:
    identifier: str
    markdown: str


@dataclass(frozen=True)
class Symbol:
    name: str


def read_examples(path: Path, prefix: str) -> list[Example]:
    text = path.read_text(encoding="utf-8")
    lines = [line.removesuffix("\r") for line in text.splitlines()]
    examples: list[Example] = []
    index = 0

    while index < len(lines):
        opening = re.match(r"^[ \t]{0,3}(`{3,}|~{3,})[ \t]*(.*)$", lines[index])
        if not opening or not re.match(
            r"(?i)^example(?:[ \t].*)?$", opening.group(2).strip()
        ):
            index += 1
            continue

        marker = opening.group(1)[0]
        minimum_length = len(opening.group(1))
        index += 1
        markdown_lines: list[str] = []
        separator_seen = False
        while index < len(lines):
            line = lines[index]
            closing = re.match(r"^[ \t]{0,3}(`+|~+)[ \t]*$", line)
            if closing and closing.group(1)[0] == marker and len(closing.group(1)) >= minimum_length:
                if not separator_seen:
                    raise ValueError(f"example in {path} has no input/output separator")
                examples.append(
                    Example(
                        f"{prefix}-{len(examples) + 1:04d}",
                        "\n".join(markdown_lines) + "\n",
                    )
                )
                index += 1
                break
            if not separator_seen:
                if line == ".":
                    separator_seen = True
                else:
                    markdown_lines.append(line)
            index += 1
        else:
            raise ValueError(f"unterminated example in {path}")

    expected = EXPECTED_COUNTS[prefix]
    if len(examples) != expected:
        raise ValueError(f"{path}: expected {expected} examples, found {len(examples)}")
    return examples


def source_lines(markdown: str) -> list[str]:
    return markdown.splitlines()


def footnote_label_from_source(lines: list[str], line_number: int, example: Example) -> str:
    if not 1 <= line_number <= len(lines):
        raise ValueError(
            f"{example.identifier}: footnote sourcepos line {line_number} is out of range"
        )

    line = lines[line_number - 1]
    search_from = 0
    while True:
        opener = line.find("[^", search_from)
        if opener < 0:
            break

        label_start = opener + 2
        cursor = label_start
        while cursor < len(line):
            character = line[cursor]
            if character == "\\" and cursor + 1 < len(line):
                cursor += 2
                continue
            if character in "] \t\r\n":
                break
            cursor += 1

        if cursor > label_start and cursor + 1 < len(line) and line[cursor] == "]" and line[cursor + 1] == ":":
            return line[label_start:cursor]
        search_from = opener + 2

    raise ValueError(
        f"{example.identifier}: cmark footnote node at source line {line_number} "
        "has no matching [^label]: opener"
    )


def sanitize_cmark_xml(xml: str) -> str:
    # cmark-gfm 0.29.0.gfm.13 reports footnote nodes as malformed <<unknown> tags.
    return xml.replace("</<unknown>>", "</unknown>").replace("<<unknown>", "<unknown")


def local_name(tag: str) -> str:
    return tag.rsplit("}", 1)[-1]


def sibling_position(index: int, count: int) -> Symbol:
    if count == 1:
        return Symbol(":only")
    if index == 0:
        return Symbol(":first")
    if index == count - 1:
        return Symbol(":last")
    return Symbol(":middle")


def xml_boolean(value: str | None) -> bool | None:
    if value is None:
        return None
    if value == "true":
        return True
    if value == "false":
        return False
    raise ValueError(f"unexpected XML boolean: {value!r}")


def lisp_boolean(value: bool | None) -> str:
    if value is None:
        return "nil"
    return "t" if value else "nil"


def xml_alignment(value: str | None) -> Symbol | None:
    if value is None:
        return None
    if value not in {"left", "center", "right"}:
        raise ValueError(f"unexpected table alignment: {value!r}")
    return Symbol(f":{value}")


def parse_signature(xml: str, example: Example) -> tuple[Any, int]:
    sanitized = sanitize_cmark_xml(xml)
    root = ET.fromstring(sanitized)
    if local_name(root.tag) != "document":
        raise ValueError(f"{example.identifier}: XML root is not document")

    lines = source_lines(example.markdown)
    footnote_count = 0

    def signature(node: ET.Element, placement: Symbol | None = None) -> list[Any]:
        nonlocal footnote_count
        name = local_name(node.tag)
        children = list(node)

        if name == "document":
            return [Symbol(":document"), None, *sequence(children)]
        if name == "block_quote":
            return [Symbol(":block-quote"), None, *sequence(children)]
        if name == "list":
            list_type = node.get("type")
            if list_type == "ordered":
                kind = Symbol(":ordered")
                start = int(node.get("start", "1"))
                delimiter = {"period": Symbol(":period"), "paren": Symbol(":paren")}.get(
                    node.get("delim")
                )
            elif list_type == "bullet":
                kind = Symbol(":unordered")
                start = 1
                delimiter = None
            else:
                raise ValueError(f"{example.identifier}: unknown list type {list_type!r}")
            attributes = [
                Symbol(":kind"),
                kind,
                Symbol(":start"),
                start,
                Symbol(":tight"),
                xml_boolean(node.get("tight")),
                Symbol(":delimiter"),
                delimiter,
            ]
            return [Symbol(":list"), attributes, *sequence(children)]
        if name in {"item", "tasklist"}:
            completed = xml_boolean(node.get("completed"))
            attributes = (
                [Symbol(":task-checked"), completed]
                if name == "tasklist"
                else None
            )
            return [Symbol(":item"), attributes, *sequence(children)]
        if name == "paragraph":
            if placement is None:
                raise ValueError(f"{example.identifier}: paragraph missing sibling position")
            return [Symbol(":paragraph"), [Symbol(":placement"), placement]]
        if name == "heading":
            return [Symbol(":heading"), [Symbol(":level"), int(node.get("level", "0"))]]
        if name == "thematic_break":
            return [Symbol(":thematic-break"), None]
        if name == "code_block":
            return [
                Symbol(":code-block"),
                [Symbol(":info"), node.get("info", ""), Symbol(":literal"), node.text or ""],
            ]
        if name == "html_block":
            return [Symbol(":html-block"), [Symbol(":literal"), node.text or ""]]
        if name == "table":
            rows = children
            if not rows or local_name(rows[0].tag) != "table_header":
                raise ValueError(f"{example.identifier}: table has no XML header row")
            header_cells = list(rows[0])
            alignments = [xml_alignment(cell.get("align")) for cell in header_cells]
            row_cells = [len(list(row)) for row in rows]
            attributes = [
                Symbol(":columns"),
                len(header_cells),
                Symbol(":alignments"),
                alignments,
                Symbol(":row-cells"),
                row_cells,
            ]
            return [Symbol(":table"), attributes, *[signature(row) for row in rows]]
        if name in {"table_header", "table_row"}:
            return [
                Symbol(":row"),
                [Symbol(":header"), name == "table_header"],
                *[signature(cell) for cell in children],
            ]
        if name == "table_cell":
            return [Symbol(":cell"), None]
        if name == "unknown":
            sourcepos = node.get("sourcepos")
            if sourcepos is None:
                raise ValueError(f"{example.identifier}: footnote XML node has no sourcepos")
            match = re.match(r"^(\d+):\d+-\d+:\d+$", sourcepos)
            if not match:
                raise ValueError(
                    f"{example.identifier}: unusable footnote sourcepos {sourcepos!r}"
                )
            label = footnote_label_from_source(lines, int(match.group(1)), example)
            footnote_count += 1
            return [
                Symbol(":footnote-definition"),
                [Symbol(":label"), label],
                *sequence(children),
            ]

        raise ValueError(f"{example.identifier}: unsupported cmark block node {name!r}")

    def sequence(nodes: list[ET.Element]) -> list[Any]:
        count = len(nodes)
        return [
            signature(node, sibling_position(index, count))
            for index, node in enumerate(nodes)
        ]

    result = signature(root)
    if result[0] != Symbol(":document"):
        raise ValueError(f"{example.identifier}: failed to build document signature")
    return result, footnote_count


def run_cmark(cmark: str, example: Example, with_gfm_extensions: bool) -> str:
    command = [cmark, "--to", "xml", "--sourcepos"]
    if with_gfm_extensions:
        for extension in GFM_EXTENSIONS:
            command.extend(("--extension", extension))
    result = subprocess.run(
        command,
        input=example.markdown.encode("utf-8"),
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        check=False,
    )
    if result.returncode != 0:
        raise RuntimeError(
            f"{example.identifier}: {cmark} exited {result.returncode}: "
            f"{result.stderr.decode('utf-8', errors='replace')}"
        )
    return result.stdout.decode("utf-8")


def render_lisp(value: Any) -> str:
    if isinstance(value, Symbol):
        return value.name
    if value is None:
        return "nil"
    if value is True:
        return "t"
    if value is False:
        return "nil"
    if isinstance(value, str):
        escaped = value.replace("\\", "\\\\").replace('"', '\\"')
        escaped = re.sub(
            r"([ \t]+)\n",
            lambda match: match.group(1) + "\\" + "\n",
            escaped,
        )
        return '"' + escaped + '"'
    if isinstance(value, int):
        return str(value)
    if isinstance(value, list):
        return "(" + " ".join(render_lisp(item) for item in value) + ")"
    raise TypeError(f"cannot render Lisp value: {value!r}")


def render_node(node: list[Any], indentation: int) -> str:
    kind, attributes, *children = node
    rendered = f"({render_lisp(kind)} {render_lisp(attributes)}"
    for child in children:
        rendered += "\n" + " " * (indentation + 2) + render_node(child, indentation + 2)
    return rendered + ")"


def render_fixture(entries: list[tuple[str, list[Any]]]) -> str:
    lines = [
        "; Generated by scripts/generate-block-structure-fixtures.py. Do not edit by hand.",
        "(",
    ]
    for identifier, signature in entries:
        lines.append(f"  ({render_lisp(identifier)} {render_node(signature, 2)})")
    lines.append(")")
    return "\n".join(lines) + "\n"


def resolve_cmark(value: str | None) -> str:
    binary = value or shutil.which("cmark-gfm") or shutil.which("cmark")
    if not binary:
        raise ValueError("set CMARK_GFM or pass --cmark to the pinned cmark-gfm binary")
    result = subprocess.run(
        [binary, "--version"], stdout=subprocess.PIPE, stderr=subprocess.STDOUT, check=False
    )
    version = result.stdout.decode("utf-8", errors="replace")
    if result.returncode != 0 or CMARK_VERSION not in version:
        raise ValueError(
            f"{binary} is not pinned cmark-gfm {CMARK_VERSION}; version output was {version.strip()!r}"
        )
    return binary


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
        default=repository / "t/data/block-structure-fixtures.lisp",
        help="fixture destination",
    )
    parser.add_argument("--check", action="store_true", help="fail if the fixture differs")
    args = parser.parse_args(argv)

    try:
        if args.commonmark_spec is None:
            raise ValueError("set COMMONMARK_TEST_SUITE or pass --commonmark-spec")
        if args.gfm_spec is None:
            raise ValueError("set GFM_TEST_SUITE or pass --gfm-spec")
        cmark = resolve_cmark(args.cmark)
        examples = read_examples(args.commonmark_spec, "commonmark")
        examples.extend(read_examples(args.gfm_spec, "gfm"))

        entries: list[tuple[str, list[Any]]] = []
        footnote_nodes = 0
        for example in examples:
            xml = run_cmark(cmark, example, example.identifier.startswith("gfm-"))
            try:
                signature, count = parse_signature(xml, example)
            except ET.ParseError as error:
                raise ValueError(f"{example.identifier}: invalid cmark XML: {error}") from error
            footnote_nodes += count
            entries.append((example.identifier, signature))

        rendered = render_fixture(entries)
        if args.check:
            if not args.output.exists():
                raise ValueError(f"fixture does not exist: {args.output}")
            current = args.output.read_text(encoding="utf-8")
            if current != rendered:
                raise ValueError(f"fixture differs from generated output: {args.output}")
        else:
            args.output.write_text(rendered, encoding="utf-8")

        print(
            f"{len(entries)} examples generated; {footnote_nodes} footnote definition nodes "
            f"with sourcepos labels; output {args.output}"
        )
        return 0
    except (OSError, ET.ParseError, RuntimeError, TypeError, ValueError) as error:
        print(f"generate-block-structure-fixtures.py: {error}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
