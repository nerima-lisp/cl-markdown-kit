# cl-markdown-kit

`cl-markdown-kit` is an SBCL-only Common Lisp parser for CommonMark 0.31.2 and
GitHub Flavored Markdown (GFM).

## Features

- Parses CommonMark block and inline syntax, plus GFM tables, task lists,
  strikethrough, extended autolinks, and footnotes.
- Builds an AST whose parsed nodes carry source spans.
- Provides heading and section queries, and extraction of code blocks, links,
  images, tables, task items, headings, and front matter.
- Edits Markdown while preserving source outside the target section or node.
- Writes ASTs as HTML or normalized Markdown.
- Recognizes leading YAML front matter as raw text. The optional
  `cl-markdown-kit/yaml` system parses that text as YAML.

The parser accepts strings, character streams, and octet vectors. Octets are
decoded using Unicode encoding detection. GFM and leading front matter parsing
are enabled by default.

## Conformance

All 652 CommonMark 0.31.2 examples matched, with none excluded. The GFM fixture
has 30 entries: one non-runnable `<IGNORE>` placeholder and 29 runnable
examples, all of which matched.

## Install

From the repository root, make the project and its dependencies discoverable
through ASDF. The core runtime dependencies are `cl-parser-kit` v1.1.1 and
`cl-codec-kit` v0.6.0. The
optional `cl-markdown-kit/yaml` system depends on `cl-yaml-kit` v0.2.1. The test
systems use `cl-weave` v1.4.0; the YAML system and its tests also require
`cl-yaml-kit`.

Check out the two core dependencies under `./sources` at their release tags:

```sh
mkdir -p sources
git clone --branch v1.1.1 https://github.com/nerima-lisp/cl-parser-kit.git sources/cl-parser-kit
git clone --branch v0.6.0 https://github.com/nerima-lisp/cl-codec-kit.git sources/cl-codec-kit
export CL_SOURCE_REGISTRY="(:source-registry
  (:directory \"$PWD\")
  (:tree \"$PWD/sources\")
  :inherit-configuration)"
```

To load `cl-markdown-kit/yaml`, also check out `cl-yaml-kit` v0.2.1 under
`./sources`:

```sh
git clone --branch v0.2.1 https://github.com/nerima-lisp/cl-yaml-kit.git sources/cl-yaml-kit
```

This repository's direct ASDF dependencies are listed for each system in
`cl-markdown-kit.asd`; consult the checked-out `cl-yaml-kit` system definition
for any ASDF dependencies it requires.

From the repository root, start an SBCL REPL:

```sh
sbcl
```

Load the core ASDF system at the REPL:

```lisp
(require :asdf)
(asdf:load-system "cl-markdown-kit")
```

## Use

After loading the system, parse a document and inspect its first heading:

```lisp
(let* ((document (cl-markdown-kit:parse "# Hello"))
       (heading (first (cl-markdown-kit:document-children document))))
  (list (cl-markdown-kit:heading-p heading)
        (cl-markdown-kit:heading-level heading)))
;; => (T 1)
```

Render the document as HTML:

```lisp
(write-string
 (cl-markdown-kit:write-html
  (cl-markdown-kit:parse "# Hello")))
;; => <h1>Hello</h1>
```

Find headings in the `/Install` section address, including the selected heading;
see the [section address rules](docs/src/reference/query.md#resolving-sections):

```lisp
(let* ((document (cl-markdown-kit:parse
                  (format nil "# Install~%~%## Nix~%")))
       (headings (cl-markdown-kit:extract document :heading
                                          :section "/Install")))
  (mapcar (lambda (result) (getf result :plain-text)) headings))
;; => ("Install" "Nix")
```

Replace a section body and write the updated Markdown:

```lisp
(write-string
 (cl-markdown-kit:replace-section-body
  (cl-markdown-kit:parse (format nil "# Intro~%Old text."))
  "/Intro" "New text."))
;; => # Intro
;;    New text.
```

## HTML safety

`write-html` omits raw HTML by default and does not write dangerous URL
schemes to link or image attributes by default. `:unsafe-urls t` disables the
URL scheme filter. With `:raw-html :pass-through` and `:gfm t` (the default),
the GFM tagfilter escapes the `<` before these tags: `title`, `textarea`,
`style`, `xmp`, `iframe`, `noembed`, `noframes`, `script`, and `plaintext`.
The tagfilter is not a sanitizer. With `:raw-html :pass-through :gfm nil`, raw
HTML passes through without the filter. Use `:raw-html :escape` to render raw
HTML as text.

## Resource limits

`parse` defaults to a maximum nesting depth of 256 and an inline delimiter
limit of 4096. The inline limit counts unresolved delimiter and bracket records
per inline fragment, such as a paragraph or table cell, including unmatched
emphasis openers. The count resets for each fragment. The
`:max-inline-delimiter-depth` option sets this record-count limit, not a nesting
level. Exceeding either limit signals its dedicated condition,
`markdown-nesting-depth-exceeded` or
`markdown-inline-delimiter-depth-exceeded`. Set `:max-nesting-depth` or
`:max-inline-delimiter-depth` on `parse` to change either limit.

## Documentation

- [AST and source locations](docs/src/reference/nodes.md)
- [Query API](docs/src/reference/query.md)
- [Edit API](docs/src/reference/edit.md)
- [HTML and Markdown writers](docs/src/reference/markdown-writer.md)
- [YAML front matter](docs/src/reference/yaml.md)

## Development

Run the test suite on `x86_64-linux` with Nix. The test app supplies pinned
CommonMark and GFM fixtures and runs both the core and YAML test systems through
[`run-tests.lisp`](run-tests.lisp):

```sh
nix run .#test
```

## Character data

The HTML5 named character reference table in `src/html-entities-data.lisp` is
generated from the `entities.json` snapshot in the `w3c/html` repository. This
snapshot contains the WHATWG HTML Standard's named character reference data.
The vendored source snapshot is the immutable
[`entities.json`](https://raw.githubusercontent.com/w3c/html/2ef7a579bcfd2693b98df35f20e51dff60e3fbef/entities.json)
file at W3C archive commit
`2ef7a579bcfd2693b98df35f20e51dff60e3fbef`; its SHA-256 is
`3d029331b82668ac319bc81802de45b24396df76816d9ba6cf8807c0a1e59a29`.
The vendored snapshot is the unmodified response body from an HTTP GET of that
commit-pinned URL. Retrieve it with:

```sh
curl --fail --location --output data/whatwg-entities.json \
  https://raw.githubusercontent.com/w3c/html/2ef7a579bcfd2693b98df35f20e51dff60e3fbef/entities.json
```

The flake pins the snapshot hash, and the generator needs no network access.
Only names ending in a semicolon are stored, matching CommonMark's
named-reference rule.

The entity data is attributed as Copyright © WHATWG.
The HTML Standard is licensed under [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/);
the license states that portions incorporated into source code are licensed
under BSD 3-Clause. The generated table preserves all named values, including
multi-codepoint sequences.

On `x86_64-linux`, regenerate the Lisp table in the Nix development shell with
`nix develop --command sbcl --script scripts/generate-html5-entities.lisp`.
The flake check runs the generator in `--check` mode and compares its output
with the committed table.

## License

MIT. See [`LICENSE`](LICENSE).
