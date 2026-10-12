# YAML front matter

The core parser preserves front matter as raw text and a source span. The
optional `cl-markdown-kit/yaml` system parses that text with `cl-yaml-kit`:

```lisp
(cl-markdown-kit/yaml:parse-front-matter document)
```

| Function | Signature | Result |
|---|---|---|
| `parse-front-matter` | `(parse-front-matter front-matter-or-document &rest yaml-parse-options)` | Parsed YAML value, or `nil` if front matter is absent or empty |
| `markdown-yaml-error-yaml-condition` | `(markdown-yaml-error-yaml-condition condition)` | Original `yaml-kit:yaml-parse-error` |
| `markdown-yaml-error-line` | `(markdown-yaml-error-line condition)` | One-based Markdown source line, or `nil` without a source span |
| `markdown-yaml-error-column` | `(markdown-yaml-error-column condition)` | One-based Markdown source column, or `nil` without a source span |

`parse-front-matter` accepts either a `front-matter` node or a `document`. It
returns the value from `yaml-kit:parse`; with the default YAML options, mappings
are equal hash tables and sequences are vectors. When passed a document without
front matter or with empty front matter, it returns `nil`. Additional keyword
arguments are forwarded to `yaml-kit:parse`.

YAML syntax errors are signaled as
`cl-markdown-kit/yaml:markdown-yaml-error`, a subtype of
`cl-markdown-kit:markdown-kit-error`. The condition retains the original
`yaml-kit:yaml-parse-error` in
`markdown-yaml-error-yaml-condition`, and exposes the absolute Markdown source
position through `markdown-yaml-error-line` and
`markdown-yaml-error-column`. Scanner marks are zero-based within the YAML text;
the adapter adds the front matter content's starting line and converts the
column to one-based Markdown coordinates. When the YAML parser has no mark, its
default position is attributed to the first front matter content character. A
manually constructed front-matter node without a span has no Markdown position,
so those readers return `nil`.

Other `cl-yaml-kit` conditions, including YAML resource-limit and composition
errors for non-empty content, pass through unchanged. The empty-stream
composition condition is treated as an empty YAML value. Input decoding and
Markdown parsing remain the responsibility of the core `cl-markdown-kit`
parser, which recognizes an initial BOM and normalizes CRLF line endings while
preserving the front matter text.
