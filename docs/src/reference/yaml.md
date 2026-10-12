# YAML front matter

The core parser preserves front matter as raw text and a source span. The
optional `cl-markdown-kit/yaml` system parses that text with `cl-yaml-kit`:

```lisp
(cl-markdown-kit/yaml:parse-front-matter document)
```

| Function | Signature | Result |
|---|---|---|
| `parse-front-matter` | `(parse-front-matter front-matter-or-document &rest yaml-parse-options)` | Parsed YAML value, or `nil` if front matter is absent or empty |
| `markdown-yaml-error-yaml-condition` | `(markdown-yaml-error-yaml-condition condition)` | Original YAML parse or composition condition |
| `markdown-yaml-error-line` | `(markdown-yaml-error-line condition)` | One-based Markdown source line, or `nil` without a source span |
| `markdown-yaml-error-column` | `(markdown-yaml-error-column condition)` | One-based Markdown source column, or `nil` without a source span |

`parse-front-matter` accepts either a `front-matter` node or a `document`. It
returns the value from `yaml-kit:parse`; with the default YAML options, mappings
are equal hash tables and sequences are vectors. When passed a document without
front matter or with empty front matter, it returns `nil`. Additional keyword
arguments are forwarded to `yaml-kit:parse`. Empty front matter contains only
blank lines (spaces or tabs) and comment-only lines. A comment-only line may
have leading spaces or tabs before `#`; a line containing YAML content, such as
`a: b#c`, is non-empty.

YAML syntax and composition errors are signaled as
`cl-markdown-kit/yaml:markdown-yaml-error`, a subtype of
`cl-markdown-kit:markdown-kit-error`. The condition retains the original
`yaml-kit:yaml-parse-error` or `yaml-kit:yaml-compose-error` in
`markdown-yaml-error-yaml-condition`, and exposes the absolute Markdown source
position through `markdown-yaml-error-line` and
`markdown-yaml-error-column`. YAML marks are zero-based within the YAML text;
the adapter adds the front matter content's starting line and converts the
column to one-based Markdown coordinates. When the YAML parser has no mark, its
default position is attributed to the first front matter content character. A
manually constructed front-matter node without a span has no Markdown position,
so those readers return `nil`.

YAML resource-limit conditions pass through unchanged. Input decoding and
Markdown parsing remain the responsibility of the core `cl-markdown-kit`
parser, which recognizes an initial BOM and normalizes CRLF line endings while
preserving the front matter text.
