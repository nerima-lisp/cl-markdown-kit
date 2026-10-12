# Query API

The query API reads headings, resolves document sections, and extracts selected
node kinds from a parsed document. Results include their source range; most
also include the AST node, while front matter returns only raw text and range.

## Headings

`document-headings` returns heading records in source order, including headings
nested inside block quotes, lists, and footnotes. Each record provides:

| Reader | Value |
|---|---|
| `heading-record-node` | The AST heading node |
| `heading-record-level` | Heading level from 1 through 6 |
| `heading-record-plain-text` | Inline content rendered as plain text |
| `heading-record-source-form` | `:atx` or `:setext` |
| `heading-record-section-span` | The section span, or `nil` for a nested heading |

Plain text includes text and code span literals, link display text, image alt
text, and footnote labels. Emphasis, strong, and strikethrough markers and raw
HTML tags are omitted. Soft and hard breaks become spaces; whitespace is
collapsed and trimmed.

```lisp
(let* ((document (parse-string (format nil "# Setup~%~%## Nix~%")))
       (headings (document-headings document)))
  (mapcar #'heading-record-plain-text headings))
```

## Resolving sections

`find-section` accepts an RFC 6901 address or a 1-based line number for a
document-level heading. Address components match heading plain text exactly.
Use `~0` for `~` and `~1` for `/`; any other tilde escape is invalid. The empty
address `""` returns the whole document. Only document-level headings define
sections; nested headings remain available through `document-headings` and
heading extraction.

The returned `section-record` provides `section-record-heading`,
`section-record-span`, and `section-record-address`. The heading is a
`heading-record`, except for the whole document where it is `nil`. The span is
a `cl-parser-kit:span` from the selected heading through the content before the
next document-level heading of the same or a higher level, or through document
end. The whole-document record has an empty address and spans the document.

```lisp
(let* ((document (parse-string (format nil "# Setup~%~%## Nix~%")))
       (section (find-section document "/Setup/Nix")))
  (section-record-span section))
```

| Condition | Exported readers |
|---|---|
| `markdown-no-match-error` | `markdown-no-match-error-address` |
| `markdown-ambiguous-match-error` | `markdown-ambiguous-match-error-address`, `markdown-ambiguous-match-error-candidates` |
| `markdown-invalid-section-address-error` | `markdown-invalid-section-address-error-address`, `markdown-invalid-section-address-error-reason` |

No match signals `markdown-no-match-error`. A duplicate sibling match signals
`markdown-ambiguous-match-error`; its candidates include a heading record and
1-based line number. Malformed addresses signal
`markdown-invalid-section-address-error`.

## Extracting nodes

```lisp
(extract document kind &key section info-string language include-source-slice)
```

`extract` returns result plists in source order. `section` may be `nil`, an
address or line number accepted by `find-section`, or a section record. A
section includes its heading and body. The available kinds and result fields
are:

| Kind | Fields |
|---|---|
| `:code-block` | `:node`, `:span`, `:body`, `:info-string` |
| `:link` | `:node`, `:span`, `:destination`, `:title`, `:display-text` (includes autolinks) |
| `:image` | `:node`, `:span`, `:destination`, `:title`, `:alt-text` |
| `:table` | `:node`, `:span`, `:header`, `:rows`, `:alignments` |
| `:task-item` | `:node`, `:span`, `:checked-p` |
| `:heading` | `:node`, `:span`, `:level`, `:plain-text`, `:source-form`, `:section-span` |
| `:front-matter` | `:raw-text`, `:span` |

Code blocks can be filtered by their complete info string with `:info-string`
and by the first token with `:language`. Every supplied filter must match.
Front matter is returned as raw text and a source range; it is not interpreted
as YAML. With `:include-source-slice t`, each result also has `:source-slice`,
the exact source substring covered by its span.

```lisp
(extract document :code-block
         :section "/Setup"
         :language "nix"
         :include-source-slice t)
```
