# Markdown writer

`write-markdown` serializes a parsed or constructed document as Markdown text.
`write-markdown-to-stream` writes the same text to a character output stream.
Both functions emit LF line endings and leave the input tree unchanged.

```lisp
(write-markdown (parse-string "# Heading"))
;; => "# Heading"

(with-open-file (stream "document.md" :direction :output
                        :if-exists :supersede)
  (write-markdown-to-stream document stream))
```

The writer emits ATX headings, `-` unordered-list markers, ordered-list
numbers, block quotes, fenced code blocks, GFM tables, task markers, and
footnotes. Code fences are longer than any matching run in the code body. Text
that could start a Markdown block, table, or bare autolink is escaped so it
remains text when parsed again. Links and images use inline destinations, so
their output does not depend on reference-definition placement.

The writer preserves the document's block and inline meaning, not its original
spelling, source spans, delimiter choices, list marker characters, or source
line endings. HTML nodes are emitted as stored. Front matter uses `---`
delimiters. Use `write-markdown` for a string result or
`write-markdown-to-stream` when writing directly to a character stream.

## HTML writer

`write-html` returns rendered HTML. `write-html-to-stream` writes the same
output to a character stream. The `:gfm` option defaults to `t`.

The `:raw-html` option accepts `:omit` (the default), `:escape`, or
`:pass-through`. With `:raw-html :pass-through`, the GFM tagfilter applies when
`:gfm` is true. Set `:gfm nil` to pass raw HTML through without that filter.
The tagfilter is not a sanitizer, and raw HTML passed through with `:gfm nil`
is not sanitized either.
