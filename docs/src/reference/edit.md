# Edit API

The edit functions take a parsed `document` and return an edited source string.
They leave the document and its nodes unchanged.

```lisp
(replace-section-body document address markdown)
(replace-section document address markdown)
(delete-section document address)
(insert-section document address markdown &key (position :after))
(replace-node document node markdown)
(set-task-checked document task-item checked-p)
```

`address` follows the section address rules used by `find-section`. An empty
address selects the document content. `replace-section-body` keeps the selected
heading; `replace-section` replaces its heading and body; `delete-section`
removes it. For the empty address, these operations act on all document content
while keeping a leading BOM and the original final-newline state.

`insert-section` accepts `:before`, `:after`, or `:last-child`. The last position
adds the Markdown after the selected heading's final direct child section, or at
the end of its body if it has no child sections. At the empty address it appends
to the document for `:after` and `:last-child`, and inserts after a leading BOM
for `:before`.

`replace-node` accepts a source-backed block node from the document, including
nodes nested in block quotes and lists. Its Markdown is relative to that node's
container. The edit layer adds the node's continuation prefix to each later
replacement line. A node replacement leaves its final line ending outside the
edit interval.

`set-task-checked` changes only the task marker. Turning a task on writes
`[x]`; turning it off writes `[ ]`. Asking for the existing state returns the
original source, preserving the marker's spelling.

```lisp
(let* ((document (cl-markdown-kit:parse-string source))
       (updated (cl-markdown-kit:replace-section-body
                 document "/Install/Nix" "Install with `nix develop`.")))
  (write-string updated))
```

## Source preservation

Replacement newlines use the first line-ending sequence in the edited range.
When that range has none, the nearest preceding sequence is used, then the
nearest following sequence, and finally LF. CRLF and CR in replacement Markdown
are normalized to that sequence. Source outside the edit interval, BOM presence,
and the document's final-newline state are preserved. An edit that reaches EOF
leaves exactly one final line ending when the input had one, and none otherwise.

After each edit, the source is parsed again with the document's original
nesting and delimiter limits. The edit is rejected with
`markdown-structure-changed-error` if a non-target block's source interval,
kind, semantic text, or semantic attributes change. The condition exposes the
edit target and the first difference; it does not contain candidate source.
