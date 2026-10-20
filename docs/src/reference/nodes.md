# AST and source locations

`parse` returns a `document` whose children are block nodes. Inline parsing fills
the inline children of paragraphs, headings, and table cells before returning.
Nodes are mutable while they are built and should be treated as immutable after
parsing.

Every node inherits `node-span`. Parsed spans are `cl-parser-kit:span` values
over the document's decoded source string. Offsets are half-open character
offsets; offsets are zero-based, while line and column values are one-based. A
tab counts as one source column. The document span includes an initial BOM;
ordinary parsing boundaries begin after that BOM at line 1, column 1.

Every node has a keyword constructor named `make-<type>` and an exported
predicate named `<type>-p`, such as `document-p`, `heading-p`, and `text-p`.
Constructors accept keyword arguments. The field tables use the
`<type>-<field>` reader naming convention; inherited fields keep the reader
from their defining type, such as `list-item-children` on a `task-list-item`.
The common fields are:

| Type | Constructor | Fields |
|---|---|---|
| `node` | `make-node` | `span` |
| `block-node` | `make-block-node` | inherited `span`; `continuation-prefix` |
| `inline-node` | `make-inline-node` | inherited `span` |

Block nodes inherit `block-node-continuation-prefix`. For a directly
constructed node, `span` may be `nil`. Parsed nodes carry spans, including
inline nodes.

## Block nodes

| Type | Constructor | Fields |
|---|---|---|
| `document` | `make-document` | `children`, `source`, `source-format`, `reference-index`, `front-matter-p`, `gfm-p`, `max-nesting-depth`, `max-inline-delimiter-depth` |
| `front-matter` | `make-front-matter` | `raw-text`, `opening-delimiter`, `closing-delimiter` |
| `block-quote` | `make-block-quote` | `children` |
| `list-node` | `make-list-node` | `kind` (`:ordered` or `:unordered`), `tight-p`, `start-number`, `marker-character`, `delimiter-character`, `items` |
| `list-item` | `make-list-item` | `children` |
| `task-list-item` | `make-task-list-item` | inherited list-item children; `checked-p`, `task-marker-span`, `original-marker-character` |
| `paragraph` | `make-paragraph` | `children` |
| `heading` | `make-heading` | `level`, `style` (`:atx` or `:setext`), `marker-character`, `opening-marker-length`, `closing-marker-length`, `children` |
| `thematic-break` | `make-thematic-break` | `marker-character`, `marker-count` |
| `code-block` | `make-code-block` | `style` (`:fenced` or `:indented`), `body`, `fence-character`, `fence-length`, `indentation`, `info-string` |
| `html-block` | `make-html-block` | `body`, `kind` |
| `link-reference-definition` | `make-link-reference-definition` | `raw-label`, `normalized-label`, `destination`, `title`, `eligible-p` |
| `table` | `make-table` | `alignments`, `rows` |
| `table-row` | `make-table-row` | `header-row-p`, `cells` |
| `table-cell` | `make-table-cell` | `alignment`, inline `children` |
| `footnote-definition` | `make-footnote-definition` | `raw-label`, `normalized-label`, `children` |

`list-node` represents the Markdown list node. The name avoids shadowing
Common Lisp's `list` function. `task-list-item` is a subtype of `list-item`, so
it shares the `list-item-children` reader. Its `task-marker-span` covers exactly
the three-character task marker.

## Inline nodes

| Type | Constructor | Fields |
|---|---|---|
| `text` | `make-text` | `value` |
| `code-span` | `make-code-span` | `literal-text`, `backtick-run-length` |
| `html-inline` | `make-html-inline` | `body` |
| `soft-break` | `make-soft-break` | none |
| `hard-break` | `make-hard-break` | none |
| `emphasis` | `make-emphasis` | `delimiter-character`, `children` |
| `strong` | `make-strong` | `delimiter-character`, `children` |
| `link` | `make-link` | `destination`, `title`, `reference-kind`, `reference-label`, `children` |
| `image` | `make-image` | `destination`, `title`, `reference-kind`, `reference-label`, inline alt-text `children` |
| `strikethrough` | `make-strikethrough` | `delimiter-character`, `children` |
| `autolink` | `make-autolink` | `destination`, `display-text`, `kind` |
| `footnote-reference` | `make-footnote-reference` | `raw-label`, `normalized-label` |

Reference kinds are `:inline`, `:full`, `:collapsed`, and `:shortcut`.
Autolink kinds are `:angle-uri`, `:angle-email`, `:bare-url`, and
`:bare-email`. Table alignments are `:left`, `:center`, `:right`, or `nil`.

## Walking nodes

Container readers return child lists in source order. `node-children` provides
the uniform child-list interface and returns `nil` for leaves. The exported
`walk-node-tree` takes a root node and a callback:

```lisp
(walk-node-tree root callback)
```

The callback receives a frame in `:enter` and `:leave` phases.
During `:enter`, return `nil` or `:descend` to visit children, `:skip` to omit
children while still receiving `:leave`, or `:stop` to end traversal
immediately. A `:leave` return value does not change traversal. Children are
visited in source order, and `walk-node-tree` returns the root node.

| Frame reader | Value |
|---|---|
| `node-tree-frame-node` | Current node |
| `node-tree-frame-phase` | `:enter` or `:leave` |
| `node-tree-frame-parent` | Parent node, or `nil` for the root |
| `node-tree-frame-grandparent` | Grandparent node, or `nil` when absent |
| `node-tree-frame-previous-sibling` | Previous sibling, or `nil` when absent |
| `node-tree-frame-next-sibling` | Next sibling, or `nil` when absent |
| `node-tree-frame-remaining-siblings` | Siblings after the current node |
| `node-tree-frame-child-index` | Zero-based index; the root has index `0` |
| `node-tree-frame-last-child-p` | Whether this is the last sibling; true for the root |
| `node-tree-frame-result` | Value stored by the callback on this frame |
| `node-tree-frame-child-results` | Child frame results in source order, available during `:leave` |

Callbacks can store a value with `(setf (node-tree-frame-result frame) value)`.
The parent frame receives each visited child's result in its
`node-tree-frame-child-results` list.

## Node metadata

| Function | Result |
|---|---|
| `node-kind` | Declared keyword kind; signals an error for abstract base nodes |
| `node-family` | `:block` or `:inline` for concrete nodes, `:abstract` for base nodes |
| `node-semantic-attributes` | Plist of declared keyword attributes and their current values |
| `node-edit-text-projection` | Slot value for a slot projection, `""` for `:empty`, or `nil` for `:container` |

## Source buffers and boundaries

`make-source-buffer` accepts a string, character input stream, or octet vector.
It snapshots the decoded source, records its source format, normalizes CR, LF,
and CRLF line endings to LF for parsing, and internally builds a boundary map.
An initial BOM stays in the source snapshot and is omitted from normalized parser text.
Thus normalized boundary zero maps to source offset 1, line 1, column 1 when a
BOM is present.

| Function | Signature | Result |
|---|---|---|
| `make-source-buffer` | `(make-source-buffer source)` | A source buffer over the snapshot |
| `source-buffer-source` | `(source-buffer-source buffer)` | Exact decoded source string, including BOM |
| `source-buffer-normalized-text` | `(source-buffer-normalized-text buffer)` | Parser-facing text with LF line endings |
| `source-buffer-format` | `(source-buffer-format buffer)` | A `source-format` record |
| `source-buffer-span` | `(source-buffer-span buffer start end)` | A span for normalized `[start,end)` boundaries |
| `source-buffer-document-span` | `(source-buffer-document-span buffer)` | A span covering source offsets `[0,length)` |
| `make-source-format` | `(make-source-format &key input-kind encoding bom-p newline-style newline-sequence)` | Source encoding and newline metadata |
| `source-buffer-p` | `(source-buffer-p object)` | Whether the object is a source buffer |
| `source-format-p` | `(source-format-p object)` | Whether the object is source-format metadata |

The source-buffer boundary map is internal; it maps normalized character
boundaries to the decoded source. Fragment maps may contain discontinuities
where a block parser removes container prefixes; a line-break span can
therefore include the physical line ending and a removed prefix. The exported
`inline-fragment-boundary-map` reader exposes a fragment map, but its
representation and boundary records are opaque and have no public
representation contract. `source-buffer-span` accepts boundaries in the
normalized text and returns a `cl-parser-kit:span` over the original decoded
source.

`source-format` records `input-kind`, `encoding`, `bom-p`, `newline-style`,
and `newline-sequence`. Its readers are `source-format-input-kind`,
`source-format-encoding`, `source-format-bom-p`,
`source-format-newline-style`, and `source-format-newline-sequence`. The style
is `:lf`, `:crlf`, `:cr`, `:mixed`, or `:none`; the sequence preserves each
observed ending in order. Octet input retains the Unicode encoding selected by
`cl-codec-kit`.

The public parser entry points are:

```lisp
(parse source &key front-matter gfm max-nesting-depth max-inline-delimiter-depth)
(parse-string string &key front-matter gfm max-nesting-depth max-inline-delimiter-depth)
(parse-stream character-stream &key front-matter gfm max-nesting-depth max-inline-delimiter-depth)
(parse-octets octet-vector &key front-matter gfm max-nesting-depth max-inline-delimiter-depth)
```

`parse-blocks` exposes the block-parsing stage used by the parser facade:

```lisp
(parse-blocks source-buffer &key (front-matter t) (gfm t) (max-nesting-depth 256))
```

It returns a `block-parse-result`. The result contains the block `document`,
the reference index, and inline fragments whose contents have not yet been
parsed. `block-parse-result` has predicate `block-parse-result-p`, constructor
`make-block-parse-result` with `:document`, `:inline-fragments`, and
`:reference-index` keywords, and readers `block-parse-result-document`,
`block-parse-result-inline-fragments`, and
`block-parse-result-reference-index`.

Each `inline-fragment` associates a block node with its deferred inline text
and boundary map. Its predicate is `inline-fragment-p`; its constructor is
`make-inline-fragment` with `:node`, `:text`, and `:boundary-map` keywords; and
its readers are `inline-fragment-node`, `inline-fragment-text`, and
`inline-fragment-boundary-map`. The text and map provide the input needed to
parse that node's inline content while retaining source boundaries. The normal
`parse` entry points complete this inline stage before returning a document.

`:front-matter` defaults to `t`. When true, a leading `---` line and a later
closing `---` line are read as a front-matter node, after an optional byte
order mark. Set it to `nil` to parse those lines as ordinary Markdown blocks.
The parser retains front-matter text without interpreting it as YAML.

`:gfm` defaults to `t`, enabling GFM block and inline extensions. Set it to
`nil` to parse CommonMark without GFM extensions.

`make-document` accepts `:front-matter-p` and `:gfm-p` keywords. A parsed
document exposes these settings through `document-front-matter-p` and
`document-gfm-p`, respectively; each reader returns `t` when its option is
enabled and `nil` when disabled. Source-preserving edit operations reuse these
values when reparsing edited source.

Limits must be positive integers. Defaults are 256 open block-container
levels and 4096 unresolved inline delimiter or bracket records in a fragment.
