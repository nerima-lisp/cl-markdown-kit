# Architecture and Public Contract

This document fixes the AST, source-span, parser-boundary, and public API
contracts for `cl-markdown-kit` v0.1.0. The parser and writer implementations
are owned by their respective streams. The versioned functional requirements
and acceptance tests are in the run specification; decisions D-1 through D-6
and its package Addendum are authoritative.

## nerima-lisp package policy

The core system uses the following pinned packages and layers:

| Package | Tag | Use |
|---|---|---|
| `cl-parser-kit` | v1.1.1 | Public source locations use `cl-parser-kit:span`; this kit defines no local position or range types. |
| `cl-codec-kit` | v0.6.0 | `parse-octets` detects BOM/encoding and decodes octet-vector input before source scanning. |

`cl-regex-kit` v2.2.0 is a permitted runtime dependency for GFM extended
autolink, HTML block start conditions, tagfilter, or URL scheme checks when a
pattern is clearer than a scanner. Its engine is linear-time; use a
hand-written scanner when simpler.

Optional and test systems remain separate from the core parser system:

| Package | Tag | Use |
|---|---|---|
| `cl-yaml-kit` | v0.1.0 | Optional `cl-markdown-kit/yaml:parse-front-matter` API; the core keeps front matter as raw text and a span. |
| `cl-json-kit` | v1.2.0 | Test fixtures and optional AST-to-JSON export if later aitools integration needs it. |
| `cl-weave` | v1.4.0 | Tests. |

Pin package inputs by release tag in `flake.nix`; give each ASDF
`:depends-on` entry a one-line reason. Nix packaging uses
`cl-nix-forge`'s `mkPackageFlake` at v0.6.1. SBCL's `sb-unicode` supplies the
case folding and Unicode character classes required by D-3; it is part of the
SBCL-only runtime policy, not a separate nerima-lisp package dependency.

## Design decisions

### Representation

Use `defstruct` with a small included-structure hierarchy. A `node` owns a
`cl-parser-kit:span`; `block-node` and `inline-node` include it. Container structures
own an ordered list of children. Lists match the sibling kit's public AST
convention and let parsers build children incrementally without exposing a
mutable vector. Leaf nodes own semantic values and any syntax attributes needed
by the normalized Markdown writer. Nodes are mutable during construction, then
treated as immutable after `parse` returns. Public constructors accept keyword
arguments; `node-children` returns the ordered child list, and `walk-nodes`
visits pre-order, depth-first, including the root.

Every parsed node has a source span, including inline nodes beyond the subset
required by FR-004. A node made directly with a public constructor may have a
`nil` span. This makes source lookup consistent without requiring a parallel
position tree. Children lists are homogeneous by contract: block containers
contain block nodes, inline containers contain inline nodes, and table rows
contain table cells.

Each parsed block node also has a `continuation-prefix`: the exact source text
that its enclosing block containers require before continuation lines. It is
empty for document children. The prefix is captured from the node's opening
line after removing the node's own syntax marker; for example it is `> ` for a
paragraph inside a block quote and the list item's content indentation for a
paragraph inside a list item. This field is source context for edits, not
semantic Markdown content.

### Node inventory

The inventory below names semantic structure, not a one-to-one encoding of
every delimiter token. Escapes and character references become text values;
their exact spelling remains available through the document source and spans.

| Family | Node kinds and required slots |
|---|---|
| Document and source | `document`: block children, original source snapshot, source-format metadata, and link-reference index. `front-matter`: exact original inner text and opening/closing delimiter values. With the public `:front-matter` option enabled (the default), front matter is recognized only when an opening `---` line and a later closing `---` line occur at document start after an optional BOM; an unclosed opener remains ordinary Markdown. Set `:front-matter nil` to leave all leading delimiters to Markdown block parsing. The core does not interpret YAML. |
| CommonMark containers | `block-quote`: block children. `list`: ordered/unordered kind, tight/loose flag, ordered start number, marker character, ordered-list delimiter character, and item children. `list-item`: block children. `task-list-item`: list-item subtype with `checked-p`, `task-marker-span` (a `cl-parser-kit:span`) covering exactly the recognized three-character `[ ]`, `[x]`, or `[X]` marker, and the original marker character; its distinct type means unchecked tasks are distinguishable from ordinary items. |
| CommonMark leaf blocks | `paragraph`: inline children. `heading`: level, `:atx` or `:setext` style, ATX marker character and opening/closing marker lengths when applicable, and inline children. `thematic-break`: marker character and marker count. `code-block`: `:fenced` or `:indented` style, literal body, fence character/length, indentation, and info string. `html-block`: literal body and CommonMark HTML block kind. `link-reference-definition`: raw and normalized labels, destination, optional title, and first-definition eligibility. |
| GFM blocks | `table`: column alignments and row children. `table-row`: header-row flag and cell children. `table-cell`: alignment and inline children. `footnote-definition`: raw and normalized label plus block children. |
| CommonMark inline leaves | `text`: decoded text. `code-span`: literal text and source backtick-run length. `html-inline`: literal HTML. `soft-break` and `hard-break`: distinct kinds. |
| CommonMark inline containers | `emphasis` and `strong`: inline children and delimiter character. `link` and `image`: destination, optional title, reference kind (`:inline`, `:full`, `:collapsed`, or `:shortcut`), source reference label when used, and inline children (alt text for images). |
| GFM and autolink inline | `strikethrough`: inline children and delimiter character. `autolink`: destination, display text, and kind (`:angle-uri`, `:angle-email`, `:bare-url`, or `:bare-email`). |
| GFM footnotes | `footnote-reference`: raw and normalized label. The definition remains a block node; HTML numbering is assigned by the writer in document order. |

Table alignments are `:left`, `:center`, `:right`, or `nil`. A task item has
`checked-p` equal to `t` or `nil`; its node type supplies the separate
task/non-task distinction. `task-marker-span` is discovered while parsing
the task-list-item grammar at the item's content start, never by searching its
body for text that resembles `[ ]`. `set-task-checked` edits only this span;
when turning an unchecked task on it writes lowercase `x`, and when turning a
checked task off it writes a space. A link's resolved destination and title
are stored even when its source used a reference. The document's reference index
maps a normalized label to the first valid definition, as required by CommonMark.
CommonMark reference labels are normalized by `normalize-link-label` from
`character-data.lisp`: leading/trailing whitespace is removed, internal
whitespace is collapsed, and case is folded with SBCL's `sb-unicode`. The raw
spelling stays on each definition and reference node. Footnote labels use the
same normalizer. All
definition nodes remain in source order, including duplicates, so the Markdown
writer can preserve their syntax as normalized Markdown.

The structure inventory keeps the attributes that normalization cannot infer
from semantic text alone: heading form, code fence form, list marker and
delimiter, link reference form/label, table alignment, and raw HTML/code text.
Source-preserving edits use the original source snapshot and spans outside the
target; they do not attempt to reconstruct that text from nodes.

## Source spans and input

Every public source location is a `cl-parser-kit:span` (v1.1.1). The kit does
not define local `source-position` or `source-range` types. A span retains its
source string and uses half-open character offsets `[start,end)`; offsets are
zero-based indices into the decoded Common Lisp character sequence, not byte
offsets. Line and column are 1-based. A tab advances one column. A CRLF pair
advances the raw offset by two characters and the line by one; CR and LF each
advance the line by one. The column resets to 1 after a line ending. The offset
includes a leading U+FEFF BOM, while line/column coordinates treat that BOM as
an invisible document prefix: the first visible source character is at line 1,
column 1, and offset 1 when a BOM is present. The document span begins at
offset 0 so it contains the BOM and every child span.

The input layer retains the exact decoded source string, detects BOM presence,
and records the line-ending style (`:lf`, `:crlf`, `:cr`, `:mixed`, or `:none`)
plus the per-line-ending sequence. For octet-vector input, `cl-codec-kit`
detects the BOM/encoding and decodes before this scan. The source layer
reinserts a detected BOM character into the original-character snapshot and
records encoding/BOM metadata, then scans the decoded characters and
creates a logical LF-normalized view for parsing. Its boundary map has one
entry per normalized character boundary and maps boundaries back to original
character offsets and 1-based coordinates. It is a per-character map, not an
arithmetic base offset: after a logical LF, the next boundary maps to the next
retained character, even when block parsing removed list/quote indentation.
The LF's source span covers the physical line ending and removed prefix
between the retained characters. A block parser must not derive offsets by
adding an index to the fragment start.

For character streams, the parser reads characters rather than using
`read-line`, so CR, LF, and CRLF received from the stream remain observable.
The contract is over the decoded characters delivered by that stream.
`parse-octets` adds octet-vector input and uses `cl-codec-kit` for BOM/encoding
detection and decoding. It reinserts any detected BOM character into the
source snapshot and retains encoding/BOM metadata. Source offsets remain
character offsets after decode.

Block spans begin at the first syntax character owned by the block and end
immediately after its last syntax/content character. They exclude the line
ending after that final line and exclude following blank separator lines.
Internal line endings and blank lines that belong to a container remain
inside its span. A container span includes its own markers/prefixes and
contains all child spans. The document span covers the complete original
input. Inline spans include their complete source spelling: for example,
link/image delimiters and destination/title, code-span backticks, autolink
angle brackets when present, and footnote-reference brackets. The end is
exclusive in offsets and denotes the position immediately after the final
source character. This ownership rule lets edits splice a node's span while
leaving the following line ending and all separator text character-for-character
intact. Parser indentation calculations use a separate CommonMark visual
column counter with four-column tab stops; public source columns remain
character counts. A U+FEFF anywhere except the initial BOM is ordinary source
text.

An AST returned by `parse` retains its decoded source snapshot for FR-020. A
document constructed by a caller has no source snapshot and cannot be passed
to a source-preserving edit operation. Normalized Markdown output uses LF and
does not include a BOM; edit operations use the source's recorded line-ending
metadata for replacement text and retain the original BOM and trailing
newline through source splicing. Edit APIs return a source string. For string
and character-stream inputs, outside-target text is preserved
character-for-character; byte identity is defined using the canonical UTF-8
representation of that text. For octet input, source metadata retains the
explicit encoding selected by `cl-codec-kit:detect-unicode-encoding`; a
detected BOM is restored as U+FEFF in the source snapshot. Edits preserve
decoded characters and line endings outside the target. Callers materialize
the returned string with `cl-codec-kit:string-to-octets` and the retained
encoding. Since decoding is strict and accepts only well-formed Unicode
sequences, encoding unchanged characters with that encoding and original BOM
state reproduces their original octets. The edit API returns a string, not an
octet vector. FR-020/T-005 byte preservation therefore applies to the
canonical UTF-8 representation for character input and to the retained Unicode
encoding for octet input.

## Parser and writer boundaries

### Source and block parsing

`source.lisp` owns source normalization, source metadata, and the
logical-to-original boundary map; it creates `cl-parser-kit:span` values and
defines no local position/range types. The exported block parser accepts a
source buffer through `(parse-blocks source-buffer &key (front-matter t) (gfm t)
(max-nesting-depth 256))` and returns a `block-parse-result` containing the
partial document, ordered `inline-fragment` records, and normalized-label
reference index. The exported result type has predicate
`block-parse-result-p`, constructor `make-block-parse-result`, and readers
`block-parse-result-document`,
`block-parse-result-inline-fragments`, and
`block-parse-result-reference-index`. Each fragment identifies its owning
paragraph, heading, or table cell, and contains logical text plus the boundary
map for that text. The exported fragment type has predicate
`inline-fragment-p`, constructor `make-inline-fragment`, and readers
`inline-fragment-node`, `inline-fragment-text`, and
`inline-fragment-boundary-map`.
Block parsing also places all link-reference-definition nodes in source order
on the partial document. It does not interpret inline emphasis, links, code
spans, or inline HTML.

The inline parser entry point is:

```lisp
(parse-inline text boundary-map reference-index
              &key max-inline-delimiter-depth)
```

`text` is the leaf's logical text with LF line breaks. `boundary-map` maps each
of its `length(text) + 1` boundaries to original source offsets and 1-based
coordinates; removed
container prefixes are represented by discontinuities between adjacent
boundaries. `reference-index` is the block pass's first-definition-wins index.
The result is an ordered list of inline nodes, each with an original-source
span. The function does not perform block parsing or fetch definitions
itself. The parse facade fills the owning node's inline children with this
result and returns only after every fragment has been consumed.

`make-source-buffer` accepts a string, character stream, or octet vector and
returns the normalized source buffer with decoded original text, source
metadata, and coordinate map. Octet vectors are decoded through
`cl-codec-kit`. This boundary is independently testable without invoking
either parser.

The public `parse` facade normalizes input, calls the block parser, parses
each deferred inline fragment, and returns the completed document. Its public
entry points are:

```lisp
(parse source &key front-matter gfm max-nesting-depth max-inline-delimiter-depth)
(parse-string string &key front-matter gfm max-nesting-depth max-inline-delimiter-depth)
(parse-stream character-stream &key front-matter gfm max-nesting-depth max-inline-delimiter-depth)
(parse-octets octet-vector &key front-matter gfm max-nesting-depth max-inline-delimiter-depth)
```

`:front-matter` defaults to `t` across all entry points. Set it to `nil` to
disable the leading front-matter prepass, which lets a leading `---` be parsed
as Markdown, including in the CommonMark structure comparison. The option does
not change the source snapshot or YAML interpretation. `parse` dispatches on
string, character input stream, or octet vector. `parse-octets` decodes through
`cl-codec-kit` before creating source spans. A missing limit argument uses the
corresponding positive default.
`:gfm` defaults to `t` and enables GFM block and inline extensions. Set it to
`nil` to parse CommonMark without GFM extensions.
`max-nesting-depth` counts
simultaneously open block containers below the document.
`max-inline-delimiter-depth` counts unresolved inline delimiter and bracket
opener records within one inline fragment. Both limits are checked before
pushing beyond the limit; they
are not disabled with `nil`. Defaults are 256 block levels and 4096 inline
records. Exceeding either signals a dedicated resource-limit subtype with the
resource name, configured limit, and observed depth. The same effective limits
are retained by the document and reused when an edit reparses its candidate.

### Optional YAML interpretation

The `cl-markdown-kit/yaml` system provides:

```lisp
(cl-markdown-kit/yaml:parse-front-matter front-matter-or-document
                                         &rest yaml-parse-options)
```

It accepts a `front-matter` node or a document, forwards the node's raw text and
YAML options to `yaml-kit:parse`, and returns the constructed YAML value. A
document without front matter or with empty YAML content returns `nil`. YAML
parse and composition errors are wrapped in `markdown-yaml-error`, a
`markdown-kit-error` subtype that retains the original YAML condition and
reports its Markdown source line and column when a source span is available.
Empty front matter is recognized before calling the YAML parser and returns
`nil`. YAML resource-limit conditions propagate unchanged. The optional system
does not change the core AST or the `:front-matter` parse option.

### Writers

```lisp
(write-html document &key (raw-html :omit) unsafe-urls gfm)
(write-html-to-stream document stream &key (raw-html :omit) unsafe-urls gfm)
(write-markdown document)
(write-markdown-to-stream document stream)
```

`write-html` returns a string and its stream variant writes the same output.
`:raw-html` is `:omit` (the default), `:escape`, or `:pass-through`. Omit
emits no raw block or inline HTML. Escape mode writes raw HTML as text;
pass-through emits it with GFM tagfilter applied when `:gfm` is true. `:gfm`
defaults to `t`; set it to `nil` for CommonMark HTML output without tagfilter.
`:unsafe-urls` defaults to
`nil`. In that mode, link and image destinations using a dangerous scheme are
written as empty strings. The check follows cmark's `_scan_dangerous_url`:
scheme checks are case-insensitive, and `javascript:`, `vbscript:`, and
`file:` are dangerous. `data:` is dangerous except for the
`data:image/(png|gif|jpeg|webp)` MIME pattern. Setting `:unsafe-urls t`
disables this URL filter. URL percent-encoding
normalization and HTML escaping are provided by `html-escaping.lisp`. The
conformance harness explicitly uses `:raw-html :pass-through :unsafe-urls t`.
`write-markdown` emits canonical LF Markdown. It uses ATX headings,
canonical `-` bullets and `.` ordered-list delimiters, preserves ordered-list
start values, emits safe fenced code blocks, writes alignment rows for tables,
and writes links with inline destinations. It emits reference definitions
from their AST nodes in source order, and emits front matter with `---`
delimiters without interpreting its contents. FR-021/T-003 compares HTML
under identical writer options on both sides of the Markdown round trip. Safe
defaults therefore apply consistently; conformance output opts into raw HTML
and unsafe URLs explicitly.

For deterministic safe code fences, the writer uses backticks unless the info
string contains a backtick, in which case it uses tildes. It chooses a fence
length of at least three and strictly greater than the longest run of that
fence character in the code body.

## Public query and edit API

The package is `cl-markdown-kit`; exported names use the sibling kit's
hyphenated `parse`-family naming. Public structures export their type name,
constructor, predicate, and documented readers. The contract is:

| Function | Signature and result |
|---|---|
| `document-headings` | `(document-headings document)`: ordered heading records with node, level, plain text, source form, and section span. All headings are listed; only direct document children participate in section hierarchy/spans, per D-2. Nested headings have no section span. |
| `find-section` | `(find-section document address)`, where `address` is an RFC 6901 string or a 1-based heading line number. `""` denotes the whole document. A nonempty pointer must begin with `/`; components decode only `~0` to `~` and `~1` to `/`, and compare exactly with heading plain text after inline markup is stripped. It returns a section record; signals no-match for zero matches, ambiguous with candidate headings/1-based line numbers for duplicate sibling matches, and invalid-address for malformed pointer escapes. |
| `extract` | `(extract document kind &key section info-string language include-source-slice)`: ordered values for `:code-block`, `:link`, `:image`, `:table`, `:task-item`, `:heading`, or `:front-matter`, optionally restricted to a section. `section` is `nil`, a section address accepted by `find-section`, or a section record; scope includes its heading and body. Each supplied code filter must match: `info-string` matches the complete info string and `language` matches its first token. Link/image values include destination and title; table values include header/cell strings; task values include checked state. Front matter returns its raw text and source range without YAML interpretation. `include-source-slice` adds the exact source substring for each result when requested. |
| `replace-section-body` | `(replace-section-body document address markdown)`: replaces content after the selected heading through its section end, retaining the heading itself. The empty address targets the whole document body. |
| `replace-section` | `(replace-section document address markdown)`: replaces the heading and its section body. |
| `delete-section` | `(delete-section document address)`: removes the heading and body. |
| `insert-section` | `(insert-section document address markdown &key (position :after))`: `position` is `:before`, `:after`, or `:last-child`; the last means the last child heading in the selected heading's implied section hierarchy. |
| `replace-node` | `(replace-node document node markdown)`: replaces one source-backed block node. |
| `set-task-checked` | `(set-task-checked document task-item checked-p)`: toggles only the task marker and returns edited source. |

`document-headings` returns `heading-record` values with the exported readers
`heading-record-node`, `heading-record-level`, `heading-record-plain-text`,
`heading-record-source-form`, and `heading-record-section-span`. `find-section`
returns a `section-record` with the exported readers
`section-record-heading`, `section-record-span`, and
`section-record-address`. The heading reader returns a heading record, or `nil`
for the whole-document address; the span is a `cl-parser-kit:span`.

All edit functions return a new source string and never mutate the document.
Replacement Markdown is normalized to the detected source newline convention;
text outside the target range, BOM presence, and trailing-newline presence
remain unchanged. When an edit touches EOF, its candidate is adjusted to have
the original document's trailing-newline state: exactly one final line ending
if the original had one, and none if it did not. For a mixed-newline source,
replacement uses the first line ending inside the replaced range; if there is
none, it uses the nearest preceding line ending, then the nearest following
one, then LF. A node
replacement stops before that node's final line ending. A section replacement
spans from its heading start through the character before the next same-or-
higher-level heading, including separator lines in that interval; a section
body replacement starts immediately after the heading's line ending. The
empty address targets all document content while retaining the BOM and final
newline state. If inserted text could merge with a following block, the edit
layer inserts a separating blank line, then reparses with the original
limits. `replace-node` accepts Markdown relative to the selected block, without
enclosing container prefixes. It keeps the source text before the first line
of the block, then inserts the node's `continuation-prefix` before every later
replacement line. This supports nested blockquotes and list items while
preserving their parent context. Replacing a container node includes its own
markers in the replacement and uses only its enclosing prefix. The edit
interval is the target node/section span (or a zero-width insertion point);
unchanged prefix and suffix nodes are paired by their source intervals after
applying the splice-length delta. The target subtree is the selected
node/section; for `set-task-checked`, it is the task item even though the
splice changes only its marker. For insertion, the corresponding candidate
interval is the inserted text, and candidate nodes fully inside it are target
nodes. For each side, the guard compares the ordered block projection
`(kind, semantic-text, semantic-attributes)` in pre-order, excluding only the
target subtree. Semantic text is the concatenated text, code, raw HTML, and
table-cell literal content for a block and its descendants; it is empty for
thematic breaks and definition-only nodes. Semantic attributes aggregate values on the block and
its descendants, including heading level, code info string, link/image
destination and title, list kind/start/delimiter/tightness, task checked state,
table alignment, HTML block kind, footnote labels, and reference labels,
destinations, and titles. If either side's projection differs, it signals
`markdown-structure-changed-error` and returns no source. This is D-6's
structural guard; a later aitools adapter should map it to the distinct error
code `refusal.structure-changed` from Q-3.

`find-section` resolves document-level headings only. Their implied parent is
the nearest preceding document-level heading of lower level; headings at the
same or higher level end the previous section. A heading's section span starts at its
own source start and ends immediately before the next document-level heading
of the same or higher level, or at document end. Headings inside block quotes,
lists, or footnotes remain queryable but do not bind sections.

Heading plain text is computed recursively from inline children: text and code
span literals, link/autolink display text, and image alt text are included;
emphasis/strong/strikethrough delimiters and raw HTML tags are omitted; soft
and hard breaks become one space; footnote references contribute their raw
label. After concatenation, runs of whitespace collapse to one space and
leading/trailing whitespace is removed. This one rule drives heading records
and exact D-1 path matching.

`insert-section :last-child` appends after the selected heading's last direct
child section, or at the end of the selected section body when it has no child
headings. At the empty document address it appends after the last document-level
section.

## Conditions

Every error condition defined by cl-markdown-kit descends from
`markdown-kit-error`:

```text
markdown-kit-error
├── markdown-no-match-error
├── markdown-ambiguous-match-error
├── markdown-invalid-section-address-error
├── markdown-yaml-error
├── markdown-resource-limit-error
│   ├── markdown-nesting-depth-exceeded
│   └── markdown-inline-delimiter-depth-exceeded
└── markdown-structure-changed-error
```

No-match carries the requested address. Ambiguous carries the address and
ordered candidate heading nodes with 1-based line numbers. Invalid-address
carries the malformed pointer and reason. Resource-limit errors carry
`limit-name` (the resource), `limit` (the configured limit), `actual` (the
observed value), and `context` (the parser/input context). Structure-changed
carries the edit target and a concise description of the first non-target
projection difference; it must not carry a candidate source string. The
condition readers are public so callers can handle these expected failures
without parsing the printed message:

| Condition | Exported readers |
|---|---|
| `markdown-kit-error` | `markdown-kit-error-message` |
| `markdown-resource-limit-error` and its subtypes | `markdown-resource-limit-error-limit-name`, `markdown-resource-limit-error-limit`, `markdown-resource-limit-error-actual`, `markdown-resource-limit-error-context` |
| `markdown-no-match-error` | `markdown-no-match-error-address` |
| `markdown-ambiguous-match-error` | `markdown-ambiguous-match-error-address`, `markdown-ambiguous-match-error-candidates` |
| `markdown-invalid-section-address-error` | `markdown-invalid-section-address-error-address`, `markdown-invalid-section-address-error-reason` |
| `markdown-structure-changed-error` | `markdown-structure-changed-error-target`, `markdown-structure-changed-error-difference` |
| `cl-markdown-kit/yaml:markdown-yaml-error` | `markdown-yaml-error-yaml-condition`, `markdown-yaml-error-line`, `markdown-yaml-error-column` |

`markdown-yaml-error` exposes the original YAML condition and one-based
Markdown source line and column when the front-matter node has a source span.
YAML parse and composition conditions are wrapped in `markdown-yaml-error`;
YAML resource-limit conditions from `cl-yaml-kit` propagate with their
original condition types.

## Files, load order, and ownership

The system uses serial ASDF components in dependency order. The skeleton
stream owns `cl-markdown-kit.asd`, the initial `src/package.lisp`,
`src/conditions.lisp`, test harness, CI, and flake. The AST/API stream owns the
exports/condition extensions and the AST/source implementation. The
`md-char-data` stream owns the three character and HTML data/helper files. The
remaining files have one owner each:

| ASDF order | File | Owner |
|---:|---|---|
| 1 | `src/package.lisp` | AST/API stream; coordinate changes with skeleton owner |
| 2 | `src/conditions.lisp` | AST/API stream; extend the skeleton base condition |
| 3 | `src/html-entities-data.lisp` | `md-char-data` stream; name-to-string HTML5 entity table |
| 4 | `src/character-data.lisp` | `md-char-data` stream; numeric-reference decoding, CommonMark character classes, and `normalize-link-label` |
| 5 | `src/html-escaping.lisp` | `md-char-data` stream; HTML escaping, URL percent-encoding normalization, and dangerous-scheme checks |
| 6 | `src/source.lisp` | AST/API stream; source normalization and `cl-parser-kit:span` construction |
| 7 | `src/nodes.lisp` | AST/API stream |
| 8 | `src/block-parser.lisp` | Block parser stream |
| 9 | `src/inline-parser.lisp` | Inline parser stream |
| 10 | `src/parse.lisp` | AST/API stream; public facade over block and inline parsers |
| 11 | `src/query.lisp` | Query API stream |
| 12 | `src/html-writer.lisp` | HTML writer stream |
| 13 | `src/markdown-writer.lisp` | Markdown writer stream |
| 14 | `src/edit.lisp` | Format-preserving edit stream |

Tests follow the sibling kit's `t/` convention and are loaded in dependency
order after the library: `t/nodes.lisp` and `t/source.lisp` belong to AST/API;
`t/block-parser.lisp`, `t/inline-parser.lisp`, `t/query.lisp`,
`t/html-writer.lisp`, `t/markdown-writer.lisp`, and `t/edit.lisp` belong to
their corresponding stream. `t/api-contract-test.lisp` and conformance
fixtures remain with the harness/skeleton owner. The AST/API stream supplies
`docs/src/reference/nodes.md` and its `docs/mkdocs.yml` navigation entry once
the types are implemented; each feature stream owns its corresponding API
reference page. This architecture decision is documented at
`docs/design/architecture.md`, outside the mkdocs `docs/src` root.

## Alternatives rejected

| Alternative | Reason rejected |
|---|---|
| CLOS instances and generic-function dispatch for all node operations | The node set is closed for this release. `defstruct` gives direct readers and low-overhead traversal while still supporting included common slots; writer dispatch can use node predicates/type cases. |
| One generic node with a kind keyword and untyped property plist | This makes invalid slot combinations easy, weakens public accessors, and hides which syntax details each node must retain. |
| One raw-source string per node instead of a document snapshot plus spans | It duplicates source memory across nested nodes and makes edits harder to reason about. One snapshot and half-open spans preserve untouched text without copying it into every node. |
| Local position and range types | `cl-parser-kit:span` already supplies the public half-open source location contract with 1-based line and column values; defining parallel local types would duplicate it. |
| Byte offsets for public spans | Public spans address decoded characters, matching parser input after `cl-codec-kit` decoding. Octet input retains its selected Unicode encoding for re-encoding edited strings; spans remain character-based. |
| Visual tab stops for columns | Tab width is renderer/editor dependent. Counting a tab as one character makes columns deterministic and keeps offsets and columns on the same character model. |
| Including following blank separators in block spans | It would make single-block replacement consume formatting owned by neighboring blocks. Spans stop at the final syntax character; source splicing leaves separators untouched. |
| Pass-through raw HTML as the HTML writer default | Safe defaults omit raw block and inline HTML and filter dangerous URLs. Conformance callers explicitly request `:raw-html :pass-through` and `:unsafe-urls t`. |
| Keeping only document-level nodes as sections | Nested headings in quotes/lists are still useful query results. Keeping all heading nodes while limiting section binding to direct document children satisfies extraction and D-2. |
| Parsing inline content inside the block parser | This couples two independently tested parser streams and prevents the block parser from handing off exact source mappings and the completed reference-definition index. |
| Reconstructing edited source from Markdown output | Normalization would change text outside an edit target, line endings, BOM, or final-newline state. Edits splice the retained original source and validate the reparsed result. |
