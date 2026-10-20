# Contributor design note: architecture and public contract

This contributor design note describes the current AST, source-span, parser,
writer, query, and edit architecture of `cl-markdown-kit`.

## nerima-lisp package policy

The core system uses the following pinned packages and layers:

| Package | Tag | Use |
|---|---|---|
| `cl-parser-kit` | v1.1.1 | Public source locations use `cl-parser-kit:span`; this kit defines no local position or range types. |
| `cl-codec-kit` | v0.6.0 | `parse-octets` detects BOM/encoding and decodes octet-vector input before source scanning. |

The optional YAML system and test systems use separate dependencies:

| Package | Tag | Use |
|---|---|---|
| `cl-yaml-kit` | v0.2.1 | Optional `cl-markdown-kit/yaml:parse-front-matter` API; the core keeps front matter as raw text and a span. |
| `cl-weave` | v1.4.0 | Tests. |

Pin package inputs by release tag in `flake.nix`; give each ASDF
`:depends-on` entry a one-line reason. Nix packaging uses
`cl-nix-forge`'s `mkPackageFlake` at v0.6.1. SBCL's `sb-unicode` supplies the
case folding and Unicode character classes used by the parser; it is part of
the SBCL-only runtime policy, not a separate nerima-lisp package dependency.

## Design decisions

### Representation

Use `defstruct` with a small included-structure hierarchy. A `node` owns a
`cl-parser-kit:span`; `block-node` and `inline-node` include it. Container structures
own an ordered list of children. Lists match the sibling kit's public AST
convention and let parsers build children incrementally without exposing a
mutable vector. Leaf nodes own semantic values and any syntax attributes needed
by the normalized Markdown writer. Nodes are mutable during construction, then
treated as immutable after `parse` returns. Public constructors accept keyword
arguments, and `node-children` returns the ordered child list.

Every parsed node has a source span, including inline nodes. A node made
directly with a public constructor may have a `nil` span. This makes source
lookup consistent without requiring a parallel position tree. Children lists
are homogeneous: block containers
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
| CommonMark containers | `block-quote`: block children. `list-node`: ordered/unordered kind, tight/loose flag, ordered start number, marker character, ordered-list delimiter character, and item children. `list-item`: block children. `task-list-item`: list-item subtype with `checked-p`, `task-marker-span` (a `cl-parser-kit:span`) covering exactly the recognized three-character `[ ]`, `[x]`, or `[X]` marker, and the original marker character; its distinct type means unchecked tasks are distinguishable from ordinary items. |
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

### Node metadata, dispatch, and traversal

Concrete node declarations use `define-node` so their structure and public
metadata stay together. The macro takes a `defstruct` form followed by
`:kind`, `:family`, `:children`, `:attributes`, and `:edit-text` metadata. For
example:

```lisp
(in-package #:cl-markdown-kit)

(define-node (defstruct (text
                         (:include inline-node)
                         (:constructor make-text (&key span value)))
              (value "" :type string))
  :kind :text
  :family :inline
  :children nil
  :attributes (:value)
  :edit-text (:slot value))
```

Concrete `:family` values are `:block` or `:inline`; abstract base node
declarations use `:abstract t`, `:kind nil`, and `:family :abstract`.
`:children` is `nil`, a child slot name, or `(:slot slot-name :accessor
accessor-name)` when the child list uses a different accessor. `:attributes`
lists distinct keyword names for declared semantic slots.

The declaration expands the supplied `defstruct` and registers its metadata.
`src/package.lisp` exports each node type, its predicate, constructor, and
declared slot readers. The predicates are public, including `document-p`,
`heading-p`, `text-p`, and `front-matter-p`. After all declarations,
`(define-node-registry)` generates the metadata readers and `node-children`
from the declarations.
`node-kind` returns the declared keyword kind and signals on abstract base
nodes. `node-family` returns `:block` or `:inline` for a concrete node and
`:abstract` for a base node. `node-semantic-attributes` returns a plist of
declared keyword names and their current slot values. The `:edit-text`
declaration is `:empty`, `:container`, or `(:slot slot-name)`.
`node-edit-text-projection` returns the current slot value, the empty string
for `:empty`, or `nil` for `:container`; container text is projected from its
children. `node-children` reads the child slots
declared by the node. Missing or invalid required metadata fails when the
declaration macro expands. Consumers can test a public node type with `typep`.

Writer operations use `define-node-dispatch` with one static clause for every
concrete node in a family. This schematic shows the form; `body-form...` and
`more-clauses...` stand for real clause bodies and the remaining concrete node
clauses, so this is not a runnable Lisp example:

```lisp
(in-package #:cl-markdown-kit)

(define-node-dispatch (%hw-render-inline-event (context node phase))
  :operation :html-renderer :family :inline
  (text body-form...)
  (code-span body-form...)
  more-clauses...)
```

The `:operation` and `:family` values select the declared dispatch set. The
macro dispatches on the lambda-list argument named `node`, and every declared
argument is in scope in each clause body. Each clause body supplies the
operation for that node type. The macro expands to a static type dispatch and
rejects missing, duplicate, or extra concrete node clauses during macro
expansion. `verify-node-dispatches` checks that the operation has registered
coverage for each required family. The HTML renderer checks both `:block` and
`:inline` at load time with
`(verify-node-dispatches :operation :html-renderer :families '(:block :inline))`.

`walk-node-tree` uses an explicit frame stack, so Lisp call depth does not grow
with tree depth. Its signature is `(walk-node-tree root callback)`. The
callback phases, actions, and exported frame readers are documented in the
[AST reference](../src/reference/nodes.md#walking-nodes).

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

Character streams are read character by character so CR, LF, and CRLF remain
observable. Octet input is decoded before source scanning; public offsets are
character offsets in the decoded text.

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

An AST returned by `parse` retains its decoded source snapshot. A document
constructed by a caller has no source snapshot and cannot be passed
to a source-preserving edit operation. Normalized Markdown output uses LF and
does not include a BOM; edit operations use the source's recorded line-ending
metadata for replacement text and retain the original BOM and trailing
newline through source splicing. Edit APIs return decoded strings. For octet
input, source metadata retains the Unicode encoding selected by
`cl-codec-kit:detect-unicode-encoding` and the original BOM state; callers can
re-encode the result with that metadata to preserve untouched octets. For
string and character-stream input, byte identity refers to the canonical UTF-8
representation: encode both the original and returned strings as UTF-8 before
comparing bytes.

## Parser and writer boundaries

### Source and block parsing

`source.lisp` owns source normalization, source metadata, and the
logical-to-original boundary map; it creates `cl-parser-kit:span` values and
defines no local position/range types. The block pass retains reference
definitions on the partial document and produces deferred inline fragments
with their owners, logical text, and source mappings. Keeping the passes
separate lets block parsing resolve definitions and establish source mappings
before inline parsing. The exported `parse-blocks` result exposes this
intermediate state; it does not have a public completion operation. Use `parse`,
`parse-string`, `parse-stream`, or `parse-octets` to obtain a complete AST.

The internal `parse-inline` function consumes the deferred text, source map,
and reference index:

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

The source-normalization layer is independently testable, keeping both parser
passes independent of whether input arrived as a string, character stream, or
octet vector. The public `parse` facade normalizes the input, runs the block
pass, parses each deferred inline fragment with its source mapping and
reference index, and returns the completed document. The public parser
signatures are listed in the [AST and source-location reference](../src/reference/nodes.md).

`:front-matter` defaults to `t`; setting it to `nil` leaves leading delimiter
lines to Markdown parsing. `:gfm` defaults to `t`; setting it to `nil` selects
CommonMark behavior. The completed document retains these options so
source-preserving edits can reparse with the same parsing mode.

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
delimiters without interpreting its contents. Compare HTML before and after a
Markdown round trip with identical writer options. Safe defaults apply on both
sides; conformance output opts into raw HTML and unsafe URLs explicitly.

For deterministic safe code fences, the writer uses backticks unless the info
string contains a backtick, in which case it uses tildes. It chooses a fence
length of at least three and strictly greater than the longest run of that
fence character in the code body.

## Public query and edit API

The package is `cl-markdown-kit`; exported names use the sibling kit's
hyphenated `parse`-family naming. The package exports each node type predicate,
such as `document-p`, `heading-p`, and `text-p`, as well as its constructor and
declared slot readers. Callers may also use `typep` with an exported node type.
The query and edit contract is:

| Function | Signature and result |
|---|---|
| `document-headings` | `(document-headings document)`: ordered heading records with node, level, plain text, source form, and section span. All headings are listed; only direct document children participate in section hierarchy and spans. Nested headings have no section span. |
| `find-section` | `(find-section document address)`, where `address` is an RFC 6901 string or a 1-based heading line number. `""` denotes the whole document. A nonempty pointer must begin with `/`; components decode only `~0` to `~` and `~1` to `/`, and compare exactly with heading plain text after inline markup is stripped. It returns a section record; signals no-match for zero matches, ambiguous with candidate headings/1-based line numbers for duplicate sibling matches, and invalid-address for malformed pointer escapes. |
| `extract` | `(extract document kind &key section info-string language include-source-slice)`: ordered values for `:code-block`, `:link`, `:image`, `:table`, `:task-item`, `:heading`, or `:front-matter`, optionally restricted to a section. `section` is `nil`, a section address accepted by `find-section`, or a section record; scope includes its heading and body. Each supplied code filter must match: `info-string` matches the complete info string and `language` matches its first token. Link/image values include destination and title; table values include header/cell strings; task values include checked state. Front matter returns its raw text and source range without YAML interpretation. `include-source-slice` adds the exact source substring for each result when requested. |
| `replace-section-body` | `(replace-section-body document address markdown)`: replaces content after the selected heading through its section end, retaining the heading itself. The empty address targets the whole document body. |
| `replace-section` | `(replace-section document address markdown)`: replaces the heading and its section body. |
| `delete-section` | `(delete-section document address)`: removes the heading and body. |
| `insert-section` | `(insert-section document address markdown &key (position :after))`: `position` is `:before`, `:after`, or `:last-child`; the last means the last child heading in the selected heading's implied section hierarchy. |
| `replace-node` | `(replace-node document node markdown)`: replaces one source-backed block node. |
| `set-task-checked` | `(set-task-checked document task-item checked-p)`: toggles only the task marker and returns edited source. |

`document-headings` returns `heading-record` values; see the
[Query API](../src/reference/query.md#headings) for their readers.
`find-section` returns a `section-record` whose `section-record-heading` is a
heading record or `nil` for the whole-document address, and whose
`section-record-span` is a `cl-parser-kit:span`. The [Query API](../src/reference/query.md#resolving-sections)
documents the remaining section-record reader.

All edit functions return a new source string and never mutate the document.
Replacement Markdown is normalized to the detected source newline convention;
text outside the target range, BOM presence, and trailing-newline presence
remain unchanged. When an edit touches EOF, its candidate is adjusted to have
the original document's trailing-newline presence. Any adjustment is confined
to the inserted or replacement Markdown; untouched source text, including
trailing blank lines and their line endings, remains unchanged. The candidate
is not normalized to exactly one final line ending. For a mixed-newline source,
replacement uses the first line ending inside the replaced range; if there is
none, it uses the nearest preceding line ending, then the nearest following
one, then LF. A node
replacement stops before that node's final line ending. A section replacement
spans from its heading start through the character before the next same-or-
higher-level heading, including separator lines in that interval; a section
body replacement starts immediately after the heading's line ending. The
empty address targets all document content while retaining the BOM and final
newline state. If inserted text could merge with a following block, the edit
layer inserts a separating blank line. The document retains its effective
`:front-matter` and `:gfm` options alongside the parser limits, and edits
reparse candidates with those original values. `replace-node` accepts Markdown
relative to the selected block, without enclosing container prefixes. It keeps
the source text before the first line of the block, then inserts the node's
`continuation-prefix` before every later replacement line. This supports nested
blockquotes and list items while preserving their parent context. Replacing a
container node includes its own markers in the replacement and uses only its
enclosing prefix. The edit
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
`markdown-structure-changed-error` and returns no source. This condition is the
edit layer's structural guard.

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
leading/trailing whitespace is removed. Heading records and section addresses
use this plain-text form.

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

## Current system organization

`cl-markdown-kit.asd` loads the core source files serially. The components are
grouped by their current role below; the ASDF file defines their load order.

| Area | Current components |
|---|---|
| Package and conditions | `package`, `conditions` |
| Character data and common parsing | `html-entities-data`, `character-data`, `character-logic`, `parser-common-data`, `parser-common`, `html-escaping`, `parser-common-links` |
| Source, AST, and traversal | `source`, `nodes`, `tree-walker` |
| Block parser | `block-parser-data`, `block-parser-source`, `block-parser-references`, `block-parser-classification`, `block-parser-nodes`, `block-parser-parse-state`, `block-parser-parse-open`, `block-parser-parse-leaf`, `block-parser` |
| Inline parser | `inline-parser-data`, `inline-parser-model`, `inline-parser-indexes`, `inline-parser-links`, `inline-parser-delimiters`, `inline-parser-build`, `inline-parser-range`, `inline-parser` |
| Parser facade and queries | `parse`, `query` |
| HTML writer | `html-writer-output`, `html-writer-footnotes`, `html-writer-inline`, `html-writer-block-elements`, `html-writer-block-events`, `html-writer` |
| Markdown writer | `markdown-writer-escape`, `markdown-writer-state`, `markdown-writer-inline`, `markdown-writer-block`, `markdown-writer-document` |
| Source-preserving edits | `edit` |

The optional YAML system loads `src/yaml/package.lisp` and
`src/yaml/front-matter.lisp`. The core and YAML test systems are defined in
`cl-markdown-kit.asd` and load their components from `t/`.

## Alternatives rejected

| Alternative | Reason rejected |
|---|---|
| CLOS instances and generic-function dispatch for all node operations | The node set is closed for this release. `defstruct` gives direct readers and low-overhead traversal while `define-node-dispatch` supplies static per-family clauses whose coverage is checked at macro expansion and load time. |
| One generic node with a kind keyword and untyped property plist | This makes invalid slot combinations easy, weakens public accessors, and hides which syntax details each node must retain. |
| One raw-source string per node instead of a document snapshot plus spans | It duplicates source memory across nested nodes and makes edits harder to reason about. One snapshot and half-open spans preserve untouched text without copying it into every node. |
| Local position and range types | `cl-parser-kit:span` already supplies the public half-open source location contract with 1-based line and column values; defining parallel local types would duplicate it. |
| Byte offsets for public spans | Public spans address decoded characters, matching parser input after `cl-codec-kit` decoding. Octet input retains its selected Unicode encoding for re-encoding edited strings; spans remain character-based. |
| Visual tab stops for columns | Tab width is renderer/editor dependent. Counting a tab as one character makes columns deterministic and keeps offsets and columns on the same character model. |
| Including following blank separators in block spans | It would make single-block replacement consume formatting owned by neighboring blocks. Spans stop at the final syntax character; source splicing leaves separators untouched. |
| Pass-through raw HTML as the HTML writer default | Safe defaults omit raw block and inline HTML and filter dangerous URLs. Conformance callers explicitly request `:raw-html :pass-through` and `:unsafe-urls t`. |
| Keeping only document-level nodes as sections | Nested headings in quotes/lists are still useful query results. Keeping all heading nodes while limiting section binding to direct document children supports extraction. |
| Parsing inline content inside the block parser | This couples two independently tested parser streams and prevents the block parser from handing off exact source mappings and the completed reference-definition index. |
| Reconstructing edited source from Markdown output | Normalization would change text outside an edit target, line endings, BOM, or final-newline state. Edits splice the retained original source and validate the reparsed result. |
