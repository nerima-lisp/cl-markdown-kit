# cl-markdown-kit

`cl-markdown-kit` is an SBCL Common Lisp parser for CommonMark and GitHub
Flavored Markdown. It accepts source strings, character input streams, and
octet vectors, and returns an AST with source spans.

The package also provides HTML and normalized Markdown writers, heading and
section queries, source-preserving edits, and optional YAML front matter
parsing through `cl-markdown-kit/yaml`.
