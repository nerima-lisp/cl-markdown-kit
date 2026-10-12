# cl-markdown-kit

`cl-markdown-kit` is an SBCL-only Common Lisp project scaffold for a CommonMark
and GitHub Flavored Markdown parser. The current ASDF system defines the
package and its base error conditions; Markdown parsing is not implemented
yet.

## Load

Add the repository to `CL_SOURCE_REGISTRY`, then load the ASDF system:

```lisp
(asdf:load-system "cl-markdown-kit")
```

## Development

Run the test suite with Nix. The test app supplies the pinned CommonMark and
GFM fixtures and the cl-weave test dependency:

```sh
nix run .#test
```

## License

MIT. See [`LICENSE`](LICENSE).

## Character data

The HTML5 named character reference table in `src/html-entities-data.lisp` is
generated from the WHATWG HTML Standard entity data. The vendored source
snapshot is the immutable
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

The generator verifies the snapshot hash and needs no network access.
The pinned snapshot has the same entity names and character sequences as the
current WHATWG `entities.json` export. Only names ending in a semicolon are
stored, matching CommonMark's named-reference rule.

The entity data is attributed as Copyright © WHATWG.
The HTML Standard is licensed under [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/);
the license states that portions incorporated into source code are licensed
under BSD 3-Clause. The generated table preserves all named values, including
multi-codepoint sequences.

Regenerate the Lisp table offline with
`python3 scripts/generate-html5-entities.py`. Check that the committed output
matches the pinned snapshot with
`python3 scripts/generate-html5-entities.py --check`.

The internal character-data functions provide exact-case named-reference
lookup, numeric-reference decoding, CommonMark whitespace and punctuation
classification, link-label folding, HTML escaping, and URI percent encoding.
HTML text and attribute escaping match commonmark.js and cmark for `&`, `<`,
`>`, and `"`. URI percent encoding is a separate operation and preserves valid
percent triplets exactly.
