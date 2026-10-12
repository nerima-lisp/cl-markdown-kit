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
