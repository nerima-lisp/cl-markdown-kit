{
  description = "Common Lisp CommonMark and GFM parser scaffold";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    cl-nix-forge = {
      url = "github:nerima-lisp/cl-nix-forge/v0.6.1";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.treefmt-nix.follows = "treefmt-nix";
    };
    cl-weave = {
      url = "github:nerima-lisp/cl-weave/v1.4.0";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.cl-nix-forge.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.paredit-cli.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.treefmt-nix.follows = "treefmt-nix";
    };
    cl-parser-kit = {
      url = "github:nerima-lisp/cl-parser-kit/v1.1.1";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.cl-nix-forge.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.cl-prolog.inputs.cl-nix-forge.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.cl-prolog.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.cl-weave.follows = "cl-codec-kit/cl-weave";
      inputs.cl-weave.inputs.cl-nix-forge.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.cl-weave.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.cl-nix-forge.follows = "cl-weave/cl-nix-forge";
      inputs.cl-prolog.inputs.cl-nix-forge.follows = "cl-weave/cl-nix-forge";
      inputs.cl-weave.inputs.cl-nix-forge.follows = "cl-weave/cl-nix-forge";
      inputs.paredit-cli.follows = "cl-codec-kit/paredit-cli";
      inputs.paredit-cli.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.treefmt-nix.follows = "treefmt-nix";
    };
    cl-codec-kit = {
      url = "github:nerima-lisp/cl-codec-kit/v0.6.0";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.cl-nix-forge.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.cl-weave.inputs.cl-nix-forge.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.cl-weave.inputs.paredit-cli.follows = "cl-weave/paredit-cli";
      inputs.cl-weave.inputs.paredit-cli.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.cl-weave.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.paredit-cli.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.cl-nix-forge.follows = "cl-weave/cl-nix-forge";
      inputs.cl-weave.inputs.cl-nix-forge.follows = "cl-weave/cl-nix-forge";
      inputs.treefmt-nix.follows = "treefmt-nix";
    };
    cl-yaml-kit = {
      url = "github:nerima-lisp/cl-yaml-kit/v0.1.0";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.cl-nix-forge.follows = "cl-nix-forge";
      inputs.cl-weave.follows = "cl-weave";
      inputs.cl-codec-kit.follows = "cl-codec-kit";
      inputs.cl-regex-kit.inputs.cl-parser-kit.follows = "cl-parser-kit";
      inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.cl-json-kit.inputs.cl-nix-forge.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.cl-json-kit.inputs.cl-weave.inputs.cl-nix-forge.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.cl-json-kit.inputs.cl-weave.inputs.paredit-cli.follows = "cl-weave/paredit-cli";
      inputs.cl-json-kit.inputs.cl-weave.inputs.paredit-cli.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.cl-json-kit.inputs.cl-weave.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.cl-json-kit.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.cl-json-kit.inputs.cl-nix-forge.follows = "cl-weave/cl-nix-forge";
      inputs.cl-json-kit.inputs.cl-weave.inputs.cl-nix-forge.follows = "cl-weave/cl-nix-forge";
      inputs.cl-json-kit.inputs.cl-weave.follows = "cl-codec-kit/cl-weave";
      inputs.cl-regex-kit.inputs.cl-cli.inputs.cl-nix-forge.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.cl-regex-kit.inputs.cl-cli.inputs.paredit-cli.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.cl-regex-kit.inputs.cl-cli.inputs.paredit-cli.inputs.crane.follows = "cl-weave/paredit-cli/crane";
      inputs.cl-regex-kit.inputs.cl-cli.inputs.paredit-cli.inputs.rust-overlay.follows = "cl-weave/paredit-cli/rust-overlay";
      inputs.cl-regex-kit.inputs.cl-cli.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.cl-regex-kit.inputs.cl-codec-kit.inputs.cl-nix-forge.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.cl-regex-kit.inputs.cl-codec-kit.inputs.cl-weave.inputs.cl-nix-forge.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.cl-regex-kit.inputs.cl-codec-kit.inputs.cl-weave.inputs.paredit-cli.follows = "cl-weave/paredit-cli";
      inputs.cl-regex-kit.inputs.cl-codec-kit.inputs.cl-weave.inputs.paredit-cli.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.cl-regex-kit.inputs.cl-codec-kit.inputs.cl-weave.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.cl-regex-kit.inputs.cl-codec-kit.inputs.paredit-cli.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.cl-regex-kit.inputs.cl-codec-kit.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.cl-regex-kit.inputs.cl-codec-kit.inputs.paredit-cli.follows = "cl-codec-kit/paredit-cli";
      inputs.cl-regex-kit.inputs.cl-codec-kit.inputs.cl-nix-forge.follows = "cl-weave/cl-nix-forge";
      inputs.cl-regex-kit.inputs.cl-codec-kit.inputs.cl-weave.inputs.cl-nix-forge.follows = "cl-weave/cl-nix-forge";
      inputs.cl-regex-kit.inputs.cl-codec-kit.inputs.cl-weave.follows = "cl-codec-kit/cl-weave";
      inputs.cl-regex-kit.inputs.cl-concurrent-kit.inputs.cl-nix-forge.follows = "cl-weave/cl-nix-forge";
      inputs.cl-regex-kit.inputs.cl-concurrent-kit.inputs.cl-weave.inputs.cl-nix-forge.follows = "cl-weave/cl-nix-forge";
      inputs.cl-regex-kit.inputs.cl-concurrent-kit.inputs.cl-weave.follows = "cl-codec-kit/cl-weave";
      inputs.cl-regex-kit.inputs.cl-concurrent-kit.inputs.cl-boundary-kit.inputs.cl-host-kit.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.cl-regex-kit.inputs.cl-concurrent-kit.inputs.cl-boundary-kit.inputs.cl-prolog.inputs.paredit-cli.follows = "cl-weave/paredit-cli";
      inputs.cl-regex-kit.inputs.cl-concurrent-kit.inputs.cl-boundary-kit.inputs.cl-prolog.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.cl-regex-kit.inputs.cl-concurrent-kit.inputs.cl-boundary-kit.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.cl-regex-kit.inputs.cl-concurrent-kit.inputs.cl-cli.inputs.paredit-cli.follows = "cl-weave/paredit-cli";
      inputs.cl-regex-kit.inputs.cl-concurrent-kit.inputs.cl-cli.inputs.paredit-cli.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.cl-regex-kit.inputs.cl-concurrent-kit.inputs.cl-cli.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.cl-regex-kit.inputs.cl-concurrent-kit.inputs.cl-date-kit.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.cl-regex-kit.inputs.cl-concurrent-kit.inputs.cl-host-kit.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.cl-regex-kit.inputs.cl-concurrent-kit.inputs.cl-cli.inputs.paredit-cli.inputs.crane.follows = "cl-weave/paredit-cli/crane";
      inputs.cl-regex-kit.inputs.cl-concurrent-kit.inputs.cl-cli.inputs.paredit-cli.inputs.rust-overlay.follows = "cl-weave/paredit-cli/rust-overlay";
      inputs.cl-regex-kit.inputs.cl-concurrent-kit.inputs.cl-nix-forge.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.cl-regex-kit.inputs.cl-concurrent-kit.inputs.cl-weave.inputs.cl-nix-forge.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.cl-regex-kit.inputs.cl-concurrent-kit.inputs.cl-weave.inputs.paredit-cli.follows = "cl-weave/paredit-cli";
      inputs.cl-regex-kit.inputs.cl-concurrent-kit.inputs.cl-weave.inputs.paredit-cli.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.cl-regex-kit.inputs.cl-concurrent-kit.inputs.cl-weave.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.cl-regex-kit.inputs.cl-concurrent-kit.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.cl-regex-kit.inputs.cl-dataflow-kit.inputs.cl-concurrent-kit.inputs.cl-boundary-kit.inputs.cl-host-kit.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.cl-regex-kit.inputs.cl-dataflow-kit.inputs.cl-concurrent-kit.inputs.cl-boundary-kit.inputs.cl-prolog.inputs.paredit-cli.follows = "cl-weave/paredit-cli";
      inputs.cl-regex-kit.inputs.cl-dataflow-kit.inputs.cl-concurrent-kit.inputs.cl-boundary-kit.inputs.cl-prolog.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.cl-regex-kit.inputs.cl-dataflow-kit.inputs.cl-concurrent-kit.inputs.cl-boundary-kit.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.cl-regex-kit.inputs.cl-dataflow-kit.inputs.cl-concurrent-kit.inputs.cl-cli.inputs.paredit-cli.follows = "cl-weave/paredit-cli";
      inputs.cl-regex-kit.inputs.cl-dataflow-kit.inputs.cl-concurrent-kit.inputs.cl-cli.inputs.paredit-cli.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.cl-regex-kit.inputs.cl-dataflow-kit.inputs.cl-concurrent-kit.inputs.cl-cli.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.cl-regex-kit.inputs.cl-dataflow-kit.inputs.cl-concurrent-kit.inputs.cl-date-kit.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.cl-regex-kit.inputs.cl-dataflow-kit.inputs.cl-concurrent-kit.inputs.cl-host-kit.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.cl-regex-kit.inputs.cl-dataflow-kit.inputs.cl-concurrent-kit.inputs.cl-nix-forge.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.cl-regex-kit.inputs.cl-dataflow-kit.inputs.cl-concurrent-kit.inputs.cl-weave.inputs.cl-nix-forge.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.cl-regex-kit.inputs.cl-dataflow-kit.inputs.cl-concurrent-kit.inputs.cl-weave.inputs.paredit-cli.follows = "cl-weave/paredit-cli";
      inputs.cl-regex-kit.inputs.cl-dataflow-kit.inputs.cl-concurrent-kit.inputs.cl-weave.inputs.paredit-cli.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.cl-regex-kit.inputs.cl-dataflow-kit.inputs.cl-concurrent-kit.inputs.cl-weave.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.cl-regex-kit.inputs.cl-dataflow-kit.inputs.cl-concurrent-kit.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.cl-regex-kit.inputs.cl-dataflow-kit.inputs.cl-concurrent-kit.inputs.cl-nix-forge.follows = "cl-weave/cl-nix-forge";
      inputs.cl-regex-kit.inputs.cl-dataflow-kit.inputs.cl-concurrent-kit.inputs.cl-weave.inputs.cl-nix-forge.follows = "cl-weave/cl-nix-forge";
      inputs.cl-regex-kit.inputs.cl-dataflow-kit.inputs.cl-concurrent-kit.inputs.cl-weave.follows = "cl-codec-kit/cl-weave";
      inputs.cl-regex-kit.inputs.cl-dataflow-kit.inputs.cl-nix-forge.follows = "cl-weave/cl-nix-forge";
      inputs.cl-regex-kit.inputs.cl-dataflow-kit.inputs.cl-weave.follows = "cl-codec-kit/cl-weave";
      inputs.cl-regex-kit.inputs.cl-dataflow-kit.inputs.cl-concurrent-kit.inputs.cl-cli.follows = "cl-yaml-kit/cl-regex-kit/cl-concurrent-kit/cl-cli";
      inputs.cl-regex-kit.inputs.cl-dataflow-kit.inputs.cl-concurrent-kit.inputs.cl-date-kit.follows = "cl-yaml-kit/cl-regex-kit/cl-concurrent-kit/cl-date-kit";
      inputs.cl-regex-kit.inputs.cl-dataflow-kit.inputs.cl-concurrent-kit.inputs.cl-boundary-kit.follows = "cl-yaml-kit/cl-regex-kit/cl-concurrent-kit/cl-boundary-kit";
      inputs.cl-regex-kit.inputs.cl-dataflow-kit.inputs.cl-concurrent-kit.follows = "cl-yaml-kit/cl-regex-kit/cl-concurrent-kit";
      inputs.cl-regex-kit.inputs.cl-dataflow-kit.inputs.cl-concurrent-kit.inputs.cl-boundary-kit.inputs.cl-prolog.follows = "cl-yaml-kit/cl-regex-kit/cl-concurrent-kit/cl-boundary-kit/cl-prolog";
      inputs.cl-regex-kit.inputs.cl-dataflow-kit.inputs.cl-weave.inputs.cl-nix-forge.follows = "cl-weave/cl-nix-forge";
      inputs.cl-regex-kit.inputs.cl-dataflow-kit.inputs.cl-nix-forge.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.cl-regex-kit.inputs.cl-dataflow-kit.inputs.cl-weave.inputs.cl-nix-forge.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.cl-regex-kit.inputs.cl-dataflow-kit.inputs.cl-weave.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.cl-regex-kit.inputs.cl-dataflow-kit.inputs.paredit-cli.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.cl-regex-kit.inputs.cl-dataflow-kit.inputs.paredit-cli.follows = "cl-codec-kit/paredit-cli";
      inputs.cl-regex-kit.inputs.cl-dataflow-kit.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.cl-regex-kit.inputs.cl-json-kit.inputs.cl-nix-forge.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.cl-regex-kit.inputs.cl-json-kit.inputs.cl-weave.inputs.cl-nix-forge.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.cl-regex-kit.inputs.cl-json-kit.inputs.cl-weave.inputs.paredit-cli.follows = "cl-weave/paredit-cli";
      inputs.cl-regex-kit.inputs.cl-json-kit.inputs.cl-weave.inputs.paredit-cli.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.cl-regex-kit.inputs.cl-json-kit.inputs.cl-weave.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.cl-regex-kit.inputs.cl-json-kit.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.cl-regex-kit.inputs.cl-json-kit.inputs.cl-nix-forge.follows = "cl-weave/cl-nix-forge";
      inputs.cl-regex-kit.inputs.cl-json-kit.inputs.cl-weave.inputs.cl-nix-forge.follows = "cl-weave/cl-nix-forge";
      inputs.cl-regex-kit.inputs.cl-json-kit.inputs.cl-weave.follows = "cl-codec-kit/cl-weave";
      inputs.cl-regex-kit.inputs.cl-nix-forge.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.cl-regex-kit.inputs.cl-prolog-kit.inputs.cl-nix-forge.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.cl-regex-kit.inputs.cl-prolog-kit.inputs.cl-weave.inputs.cl-nix-forge.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.cl-regex-kit.inputs.cl-prolog-kit.inputs.cl-weave.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.cl-regex-kit.inputs.cl-prolog-kit.inputs.paredit-cli.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.cl-regex-kit.inputs.cl-prolog-kit.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.cl-regex-kit.inputs.cl-weave.inputs.cl-nix-forge.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.cl-regex-kit.inputs.cl-weave.inputs.paredit-cli.follows = "cl-weave/paredit-cli";
      inputs.cl-regex-kit.inputs.cl-weave.inputs.paredit-cli.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.cl-regex-kit.inputs.cl-weave.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.cl-regex-kit.inputs.cl-nix-forge.follows = "cl-weave/cl-nix-forge";
      inputs.cl-regex-kit.inputs.cl-prolog-kit.inputs.cl-nix-forge.follows = "cl-weave/cl-nix-forge";
      inputs.cl-regex-kit.inputs.cl-weave.inputs.cl-nix-forge.follows = "cl-weave/cl-nix-forge";
      inputs.cl-regex-kit.inputs.cl-weave.follows = "cl-codec-kit/cl-weave";
      inputs.cl-regex-kit.inputs.paredit-cli.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.cl-regex-kit.inputs.treefmt-nix.follows = "treefmt-nix";
      inputs.cl-regex-kit.inputs.cl-json-kit.follows = "cl-yaml-kit/cl-json-kit";
      inputs.cl-regex-kit.inputs.cl-concurrent-kit.inputs.cl-boundary-kit.inputs.cl-host-kit.follows = "cl-yaml-kit/cl-regex-kit/cl-concurrent-kit/cl-host-kit";
      inputs.cl-regex-kit.inputs.cl-dataflow-kit.inputs.cl-concurrent-kit.inputs.cl-boundary-kit.inputs.cl-host-kit.follows = "cl-yaml-kit/cl-regex-kit/cl-concurrent-kit/cl-host-kit";
      inputs.cl-regex-kit.inputs.cl-dataflow-kit.inputs.cl-concurrent-kit.inputs.cl-host-kit.follows = "cl-yaml-kit/cl-regex-kit/cl-concurrent-kit/cl-host-kit";
      inputs.cl-regex-kit.inputs.cl-dataflow-kit.inputs.cl-concurrent-kit.inputs.cl-cli.inputs.cl-boundary-kit.follows = "cl-yaml-kit/cl-regex-kit/cl-concurrent-kit/cl-cli/cl-boundary-kit";
      inputs.cl-regex-kit.inputs.cl-dataflow-kit.inputs.cl-concurrent-kit.inputs.cl-cli.inputs.cl-host-kit.follows = "cl-yaml-kit/cl-regex-kit/cl-concurrent-kit/cl-cli/cl-host-kit";
      inputs.cl-regex-kit.inputs.cl-dataflow-kit.inputs.cl-concurrent-kit.inputs.cl-cli.inputs.cl-json-kit.follows = "cl-yaml-kit/cl-regex-kit/cl-concurrent-kit/cl-cli/cl-json-kit";
      inputs.cl-regex-kit.inputs.cl-dataflow-kit.inputs.cl-concurrent-kit.inputs.cl-cli.inputs.cl-log-kit.follows = "cl-yaml-kit/cl-regex-kit/cl-concurrent-kit/cl-cli/cl-log-kit";
      inputs.cl-regex-kit.inputs.cl-dataflow-kit.inputs.cl-concurrent-kit.inputs.cl-cli.inputs.cl-process-kit.follows = "cl-yaml-kit/cl-regex-kit/cl-concurrent-kit/cl-cli/cl-process-kit";
      inputs.cl-regex-kit.inputs.cl-dataflow-kit.inputs.cl-concurrent-kit.inputs.cl-cli.inputs.cl-prolog.follows = "cl-yaml-kit/cl-regex-kit/cl-concurrent-kit/cl-cli/cl-prolog";
      inputs.cl-regex-kit.inputs.cl-dataflow-kit.inputs.cl-concurrent-kit.inputs.cl-cli.inputs.paredit-cli.inputs.crane.follows = "cl-weave/paredit-cli/crane";
      inputs.cl-regex-kit.inputs.cl-dataflow-kit.inputs.cl-concurrent-kit.inputs.cl-cli.inputs.paredit-cli.inputs.rust-overlay.follows = "cl-weave/paredit-cli/rust-overlay";
      inputs.cl-regex-kit.inputs.cl-dataflow-kit.inputs.cl-concurrent-kit.inputs.cl-cli.inputs.cl-weave.follows = "cl-yaml-kit/cl-regex-kit/cl-concurrent-kit/cl-cli/cl-weave";
      inputs.cl-regex-kit.inputs.cl-dataflow-kit.inputs.cl-concurrent-kit.inputs.cl-date-kit.inputs.cl-weave.follows = "cl-yaml-kit/cl-regex-kit/cl-concurrent-kit/cl-date-kit/cl-weave";
      inputs.cl-regex-kit.inputs.cl-prolog-kit.inputs.cl-weave.inputs.cl-nix-forge.follows = "cl-yaml-kit/cl-regex-kit/cl-cli/cl-nix-forge";
      inputs.cl-regex-kit.inputs.cl-prolog-kit.inputs.paredit-cli.follows = "cl-yaml-kit/cl-regex-kit/cl-cli/paredit-cli";
      inputs.cl-regex-kit.inputs.cl-prolog-kit.inputs.paredit-cli.inputs.crane.follows = "cl-weave/paredit-cli/crane";
      inputs.cl-regex-kit.inputs.cl-prolog-kit.inputs.paredit-cli.inputs.rust-overlay.follows = "cl-weave/paredit-cli/rust-overlay";
      inputs.cl-regex-kit.inputs.paredit-cli.inputs.crane.follows = "cl-codec-kit/paredit-cli/crane";
      inputs.cl-regex-kit.inputs.paredit-cli.inputs.rust-overlay.follows = "cl-codec-kit/paredit-cli/rust-overlay";
    };
    commonmark-spec = {
      url = "github:commonmark/commonmark-spec/0.31.2";
      flake = false;
    };
    cmark-gfm = {
      url = "github:github/cmark-gfm/0.29.0.gfm.13";
      flake = false;
    };
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      cl-nix-forge,
      cl-weave,
      cl-parser-kit,
      cl-codec-kit,
      cl-yaml-kit,
      commonmark-spec,
      cmark-gfm,
      treefmt-nix,
    }:
    cl-nix-forge.lib.x86_64-linux.mkPackageFlake {
      inherit self nixpkgs;
      pname = "cl-markdown-kit";
      systems = [ "x86_64-linux" ];
      asd = ./cl-markdown-kit.asd;
      root = ./.;
      lispDependencies = ctx: [
        cl-parser-kit.packages.${ctx.system}.cl-parser-kit
        cl-codec-kit.packages.${ctx.system}.cl-codec-kit
      ];
      lispCheckDependencies = ctx: [
        cl-weave.packages.${ctx.system}.cl-weave
        cl-yaml-kit.packages.${ctx.system}.cl-yaml-kit
      ];
      packageArgs = ctx: {
        COMMONMARK_TEST_SUITE = "${commonmark-spec}/spec.txt";
        GFM_TEST_SUITE = "${cmark-gfm}/test/extensions.txt";
        nativeBuildInputs = [ ctx.pkgs.perl ];
        preCheck = ''
          export PATH="${ctx.pkgs.perl}/bin:$PATH"
        '';
      };
      sourceInclude = [
        ./docs
        ./scripts
      ];
      timeoutSeconds = 1200;
      docs.root = ./docs;
      treefmt.evalModule = treefmt-nix.lib.evalModule;
      extraOutputs = ctx: {
        checks.coverage = ctx.cl.mkCoverageReport {
          drv = ctx.package;
          entryPoint = "scripts/run-coverage.lisp";
          timeoutSeconds = 1200;
          killAfterSeconds = 30;
        };
      };
      overrideOutputs = ctx: {
        apps.test = {
          type = "app";
          program = "${
            ctx.pkgs.writeShellApplication {
              name = "cl-markdown-kit-test-with-fixtures";
              text = ''
                export COMMONMARK_TEST_SUITE=${commonmark-spec}/spec.txt
                export GFM_TEST_SUITE=${cmark-gfm}/test/extensions.txt
                exec ${ctx.generated.apps.test.program} "$@"
              '';
            }
          }/bin/cl-markdown-kit-test-with-fixtures";
        };
      };
    };
}
