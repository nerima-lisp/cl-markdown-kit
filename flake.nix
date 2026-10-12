{
  description = "Common Lisp CommonMark and GFM parser scaffold";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    cl-nix-forge = {
      url = "github:nerima-lisp/cl-nix-forge/v0.6.1";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    cl-weave = {
      url = "github:nerima-lisp/cl-weave/v1.4.0";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    cl-parser-kit = {
      url = "github:nerima-lisp/cl-parser-kit/v1.1.1";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    cl-codec-kit = {
      url = "github:nerima-lisp/cl-codec-kit/v0.6.0";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    cl-yaml-kit = {
      url = "github:nerima-lisp/cl-yaml-kit/v0.1.0";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.cl-nix-forge.follows = "cl-nix-forge";
      inputs.cl-weave.follows = "cl-weave";
      inputs.cl-codec-kit.follows = "cl-codec-kit";
      inputs.cl-regex-kit.inputs.cl-parser-kit.follows = "cl-parser-kit";
      inputs.treefmt-nix.follows = "treefmt-nix";
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
