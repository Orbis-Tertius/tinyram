{
  nixConfig.bash-prompt = "[nix-develop-tinyram:] ";
  description = "A vnTinyRAM emulator";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.sydtest-src = {
      url = "github:NorfairKing/sydtest/a230bf791b0bced918092bbc9c3b54608b2a3a48";
      flake = false;
    };

  outputs = { self, nixpkgs, flake-utils, sydtest-src, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        deferPluginErrors = true;
        overlays = [
          haskellNix.overlay
          (import "${sydtest-src}/nix/overlay.nix")
          (final: prev: {
            # This overlay adds our project to pkgs
            tinyram =
              final.haskell-nix.project' {
                src = ./.;
                compiler-nix-name = "ghc8107";
                projectFileName = "stack.yaml";
                modules = [{
                  packages = {
                  };
                }];
                shell.tools = {
                  cabal = { };
                  ghcid = { };
                  hlint = { };
                  haskell-language-server = { };
                  stylish-haskell = { };
                  sydtest-discover = { };
                };
                # Non-Haskell shell tools go here
                shell.buildInputs = with pkgs; [
                  nixpkgs-fmt
                ];
                shell.shellHook =
                  ''
                  manual-ci() (
                    set -e

                    ./ci/lint.sh
                    cabal test
                    nix-build
                    ./ci/examples.sh
                  )
                  '';
              };
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        flake = pkgs.tinyram.flake { };
      in flake // {
        defaultPackage = flake.packages."tinyram:exe:tinyram";
      });
}
