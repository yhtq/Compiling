{
  description = "Haskell development environment with Stack and Nix integration";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config = { allowBroken = true; };
        };
        hPkgs = pkgs.haskell.packages.ghc912;
        # haskellPackages = hPkgs.override {
        #   overrides = haskellPackagesNew: haskellPackagesOld: {
        #     hakyll = haskellPackagesOld.hakyll.overrideAttrs(old: {
        #       jailbreak = true; # jailbreak dependecies
        #     });
        #   };
        # };
        # allowNewer = pkgs.haskell.lib.doJailbreak;

        myDevTools = [
          hPkgs.ghc
          pkgs.cabal-install
          pkgs.stack
          pkgs.ormolu # Haskell formatter
          pkgs.hlint # Haskell codestyle checker
          # pkgs.hoogle # Lookup Haskell documentation
          hPkgs.haskell-language-server # LSP server for editor
          hPkgs.implicit-hie # auto generate LSP hie.yaml file from cabal
          # haskellPackages.heftia-effects
          # haskellPackages.text
          # haskellPackages.co-log
          # stack-wrapped
          pkgs.zlib # External C library needed by some Haskell packages
          # pkgs.gmp
        ];

        # Wrap Stack to work with our Nix integration. We do not want to modify
        # stack.yaml so non-Nix users do not notice anything.
        # - no-nix: We do not want Stack's way of integrating Nix.
        # --system-ghc    # Use the existing GHC on PATH (will come from this Nix file)
        # --no-install-ghc  # Do not try to install GHC if no matching GHC found on PATH
        # stack-wrapped = pkgs.symlinkJoin {
        #   name = "stack"; # will be available as the usual `stack` in terminal
        #   paths = [ pkgs.stack ];
        #   buildInputs = [ pkgs.makeWrapper ];
        #   postBuild = ''
        #     wrapProgram $out/bin/stack \
        #       --add-flags "\
        #         --no-nix \
        #         --system-ghc \
        #         --no-install-ghc \
        #       "
        #   '';
        # };
      in {
        devShells.default = pkgs.mkShell {
          buildInputs = myDevTools;

          # Make external Nix c libraries like zlib known to GHC, like
          # pkgs.haskell.lib.buildStackProject does
          # https://github.com/NixOS/nixpkgs/blob/d64780ea0e22b5f61cd6012a456869c702a72f20/pkgs/development/haskell-modules/generic-stack-builder.nix#L38
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath myDevTools;
        };
      });
}
