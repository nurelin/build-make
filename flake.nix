{
  description = "haskell-template's description";
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };
  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        overlays = [ ];
        pkgs =
          import nixpkgs { inherit system overlays; config.allowBroken = true; };
        project = returnShellEnv:
          pkgs.haskellPackages.developPackage {
            inherit returnShellEnv;
            name = "build-make";
            root = ./.;
            withHoogle = false;
            overrides = self: super: with pkgs.haskell.lib; {
              # Use callCabal2nix to override Haskell dependencies here
              # cf. https://tek.brick.do/K3VXJd8mEKO7
              build =
                pkgs.haskell.lib.dontCheck (
                  self.callCabal2nix "build" (pkgs.fetchgit {
                    url = "https://github.com/snowleopard/build.git";
                    sha256 = "sha256-qFGyDlCGq/QMlSJ8/hISskz+DANNdxENKWGtmd3iM4o=";
                    rev = "89adf989e2276075915e4c9b53a41315a013e822";
                    fetchSubmodules = true;
                  }) { });
              #algebraic-graphs = pkgs.haskell.lib.dontCheck (
              #  self.callHackageDirect {
              #    pkg = "algebraic-graphs";
              #    ver = "0.1.1.1";
              #    sha256 = "sha256-t81zoJnhpmPBQHdJNrwC6Y1RM2yRY0daYs9XifSsq0g=";
              #  } { });
              #base-compat = pkgs.haskell.lib.dontCheck (
              #  self.callHackageDirect {
              #    pkg = "base-compat";
              #    ver = "0.10.5";
              #    sha256 = "sha256-huFjfRijAFAPNRjXLDVnbKDPNNWO5raMHJ/TdkhHAzs=";
              #  } { });
              #containers = pkgs.haskell.lib.dontCheck (
              #  self.callHackageDirect {
              #    pkg = "containers";
              #    ver = "0.5.11.0";
              #    sha256 = "sha256-y2OtzXTc17NQMlS8/TYQmZ/K2BorY0Wo/RK/TmDADZU=";
              #  } { });
              #extra = pkgs.haskell.lib.dontCheck (
              #  self.callHackageDirect {
              #    pkg = "extra";
              #    ver = "1.6.21";
              #    sha256 = "sha256-x8z9kPP/TSBlwGDOVSh4XSwoWeakukoMPh5WDtYjE3Q=";
              #  } { });
            };
            modifier = drv:
              pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
              [
                # Specify your build/dev dependencies here. 
                cabal-fmt
                cabal-install
                ghcid
                haskell-language-server
                ormolu
                pkgs.nixpkgs-fmt
              ]);
          };
      in
      {
        # Used by `nix build` & `nix run` (prod exe)
        defaultPackage = project false;

        # Used by `nix develop` (dev shell)
        devShell = project true;
      });
}
