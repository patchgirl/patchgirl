let
  compiler = "ghc883";
  bootstrap = import <nixpkgs> {};

  patchgirl-web = {
    name = "patchgirl-web";
    src = pkgs.lib.sourceFilesBySuffices (pkgs.gitignoreSource ./patchgirl-web)[ ".cabal" ".hs" ".lhs" "LICENSE" ];
  };
  patchgirl-runner = {
    name = "patchgirl-runner";
    src = pkgs.lib.sourceFilesBySuffices (pkgs.gitignoreSource ./patchgirl-runner)[ ".cabal" ".hs" ".lhs" "LICENSE" ];
  };

  # Commit of the Nixpkgs repository that we want to use.
  nixpkgsVersion = import nix/nixpkgs-version.nix;

  # Nix files that describe the Nixpkgs repository. We evaluate the expression
  # using `import` below.
  nixpkgs =
    builtins.fetchTarball {
      url = "https://github.com/nixos/nixpkgs/archive/${nixpkgsVersion.rev}.tar.gz";
      sha256 = nixpkgsVersion.tarballHash;
    };

  allOverlays = import nix/overlays;

  overlays = [
    allOverlays.postgresql-default
    allOverlays.postgresql-legacy
    allOverlays.gitignore
    allOverlays.ghr
    (allOverlays.haskell-packages { inherit compiler; })
  ];

  pkgs = import nixpkgs { inherit overlays; };
  patches = pkgs.callPackage nix/patches {};

  lib = pkgs.haskell.lib;
  staticHaskellPackage = import nix/static-haskell-package.nix { inherit nixpkgs compiler patches allOverlays; } patchgirl-web patchgirl-runner;
in
rec {
  inherit nixpkgs pkgs;

  patchgirl-runner = pkgs.haskell.packages.${compiler}.patchgirl-runner;
  patchgirl-web = pkgs.haskell.packages.${compiler}.patchgirl-web;
  patchgirl-runner-static = lib.justStaticExecutables (lib.dontCheck staticHaskellPackage.patchgirl-runner);
  patchgirl-web-static = lib.justStaticExecutables (lib.dontCheck staticHaskellPackage.patchgirl-web);
}
