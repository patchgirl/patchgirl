let
  compiler = "ghc883";
  bootstrap = import <nixpkgs> {};

  # pin nix channel
  nixpkgs = builtins.fromJSON (builtins.readFile ./nixpkgs.json);
  src = bootstrap.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpgs";
    inherit (nixpkgs) rev sha256;
  };

  config = {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages =
            pkgs.haskell.packages // {
              "${compiler}" = pkgs.haskell.packages."${compiler}".override {
                overrides =
                  let
                    toPackage = haskellPackagesNew: file : _: {
                      name = builtins.replaceStrings [ ".nix" ] [ "" ] file;
                      value = haskellPackagesNew.callPackage (./. + "/nix/${file}") {};
                    };
                  in
                    haskellPackagesNew: haskellPackagesOld: rec {
                      patchgirl-web =
                        pkgs.haskell.lib.dontHaddock
                          (pkgs.haskell.lib.dontCheck
                            (haskellPackagesNew.callPackage ./patchgirl-web/project.nix {}));
                      patchgirl-runner =
                        pkgs.haskell.lib.dontHaddock
                          (pkgs.haskell.lib.dontCheck
                            (haskellPackagesNew.callPackage ./patchgirl-runner/project.nix {}));
                    } // pkgs.lib.mapAttrs' (toPackage haskellPackagesNew) (builtins.readDir ./nix);
              };
            };
      };
    };
  };

  pkgs = import src { inherit config; };

in
{
  patchgirl-runner = pkgs.haskell.packages.${compiler}.patchgirl-runner;
  patchgirl-web = pkgs.haskell.packages.${compiler}.patchgirl-web;
}
