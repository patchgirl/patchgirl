# Derive a fully static Haskell package based on musl instead of glibc.
{ nixpkgs, compiler, patches, allOverlays }:

patchgirl-web: patchgirl-runner:
let
  # The nh2/static-haskell-nix project does all the hard work for us.
  static-haskell-nix =
    let
      rev = "749707fc90b781c3e653e67917a7d571fe82ae7b";
    in
    builtins.fetchTarball {
      url = "https://github.com/nh2/static-haskell-nix/archive/${rev}.tar.gz";
      sha256 = "155spda2lww378bhx68w6dxwqd5y6s9kin3qbgl2m23r3vmk3m3w";
    };

  patched-static-haskell-nix = patches.applyPatches
    "patched-static-haskell-nix"
    static-haskell-nix
    [
      patches.static-haskell-nix-postgrest-openssl-linking-fix
      patches.static-haskell-nix-patchgirl-openssl-linking-fix
      patches.static-haskell-nix-hasql-notifications-openssl-linking-fix
    ];

  patchedNixpkgs = patches.applyPatches
    "patched-nixpkgs"
    nixpkgs
    [
      patches.nixpkgs-revert-ghc-bootstrap
      patches.nixpkgs-openssl-split-runtime-dependencies-of-static-builds
    ];

  extraOverrides = final: prev:
    rec {
      "${patchgirl-web.name}" = prev.callCabal2nix "${patchgirl-web.name}" patchgirl-web.src {};
      "${patchgirl-runner.name}" = prev.callCabal2nix "${patchgirl-runner.name}" patchgirl-runner.src {};
    };

  overlays = [
    (allOverlays.haskell-packages { inherit compiler extraOverrides; })
  ];

  normalPkgs = import patchedNixpkgs { inherit overlays; };

  # Each version of GHC needs a specific version of Cabal.
  defaultCabalPackageVersionComingWithGhc = { ghc883 = "Cabal_3_2_0_0"; }."${compiler}";

  # The static-haskell-nix 'survey' derives a full static set of Haskell
  # packages, applying fixes where necessary.
  survey = import "${patched-static-haskell-nix}/survey" { inherit normalPkgs compiler defaultCabalPackageVersionComingWithGhc; };
in
{
  "${patchgirl-web.name}" = survey.haskellPackages."${patchgirl-web.name}";
  "${patchgirl-runner.name}" = survey.haskellPackages."${patchgirl-runner.name}";
}
