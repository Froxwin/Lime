{ pkgs ? import <nixpkgs> {
  overlays = [
    (nSelf: nSuper: {
      haskellPkgs = nSuper.haskellPackages.override (old: {
        overrides = nSelf.lib.fold nSelf.lib.composeExtensions
          (old.overrides or (_: _: { })) [
            (nSelf.haskell.lib.packageSourceOverrides { Lime = ./.; })
            (nSelf.haskell.lib.packagesFromDirectory { directory = ./nix; })
          ];
      });
    })
  ];
} }:
pkgs.haskellPkgs.Lime
