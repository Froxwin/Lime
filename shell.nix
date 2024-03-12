{ pkgs ? import <nixpkgs> {
  overlays = [
    (nSelf: nSuper: {
      haskellPkgs = nSelf.haskellPackages.override (old: {
        overrides = nSelf.lib.composeExtensions (old.overrides or (_: _: { }))
          (hSelf: hSuper: { Lime = hSelf.callCabal2nix "Lime" ./. { }; });
      });
    })
  ];
} }:
let
  tools = with pkgs; [ cabal-install ghc ffmpeg ];

  limeShellHook = ''
    alias repl="cabal repl"
    alias build="cabal build"
    alias test="cabal test"
    alias doc="cabal haddock"
    alias purge="cabal clean"
  '';
in pkgs.haskellPkgs.Lime.env.overrideAttrs (oldEnv: {
  nativeBuildInputs = oldEnv.nativeBuildInputs ++ tools;
  shellHook = limeShellHook;
})
