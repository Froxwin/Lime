let
  nixPkgs = import <nixpkgs> {
    overlays = [
      (nSelf: nSuper: {
        haskellPkgs = nSelf.haskellPackages.override (old: {
          overrides = nSelf.lib.composeExtensions (old.overrides or (_: _: {})) cabalOverlay;
        });
      })
    ];
  };

  cabalOverlay = (hSelf: hSuper: {
    Lime = hSelf.callCabal2nix "Lime" ./. {};
  });

  tools = with nixPkgs; [
    cabal-install
    ghc
    ffmpeg
  ];

  limeShellHook = ''
    alias repl="cabal repl"
    alias build="cabal build"
  '';
in
nixPkgs.haskellPkgs.Lime.env.overrideAttrs (oldEnv: {
  nativeBuildInputs = oldEnv.nativeBuildInputs ++ tools;
  shellHook = limeShellHook;
})