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
let
  tools = with pkgs; [
    cabal-install
    ghc
    stylish-haskell
    hlint
    haskellPackages.fourmolu
    haskellPackages.cabal-fmt
  ];

  hook = ''
    alias repl="cabal repl"
    alias build="cabal build"
    alias test="cabal test"
    alias doc="cabal haddock"
    alias purge="cabal clean"
    alias fmt="./Scripts/fmt.sh"
    alias lint="hlint ./Source"
    bin=$(cabal list-bin Lime)
    runLime () {
      if [ -f $bin  ]; then $bin; else echo "Executable not built"; fi;
    }
    alias Lime=runLime
    alias lime=runLime
  '';
in pkgs.haskellPkgs.Lime.env.overrideAttrs (oldEnv: {
  nativeBuildInputs = oldEnv.nativeBuildInputs ++ tools;
  shellHook = hook;
})
