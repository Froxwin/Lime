{
  description = "Lime flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = inputs@{self, nixpkgs}: let
    system = "x86_64-linux";

    overlays = [
      (final: prev: {
        haskellPkgs = prev.haskellPackages.override (old: {
          overrides = final.lib.fold final.lib.composeExtensions
            (old.overrides or (_: _: {})) [
              (final.haskell.lib.packageSourceOverrides { Lime = ./.; })
              (final.haskell.lib.packagesFromDirectory { directory = ./nix; })
            ];
        });
      })
    ];

    pkgs = import nixpkgs {
      inherit system overlays;
    };

    tools = with pkgs; [
      cabal-install
      ghc
      stylish-haskell
      hlint
      haskellPkgs.fourmolu
      haskellPkgs.cabal-fmt
    ];

    shellHook = ''
      alias repl="cabal repl"
      alias build="cabal build"
      alias test="cabal test"
      alias doc="cabal haddock"
      alias purge="cabal clean"
      alias fmt="./Scripts/fmt.sh"
      alias lint="hlint ./Source"

      bin=$(cabal list-bin Lime)
      runLime () {
        if [ -f $bin ]; then $bin; else echo "Executable not built"; fi
      }
      alias Lime=runLime
      alias lime=runLime
    '';
  in {
    packages.${system}.default = pkgs.haskellPkgs.Lime;

    devShells.${system}.default = pkgs.haskellPkgs.Lime.env.overrideAttrs (old: {
      nativeBuildInputs = (old.nativeBuildInputs or []) ++ tools;
      inherit shellHook;
    });
  };
}
