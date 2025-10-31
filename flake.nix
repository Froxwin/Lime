{
  description = "Lime flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = inputs @ {
    self,
    nixpkgs,
  }: let
    system = "x86_64-linux";

    overlays = [
      (final: prev: {
        haskellPkgs = prev.haskellPackages.override (old: {
          overrides =
            final.lib.fold final.lib.composeExtensions
            (old.overrides or (_: _: {})) [
              (final.haskell.lib.packageSourceOverrides {Lime = ./.;})
              (final.haskell.lib.packagesFromDirectory {directory = ./nix;})
            ];
        });

        profilingHaskellPkgs = prev.haskellPackages.override (old: {
          overrides =
            final.lib.fold final.lib.composeExtensions
            (old.overrides or (_: _: {})) [
              (final.haskell.lib.packageSourceOverrides {Lime = ./.;})
              (final.haskell.lib.packagesFromDirectory {directory = ./nix;})
              (self: super: {
                mkDerivation = args:
                  super.mkDerivation (args // { enableLibraryProfiling = true; });
              })
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
      hlint
      haskellPkgs.fourmolu
      haskellPkgs.cabal-fmt
      haskellPkgs.haskell-language-server
    ];

    shellHook = ''
    '';
  in {
    packages.${system}.default = pkgs.haskellPkgs.Lime;

    devShells.${system} = {
      default = pkgs.haskellPkgs.Lime.env.overrideAttrs (old: {
        nativeBuildInputs = (old.nativeBuildInputs or []) ++ tools;
        inherit shellHook;
      });

      profiling = pkgs.profilingHaskellPkgs.Lime.env.overrideAttrs (old: {
        nativeBuildInputs = (old.nativeBuildInputs or []) ++ tools;
        buildInputs = (old.buildInputs or []) ++ [ pkgs.profilingHaskellPkgs.ghc ];
      });
    };
  };
}
