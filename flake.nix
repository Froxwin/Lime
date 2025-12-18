{
  description = "Lime flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs =
    inputs@{ self, nixpkgs }:
    let
      system = "x86_64-linux";

      overlays = [
        (final: prev: {
          haskellPkgs = prev.haskell.packages.ghc912.override (old: {
            overrides = final.lib.fold final.lib.composeExtensions (old.overrides or (_: _: { })) [
              (final.haskell.lib.packageSourceOverrides { Lime = ./.; })
              (final.haskell.lib.packagesFromDirectory { directory = ./nix; })
            ];
          });
        })
      ];

      pkgs = import nixpkgs {
        inherit system overlays;
        config.allowUnfree = true;
      };

      tools = with pkgs; [
        cabal-install
        ghc
        hlint
        haskellPkgs.fourmolu
        # haskellPkgs.cabal-fmt
        haskellPkgs.haskell-language-server
        pkg-config
        cudaPackages.cuda_cudart
        cudaPackages.cuda_nvcc
        cudaPackages.cuda_cccl
        clang-tools
        # llvmPackages_15.llvm
        # llvmPackages_15.clang
      ];

      shellHook = ''
        export LIBRARY_PATH=/run/opengl-driver/lib:/run/opengl-driver/lib64:$LIBRARY_PATH
        export CUDA_PATH=${pkgs.cudaPackages.cuda_nvcc}
        export LD_LIBRARY_PATH=/run/opengl-driver/lib:/run/opengl-driver/lib64:$LD_LIBRARY_PATH
      '';
    in
    {
      packages.${system}.default = pkgs.haskellPkgs.Lime;

      devShells.${system} = {
        default = pkgs.haskellPkgs.Lime.env.overrideAttrs (old: {
          nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ tools;
          inherit shellHook;
        });
      };
    };
}
