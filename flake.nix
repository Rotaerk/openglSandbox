{
  description = "A sandbox for tinkering with OpenGL in Haskell.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      pkgs = import nixpkgs { system = "x86_64-linux"; };
      hs = pkgs.haskellPackages;
      openglSandbox = hs.callCabal2nix "openglSandbox" ./. {};
    in {

      packages.x86_64-linux = { inherit openglSandbox; };

      packages.x86_64-linux.default = hs.shellFor {
        packages = p: [ openglSandbox ];
        nativeBuildInputs = with hs; [
          cabal-install
          haskell-language-server
        ];
      };

    };
}
