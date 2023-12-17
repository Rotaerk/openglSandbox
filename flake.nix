{
  description = "A sandbox for tinkering with OpenGL in Haskell.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      pkgs = import nixpkgs { system = "x86_64-linux"; };
      hs = pkgs.haskellPackages;
      OpenGLSandbox = hs.callCabal2nix "OpenGLSandbox" ./OpenGLSandbox {};
    in {

      packages.x86_64-linux = { inherit OpenGLSandbox; };

      packages.x86_64-linux.default = hs.shellFor {
        packages = p: [ OpenGLSandbox ];
        nativeBuildInputs =
          (with hs; [
            cabal-install
            haskell-language-server
          ]) ++
          (with pkgs; [
            libGL
          ]);
      };

    };
}
