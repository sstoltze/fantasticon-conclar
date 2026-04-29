{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  outputs =
    { nixpkgs, ... }:
    let
      forEachSystem = (
        f:
        nixpkgs.lib.genAttrs
          [
            "aarch64-darwin"
            "x86_64-linux"
          ]
          (
            system:
            f {
              inherit system;
              pkgs = import nixpkgs { inherit system; };
            }
          )
      );
    in
    {
      devShells = forEachSystem (
        { pkgs, ... }:
        {
          default = pkgs.mkShell { packages = with pkgs; [ nodejs racket ]; };
        }
      );
    };
}
