{
  description = "miscellaneous nix pkgs, grouped in one place";

  inputs = {
    nixpkgs.url     = github:nixos/nixpkgs/be44bf67; # nixos-22.05 2022-10-15
    flake-utils.url = github:numtide/flake-utils/c0e246b9;
    bashHeader      = {
      url    = github:sixears/bash-header/5206b087;
      inputs = { nixpkgs.follows = "nixpkgs"; };
    };
  };

  outputs = { self, nixpkgs, flake-utils, bashHeader }:
    flake-utils.lib.eachSystem ["x86_64-linux"] (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        bash-header = bashHeader.packages.${system}.bash-header;
      in
        rec {
          packages = flake-utils.lib.flattenTree (with pkgs; {
            touchpad      = import ./touchpad.nix { inherit pkgs bash-header; };
            byobu         = import ./byobu.nix    { inherit pkgs; };
            swap-summarize= import ./swap-summarize.nix { inherit pkgs; };
          });
        }
    );
}
