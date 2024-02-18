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
        pkgs              = nixpkgs.legacyPackages.${system};
        bash-header       = bashHeader.packages.${system}.bash-header;
      in
        {
          settings = { vlc-lockfile      = "/run/user/$uid/vlc.pid";
                       swap-summary-fifo = "/run/user/$uid/swap-summary";
                       cpu-temp-fifo     = "/run/user/$uid/cpu-temp";
                     };

          packages = flake-utils.lib.flattenTree (with pkgs; rec {
            swap-summarize = import ./swap-summarize.nix { inherit pkgs; };
            swap-summary  = import ./swap-summary.nix
                                               { inherit pkgs swap-summarize; };
            cpu-temperature  = import ./cpu-temperature.nix
                                               { inherit pkgs cpu-temp; };
            touchpad      = import ./touchpad.nix
                                               { inherit pkgs bash-header; };
            pidkill       = import ./pidkill.nix       { inherit pkgs; };
            vlcp          = import ./vlcp.nix          { inherit pkgs; };
            cpu-temp      = import ./cpu-temp.nix      { inherit pkgs; };
            flock-pid-run = import ./flock-pid-run.nix { inherit pkgs; };
            replace       =
              let
                src             = pkgs.lib.strings.fileContents ./replace.hs;
                libraries       =
                  with pkgs.haskellPackages; [ base-unicode-symbols
                                               classy-prelude
                                               optparse-applicative ];
                writeHaskellBin = pkgs.writers.writeHaskellBin;
              in
                writeHaskellBin "replace" { inherit libraries; } src;
          });
        }
    );
}
