{
  description = "my accounting";

  inputs = { nixpkgs.url = "github:nixos/nixpkgs/nixos-22.11"; };

  outputs = { self, nixpkgs }:
    let
      # Builds a map from <attr>=value to <attr>.<system>=value for each system.
      #
      #
      eachSystem = systems: f:
        let
          op = attrs: system:
            let
              ret = f system;
              op = attrs: key:
                attrs // {
                  ${key} = (attrs.${key} or { }) // { ${system} = ret.${key}; };
                };
            in builtins.foldl' op attrs (builtins.attrNames ret);
        in builtins.foldl' op { } systems;
      defaultSystems = [
        "aarch64-linux"
        "aarch64-darwin"
        "i686-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];

    in eachSystem defaultSystems (system:
      let
        pkgs = import nixpkgs { localSystem = system; };
        x86 = import nixpkgs {
          localSystem = "x86_64-darwin";
          overlays = [ ];
        };
      in with pkgs; {
        devShell = mkShell {
          buildInputs = [
            nixpkgs-fmt
            (if (system == "aarch64-darwin") then x86.ledger else ledger)
            gnuplot
            stack
            hledger
            hledger-web
            ghc
            zlib
            llvm
          ];

          shellHook = ''
            export LEDGER_FILE=~/mm/modes/ledgers/targets/all.journal
          '';
        };
      });
}
