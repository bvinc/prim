# spell-checker:ignore bintools gnum gperf ldflags libclang nixpkgs numtide pkgs texinfo gettext
{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";

    # <https://github.com/nix-systems/nix-systems>
    systems.url = "github:nix-systems/default";
  };

  outputs = inputs: let
    inherit (inputs.nixpkgs) lib legacyPackages;
    eachSystem = lib.genAttrs (import inputs.systems);
    pkgsFor = legacyPackages;
  in {
    devShells = eachSystem (
      system: let
        build_deps = with pkgsFor.${system}; [
          clang
          llvmPackages.bintools
          grcov
          rustup

          pre-commit

          # debugging
          gdb
        ];
      in {
        default = pkgsFor.${system}.pkgs.mkShell {
          packages = build_deps;

          RUSTC_VERSION = "1.88";
          LIBCLANG_PATH = pkgsFor.${system}.lib.makeLibraryPath [pkgsFor.${system}.llvmPackages_latest.libclang.lib];
          shellHook = ''
            export PATH=$PATH:''${CARGO_HOME:-~/.cargo}/bin
            export PATH=$PATH:''${RUSTUP_HOME:-~/.rustup}/toolchains/$RUSTC_VERSION-x86_64-unknown-linux-gnu/bin/
          '';

        };
      }
    );
  };
}
