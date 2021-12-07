{
  # Flake dependency specification
  inputs = {
    # Nix package repository
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    # Convenience functions for writing flakes
    flake-utils.url = "github:numtide/flake-utils";
    # Legacy shim used in `./default.nix` and `./shell.nix`
    flake-compat = { url = "github:edolstra/flake-compat"; flake = false; };

    # Build rust crates from `Cargo.lock` dependencies
    naersk.url = "github:nmattia/naersk";
    # Avoid duplicate dependencies in `flake.lock`
    naersk.inputs.nixpkgs.follows = "nixpkgs";

    # Rust toolchain
    rust-overlay.url = "github:oxalica/rust-overlay";
    # Avoid duplicate dependencies in `flake.lock`
    rust-overlay.inputs.nixpkgs.follows = "nixpkgs";
    rust-overlay.inputs.flake-utils.follows = "flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, naersk, rust-overlay, ... }:
    # Build the output set for each default system and map system sets into
    # attributes, resulting in paths such as:
    #
    #     nix build .#packages.<system>.<name>
    flake-utils.lib.eachDefaultSystem (system:
      let
        # The rust toolcheckian to use for development
        # TODO: Figure out how to let users select this?
        rustChannel = "1.56.0";
        crateName = "fathom";

        # Package set with the rust overlay included added
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ (import rust-overlay) ];
        };
      in
      {
        # Used by `nix build .#<name>`
        packages.${crateName} = naersk.lib.${system}.buildPackage {
          pname = crateName;
          root = ./.;
        };

        # Used by `nix run`
        defaultPackage = self.packages.${system}.${crateName};

        # Used by `nix run .#<name>`
        apps.${crateName} = flake-utils.lib.mkApp {
          drv = self.packages.${system}.${crateName};
        };

        # Executed by `nix run . -- <args?>`
        defaultApp = self.apps.${system}.${crateName};

        # Used by `nix develop`
        devShell = pkgs.mkShell {
          # Tools needed for development go here.
          buildInputs = [
            pkgs.rust-bin.stable.${rustChannel}.default
            pkgs.clippy
          ];

          # Certain Rust tools (like `rust-analyzer`) won't work without this
          RUST_SRC_PATH =
            "${pkgs.rust-bin.stable.${rustChannel}.rust-src}/lib/rustlib/src/rust/library";
        };
      }
    );
}
