{
  # Flake dependency specification
  inputs = {
    # Nix package repository
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    # Convenience functions for writing flakes
    flake-utils.url = "github:numtide/flake-utils";
    # Legacy shim used in `./default.nix` and `./shell.nix`
    flake-compat = { url = "github:edolstra/flake-compat"; flake = false; };

    # Rust toolchain
    rust-overlay.url = "github:oxalica/rust-overlay";
    # Avoid duplicate dependencies in `flake.lock`
    rust-overlay.inputs.nixpkgs.follows = "nixpkgs";
    rust-overlay.inputs.flake-utils.follows = "flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, rust-overlay, ... }:
    # Build the output set for each default system and map system sets into
    # attributes, resulting in paths such as:
    #
    #     nix build .#packages.<system>.<name>
    flake-utils.lib.eachDefaultSystem (system:
      let
        # The rust toolcheckian to use for development
        # TODO: Figure out how to let users select this?
        rustChannel = "1.56.0";

        # Package set with the rust overlay included added
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ (import rust-overlay) ];
        };
      in
      {
        # Used by `nix develop`
        devShell = pkgs.mkShell {
          # Tools needed for development go here.
          buildInputs = [
            pkgs.rust-bin.stable.${rustChannel}.default
            pkgs.clippy
          ];

          # Certain Rust tools won't work without this
          RUST_SRC_PATH =
            "${pkgs.rust-bin.stable.${rustChannel}.rust-src}/lib/rustlib/src/rust/library";
        };
      }
    );
}
