{
  # Flake dependency specification
  #
  # To update individual inputs use:
  #
  # ```
  # nix flake lock --update-input <input>
  # ```
  inputs = {
    # Nix package repository
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    # Convenience functions for writing flakes
    flake-utils.url = "github:numtide/flake-utils";
    # Legacy shim used in `./default.nix` and `./shell.nix`
    flake-compat = { url = "github:edolstra/flake-compat"; flake = false; };

    # Build rust crates from `Cargo.lock` dependencies
    naersk.url = "github:nmattia/naersk";
    naersk.inputs.nixpkgs.follows = "nixpkgs";

    # Rust toolchain
    rust-overlay.url = "github:oxalica/rust-overlay";
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
        # Package set with the rust overlay included added
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ (import rust-overlay) ];
        };

        # Crate containing the `fathom` binary
        crateName = "fathom";
        # TODO: find the manifest for ${crateName} in the workspace
        # minRustVersion = (builtins.fromTOML (builtins.readFile ./Cargo.toml)).package.rust-version;
        minRustVersion = "1.56.0";

        # Setup Rust toolchains to build and test against
        rustNightly = pkgs.rust-bin.nightly.latest.minimal;
        rustStable = pkgs.rust-bin.stable.latest.minimal;
        rustMin = pkgs.rust-bin.stable.${minRustVersion}.minimal;

        # Override Naersk with the MSRV version of Rust
        naerskLibMin = naersk.lib."${system}".override {
          cargo = rustMin;
          rustc = rustMin;
        };

        # Creates a development shell using a specific version of Rust
        createShell = { rust }: pkgs.mkShell {
          packages = [ (rust.override { extensions = [ "rust-src" "rustfmt" ]; }) ];
          # Certain tools like `rust-analyzer` won't work without this
          RUST_SRC_PATH = "${rust}/lib/rustlib/src/rust/library";
        };
      in
      {
        # Executed by `nix build .#<name>`
        packages.${crateName} = naerskLibMin.buildPackage {
          pname = crateName;
          root = ./.;
        };

        # Executed by `nix build`
        defaultPackage = self.packages.${system}.${crateName};


        # Executed by `nix run .#<name>`
        apps.${crateName} = flake-utils.lib.mkApp {
          drv = self.packages.${system}.${crateName};
        };

        # Executed by `nix run . -- <args?>`
        defaultApp = self.apps.${system}.${crateName};


        # Use `nix develop .#stableShell` to enter each dev env.
        packages.nightlyShell = createShell { rust = rustNightly; };
        packages.stableShell = createShell { rust = rustStable; };
        packages.msrvShell = createShell { rust = rustMin; };

        # Used by `nix develop`
        devShell = self.packages.${system}.stableShell;
      }
    );
}
