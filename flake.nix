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
        # minimumRustVersion = (builtins.fromTOML (builtins.readFile ./Cargo.toml)).package.rust-version;
        minimumRustVersion = "1.56.0";

        # Setup Rust toolchains to build and test against
        rust = {
          nightly = pkgs.rust-bin.nightly.latest.minimal;
          stable = pkgs.rust-bin.stable.latest.minimal;
          minimum = pkgs.rust-bin.stable.${minimumRustVersion}.minimal;
        };

        # Override Naersk with the MSRV version of Rust
        naersk-lib = {
          # nightly = naersk.lib."${system}".override { cargo = rust.nightly; rustc = rust.nightly; };
          # stable = naersk.lib."${system}".override { cargo = rust.stable; rustc = rust.stable; };
          minimum = naersk.lib."${system}".override { cargo = rust.minimum; rustc = rust.minimum; };
        };
      in
      {
        # Executed by `nix flake check`
        checks = {
          # Check Rust crate tests
          # TODO: test using `rust.nightly`, `rust.stable`, and `rust.minimum`
          ${crateName} = naersk-lib.minimum.buildPackage {
            pname = crateName;
            root = ./.;
            doCheck = true;
          };

          # Check Rust formatting
          rustfmt = pkgs.runCommand "check-rustfmt"
            {
              buildInputs = [
                (rust.stable.override { extensions = [ "rustfmt" ]; })
              ];
            }
            ''
              mkdir $out
              cargo fmt --manifest-path ${./.}/Cargo.toml -- --check
            '';

          # Check Nix formatting
          nixpkgs-fmt = pkgs.runCommand "check-nixpkgs-fmt"
            {
              buildInputs = [ pkgs.nixpkgs-fmt ];
            }
            ''
              mkdir $out
              nixpkgs-fmt --check ${./.}
            '';
        };

        # Executed by `nix build .#<name>`
        packages.${crateName} = naersk-lib.minimum.buildPackage {
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


        # Used by `nix develop .#<name>`
        devShells = (
          let
            # Creates a development shell using a specific Rust toolchain
            createShell = { rust }: pkgs.mkShell {
              packages = [ (rust.override { extensions = [ "rust-src" "rustfmt" ]; }) ];
              # Certain tools like `rust-analyzer` won't work without this
              RUST_SRC_PATH = "${rust}/lib/rustlib/src/rust/library";
            };
          in
          {
            nightly = createShell { rust = rust.nightly; };
            stable = createShell { rust = rust.stable; };
            minimum = createShell { rust = rust.minimum; };
          }
        );

        # Used by `nix develop`
        devShell = self.devShells.${system}.stable;
      }
    );
}
