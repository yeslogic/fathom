# A Nix flake that initialises a development environment for Fathom
#
# NOTE: A Nix environment is not required to work on Fathom, but this Flake is
# provided for your convinience if you already are.

{
  # Flake dependency specification
  #
  # To update all flake inputs:
  #
  #     $ nix flake update --commit-lockfile
  #
  # To update individual flake inputs:
  #
  #     $ nix flake lock --update-input <input> ... --commit-lockfile
  #
  inputs = {
    # Nix package repository
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    # Convenience functions for writing flakes
    flake-utils.url = "github:numtide/flake-utils";

    # Rust toolchain
    rust-overlay.url = "github:oxalica/rust-overlay";
    rust-overlay.inputs.nixpkgs.follows = "nixpkgs";
    rust-overlay.inputs.flake-utils.follows = "flake-utils";
  };

  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    # Build the output set for each default system and map system sets into
    # attributes, resulting in paths such as:
    #
    #     $ nix build .#packages.<system>.<name>
    #
    flake-utils.lib.eachDefaultSystem (system:
      let
        # Package set with the rust overlay included added
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ (import inputs.rust-overlay) ];
        };

        # Library functions from nixpkgs
        lib = pkgs.lib;

        # Load the minimum supported Rust version (MSRV) from the manifest
        fathomManifest = lib.importTOML ./fathom/Cargo.toml;
        minimumRustVersion = fathomManifest.package.rust-version;

        # Setup Rust toolchains to build and test against
        #
        # The names of the toolchains will be used as the names of the
        # development shells loaded by the `nix develop .#<name>` command (see
        # the `devShells` flake output defined below).
        rustToolchains = {
          nightly = pkgs.rust-bin.nightly.latest.minimal;
          stable = pkgs.rust-bin.stable.latest.minimal;
          minimum = pkgs.rust-bin.stable.${minimumRustVersion}.minimal;
        };
      in
      {
        # Development shells
        #
        #    $ nix develop .#<name>
        #    $ nix develop .#<name> --command cargo check
        #
        # [Direnv](https://direnv.net/) is recommended for automatically loading the
        # development environemnts provided in your current shell. For example:
        #
        #    $ echo "use flake" > .envrc && direnv allow
        #    $ cargo check
        #
        # If you want to live on the bleeding edge, you could also try using the
        # nightly shell with the following `.envrc` file:
        #
        #    use flake .#nightly
        #
        # If you choose to use Direnv, note that `.envrc` should be added to
        # your local git excludes, or added to to your global gitignore.
        devShells = {
          # Use the stable toolchain by default for development, to get the
          # latest diagnostics and compiler improvements.
          #
          #    $ nix develop
          #    $ nix develop --command cargo check
          #
          default = self.devShells.${system}.stable;
        } // (
          # Map over the `rustToolchains` defined above, creating a shell
          # environment for each. This is useful for testing regressions against
          # the minimum supported Rust version. For example:
          #
          #     $ nix develop .#minimum --command cargo check
          #
          pkgs.lib.mapAttrs
            (name: rustToolchain:
              let
                rustWithExtensions = rustToolchain.override {
                  extensions = [ "rust-src" "rustfmt" "clippy" ];
                };
              in
              pkgs.mkShell {
                name = "${name}-shell";

                packages = [
                  rustWithExtensions
                  pkgs.nixpkgs-fmt
                ];

                # Print backtraces on panics
                RUST_BACKTRACE = 1;

                # Certain tools like `rust-analyzer` won't work without this
                RUST_SRC_PATH = "${rustWithExtensions}/lib/rustlib/src/rust/library";
              })
            rustToolchains
        );


        # Flake checks
        #
        #     $ nix flake check
        #
        checks = {
          # Check Nix formatting
          nixpkgs-fmt = pkgs.runCommand "check-nixpkgs-fmt"
            { buildInputs = [ pkgs.nixpkgs-fmt ]; }
            ''
              echo "checking nix formatting"
              nixpkgs-fmt --check ${./flake.nix}
              touch $out
            '';
        };
      }
    );
}
