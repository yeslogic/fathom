# For compatability with legacy tools that are not yet flake-aware
#
# - https://nixos.wiki/wiki/Flakes#Using_flakes_project_from_a_legacy_Nix

let
  # Use lockfile to simplify hash updates for `flake-compat`.
  lock = builtins.fromJSON (builtins.readFile ./flake.lock);
  flake-compat = fetchTarball {
    url = "https://github.com/edolstra/flake-compat/archive/${lock.nodes.flake-compat.locked.rev}.tar.gz";
    sha256 = lock.nodes.flake-compat.locked.narHash;
  };
in

(import flake-compat {
  src = ./.;
}).shellNix
