# TODO: this sucks: bootstrap is required, upgrading is janky, gc breaks it. try https://github.com/lf-/flakey-profile
# TODO: move more stuff here from homebrew
# TODO: set flags in pkgs config (within this flake) instead of env vars
# NOTE: need env var workarounds because https://github.com/NixOS/nixpkgs/issues/42900
# NOTE: this replaces bodata/nix/global-deps.nix

# to upgrade:
# $ rm -rf ~/.local/state/nix/profiles/profile*
# $ NIXPKGS_ALLOW_UNFREE=1 nix profile install --impure ~/nix-global
{
  description =
    "packages installed into default/global profile, for use on non-NixOS systems";
  inputs = { nixpkgs.url = "github:NixOS/nixpkgs/24.11"; };
  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-darwin" "aarch64-darwin" ];
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
      pkgs = forAllSystems (system: nixpkgs.legacyPackages.${system});
    in {
      packages = forAllSystems (system: {
        default = pkgs.${system}.buildEnv {
          name = "global-env";
          paths = with pkgs.${system}; [
            direnv
            bash # nix-direnv needs a modern bash
            nix-direnv
            # databricks-sql-cli
            # snowsql # fails to build :(
            trino-cli
            ngrok
            sloccount
            gnupg
            aws-vault
            htop
            teleport_16
          ];
        };

      });
    };
}

