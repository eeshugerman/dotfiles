# TODO: bootstrap is required and upgrading is janky. try https://github.com/lf-/flakey-profile
# NOTE: need env var workarounds because https://github.com/NixOS/nixpkgs/issues/42900
# NOTE: this replaces bodata/nix/global-deps.nix
# TODO: move more stuff here from homebrew

# to upgrade:
# $ rm -rf ~/.local/state/nix/profiles/profile*
# $ NIXPKGS_ALLOW_UNSUPPORTED_SYSTEM=1 NIXPKGS_ALLOW_BROKEN=1 NIXPKGS_ALLOW_UNFREE=1 nix profile install --impure ~/nix-global
{
  description = "packages installed into default/global profile, for use on non-NixOS systems";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/23.11";
  };
  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-darwin" ];
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
      pkgs = forAllSystems (system: nixpkgs.legacyPackages.${system});
    in {
      packages = forAllSystems (system: {
        default = pkgs.${system}.buildEnv {
          name = "global-env";
          paths = with pkgs.${system};
            [
              direnv
              # bash # try adding this if nix-direnv throws syntax errors
              nix-direnv
              databricks-sql-cli
              # snowsql # fails to build :(
              trino-cli
              ngrok
            ];

          # the below is from bodata/nix/global-deps.nix, not sure if we want it
          # pathsToLink = [ "/share" "/bin" ];
        };

      });
    };
}

