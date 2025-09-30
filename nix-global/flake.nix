# TODO: this sucks: bootstrap is required, upgrading is janky, gc breaks it. try https://github.com/lf-/flakey-profile
# TODO: move more stuff here from homebrew
# TODO: set flags in pkgs config (within this flake) instead of env vars

# to rebuild (eg after upgrade or changing packages):
# $ rm -rf ~/.local/state/nix/profiles/profile* && nix profile install --impure ~/nix-global
{
  description =
    "packages installed into default/global profile, for use on non-NixOS systems";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
  };
  outputs = { self, nixpkgs, nixpkgs-unstable }:
    let
      supportedSystems = [ "aarch64-darwin" ];
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
      pkgs = forAllSystems (system:
        import nixpkgs {
          inherit system;
          config.allowUnfreePredicate = pkg:
            builtins.elem (nixpkgs.lib.getName pkg) [
              "ngrok"
              "databricks-sql-cli"
              "snowsql"
            ];
        });
      pkgs-unstable =
        forAllSystems (system: nixpkgs-unstable.legacyPackages.${system});
    in {
      packages = forAllSystems (system: {
        default = pkgs.${system}.buildEnv {
          name = "global-env";
          paths = (with pkgs.${system}; [
            # ghostty # build is broken https://github.com/NixOS/nixpkgs/issues/388984
            # snowsql # arm64-apple-darwin not supported :(
            aws-vault # TODO: probably don't need this anymore since it's in bodata/mise/config.toml
            bash # nix-direnv needs a modern bash
            databricks-sql-cli
            direnv
            git # macos' built-in git is quite old
            gnupg
            helix
            htop
            ngrok
            nix-direnv
            postgresql # for psql
            sloccount
            teleport_16
            trino-cli
            yadm
            # TODO: Switch to https://github.com/nix-community/emacs-overlay, pull out into
            # dedicated repo that can be included here and in nixos config. combine with spacemacs
            # flake, add vterm, treesit grammars.
            emacs
          ]) ++ (with pkgs-unstable.${system};
            [
              mise # need a recent mise for gpg verification to work
            ]);
        };

      });
    };
}

