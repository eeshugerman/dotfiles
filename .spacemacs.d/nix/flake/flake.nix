# TODO:
# - treesitter modules
# - clang ls
# - pyright
# - fonts: all-the-icons, nerd icons. or just stick with the `install-fonts` commands
# - vscode-js-debug -- currently using (dap-js-setup)

{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, nixpkgs-unstable, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        pkgs-unstable = nixpkgs-unstable.legacyPackages.${system};
      in {
        packages.default = pkgs.buildEnv {
          name = "emacs-external-deps-env";
          paths = (with pkgs; [
            # pkgs.nerdfonts # didn't seem to work :/ also is slow

            coreutils-prefixed
            delta # diff syntax highlighter
            ispell
            # jdt-language-server # currently using lsp auto install
            nixd
            nixfmt-classic
            nodePackages.bash-language-server
            nodePackages.sql-formatter
            nodePackages.vscode-langservers-extracted # provides html, css, json, eslint
            nodePackages.yaml-language-server
            postgres-lsp
            python312 # for treemacs-use-git-mode 'extended or 'deferred
            ripgrep
            rust-analyzer
            shfmt
            vscode-extensions.angular.ng-template
          ]) ++ (with pkgs-unstable; [
            nodePackages.typescript # lsp-mode wants this (in addition to the language server)
            nodePackages.typescript-language-server
          ]);
        };
      });
}
