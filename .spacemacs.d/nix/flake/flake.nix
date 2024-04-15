# TODO:
# - treesitter modules
# - clang ls
# - pyright
# - ripgrep
# - libgccjit
# - fonts: all-the-icons, others?
# - gls (mac only)
# - vscode-js-debug -- currently using (dap-js-setup)

{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.11";
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

            ispell
            jdt-language-server
            nixd
            nixfmt
            nodePackages.bash-language-server
            nodePackages.pyright
            nodePackages.sql-formatter
            nodePackages.vscode-langservers-extracted # provides html, css, json, eslint
            nodePackages.yaml-language-server
            python312 # for treemacs-use-git-mode 'extended or 'deferred
            rust-analyzer
            shfmt
            tree-sitter-grammars.tree-sitter-yaml
            vscode-extensions.angular.ng-template
            (tree-sitter.withPlugins (p: [ p.tree-sitter-yaml ]))

          ]) ++ (with pkgs-unstable; [
            nodePackages.typescript # lsp-mode wants this (in addition to the language server)
            nodePackages.typescript-language-server
          ]);
        };
      });
}
