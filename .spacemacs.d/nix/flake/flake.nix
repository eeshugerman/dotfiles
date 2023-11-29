{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.11";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in {
        packages.default = pkgs.buildEnv {
          name = "emacs-external-deps-env";
          paths = with pkgs; [
            # TODO:
            # - treesitter modules
            # - clang ls
            # - pyright
            # - ripgrep
            # - libvterm
            # - libgccjit
            # - fonts: all-the-icons, others?
            # - gls (mac only)
            # - vscode-js-debug -- currently using (dap-js-setup)

            # pkgs.nerdfonts # didn't seem to work :/ also is slow
            ispell
            nixfmt
            nodePackages.bash-language-server
            nodePackages.pyright
            nodePackages.sql-formatter
            nodePackages.typescript
            nodePackages.typescript-language-server
            nodePackages.vscode-html-languageserver-bin # do we need this given the below?
            nodePackages.vscode-langservers-extracted # provides html, css, json, eslint (but see note re: eslint in my/install-external-deps)
            nodePackages.yaml-language-server
            nodejs # not 100% sure this is needed
            python312 # for treemacs-use-git-mode 'extended or 'deferred
            # rnix-lsp
            rust-analyzer
            shfmt
            vscode-extensions.angular.ng-template
            # for yaml-ts-mode, but not working, more config needed probably
            # tree-sitter-grammars.tree-sitter-yaml
            (tree-sitter.withPlugins (p: [ p.tree-sitter-yaml ]))
          ];
        };
      });
}
