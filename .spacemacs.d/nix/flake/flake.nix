{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
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

            # pkgs.nerdfonts # didn't seem to work :/ also is slow
            ispell
            nixfmt
            nodePackages.bash-language-server # configuration needed?
            nodePackages.pyright
            nodePackages.sql-formatter
            nodePackages.typescript-language-server
            nodePackages.vscode-html-languageserver-bin # do we need this given the below?
            nodePackages.vscode-langservers-extracted # provides html, css, json, eslint (but see note re: eslint in my/install-external-deps)
            nodejs #  not 100% sure this is needed
            rnix-lsp
            rubyPackages_3_0.solargraph
            shfmt
            vscode-extensions.angular.ng-template
          ];
        };
      });
}
