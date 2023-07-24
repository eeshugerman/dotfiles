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
          paths = [
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
            pkgs.nodePackages.bash-language-server # configuration needed?
            pkgs.nodePackages.pyright
            pkgs.nodePackages.sql-formatter
            pkgs.nodePackages.typescript-language-server
            pkgs.nodePackages.vscode-html-languageserver-bin # do we need this given the below?
            pkgs.nodePackages.vscode-langservers-extracted # provides html, css, json, eslint (but see note re: eslint in my/install-external-deps)
            pkgs.nodejs #  not 100% sure this is needed
            pkgs.rubyPackages_3_0.solargraph
            pkgs.vscode-extensions.angular.ng-template

          ];
        };
      });
}
