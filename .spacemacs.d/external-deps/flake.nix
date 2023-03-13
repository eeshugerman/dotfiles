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
            # TODO: pyright, ripgrep, gls (on mac)
            pkgs.nodePackages.sql-formatter
            pkgs.nodePackages.typescript-language-server
            pkgs.nodePackages.vscode-html-languageserver-bin
            # provides html, css, json, eslint (but see note below re: eslint in my/install-external-deps)
            pkgs.nodePackages.vscode-langservers-extracted
            pkgs.nodejs #  not 100% sure this is neede
            pkgs.rubyPackages_3_0.solargraph
            pkgs.vscode-extensions.angular.ng-template
          ];
        };
      });
}
