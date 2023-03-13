{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in {
        packages = {
          # TODO: pyright, ripgrep, gls (on mac)

          sql-formatter = pkgs.nodePackages.sql-formatter;
          typescript-language-server  = pkgs.nodePackages.typescript-language-server ;
          vscode-html-languageserver-bin = pkgs.nodePackages.vscode-html-languageserver-bin;
          # provides html, css, json, eslint (but see note below re: eslint in my/install-external-deps)
          vscode-langservers-extracted = pkgs.nodePackages.vscode-langservers-extracted;
          nodejs = pkgs.nodejs; #  not 100% sure this is needed
          solargraph = pkgs.rubyPackages_3_0.solargraph;
          ng-template = pkgs.vscode-extensions.angular.ng-template;
          default = pkgs.buildEnv {
            name = "everything";
            paths = builtins.attrValues (
              pkgs.lib.filterAttrs (name: _: name != "default") self.packages.${system}
            );
          };
        };
      });
}
