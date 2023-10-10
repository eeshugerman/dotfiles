# sudo nixos-rebuild switch --impure --flake ~/.config/nixos
{
  description = "My NixOS configuration flake";
  inputs = {
    # nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
    # nixpkgs.url = "path:/home/elliott/devel/nixpkgs";

    # nix flake lock --override-input xremap-flake/xremap 'github:eeshugerman/xremap?ref=device-aware-config'
    xremap-flake.url = "github:xremap/nix-flake";

    # https://nix-community.github.io/home-manager/index.html#sec-flakes-nixos-module
    home-manager.url = "github:nix-community/home-manager/release-23.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs = { self, nixpkgs, xremap-flake, home-manager, ... }:
    let system = "x86_64-linux";
    in {
      # must either match hostname or use #foo to specify in nixos-rebuild command
      nixosConfigurations.dell9560 = nixpkgs.lib.nixosSystem {
        inherit system;
        modules = [
          ./configuration.nix
          xremap-flake.nixosModules.default
          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.elliott = import ./home.nix;

          }
        ];
      };
    };
}
