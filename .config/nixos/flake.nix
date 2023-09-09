# sudo nixos-rebuild switch --impure --flake ~/.config/nixos#dell9560
{
  description = "My NixOS configuration flake";
  inputs = {
    # nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
    xremap-flake.url = "github:xremap/nix-flake";
  };
  outputs = { self, nixpkgs, xremap-flake, ... }:
    let system = "x86_64-linux";
    in {
      nixosConfigurations.dell9560 = nixpkgs.lib.nixosSystem {
        inherit system;
        modules = [
          ./configuration.nix
          xremap-flake.nixosModules.default
        ];
      };
    };
}
