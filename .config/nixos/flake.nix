# sudo nixos-rebuild switch --impure --flake ~/.config/nixos#
{
  description = "My NixOS configuration flake";
  inputs = {
    # nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
    xremap-flake.url = "github:xremap/nix-flake";
  };
  outputs = { self, nixpkgs, ... }:
    let
      system = "x86_64-linux";
    in {
      # must match hostname (nixos)?
      nixosConfigurations.nixos = nixpkgs.lib.nixosSystem {
        inherit system;
        modules = [ ./configuration.nix ];
      };
    };
}
