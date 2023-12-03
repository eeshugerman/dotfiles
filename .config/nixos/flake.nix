{
  description = "My NixOS configuration flake";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    home-manager.url = "github:nix-community/home-manager/release-23.11";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    xremap-flake.url = "github:xremap/nix-flake";
  };
  outputs = { self, nixpkgs, nixos-hardware, home-manager, xremap-flake, ... }:
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
          # TODO: get hybrid graphics working. there's `nixos-hardware.nixosModules.dell-xps-15-9560`
          # but it uses janky/old tech (optirun/bumblebee). see https://nixos.wiki/wiki/Nvidia for
          # PRIME setup
          nixos-hardware.nixosModules.dell-xps-15-9560-intel
        ];
      };
    };
}
