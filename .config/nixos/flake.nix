{
  description = "My NixOS configuration flake";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    home-manager.url = "github:nix-community/home-manager/release-24.11";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    xremap-flake.url = "github:xremap/nix-flake";
  };
  outputs = { ... }@inputs:
    let system = "x86_64-linux";
    in {
      # must either match hostname or use #foo to specify in nixos-rebuild command
      nixosConfigurations.dell9560 = inputs.nixpkgs.lib.nixosSystem {
        inherit system;
        modules = [
          ./configuration.nix
          inputs.xremap-flake.nixosModules.default
          inputs.home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.elliott = import ./home.nix;
          }
          # uses intel gpu unless nvidia specified with nvidia-offload command
          # TODO: maybe try sync mode or reverse sync mode -- see https://nixos.wiki/wiki/Nvidia
          inputs.nixos-hardware.nixosModules.dell-xps-15-9560-nvidia
        ];
        specialArgs = {
          pkgsUnstable = (import inputs.nixpkgs-unstable {
            inherit system;
            config = {
              # for eddie. set here, not in configuration.nix, so that it applies to unstable
              permittedInsecurePackages =
                [ "dotnet-sdk-6.0.428" "dotnet-runtime-6.0.36" ];
            };
          });
        };
      };
    };
}
