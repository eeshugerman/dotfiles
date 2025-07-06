# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, pkgsUnstable, ... }:

{
  imports = [
    # Include the results of the hardware scan.
    /etc/nixos/hardware-configuration.nix
  ];

  services.flatpak.enable = true;

  services.xremap = {
    withGnome = true;
    serviceMode = "user";
    userName = "elliott";
    deviceNames = [ "AT Translated Set 2 keyboard" "Glove80 Keyboard" ];
    watch = true;
    yamlConfig = builtins.readFile /home/elliott/.config/xremap.yml;
  };

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Setup keyfile
  boot.initrd.secrets = { "/crypto_keyfile.bin" = null; };

  # Configure encrypted swap for hibernation support
  # NOTE: Also commented out swapDevices in /etc/hardware-configuration.nix
  # TODO: Try a swapfile setup https://www.worldofbs.com/nixos-framework/#setting-up-hibernate
  swapDevices = [{
    device = "/dev/disk/by-uuid/bb9f65cb-e2e6-4032-9c79-0f68dc4b12f6";
    encrypted = {
      label = "swap";
      blkDev = "/dev/disk/by-uuid/26938550-263d-4e74-a805-00fc144bebc9";
      enable = true;
    };
  }];
  boot.resumeDevice = "/dev/disk/by-label/swap";

  services.logind = {
    # NOTE: Unfortunately there's no simple way to skip Gnome screen lock on resume
    # from hibernation (except by disablig the lock screen altogether), so password
    # must be entered twice.
    extraConfig = ''
      HandleLidSwitch=suspend-then-hibernate
      HandleLidSwitchExternalPower=suspend
      # HandlePowerKey=hibernate # doesn't work, set in gnome settings/dconf instead
      HandlePowerKeyLongPress=reboot
    '';
  };
  systemd.sleep.extraConfig = "HibernateDelaySec=90m";

  boot.loader.systemd-boot.configurationLimit = 10;

  networking.hostName = "dell9560"; # Define your hostname.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking
  networking.networkmanager.enable = true;

  # use iwd instead of wpa_supplicant
  networking.wireless.iwd.enable = true;
  networking.networkmanager.wifi.backend = "iwd";

  networking.extraHosts = ''
    192.168.1.1 router.home
    192.168.1.3 kodi.home
  '';

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_US.UTF-8";
    LC_IDENTIFICATION = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_NAME = "en_US.UTF-8";
    LC_NUMERIC = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_TELEPHONE = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
  };

  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # Enable the GNOME Desktop Environment.
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.desktopManager.gnome.enable = true;

  # Configure keymap in X11
  services.xserver.xkb = {
    variant = "";
    layout = "us";
  };

  # Enable CUPS to print documents.
  services.printing.enable = true;

  services.pulseaudio.enable = false;
  # rtkit (optional, recommended) allows Pipewire to use the realtime scheduler for increased performance.
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    jack.enable = true;
  };

  services.fwupd.enable = true;
  services.mullvad-vpn.enable = true;

  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.elliott = {
    isNormalUser = true;
    description = "Elliott Shugerman";
    extraGroups = [ "networkmanager" "wheel" "docker" "adbusers" ];
    shell = pkgs.zsh;
    packages = (with pkgs; [
      bitwarden
      direnv
      dmidecode
      docker
      chromium
      eddie
      # TODO: manage packages and external deps with nix
      # https://github.com/noctuid/dotfiles/blob/94c6f3e8a/nix/overlays/emacs.nix
      emacs30-pgtk
      foliate
      fragments
      git
      gnome-firmware
      gnome-tweaks
      gnomeExtensions.ddterm
      gnomeExtensions.grand-theft-focus
      gnomeExtensions.night-theme-switcher
      gnomeExtensions.pano
      gnomeExtensions.xremap # needed in addition to the module
      jetbrains-mono # a font
      mullvad-vpn
      nushell
      # powertop maybe causing dock resume issues?
      ripgrep
      unzip
      vim
      vlc
      yadm
      zsh
      zeroad
      xorg.xeyes
      gcc
      qbittorrent

      ## music/audio stuff ##
      # TODO: move to a self-contained profile or module or something?
      chow-kick
      geonkick
      helvum # pipewire gui
      reaper
      reaper-reapack-extension
      reaper-sws-extension
      tunefish
      vital
      x42-avldrums
      gxplugins-lv2
      guitarix
    ]) ++ (with pkgsUnstable; [ ]);
  };

  virtualisation.docker.enable = true;
  virtualisation.virtualbox.host.enable = true;
  # https://github.com/NixOS/nixpkgs/issues/76108#issuecomment-1977580798
  virtualisation.virtualbox.host.enableHardening = false;
  users.extraGroups.vboxusers.members = [ "elliott" ];

  # commented-out because prompts for password after login anyway
  # services.xserver.displayManager.autoLogin.enable = true;
  # services.xserver.displayManager.autoLogin.user = "elliott";
  # # Workaround for GNOME autologin: https://github.com/NixOS/nixpkgs/issues/103746#issuecomment-945091229
  # systemd.services."getty@tty1".enable = false;
  # systemd.services."autovt@tty1".enable = false;

  # https://github.com/NixOS/nixpkgs/issues/180175
  systemd.services.NetworkManager-wait-online.enable = false;

  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.nvidia.acceptLicense = true;
  # See flake.nix to set this for the unstable channel.
  nixpkgs.config.permittedInsecurePackages =
    [ "dotnet-sdk-6.0.428" "dotnet-runtime-6.0.36" ];

  ### from https://nixos.wiki/wiki/Nvidia
  # Enable OpenGL
  hardware.graphics = { enable = true; };

  # Load nvidia driver for Xorg and Wayland
  services.xserver.videoDrivers = [ "nvidia" ];

  hardware.nvidia = {

    # Modesetting is required.
    modesetting.enable = true;

    # Nvidia power management. Experimental, and can cause sleep/suspend to fail.
    # Enable this if you have graphical corruption issues or application crashes after waking
    # up from sleep. This fixes it by saving the entire VRAM memory to /tmp/ instead
    # of just the bare essentials.
    # powerManagement.enable = false;

    # Fine-grained power management. Turns off GPU when not in use.
    # Experimental and only works on modern Nvidia GPUs (Turing or newer).
    # powerManagement.finegrained = false;

    # Use the NVidia open source kernel module (not to be confused with the
    # independent third-party "nouveau" open source driver).
    # Support is limited to the Turing and later architectures. Full list of
    # supported GPUs is at:
    # https://github.com/NVIDIA/open-gpu-kernel-modules#compatible-gpus
    # Only available from driver 515.43.04+
    # Currently alpha-quality/buggy, so false is currently the recommended setting.
    open = false;

    # Enable the Nvidia settings menu,
    # accessible via `nvidia-settings`.
    nvidiaSettings = true;

    # Optionally, you may need to select the appropriate driver version for your specific GPU.
    # package = config.boot.kernelPackages.nvidiaPackages.stable;
  };

  ### end from https://nixos.wiki/wiki/Nvidia
  # nixos-hardware handles the rest (see ./flake.nix)

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs;
    [
      #  vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
      #  wget
    ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  programs.zsh.enable = true;

  programs.steam.enable = true;

  programs.adb.enable = true;

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # use nftables for firewall instead of iptables
  networking.nftables.enable = true;

  networking.firewall = {
    enable = true;
    allowedTCPPorts = [ ];
    allowedUDPPorts = [ ];
    interfaces."Eddie" = {
      allowedTCPPorts = [ 13626 ];
      allowedUDPPorts = [ 13626 ];
    };
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?

}
