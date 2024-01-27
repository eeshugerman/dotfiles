# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports = [
    # Include the results of the hardware scan.
    /etc/nixos/hardware-configuration.nix
  ];

  services.xremap = {
    withGnome = true;
    serviceMode = "user";
    userName = "elliott";
    deviceName = "AT Translated Set 2 keyboard,Logitech ERGO K860";
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
    device =  "/dev/disk/by-uuid/bb9f65cb-e2e6-4032-9c79-0f68dc4b12f6";
    encrypted = {
      label = "swap";
      blkDev = "/dev/disk/by-uuid/26938550-263d-4e74-a805-00fc144bebc9";
      enable = true;
    };
  }];
  boot.resumeDevice = "/dev/disk/by-label/swap";

  services.logind = {
    extraConfig = ''
      HandleLidSwitch=suspend-then-hibernate
      HandleLidSwitchExternalPower=suspend
      HandlePowerKey=hibernate # doesn't seem to work? suspends instead
      HandlePowerKeyLongPress=reboot
    '';
  };
  systemd.sleep.extraConfig = "HibernateDelaySec=90m";

  boot.loader.systemd-boot.configurationLimit = 10;

  networking.hostName = "dell9560"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking
  networking.networkmanager.enable = true;

  networking.extraHosts = ''
    192.168.1.1 router.home
    192.168.1.3 kodi.home
  '';

  # Set your time zone.
  time.timeZone = "America/New_York";

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
  services.xserver = {
    layout = "us";
    xkbVariant = "";
  };

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound with pipewire.
  sound.enable = true;
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    #jack.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };

  services.fwupd.enable = true;

  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.elliott = {
    isNormalUser = true;
    description = "Elliott Shugerman";
    extraGroups = [ "networkmanager" "wheel" "docker"];
    shell = pkgs.zsh;
    packages = with pkgs; [
      bitwarden
      direnv
      dmidecode
      docker
      chromium
      emacs29-pgtk
      git
      gnome-firmware
      gnome.gnome-tweaks
      # not working, but maybe just need to wait for this fix to be released?
      # https://github.com/NixOS/nixpkgs/pull/272555
      # https://github.com/NixOS/nixpkgs/pull/272657
      gnomeExtensions.ddterm # not working on 45 so far :(
      gnomeExtensions.night-theme-switcher
      gnomeExtensions.pano
      gnomeExtensions.xremap # needed in addition to the module
      jetbrains-mono
      nushell
      # powertop maybe causing dock resume issues?
      ripgrep
      unzip
      vim
      yadm
      zsh
      zeroad
      xorg.xeyes
    ];
  };

  virtualisation.docker.enable = true;

  # commented-out because prompts for password after login anyway
  # services.xserver.displayManager.autoLogin.enable = true;
  # services.xserver.displayManager.autoLogin.user = "elliott";
  # # Workaround for GNOME autologin: https://github.com/NixOS/nixpkgs/issues/103746#issuecomment-945091229
  # systemd.services."getty@tty1".enable = false;
  # systemd.services."autovt@tty1".enable = false;

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.nvidia.acceptLicense = true;

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

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  networking.firewall.allowedTCPPorts = [ 9222 ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?

}
