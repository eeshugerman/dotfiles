# nixos-rebuild [...] should work, but sometimes `systemctl restart "home-manager-$USER.service"` is needed
{ config, pkgs, ... }:

{
  home.username = "elliott";
  home.homeDirectory = "/home/elliott";

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You can update Home Manager without changing this value. See the Home
  # Manager release notes for a list of state version changes in each release.
  home.stateVersion = "23.05";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # TODO: Maybe take another stab at managing profiles (mainly prefs.js) with
  # plain yadm so home manager is not needed
  # NOTE: Oof Firefox is building from source, probably due to this override
  # https://discourse.nixos.org/t/how-to-ensure-all-packages-are-available-in-cache-nixos-org-on-nix-flake-update/37209/3
  programs.firefox = {
    enable = true;
    package = pkgs.firefox-devedition.override {
      # deprecation warning
      cfg = {
        # deprecation warning but the new way doesn't work?
        enableTridactylNative = true;
      };
    };
    profiles.elliott = {
      id = 0;
      name = "elliott";
      # search.engines = {}; # TODO
      settings = {
        "browser.compactmode.show" = true;
        "browser.toolbars.bookmarks.visibility" = "never";

        # allow unsigned extensions in dev edition
        "xpinstall.signatures.required" = false;

        # https://bugzilla.mozilla.org/show_bug.cgi?id=1752862
        # These only work with home manager, not the vanilla firefox nixos module,
        # because the latter uses policies which only works for certain prefs.
        "apz.gtk.pangesture.delta_mode" = 2;
        # TODO: not working ?
        "apz.gtk.pangesture.pixel_delta_mode_multiplier" = 25;
      };
    };
    # workaround for dev edition profile weirdness
    # https://www.reddit.com/r/firefox/comments/4uhkwq/how_do_i_get_dev_edition_to_use_my_profile_by/d5pve4b
    profiles."ignore-dev-edition-profile" = { id = 1; };
  };
}
