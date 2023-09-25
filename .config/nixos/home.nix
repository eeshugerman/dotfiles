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

  # TODO: Maybe take another stab at managing profiles with plain yadm so home manager is not needed
  programs.firefox = {
    enable = true;
    package = pkgs.firefox-devedition.override {
      cfg = { enableTridactylNative = true; };
    };
    # NOTE: Need to select this profile on first use (hint: right-click on Firefox icon)
    profiles.elliott = {
      id = 0;
      name = "elliott";
      # search.engines = {}; # TODO
      settings = {
        "browser.compactmode.show" = true;
        "browser.toolbars.bookmarks.visibility" = "never";

        # https://bugzilla.mozilla.org/show_bug.cgi?id=1752862
        # These only work with home manager, not the vanilla firefox nixos module,
        # because the latter uses policies which only works for certain prefs.
        "apz.gtk.pangesture.delta_mode" = 2;
        "apz.gtk.pangesture.pixel_delta_mode_multiplier" = 25;
      };
    };
  };
}
