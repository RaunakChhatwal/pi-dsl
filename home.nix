{ pkgs, ... }: {
  home.username = "dev";
  home.homeDirectory = "/home/dev";
  home.stateVersion = "25.11";

  programs.home-manager.enable = true;
  home.packages = [ pkgs.neovim ];
}
