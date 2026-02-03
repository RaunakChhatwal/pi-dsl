echo 'export PATH="$HOME/.nix-profile/bin:$PATH"' >> ~/.bashrc
echo 'eval "$(/home/dev/.nix-profile/bin/direnv hook bash)"' >> ~/.bashrc
echo 'export PS1="\u \W> "' >> ~/.bashrc
source ~/.bashrc
# nix profile add nixpkgs#python312
cd /workspaces/pi-dsl
echo 'use flake' >> .envrc
/home/dev/.nix-profile/bin/direnv allow
