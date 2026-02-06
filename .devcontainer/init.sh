direnv=/home/dev/.nix-profile/bin/direnv
echo 'export PATH="$HOME/.nix-profile/bin:$PATH"' >> ~/.bashrc
echo "eval \"\$($direnv hook bash)\"" >> ~/.bashrc
echo 'export PS1="\u \W> "' >> ~/.bashrc
source ~/.bashrc
cd /workspaces/pi-dsl
echo 'use flake' >> .envrc
$direnv allow
$direnv exec . cabal update
