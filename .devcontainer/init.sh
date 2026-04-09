set -eu

nix run github:nix-community/home-manager -- switch --flake /workspaces/pi-dsl#dev

direnv=/home/dev/.nix-profile/bin/direnv

grep -qxF 'export PATH="$HOME/.nix-profile/bin:$PATH"' ~/.bashrc || printf '%s\n' 'export PATH="$HOME/.nix-profile/bin:$PATH"' >> ~/.bashrc
grep -qxF "eval \"\$($direnv hook bash)\"" ~/.bashrc || printf '%s\n' "eval \"\$($direnv hook bash)\"" >> ~/.bashrc
grep -qxF 'export PS1="\u \W> "' ~/.bashrc || printf '%s\n' 'export PS1="\u \W> "' >> ~/.bashrc

cd /workspaces/pi-dsl
grep -qxF 'use flake' .envrc || printf '%s\n' 'use flake' >> .envrc
$direnv allow
$direnv exec . cabal update
