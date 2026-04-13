set -eu

sudo sh /workspaces/pi-dsl/.devcontainer/start-nix-daemon.sh
nix run github:nix-community/home-manager -- switch --flake /workspaces/pi-dsl#dev

# Materialize workspace-local VS Code config without relying on devcontainer customizations.
mkdir -p /workspaces/pi-dsl/.vscode
cat >/workspaces/pi-dsl/.vscode/settings.json <<'EOF'
{
  "editor.rulers": [120],
  "workbench.editor.enablePreview": false,
  "files.autoSave": "off",
  "editor.fontSize": 12,
  "editor.inlayHints.enabled": "off",
  "python.analysis.extraPaths": ["${workspaceFolder}"]
}
EOF
cat >/workspaces/pi-dsl/.vscode/extensions.json <<'EOF'
{
  "recommendations": [
    "haskell.haskell",
    "ms-python.python",
    "mkhl.direnv",
    "jnoortheen.nix-ide"
  ]
}
EOF

npm config set prefix ~/.local
npm install -g opencode-ai

echo 'eval "$(direnv hook bash)"' >> ~/.bashrc

sudo pkill -x nix-daemon