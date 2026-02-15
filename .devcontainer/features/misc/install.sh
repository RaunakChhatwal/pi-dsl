#!/bin/bash
set -e

# Install python
nix profile add nixpkgs#python312
export PATH="/home/dev/.nix-profile/bin:$PATH"

# Install opencode
su dev -c "curl -fsSL https://opencode.ai/install | bash"
install -o dev -g dev "$(dirname "$0")/opencode.json" /home/dev/.config/opencode/opencode.json
