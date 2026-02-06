#!/bin/bash
set -e

# Install python
nix profile add nixpkgs#python312
export PATH="/home/dev/.nix-profile/bin:$PATH"

# Install opencode
su dev -c "curl -fsSL https://opencode.ai/install | bash"