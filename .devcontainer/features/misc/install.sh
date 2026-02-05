#!/bin/bash
set -e

# Install python
nix profile add nixpkgs#python312

# Install opencode
curl -fsSL https://opencode.ai/install | bash
export PATH="/home/dev/.nix-profile/bin:$PATH"