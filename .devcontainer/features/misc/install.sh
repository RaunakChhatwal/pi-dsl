#!/bin/bash
set -e

# Install opencode
curl -fsSL https://opencode.ai/install | bash

# Pre-populate cabal package index
export PATH="/home/dev/.nix-profile/bin:$PATH"
nix develop github:RaunakChhatwal/pi-dsl --command cabal update
