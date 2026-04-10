#!/bin/sh
set -eu

su - dev -c "bash -lc 'sh <(curl --proto \"=https\" --tlsv1.2 -fsSL https://nixos.org/nix/install) --daemon --yes'"
printf '%s\n' 'experimental-features = nix-command flakes' >> /etc/nix/nix.conf
