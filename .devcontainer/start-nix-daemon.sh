rm -f /tmp/nix-daemon.log
/nix/var/nix/profiles/default/bin/nix-daemon </dev/null >/tmp/nix-daemon.log 2>&1 &
while [ ! -S /nix/var/nix/daemon-socket/socket ]; do
  sleep 0.1
done
