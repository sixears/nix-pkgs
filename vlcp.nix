{ pkgs }: pkgs.writers.writeBashBin "vlcp" ''

flock=${pkgs.util-linux}/bin/flock
vlc=${pkgs.vlc}/bin/vlc

uid="$(${pkgs.coreutils}/bin/id --user)"
lockf=/run/user/$uid/vlc.pid

exec $flock --shared --no-fork $lockf $vlc --fullscreen --play-and-exit "$@"
''

# Local Variables:
# mode: sh
# End:
