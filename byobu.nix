{ pkgs }: pkgs.writers.writeBashBin "byobu" ''
set -eu -o pipefail

case "$TERM" in
  # byobu / termcap doesn't like rxvt-unicode-256color
  rxvt-unicode-256color ) TERM=vt102 ;;
esac

# byobu particularly relies on the path to access its own utility programs :-(
exec                                                                          \
  env -i USER="$USER" HOME="$HOME" TERM="$TERM"                               \
         DBUS_SESSION_BUS_ADDRESS="$DBUS_SESSION_BUS_ADDRESS"                 \
         XDG_RUNTIME_DIR="$XDG_RUNTIME_DIR" SWAYSOCK="$SWAYSOCK"              \
         DISPLAY="$DISPLAY" XAUTHORITY="''${XAUTHORITY:-$HOME/.Xauthority}"   \
         PATH="${pkgs.byobu}/bin:${pkgs.tmux}/bin:/run/current-system/sw/bin" \
       ${pkgs.byobu}/bin/byobu "$@"

''

# Local Variables:
# mode: sh
# sh-basic-offset: 2
# End:
