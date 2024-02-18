{ pkgs, cpu-temp }: pkgs.writers.writeBashBin "cpu-temperature" ''
PATH=/dev/null
set -u -o pipefail -o noclobber; shopt -s nullglob

cpu_temp=${cpu-temp}/bin/cpu-temp
env=${pkgs.coreutils}/bin/env
flock=${pkgs.util-linux}/bin/flock
mkfifo=${pkgs.coreutils}/bin/mkfifo
sensors=${pkgs.lm_sensors}/bin/sensors

# flock on the script file itself; re-exec if we successfully flock
[[ ''${FLOCKER:-} != $0 ]] && exec $env FLOCKER="$0" $flock -en "$0" "$0" "$@" \
                           || :

case $# in
  1) fifo="$1"                           ;;
  *) echo "usage: $0 <fifo>" >&2; exit 2 ;;
esac

if [[ -e $fifo ]]; then
  [[ -p $fifo ]] || { echo "not a fifo: '$fifo'" >&2; exit 3; }
else
  $mkfifo $fifo
fi

while true; do
  $sensors -u | $cpu_temp >> $fifo
done
''

# -- that's all, folks! --------------------------------------------------------

# Local Variables:
# mode: sh
# sh-basic-offset: 2
# End:
