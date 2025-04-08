{ pkgs, cpu-temp }: pkgs.writers.writeBashBin "cpu-temperature" ''
PATH=/dev/null
set -u -o pipefail -o noclobber; shopt -s nullglob

# write the cpu-temperature to a fifo.  flock for sanity in pipe writing
# usage $0 [fifo]; # no fifo => write to tty (without flocking)

cpu_temp=${cpu-temp}/bin/cpu-temp
env=${pkgs.coreutils}/bin/env
flock=${pkgs.util-linux}/bin/flock
mkfifo=${pkgs.coreutils}/bin/mkfifo
sensors=${pkgs.lm_sensors}/bin/sensors

case $# in
  0) fifo=/dev/stdout ;;

  1) fifo="$1"

     if [[ -e $fifo ]]; then
       # allow for character special on stdout; e.g., /dev/stdout or /dev/tty
       [[ -p $fifo ]] || [[ -c $fifo ]] || { echo "not a fifo: '$fifo'" >&2; exit 3; }
     else
       $mkfifo "$fifo"
     fi

     exec 3<>"fifo"
     $flock -n 3 || { echo "failed to flock '$fifo'"; exit 3; }
     ;;
  *) echo "usage: $0 <fifo>" >&2; exit 2 ;;
esac


while true; do
  $sensors -u 2>/dev/null | $cpu_temp >> $fifo
done
''

# -- that's all, folks! --------------------------------------------------------

# Local Variables:
# mode: sh
# sh-basic-offset: 2
# End:
