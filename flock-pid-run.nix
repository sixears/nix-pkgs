{ pkgs }: pkgs.writers.writeBashBin "flock-pid-run" ''
set -eu -o pipefail

# we don't do this here, so the caller can use things from the path if they
# choose (though of course we encourage the callers to nuke PATH themselves)
# PATH=/dev/null

flock=${pkgs.util-linux}/bin/flock
rm=${pkgs.coreutils}/bin/rm

lockfile="$1"; shift

# open lockfile on fd#3 for reading & writing...
exec 3<> "$lockfile"
# flock on it (fd#3)
$flock --exclusive --nonblock 3
# write the PID into the lockfile
echo $$ > "$lockfile"
# now background an rm to remove the file as soon as the flock is available
# note the closing of fd#3 in the sub-process, else it will be holding open
# the very lock its waiting on and thus it will never get the lock
(exec 3>&-; $flock --exclusive "$lockfile" $rm "$lockfile") &
# now exec the real action.  Note we exec so the PID written to the lockfile
# is the pid of the exec'd process
exec "$@"

# exec $flock --no-fork --exclusive --nonblock "$lockfile" \
#      /home/martyn/bin/pid-file-run "$lockfile" "$@"

# -- that's all, folks! --------------------------------------------------------

# Local Variables:
# mode: sh
# End:
''
