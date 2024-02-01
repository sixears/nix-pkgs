{ pkgs }: pkgs.writers.writeBashBin "pidkill" ''

kill=${pkgs.util-linux}/bin/kill

warn() { echo "$@" >&2; }

args=()
[[ "-9" == "$1" ]] && args=( -9 ) && shift;

warned=false
for i in "$@"; do
  j="$(/run/current-system/sw/bin/cat "$i" || /bin/true)"
  if [[ $j =~ ^[0-9]+$ ]]; then
    $kill "''${args[@]}" $j
  else
    $warned && warn ""
    warn "ignoring garbage from file '$i':"
    while read a; do
      warn "> $a"
    done <<<"$j"
    warned=true
  fi
done

''

# Local Variables:
# mode: sh
# End:
