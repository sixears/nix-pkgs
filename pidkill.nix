{ pkgs }: pkgs.writers.writeBashBin "pidkill" ''

kill=${pkgs.util-linux}/bin/kill

warn() { echo "$@" >&2; }

args=()
[[ "-9" == "$1" ]] && args=( -9 ) && shift;

warned=false
for i in "$@"; do
  j="$(/run/current-system/sw/bin/cat "$i" || true)"
  if [[ $j =~ ^[0-9]+$ ]]; then
    $kill "''${args[@]}" $j
  else
    if [[ -s "$i" ]]; then
    $warned && warn ""
    warn "ignoring garbage from file '$i':"
    while read a; do
      warn "> $a"
    done <<<"$j"
    warned=true
    elif [[ -e "$i" ]]; then
      $warned && warn ""
      warn "ignoring empty file '$i'"
      warned=true
    else
      $warned && warn ""
      warn "no such file '$i'"
      warned=true
    fi
  fi
done

''

# Local Variables:
# mode: sh
# End:
