{ pkgs, bash-header }: pkgs.writers.writeBashBin "touchpad" ''
PATH=/dev/null
set -u -o pipefail -o noclobber; shopt -s nullglob

source ${bash-header}

Cmd[xinput]=${pkgs.xorg.xinput}/bin/xinput

# ------------------------------------------------------------------------------

Usage="$(''${Cmd[cat]} <<EOF
usage: $Progname [enable|disable|toggle]

Options:
  -s | --touchscreen   Manage the touchscreen, rather than the touchpad.
  -t | --trackpoint | --nipple
                       Manage the trackpointer (nipple) rather than the
                       touchpad.

Standard Options:
  -v | --verbose  Be more garrulous, including showing external commands.
  --dry-run       Make no changes to the so-called real world.
  --help          This help.
EOF
)"

# ------------------------------------------------------------------------------

main() {
  local device="$1" arg="''${2:-}"

  touchpad="$(gocmd 4 xinput --list --name-only | \
              gocmd 5 grep --ignore-case "$device")"

  check_ touchpad

  if [[ -n $touchpad ]]; then
    enabled="$(gocmd 6 xinput --list-props "$touchpad" | \
               gocmd 7 grep "	Device Enabled"        | \
               gocmd 8 cut --fields 3 )"

    case "$arg" in
      ""      )  go 9 echo -e "touchpad: $touchpad\tenabled: $enabled" ;;
      toggle  ) if [[ 1 -eq $enabled ]]; then
                  gocmd 10 xinput --disable "$touchpad"
                else
                  gocmd 11 xinput --enable  "$touchpad"
                fi                                                     ;;
      enable  ) gocmd 12 xinput --enable   "$touchpad"                 ;;
      disable ) gocmd 13 xinput --disable  "$touchpad"                 ;;
      *       ) usage                                                  ;;
    esac
  else
    die 3 "no touchpad found"
  fi
}

# ------------------------------------------------------------------------------

orig_args="$@"
getopt_opts=( -o vst --long touchscreen,trackpoint,nipple,verbose,dry-run,help )
OPTS=$( ''${Cmd[getopt]} "''${getopt_opts[@]}" -n "$Progname" -- "$@" )

[ $? -eq 0 ] || die 2 "options parsing failed (--help for help)"

# copy the values of OPTS (getopt quotes them) into the shell's $@
eval set -- "$OPTS"

args=()
# device to manage; touchpad by default.
device=touchpad
while true; do
  case "$1" in
    -s | --touchscreen           ) device='multitouch sensor Finger' ; shift ;;
    -t | --trackpoint | --nipple ) device='TrackPoint'               ; shift ;;
    # !!! don't forget to update usage !!!
    -v | --verbose  ) Verbose=$((Verbose+1)) ; shift   ;;
    --help          ) usage                            ;;
    --dry-run       ) DryRun=true            ; shift   ;;
    --              ) shift; args+=( "$@" )  ; break   ;;
    *               ) args+=( "$1" )         ; shift   ;;
  esac
done

case "''${#args[@]}" in
  0 ) main "$device"                ;;
  1 ) main "$device" "''${args[0]}" ;;
  * ) usage                         ;;
esac

''

# -- that's all, folks! --------------------------------------------------------
