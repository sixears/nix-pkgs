{ pkgs, bash-header }: ''

source ${bash-header}

Cmd[echo]=${pkgs.coreutils}/bin/echo
Cmd[pacmd]=${pkgs.pulseaudio}/bin/pacmd
Cmd[perl]=${pkgs.perl}/bin/perl
Cmd[xargs]=${pkgs.findutils}/bin/xargs

# ------------------------------------------------------------------------------

Usage="$(''${Cmd[cat]} <<EOF
usage: $Progname [mute|unmute|toggle]

Toggle all the pulseaudio mic inputs.  Or, just blindly mute/unmute them if you
choose.  Defaults to toggle.

Standard Options:
  -v | --verbose  Be more garrulous, including showing external commands.
  --dry-run       Make no changes to the so-called real world.
  --debug         Output development/debugging messages.
  --help          This help.
EOF
)"

# ------------------------------------------------------------------------------

main() {
  local action="$1"

  local -a stati

  capture_array stati \
    gocmdnodryrun 11 perl -nlE 'say $1 if /muted: (yes|no)\b/' < <(
      gocmdnodryrun 10 pacmd list-sources
    )

  local muted=0 unmuted=0
  if [[ $action == toggle ]]; then
    for s in "''${stati[@]}"; do
      case "$s" in
        yes ) (( muted++   ))             ;;
        no  ) (( unmuted++ ))             ;;
        *   ) die 12 "unrecognized: '$s'" ;;
      esac
    done
    debug "muted: $muted" "unmuted: $unmuted"
  fi

  local mute
  if    [[ $action == mute ]] \
     || [[ $action == toggle ]] && [[ $unmuted -ge $muted ]]; then
    warn "muting mics"
    mute=1
  else
    warn "unmuting mics"
    mute=0
  fi

  local -a cmd; $DryRun && cmd=( ''${Cmd[echo]} ) || cmd=()
  cmd+=( ''${Cmd[pacmd]} set-source-mute {} $mute )

  gocmdnodryrun 13 pacmd list-sources                         \
    | gocmdnodryrun 14 perl -nlE 'say $1 if /index: (\d+)\b/' \
    | gocmdnodryrun 15 xargs -I{} "''${cmd[@]}"

  #  touchpad="$(gocmd 4 xinput --list --name-only | \
  #              gocmd 5 grep --ignore-case "$device")"
  #  check_ touchpad
}

# ------------------------------------------------------------------------------

orig_args="$@"
getopt_opts=( -o v --long verbose,dry-run,debug,help )
OPTS=$( ''${Cmd[getopt]} "''${getopt_opts[@]}" -n "$Progname" -- "$@" )

[ $? -eq 0 ] || die 2 "options parsing failed (--help for help)"

# copy the values of OPTS (getopt quotes them) into the shell's $@
eval set -- "$OPTS"

args=()
action=toggle
while true; do
  case "$1" in
    # !!! don't forget to update usage !!!
    -v | --verbose  ) Verbose=$((Verbose+1)) ; shift   ;;
    --help          ) usage                            ;;
    --dry-run       ) DryRun=true            ; shift   ;;
    --debug         ) Debug=true             ; shift   ;;
    --              ) shift; args+=( "$@" )  ; break   ;;
    *               ) args+=( "$1" )         ; shift   ;;
  esac
done

case "''${#args[@]}" in
  0 ) main toggle ;;
  1 ) action="''${args[0]}"
      case "''$action" in
        mute | unmute | toggle ) main "$action" ;;
        *                      ) die 2 "bad action '$action'"
      esac
      ;;
  * ) usage ;;
esac

# -- that's all, folks! --------------------------------------------------------

# Local Variables:
# mode: sh
# sh-basic-offset: 2
# fill-column: 80
# End:
''
