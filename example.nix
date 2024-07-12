{ pkgs, bash-header }: ''

source ${bash-header}

Cmd[bat]=${pkgs.bat}/bin/bat

# ------------------------------------------------------------------------------

Usage="$(''${Cmd[cat]} <<EOF
usage: $Progname FILENAME*

do some stuff

Standard Options:
  -v | --verbose  Be more garrulous, including showing external commands.
  --dry-run       Make no changes to the so-called real world.
  --debug         Output development/debugging messages.
  --help          This help.
EOF
)"

# ------------------------------------------------------------------------------

main() {
  local fn="$1" args=( "''${@:2}" )

  local -a stuff

  capture_array stuff \
    gocmdnodryrun 10 cat < <(
      gocmdnodryrun 10 bat "$fn"
    )

  miffy="$(gocmd 11 cat "''${args[0]}" | gocmd 12 grep root)"
  check_ 'cat | grep'
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
  0 ) main /etc/passwd    ;;
  * ) main "''${args[@]}" ;;
esac

# -- that's all, folks! --------------------------------------------------------

# Local Variables:
# mode: sh
# sh-basic-offset: 2
# fill-column: 80
# End:
''
