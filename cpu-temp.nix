# -*- mode: perl -*-

{ pkgs }: pkgs.writers.writePerlBin "cpu-temp" { libraries = [ ]; } ''
# Pragma ------------------------------

use 5.30.0;
use strict;
use warnings;

while (<>) {
  chomp;
  if ( /^(coretemp-isa-0000|k10temp-pci-00c3)/../^  temp1_input/ ) {
    say $1 if /^  temp1_input: (\d+)/
  }
}
''

# Local Variables:
# mode: sh
# End:
