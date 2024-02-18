# -*- mode: perl -*-

{ pkgs }: pkgs.writers.writePerlBin "cpu-temp" { libraries = [ ]; } ''
# Pragma ------------------------------

use 5.30.0;
use strict;
use warnings;

my $said = 0;
while (<>) {
  chomp;
#  if ( /^(coretemp-isa-0000|k10temp-pci-00c3)/../^  temp1_input/ ) {
  if ( /^(coretemp-isa-0000|k10temp-pci-00c3)/../^  temp1_input/ ) {
    if ( /^  temp1_input: (\d+)/ ) { say $1; $said++ }
  }
}
say "?" unless $said;
''

# Local Variables:
# mode: sh
# End:
