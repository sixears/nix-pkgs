# -*- mode: perl -*-

{ pkgs, ... }:

pkgs.writers.writePerlBin "swap-summarize" { libraries = [ ]; } ''
# Pragma ------------------------------

use 5.30.0;
use strict;
use warnings;

while (<>) {
  chomp;
  my @cols = split ' ', $_;
  $total += $cols[0];
  $used  += $cols[1];
}

printf "%d%% %3.1f GiB\n", (($total-$used)/$total*100), ($total/1024**3);
''
