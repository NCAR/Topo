#!/bin/tcsh
#
# this scripts is for low res testing/debugging
#
set topo_dir = /glade/scratch/pel/topowopts_new/
echo $topo_dir
$topo_dir/cube_to_target/cube_to_target -i $topo_dir/regression-test-data/gmted2010_bedmachine-ncube0540.nc -c 12 -f 1 -p -x -q $topo_dir/cube_to_target/scripts/pre-smooth
