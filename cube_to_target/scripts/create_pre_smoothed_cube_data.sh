#!/bin/tcsh
#
#****************************************************************
#
# The purpose of this script is to create pre-computed smoothed
# cubed-sphere data using several smoothing radii
# (see "foreach c" loop)
#
# Author: Peter Hjort Lauritzen (NCAR, pel@ucar)
# Date  : 3/1/2022
#
#****************************************************************
#
unset raw_data_dir
unset topo_dir
unset raw_data_dir
set topo_dir     = /glade/scratch/pel/topowopts_new/                  # location of cube_to_target binary
set raw_data_dir = /glade/p/cgd/amp/pel/topo/cubedata/               # location of intermediate cubed-sphere data
#set raw_data_dir = $topo_dir/regression-test-data/                    # low resolution testinf
set output_dir   = /glade/scratch/$user/output                        # location of output files
#
# check if directories exist
#
foreach dir ( $topo_dir $raw_data_dir $output_dir )
  if (! -d $dir ) then
    echo "directory "$dir" does NOT exist - abort"
    exit
  endif
end

echo "raw_data_dir = "$raw_data_dir
echo "topo_dir     = "$topo_dir
echo "output_dir   = "$output_dir
foreach c ( 30 )
  echo "creating pre-smoothed file using smoothing c="$c
#  $topo_dir/cube_to_target/cube_to_target -i $raw_data_dir/gmted2010_bedmachine-ncube3000.nc -c $c -p -x -q $output_dir
  qcmd -l walltime=12:00:00 -- $topo_dir/cube_to_target/cube_to_target -i $raw_data_dir/gmted2010_bedmachine-ncube3000.nc -c $c -p -x -q $output_dir >& out &
end
#foreach c ( 2 4 )
#  echo "creating pre-smoothed file using smoothing c="$c
#  $topo_dir/cube_to_target/cube_to_target -i $topo_dir/regression-test-data/gmted2010_bedmachine-ncube0540.nc -c $c -p -x -q $output_dir
#end
