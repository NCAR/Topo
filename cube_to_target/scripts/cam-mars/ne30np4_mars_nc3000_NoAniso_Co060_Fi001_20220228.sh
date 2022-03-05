#!/bin/tcsh
set topo_dir = /glade/scratch/pel/topowopts_new/
echo $topo_dir
#
# pre-smooth data
#
../../cube_to_target  --intermediate_cs_name='/glade/p/cgd/amp/pel/topo/cubedata/mars-ncube3000.nc' --coarse_radius=060 --fine_radius=001 -p -x -n mars
