#!/bin/tcsh
./cube_to_target --grid_descriptor_file='../regression-test-data/ne0_15x2.nc' --intermediate_cs_name='../regression-test-data/gmted2010_bedmachine-ncube0540.nc' --output_grid='ne0_15x2_SA' --coarse_radius=006 -p -r -y 2 -d -u "Peter Hjort Lauritzen, pel@ucar.eu"
#set file=`ls -t1 output/*.nc |  head -n 1`
#echo $file
#/glade/p/cesm/cseg/tools/cprnc/cprnc -m $file ../regression-test-data/ne0_15x2_SA_gmted2010_bedmachine_nc0540_Co006_20220310.nc
#source plot.sh
