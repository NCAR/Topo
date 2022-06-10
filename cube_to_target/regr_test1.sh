#!/bin/tcsh
make
./cube_to_target --grid_descriptor_file='../regression-test-data/ne30pg3.nc' --intermediate_cs_name='../regression-test-data/gmted2010_bedmachine-ncube0540-220518.nc' --output_grid='ne30pg3' --smoothing_scale=100.0 -u 'Peter Hjort Lauritzen, pel@ucar.edu' -q 'output/' --grid_descriptor_file_gll='../regression-test-data/ne30np4.nc'
set file=`ls -t1 output/*.nc |  head -n 1`
echo $file
/glade/p/cesm/cseg/tools/cprnc/cprnc -m $file ../regression-test-data/ne30pg3_gmted2010_modis_bedmachine_nc0540_Laplace0100_20220610.nc
#/fs/cgd/csm/tools/cprnc/cprnc -m $file ../regression-test-data/ne30pg3_gmted2010_modis_bedmachine_nc0540_Laplace0100_20220610.nc
source plot.sh
