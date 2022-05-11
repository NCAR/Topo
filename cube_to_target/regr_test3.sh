#!/bin/tcsh
make
#./cube_to_target --grid_descriptor_file='../regression-test-data/fv_0.9x1.25.nc'  --intermediate_cs_name='../regression-test-data/gmted2010_modis-ncube540.nc' --output_grid='fv0.9x1.25_'  -u 'Peter Hjort Lauritzen, pel@ucar' -q 'output/' -b 20E7 -l 60
./cube_to_target --grid_descriptor_file='../regression-test-data/fv0.9x1.25.nc'  --intermediate_cs_name='../regression-test-data/gmted2010_modis-ncube540.nc' --output_grid='fv0.9x1.25'  -u 'Peter Hjort Lauritzen, pel@ucar' -q 'output/' --smoothing_scale=100.0 -l 60 -r
set file=`ls -t1 output/*.nc |  head -n 1`
echo $file
#/glade/p/cesm/cseg/tools/cprnc/cprnc -m $file ../regression-test-data/fv0.9x1.25_gmted2010_bedmachine_nc0540_NoAniso_Laplace0060_20220504.nc
/fs/cgd/csm/tools/cprnc/cprnc -m $file ../regression-test-data/fv0.9x1.25_gmted2010_bedmachine_nc0540_Laplace0060_20220511.nc
