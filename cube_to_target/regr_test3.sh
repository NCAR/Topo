#!/bin/tcsh
make
./cube_to_target --grid_descriptor_file='../regression-test-data/fv0.9x1.25.nc'  --intermediate_cs_name='../regression-test-data/gmted2010_bedmachine-ncube0540-220518.nc' --output_grid='fv0.9x1.25'  -u 'Peter Hjort Lauritzen, pel@ucar' -q 'output/' --smoothing_scale=100.0 
set file=`ls -t1 output/*.nc |  head -n 1`
echo $file
/glade/p/cesm/cseg/tools/cprnc/cprnc -m $file ../regression-test-data/fv0.9x1.25_gmted2010_modis_bedmachine_nc0540_Laplace0100_noleak_20230209.nc
#/fs/cgd/csm/tools/cprnc/cprnc -m $file ../regression-test-data/fv0.9x1.25_gmted2010_bedmachine_nc0540_NoAniso_Laplace0100_20220610.nc
