#!/bin/tcsh
make
./cube_to_target --grid_descriptor_file='../regression-test-data/ne0_15x2.nc' --intermediate_cs_name='../regression-test-data/gmted2010_bedmachine-ncube0540-220518.nc' --output_grid='ne0_15x2_SA' -y 2 -u "Peter Hjort Lauritzen, pel@ucar.edu" --smoothing_scale=200.0 -v
set file=`ls -t1 output/*.nc |  head -n 1`
echo $file
/glade/campaign/cesm/cesmdata/cprnc/cprnc -m $file ../regression-test-data/ne0_15x2_SA_gmted2010_modis_bedmachine_nc0540_Laplace0200_noleak_20241211.nc
#/fs/cgd/csm/tools/cprnc/cprnc  -m $file ../regression-test-data/ne0_15x2_SA_gmted2010_bedmachine_nc0540_Laplace0200_20220610.nc
#source plot.sh
