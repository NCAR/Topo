#!/bin/tcsh
./cube_to_target --grid_descriptor_file='../regression-test-data/ne30pg3.nc' --intermediate_cs_name='../regression-test-data/gmted2010_bedmachine-ncube0540.nc' --output_grid='ne30pg3' --coarse_radius=012 --fine_radius=001 -p -r -2 1
set file=`ls -t1 output/*.nc |  head -n 1`
echo $file
/glade/p/cesm/cseg/tools/cprnc/cprnc -m $file ../regression-test-data/ne30pg3_nc0540_Nsw008_Nrs-001_Co012_Fi001_20220202.nc
