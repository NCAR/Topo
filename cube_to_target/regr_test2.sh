#!/bin/tcsh
./cube_to_target --grid_descriptor_file='/glade/p/cesmdata/inputdata/atm/cam/coords/ne0ARCTICne30x4_scrip_c191212.nc' --intermediate_cs_name='../regression-test-data/gmted2010_bedmachine-ncube0540.nc' --output_grid='ne-ARCTIC' --coarse_radius=012 --fine_radius=001 -p -r -y 4 -s
set file=`ls -t1 output/*.nc |  head -n 1`
echo $file
#/glade/p/cesm/cseg/tools/cprnc/cprnc -m $file ../regression-test-data/ne30pg3_nc0540_Nsw008_Nrs-001_Co012_Fi001_20220202.nc
