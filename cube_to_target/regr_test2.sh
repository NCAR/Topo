#!/bin/tcsh
rm output/ne0_15x2_SA_nc0540_Co006_Fi001_PF_RR_Nsw004.nc
./cube_to_target --grid_descriptor_file='../regression-test-data/ne0_15x2.nc' --intermediate_cs_name='../regression-test-data/gmted2010_bedmachine-ncube0540.nc' --output_grid='ne0_15x2_SA' --coarse_radius=006 --fine_radius=001 -p -r -y 2 -s
/glade/p/cesm/cseg/tools/cprnc/cprnc -m output/ne0_15x2_SA_nc0540_Co006_Fi001_PF_RR_Nsw004.nc ../regression-test-data/ne0_15x2_SA_nc0540_Co006_Fi001_PF_RR_Nsw004.nc
