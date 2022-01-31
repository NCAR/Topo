
rm output/ne30pg3_nc0540_Co012_Fi001_PF_Nsw008.nc
./cube_to_target --grid_descriptor_file='../regression-test-data/ne30pg3.nc' --intermediate_cs_name='../regression-test-data/gmted2010_bedmachine-ncube0540.nc' --output_grid='ne30pg3' --coarse_radius=012 --fine_radius=001 -p -r -u
/glade/p/cesm/cseg/tools/cprnc/cprnc -m output/ne30pg3_nc0540_Co012_Fi001_PF_Nsw008.nc ../regression-test-data/ne30pg3_nc0540_Co012_Fi001_PF_Nsw008.nc
