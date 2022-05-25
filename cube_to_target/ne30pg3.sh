#!/bin/tcsh
./cube_to_target --grid_descriptor_file='../regression-test-data/ne30pg3.nc' --intermediate_cs_name='/glade/p/cgd/amp/pel/topo/cubedata/gmted2010_modis_bedmahcine-ncube3000-220518.nc' --output_grid='ne30pg3' --smoothing_scale=100.0 -u 'Peter Hjort Lauritzen, pel@ucar.edu' -q 'output/' -r


#
# test
#
#./cube_to_target --grid_descriptor_file='../regression-test-data/ne30pg3.nc' --intermediate_cs_name='gmted2010_modis_bedmahcine-ncube1000-220518.nc' --output_grid='ne30pg3' --smoothing_scale=100.0 -u 'Peter Hjort Lauritzen, pel@ucar.edu' -q 'new-ne540-distance-weighted-smoother/' -m -b

#./cube_to_target --grid_descriptor_file='../regression-test-data/ne30pg3.nc' --intermediate_cs_name='gmted2010_modis_bedmahcine-ncube1000-220518.nc' --output_grid='ne30pg3' --smoothing_scale=100.0 -u 'Peter Hjort Lauritzen, pel@ucar.edu' -q 'new-ne540/' 

#./cube_to_target --grid_descriptor_file='../regression-test-data/ne30pg3.nc' --intermediate_cs_name='gmted2010_modis_bedmahcine-ncube1000-220518.nc' --output_grid='ne30pg3' --smoothing_scale=100.0 -u 'Peter Hjort Lauritzen, pel@ucar.edu' -q 'new-ne540-smooth-over-ocean' -m





