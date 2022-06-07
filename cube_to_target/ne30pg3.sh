#!/bin/tcsh
#
# sample scripts to produce 1 degree spectra-element topography data used for CAM6
#
./cube_to_target --grid_descriptor_file ../regression-test-data/ne30pg3.nc  --intermediate_cs_name /glade/p/cgd/amp/pel/topo/cubedata/gmted2010_modis_bedmachine-ncube3000-220518.nc --output_grid ne30pg3 --smoothing_scale 100.0 --fine_radius 0 --find_ridges --name_email_of_creator 'Peter Hjort Lauritzen, pel@ucar.edu' --output_data_directory output/





