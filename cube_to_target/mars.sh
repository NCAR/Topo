#
# ne16pg3
#

# ./cube_to_target --grid_descriptor_file /glade/campaign/cesm/cesmdata/inputdata/atm/cam/coords/ne16pg3_scrip_170725.nc --smoothing_over_ocean --compute_sgh30_from_sgh_fac 0.50 --grid_descriptor_file_gll /glade/campaign/cesm/cesmdata/inputdata/atm/cam/coords/ne16np4_esmf_c210305.nc --source_data_identifier "mars" --intermediate_cs_name /glade/campaign/cgd/amp/pel/topo/cubedata/mars-ncube3000.nc  --output_grid ne16pg3 --smoothing_scale 200.0 --name_email_of_creator "Peter Hjort Lauritzen, pel@ucar.edu" --output_data_directory output/  --jmax_segments 100000 --gravity 3.72076000 
#
# ne16np4
#
#qcmd -l walltime=24:00:00 -A P03010039 -l select=1:ncpus=4:mem=200GB -- ./cube_to_target --compute_sgh30_from_sgh_fac 0.50000000000000000 --jmax_segments 5000000 --smoothing_over_ocean --grid_descriptor_file /glade/campaign/cesm/cesmdata/inputdata/share/meshes/ne16np4_ESMFmesh_cdf5_c20211018.nc --intermediate_cs_name /glade/campaign/cgd/amp/pel/topo/cubedata/mars-ncube3000.nc --output_grid ne16np4 --smoothing_scale 200.0 --name_email_of_creator Lauritzen --gravity 3.72076000 >& output.mars.ne16np4.txt &

qcmd -l walltime=24:00:00 -A P03010039 -l select=1:ncpus=4:mem=200GB -- ./cube_to_target --compute_sgh30_from_sgh_fac 0.50000000000000000 --jmax_segments 5000000 --smoothing_over_ocean --grid_descriptor_file /glade/campaign/cesm/cesmdata/inputdata/share/meshes/ne16np4_ESMFmesh_cdf5_c20211018.nc --source_data_identifier mars --intermediate_cs_name /glade/campaign/cgd/amp/pel/topo/cubedata/mars-ncube3000.nc --output_grid ne16np4 --smoothing_scale 200.0 --name_email_of_creator Lauritzen --gravity 3.72076000 >& output.mars.ne16pg3.txt &
