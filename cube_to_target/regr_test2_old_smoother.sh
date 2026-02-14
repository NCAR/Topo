#!/bin/bash

# Compile the program
make

# Run the cube_to_target executable with the specified arguments
./cube_to_target --grid_descriptor_file='../regression-test-data/ne0_15x2.nc' \
                 --intermediate_cs_name='../regression-test-data/gmted2010_bedmachine-ncube0540-220518.nc' \
                 --output_grid='ne0_15x2_SA' \
                 --rrfac_max 2 --write_rrfac_to_topo_file \
                 --name_email_of_creator "Peter Hjort Lauritzen, pel@ucar.edu" \
                 --smoothing_over_ocean --distance_weighted_smoother \
                 --smoothing_scale=200.0 \
                 --rrfac_manipulation

# Find the most recent output file
file=$(ls -t1 output/*.nc | head -n 1)

# Print the filename
echo "$file"

# Run CPRNC comparison
/glade/campaign/cesm/cesmdata/cprnc/cprnc -m "$file" ../regression-test-data/ne0_15x2_SA_gmted2010_modis_bedmachine_nc0540_Co022_20260213.nc

# Optional: Another comparison command (commented out in the original script)
# /fs/cgd/csm/tools/cprnc/cprnc -m "$file" ../regression-test-data/ne0_15x2_SA_gmted2010_bedmachine_nc0540_Co022_20220610.nc

# Source the plotting script
source plot.sh
