#!/bin/bash

# Compile the program
make

# Run the cube_to_target executable with the specified arguments
./cube_to_target --grid_descriptor_file='../regression-test-data/ne0_15x2.nc' \
                 --intermediate_cs_name='../regression-test-data/gmted2010_bedmachine-ncube0540-220518.nc' \
                 --output_grid='ne0_15x2_SA' \
                 -y 2 \
                 -u "Peter Hjort Lauritzen, pel@ucar.edu" \
                 --smoothing_scale=200.0 \
                 -v

# Find the most recent output file
file=$(ls -t1 output/*.nc | head -n 1)

# Print the filename
echo "$file"

# Run CPRNC comparison
/glade/campaign/cesm/cesmdata/cprnc/cprnc -m "$file" ../regression-test-data/ne0_15x2_SA_gmted2010_modis_bedmachine_nc0540_Laplace0200_noleak_20250319.nc

# Optional: Another comparison command (commented out in the original script)
# /fs/cgd/csm/tools/cprnc/cprnc -m "$file" ../regression-test-data/ne0_15x2_SA_gmted2010_modis_bedmachine_nc0540_Laplace0200_noleak_20250319.nc

# Optional: Uncomment to source the plotting script
# source plot.sh
