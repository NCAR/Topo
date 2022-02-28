#!/bin/tcsh
../../cube_to_target --grid_descriptor_file='/glade/p/cesmdata/inputdata/atm/cam/coords/ne30np4_esmf_c210305.nc' --intermediate_cs_name='/glade/p/cgd/amp/pel/topo/cubedata/mars-ncube3000.nc' --output_grid='ne30pg3' --coarse_radius=060 --fine_radius=001 -p -r -u 'Peter Hjort Lauritzen, pel@ucar' -q 'scripts/cam-mars'
