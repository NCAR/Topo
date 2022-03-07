#!/bin/tcsh



set case = "vrreg_02"

mkdir -p ../cases/${case}/output
cp *.F90 ../cases/${case}
cp Makefile ../cases/${case}
cp ../machine_settings.make ../cases/${case}
#cp clean_topo_files.csh ../cases/${case}
#cp -r analysis ../${case}


cd ../cases/${case}


# Assumes you are in the right directory, i.e, the one with F90 files and namelists
#----------------------------------------------------------------------------------
mkdir -p output
mkdir -p output/raw
mkdir -p output/clean


module load compiler/gnu/default
gmake clean
gmake

set ogrid = "ne30pg3"
set Co = "20"
set Fi = "0"



#./cube_to_target --grid_descriptor_file='../../regression-test-data/ne30pg3.nc' --intermediate_cs_name='../../regression-test-data/gmted2010_bedmachine-ncube0540.nc' --output_grid=$ogrid --coarse_radius=$Co --fine_radius=$Fi -r -u 'snoopdog@poo.com' -q 'output/'

# Variable res
./cube_to_target --grid_descriptor_file='../../regression-test-data/ne0_15x2.nc' --intermediate_cs_name='../../regression-test-data/gmted2010_bedmachine-ncube0540.nc' --output_grid='ne0_15x2_SA' --coarse_radius=$Co --fine_radius=001 -r -y 2 -u 'snoopdog@poo.com' -q 'output/'



exit
