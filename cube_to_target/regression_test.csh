#!/bin/tcsh



set case = "vrreg_test_co20x2_x01"

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
set Fi = "1"


# This is now calculated internally. Eliminate from inputs.
#set Nsw = "-999"
# This is no longer used. Doesn't matter. Eliminate
#set Nrs=00

set smtopo = '/project/amp/juliob/Topo-generate-devel/Topo/Topo.git/cases/vrreg_200/output/topo_smooth_nc0540_Co012_Fi001_VRtest.dat'

#./cube_to_target --grid_descriptor_file='../../regression-test-data/ne30pg3.nc' --intermediate_cs_name='../../regression-test-data/gmted2010_bedmachine-ncube0540.nc' --output_grid=$ogrid --coarse_radius=$Co --fine_radius=$Fi -r

# Read pre-computrd smooth topo
#./cube_to_target --grid_descriptor_file='../../regression-test-data/ne30pg3.nc' --intermediate_cs_name='../../regression-test-data/gmted2010_bedmachine-ncube0540.nc' --output_grid=$ogrid --coarse_radius=06 --fine_radius=$Fi --smooth_topo_file=$smtopo -r -q


# Variable res
./cube_to_target --grid_descriptor_file='../../regression-test-data/ne0_15x2.nc' --intermediate_cs_name='../../regression-test-data/gmted2010_bedmachine-ncube0540.nc' --output_grid='ne0_15x2_SA' --coarse_radius=$Co --fine_radius=001 -r -y 2 -s


exit
