#!/bin/tcsh

if ( "$#argv" != 4) then
  echo "Wrong number of arguments specified:"
  set n = 1
  echo "ogrid = argv[1]"
  echo "Co    = argv[2]"
  echo "Fi    = argv[3]"
  echo "tag   = argv[4]"
  echo "     "
  echo "possible ogrid values: fv_0.9x1.25, ne30pg3 ..."
  exit
endif

set case = "Reg_"$argv[1]"_co"$argv[2]"_fi"$argv[3]"_"$argv[4]
echo $case

mkdir -p ../cases/${case}/output
cp *.F90 ../cases/${case}
cp Makefile ../cases/${case}
cp ../machine_settings.make ../cases/${case}

cd ../cases/${case}


# Assumes you are in the right directory, i.e, the one with F90 files and namelists
#----------------------------------------------------------------------------------
mkdir -p output
mkdir -p output/raw
mkdir -p output/clean


module load compiler/gnu/default
gmake clean
gmake

set n = 1
set ogrid = "$argv[$n]"
set n = 2
set Co = "$argv[$n]"
set n = 3
set Fi = "$argv[$n]"

if ( $ogrid == 'ne30pg3' ) then
   set scrip='ne30pg3.nc'
endif
if ( $ogrid == 'scam' ) then
   set scrip='fv_0.9x1.25.nc'
endif
if ( $ogrid == 'SA' ) then
   set scrip='ne0_15x2.nc' 
   set Yfac = '2'
endif

set scale = 112

./cube_to_target --grid_descriptor_file='../../regression-test-data/ne30pg3.nc' --intermediate_cs_name='../../regression-test-data/gmted2010_bedmachine-ncube0540-220518.nc' --output_grid=$ogrid --smoothing_scale=$scale --fine_radius=$Fi -r -u 'userid@ucar.edu' -q 'output/' -z

# Variable res
#./cube_to_target --grid_descriptor_file='../../regression-test-data/ne0_15x2.nc' --intermediate_cs_name='../../regression-test-data/gmted2010_bedmachine-ncube0540.nc' --output_grid='ne0_15x2_SA' --coarse_radius=$Co --fine_radius=001 -r -y 2 -u 'userid@ucar.edu' -q 'output/'



exit
