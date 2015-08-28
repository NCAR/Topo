#!/bin/csh
echo $argv[1]

if ($argv[1] == fv0.9x1.25_gmted2010) then
  (cd cube_to_target; make; chmod +x run.sh; ./run.sh; fv0.9x1.25 gmted2010)
endif




#
# create all source data files for cube_to_target
#
if ($argv[1] == source_gmted2010) then
  make raw_netCDF_gmted2010_modis
  make cam_fv_smooth_gmted2010_modis
  make bin_to_cube_gmted2010
else if ($argv[1] == source_gtopo30) then
  make raw_netCDF_gtopo30
  make cam_fv_smooth_gtopo30
  make bin_to_cube_gtopo30
else
  echo "ERROR: invalid argument"
endif
