#!/bin/csh
echo $argv[1]
if ($argv[1] == gmted2010) then
  make raw_netCDF_gmted2010_modis
  make cam_fv_smooth_gmted2010_modis
  make bin_to_cube_gmted2010
else if ($argv[1] == gtopo30) then

else
  echo "ERROR: invalid argument"
endif
