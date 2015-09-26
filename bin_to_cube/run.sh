#!/bin/csh
echo "argument is:" $argv[1]
if ($argv[1] == gmted2010) then
  cat > bin_to_cube.nl <<EOF
&binparams
  raw_latlon_data_file = '../create_netCDF_from_rawdata/gmted2010_elevation_and_landfrac_modis_sft_fix_inland_water_elevation.nc'
  output_file = 'gmted2010_modis-ncube3000.nc'
/

EOF
  ./bin_to_cube
else if ($argv[1] == gtopo30) then
  cat > bin_to_cube.nl <<EOF
&binparams
  raw_latlon_data_file = '../create_netCDF_from_rawdata/gtopo30/gtopo30-rawdata.nc'
  output_file = 'gtopo30-ncube3000.nc'
/

EOF
  ./bin_to_cube
else
 echo "ERROR: no valid argument for run.sh"
 echo "valid options are gmted2010 and gtopo30"
endif


