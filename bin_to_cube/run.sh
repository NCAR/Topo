#!/bin/csh
echo "argument is:" $argv[1]
if ($argv[1] == gmted2010_modis) then
  echo "Using "$argv[1]
else if ($argv[1] == gtopo30) then
else
 echo "ERROR: no valid argument for run.sh"
 echo "valid options are gmted2010_modis and gtopo30"
endif


cat > bin_to_cube.nl <<EOF
&binparams
  raw_latlon_data_file = '../create_netCDF_from_rawdata/$argv[1]-rawdata.nc'
  output_file = '$argv[1]-ncube3000.nc'
/

EOF
  ./bin_to_cube


