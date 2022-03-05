#!/bin/csh
cat > bin_to_cube.nl <<EOF
&binparams
  raw_latlon_data_file = '/glade/p/cgd/amp/pel/topo/rawdata/gmted2010_bedmachine-rawdata.nc'
  output_file = 'gmted2010_bedmachine-ncube3000.nc'
  ncube=3000
/
EOF
#
# Mars topography
#
#cat > bin_to_cube.nl <<EOF
#&binparams
#  raw_latlon_data_file = '/glade/p/cgd/amp/pel/topo/rawdata/mars-rawdata.nc'
#  output_file = 'mars-ncube3000.nc'
#  ncube=3000
#/
#EOF
./bin_to_cube


