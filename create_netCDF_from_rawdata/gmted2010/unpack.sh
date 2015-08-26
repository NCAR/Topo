#!/bin/csh
#
# gdal library needed; install with, e.g., :
#
# sudo port install gdal +hdf4 +hdf5 +netcdf
#
set field      = mea
set resolution = 300

foreach f (data/*_$resolution*zip)    
  unzip $f 
  echo $f
end
set all_tif_files = (*$field*.tif)
echo "all_tif_files:" 
echo " "
echo $all_tif_files
echo
echo "/gdal_merge.py"
/opt/local/bin/python2.7 /opt/local/share/doc/py27-gdal/examples/scripts/gdal_merge.py $all_tif_files -o $field.tif
echo "dal_translate -of netCDF 1.tif 1.nc"
gdal_translate -of netCDF $field.tif $field.nc
#rm *.tif
