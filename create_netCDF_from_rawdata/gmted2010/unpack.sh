#!/bin/bash
#
# gdal library needed; install with, e.g., :
#
# sudo port install gdal +hdf4 +hdf5 +netcdf
#
field=mea
resolution=300

for f in $( ls rawdata/*_$resolution*zip); do
  unzip $f 
  echo $f
done
all_tif_files=`ls *$field*.tif`
echo "all_tif_files:" 
echo " "
echo $all_tif_files
echo
echo "gdal_merge.py $all_tif_files -o $field.tif"
./gdal_merge.py $all_tif_files -o $field.tif
#python /usr/local/anaconda-2.4.0/bin/gdal_merge.py $all_tif_files -o $field.tif
echo "dal_translate -of netCDF 1.tif 1.nc"
gdal_translate -of netCDF $field.tif $field.nc
#rm *.tif
