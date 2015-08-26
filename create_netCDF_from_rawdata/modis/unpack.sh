#!/bin/csh
#
# gdal library needed; install with, e.g., :
#                                           
# sudo port install gdal +hdf4 +hdf5 +netcdf# 
#
#
set raw_data_dir = "rawdata"
set work_dir     = "work"
mkdir work
#
echo "Convert hdf files to netCDF files (extracts all fields)"
cd $raw_data_dir
foreach f (*.hdf)
  gdal_translate -sds $f ../$work_dir/$f.tif
  echo $f
end    
cd ..
set field         = "01"
set field_name    = "landwater"
set all_tif_files = ($work_dir/*.hdf_$field.tif)
foreach f ($all_tif_files)
  echo "$f"
end  
/opt/local/bin/python2.7 /opt/local/share/doc/py27-gdal/examples/scripts/gdal_merge.py $all_tif_files -o $field.all.tif
#
# change data format and projection (to lat lon)
#
gdalwarp -of netCDF -t_srs "EPSG:4326" $field.all.tif $field_name.nc
rm $field.all.tif
rm -rf $work_dir
