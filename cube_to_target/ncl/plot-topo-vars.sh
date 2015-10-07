#!/bin/csh
#
# Example:
#
#     ./plot-topo-vars.sh fv0.9x1.25 gtopo30 smooth_cam pdf
#
echo "arguments are:",$argv[1],$argv[2],$argv[3],$argv[4],$argv[5],$argv[6],$argv[7]
#fv0.9x1.25
set model=$argv[1]_$argv[2]
set raw_data=$argv[3]
set smoothing=$argv[4]
set ncube=$argv[5]
set aniso=$argv[6]
set format=$argv[7]
set dataFile='"'../output/$model-$raw_data-$smoothing-intermediate_ncube$ncube-$aniso.nc'"'
set outputFile='"'topo-vars-$model-$raw_data-$smoothing-intermediate_ncube$ncube-$aniso'"'
set out_format='"'$format'"'

if ($argv[1] == fv) then
  set not_latlon=False
else
  set not_latlon=True
endif

echo $dataFile
ncl  'lmanual_levels=False' 'ldiff=False' 'not_latlon='$not_latlon 'area=-1' 'dataFile='$dataFile  'outputFile='$outputFile 'format='$out_format 'title_str=''"'$model' based on '$raw_data' raw data and '$smoothing' smoothing''"' < plot-topo-vars.ncl

#ncl  'lmanual_levels=False' 'ldiff=True' 'not_latlon=False' 'area=-1' 'dataFile="../output/fv0.9x1.25-gmted2010_modis-smooth_cam.nc"' 'dataFile2="../output/fv0.9x1.25-gmted2010_modis-smooth_cam-old.nc"'  'outputFile="gmted2010-minus-gtopo30-fv-0.9x1.25-topo-vars"' 'format="pdf"' 'title_str="GMTED2010 minus GMTED2010bug (FV0.9x1.25)"' < plot-topo-vars.ncl


#ncl  'lmanual_levels=False' 'ldiff=True' 'not_latlon=False' 'area=-1' 'dataFile="../output/fv0.9x1.25-gtopo30-smooth_cam.nc"' 'dataFile2="fv0.9x1.25-default-PHIS-new-SGH-SGH30.nc"' 'outputFile="gtopo30new-minus-old-fv-0.9x1.25-topo-vars"' 'format="pdf"' 'title_str="GTOPO30 new minus GTOPO30 old"' < plot-topo-vars.ncl


