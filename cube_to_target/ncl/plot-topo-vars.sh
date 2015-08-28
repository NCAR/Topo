ncl  'lmanual_levels=False' 'ldiff=False' 'not_latlon=False' 'area=-1' 'dataFile="../gmted2010-fv-0.9x1.25.nc"'  'outputFile="gmted2010-fv-0.9x1.25-topo-vars"' 'format="pdf"' 'title_str="Variables based on GMTED2010 elevation and MODIS land fraction"' < plot-topo-vars.ncl
ncl  'lmanual_levels=False' 'ldiff=False' 'not_latlon=False' 'area=-1' 'dataFile="../gtopo30-fv-0.9x1.25.nc"'  'outputFile="gtopo30-fv-0.9x1.25-topo-vars"' 'format="pdf"' 'title_str="Variables based on GTOPO30 elevation and land fraction"' < plot-topo-vars.ncl
ncl  'lmanual_levels=True' 'ldiff=True' 'not_latlon=False' 'area=-1' 'dataFile="../gmted2010-fv-0.9x1.25.nc"' 'dataFile2="../gtopo30-fv-0.9x1.25.nc"'  'outputFile="gmted2010-minus-gtopo30-fv-0.9x1.25-topo-vars"' 'format="pdf"' 'title_str="GMTED2010 minus GTOPO30"' < plot-topo-vars.ncl


ncl  'lmanual_levels=False' 'ldiff=True' 'not_latlon=False' 'area=-1' 'dataFile="../gtopo30-fv-0.9x1.25.nc"' 'dataFile2="fv0.9x1.25-default-PHIS-new-SGH-SGH30.nc"' 'outputFile="gtopo30new-minus-old-fv-0.9x1.25-topo-vars"' 'format="pdf"' 'title_str="GTOPO30 new minus GTOPO30 old"' < plot-topo-vars.ncl
