
;
  ftarget   = addfile ("10min_grid.nc", "r")
;
; GMTED2010
;
if (gmted2010_modis)
  str = "../../create_netCDF_from_rawdata/gmted2010_elevation_and_landfrac_modis_sft_fix_inland_water_elevation.nc"
  print("Using GMTED2010/MODIS raw data")
  fsource     = addfile(str,"r")
  fname_out = "10min-gmted2010-modis-phis-raw.nc"
else
  print("Using GTOP030 raw data")
  str="../../create_netCDF_from_rawdata/gtopo30/gtopo30-rawdata.nc"
  fsource     = addfile(str,"r")
  fname_out = "10min-gtopo30-phis-raw.nc"
end if
;
; GTOPO30 data
;
;  fsource     = addfile("../../create_netCDF_from_rawdata/gtopo30/usgs-rawdata-gtopo30.nc","r")
;
  h = fsource->htopo
  l = fsource->landfract  
;
;  printVarSummary( h )
;  
  lon = ftarget->lon
  lat = ftarget->lat
;
  htopo  = area_hi2lores (h&lon,h&lat, h , True, 1,  lon, lat, True)  ; (ntim,49,120)
  ftopo  = area_hi2lores (l&lon,l&lat, l , True, 1,  lon, lat, True)  ; (ntim,49,120)
;
  system("/bin/rm -f " + fname_out)
  fout = addfile (fname_out, "c")
;
  htopo!0   = "lat"
  htopo&lat = lat 
  htopo!1   = "lon"
  htopo&lon = lon 
;
  ftopo!0   = "lat"
  ftopo&lat = lat
  ftopo!1   = "lon"
  ftopo&lon = lon

  fout->lon=lon
  fout->lat=lat
  fout->htopo=htopo
  fout->ftopo=ftopo

  fout@history = "created from "+str

