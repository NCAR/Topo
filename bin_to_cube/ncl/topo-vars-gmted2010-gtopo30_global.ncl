load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
;load "regions.ncl"
begin
;******************************************************
;
; plot topgraphy variables on native grid 
;
;******************************************************

  dataFile_gmted2010 = "../gmted2010_modis-ncube3000.nc"
  dataFile_gtopo30   = "../gmted2010_modis-ncube3000-fix_inland_water_elevation.nc"
;  dataFile_gtopo30   = "../gtopo30-ncube3000.nc"
  format   = "pdf" 
  outputFile = "topo-vars-gmted2010-gtopo30"
  f_gmted2010     = addfile(dataFile_gmted2010,"r")
  f_gtopo30       = addfile(dataFile_gtopo30  ,"r")

  lat1d = f_gmted2010->lat          ; Pull off lat/lon variable and
  lon1d = f_gmted2010->lon          ; convert both to 1D.

  h_gmted2010      = f_gmted2010->terr
  h_gtopo30        = f_gtopo30  ->terr
  sgh30_gtopo30       = f_gtopo30  ->var30
  sgh30_gtopo30 = sqrt(sgh30_gtopo30)
  sgh30_gmted2010     = f_gmted2010->var30
  sgh30_gmted2010 = sqrt(sgh30_gmted2010)
;  landfrac  = f->LANDFRAC

;******************************************************

  res                     = True         ; plot modifications desired
  res@gsnMaximize         = True         ; Maximize size of plot in frame
  res@gsnSpreadColors     = True         ; Use full colormap, but start

  res@sfXArray            = lon1d        ; Required to tell NCL where to
  res@sfYArray            = lat1d        ; overlay data on globe.

  res@cnFillOn            = True         ; Turn on contour fill
;  res@cnFillMode         = "AreaFill"   ; Style of fill. You can also
  res@cnFillMode          = "RasterFill"
                                         ; use "CellFill" and "RasterFill"
  res@cnLinesOn           = False        ; Turn off contour lines
  res@cnLineLabelsOn      = False        ; Turn off contour line labels

  res@lbLabelAutoStride   = True         ; Clean up labelbar labels.

  res@gsnDraw              = False        ; do not draw picture
  res@gsnFrame             = False        ; do not advance frame
  res@mpOutlineOn = False

  area_string = "global"
;  setRegionAttributes(res,area,area_string); for regional plot

  wks = gsn_open_wks(format,outputFile+"_"+area_string)               ; open workstation
;  gsn_define_colormap(wks,"BlAqGrYeOrReVi200")   ; choose colormap
  gsn_define_colormap(wks,"testcmap")   ; choose colormap
  plot = new(2,graphic)
  res@gsnLeftString   = "height"
  res@gsnRightString   = "GMTED2010-GTOPO30"
;  res@cnLevelSelectionMode = "ManualLevels"; manually set the contour levels with the following 3 resources
;  res@cnMinLevelValF  = -500.; set the minimum contour level
;  res@cnMaxLevelValF  = 500.; set the maximum contour level
;  res@cnLevelSpacingF = 50.; set the interval between contours
  plot(0) = gsn_csm_contour_map_ce(wks,h_gmted2010-h_gtopo30,res)
  res@gsnLeftString   = "SGH30"
;  res@gsnRightString   = "GTOPO30"
  res@gsnRightString   = "GMTED2010-GTOPO30"
  res@cnLevelSelectionMode = "ManualLevels"; manually set the contour levels with the following 3 resources
;  res@cnMinLevelValF  = -150.; set the minimum contour level
;  res@cnMaxLevelValF  = 150.; set the maximum contour level
;  res@cnLevelSpacingF = 20.; set the interval between contours
  plot(1) = gsn_csm_contour_map_ce(wks,sgh30_gmted2010-sgh30_gtopo30  ,res)


  gsn_panel(wks,plot,(/1,2/),True)


end
