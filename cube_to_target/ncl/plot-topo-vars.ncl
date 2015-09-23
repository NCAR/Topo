load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "regions.ncl"
begin
;******************************************************
;
; plot topgraphy variables (PHIS,SGH,SGH30,LANDFRAC) on native grid 
;
; driver: ./plot-topo-vars.sh
; 
; author: Peter Hjort Lauritzen, NCAR (pel@ucar.edu)
; date  : July 2014
;
;******************************************************
  print(title_str)

  f     = addfile(dataFile,"r")

  lat1d = f->lat          ; Pull off lat/lon variable and
  lon1d = f->lon          ; convert both to 1D.

  phis      = f->PHIS
  sgh       = f->SGH
  sgh30     = f->SGH30
  landfrac  = f->LANDFRAC

  var1=phis
  var2=sgh
  var3=sgh30
  var4=landfrac

  if (ldiff) then
    f2 = addfile(dataFile2,"r")
    phis_2      = f2->PHIS
    sgh_2       = f2->SGH
    sgh30_2     = f2->SGH30
    landfrac_2  = f2->LANDFRAC

    var1=phis-phis_2
    var2=sgh-sgh_2
    var3=sgh30-sgh30_2
    var4=landfrac-landfrac_2
  end if

;******************************************************

  res                     = True         ; plot modifications desired
  res@gsnMaximize         = True         ; Maximize size of plot in frame
  res@gsnSpreadColors     = True         ; Use full colormap, but start

  if (not_latlon) then
    res@sfXArray            = lon1d        ; Required to tell NCL where to
    res@sfYArray            = lat1d        ; overlay data on globe.
  end if

  res@cnFillOn            = True         ; Turn on contour fill
;  res@cnFillMode         = "AreaFill"   ; Style of fill. You can also
  res@cnFillMode          = "RasterFill"
                                         ; use "CellFill" and "RasterFill"
  res@cnLinesOn           = False        ; Turn off contour lines
  res@cnLineLabelsOn      = False        ; Turn off contour line labels

  res@lbLabelAutoStride   = True         ; Clean up labelbar labels.

  res@gsnDraw              = False        ; do not draw picture
  res@gsnFrame             = False        ; do not advance frame

;  area_string = ""
;  setRegionAttributes(res,area,area_string); for regional plot

  wks = gsn_open_wks(format,outputFile)               ; open workstation
;  gsn_define_colormap(wks,"BlAqGrYeOrReVi200")   ; choose colormap

  if (ldiff) then  
    gsn_define_colormap(wks,"testcmap")
  else
    gsn_define_colormap(wks,"ncl_default")
  end if

  plot = new(4,graphic)
  res@gsnLeftString   = "PHIS"
  if (lmanual_levels) then
    res@cnLevelSelectionMode = "ManualLevels"
    res@cnMinLevelValF  = -4250.
    res@cnMaxLevelValF  =  4250.
    res@cnLevelSpacingF = 500.
  else
    res@cnLevelSelectionMode = "AutomaticLevels"
  end if
  plot(0) = gsn_csm_contour_map_ce(wks,var1,res)
  res@gsnLeftString   = "SGH"
  if (lmanual_levels) then
    res@cnLevelSelectionMode = "ManualLevels"
    res@cnMinLevelValF  = -375.
    res@cnMaxLevelValF  = 375.
    res@cnLevelSpacingF = 50.
  end if
  plot(1) = gsn_csm_contour_map_ce(wks,var2,res)
  if (lmanual_levels) then
    res@cnLevelSelectionMode = "ManualLevels"
    res@cnMinLevelValF  = -75.
    res@cnMaxLevelValF  = 75.
    res@cnLevelSpacingF = 10.
  end if
  res@gsnLeftString   = "SGH30"
  plot(2) = gsn_csm_contour_map_ce(wks,var3,res)
  res@gsnLeftString   = "LANDFRAC"
  if (lmanual_levels) then
    res@cnLevelSelectionMode = "ManualLevels"
    res@cnMinLevelValF  = -1.
    res@cnMaxLevelValF  = 1.
    res@cnLevelSpacingF = 0.08
  end if
  plot(3) = gsn_csm_contour_map_ce(wks,var4,res)

;************************************************
; create panel
;************************************************
  resP                  = True                   ; modify the panel plot
  resP@gsnFrame         = False                  ; don't advance panel plot
  resP@gsnPanelBottom   = 0.05
  resP@txString   = title_str
  gsn_panel(wks,plot,(/2,2/),resP)

;  txresCopyright               = True                      ; text mods desired
;  txresCopyright@txJust        = "CenterLeft"              ; Default is "CenterCenter".
;  txresCopyright@txFontHeightF = 0.01                     ; text font height 
;  gsn_text_ndc(wks,"~F135~c"+"~F30~Lauritzen(pel@ucar.edu,NCAR)",.1,.01,txresCopyright)
  frame(wks)
end
