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
  f     = addfile(dataFile,"r")

  lat1d = f->lat          ; Pull off lat/lon variable and
  lon1d = f->lon          ; convert both to 1D.

  phis      = f->PHIS
  sgh       = f->SGH
  sgh30     = f->SGH30
  landfrac  = f->LANDFRAC

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

  area_string = ""
  setRegionAttributes(res,area,area_string); for regional plot

  wks = gsn_open_wks(format,outputFile+"_"+area_string)               ; open workstation
  gsn_define_colormap(wks,"BlAqGrYeOrReVi200")   ; choose colormap

  plot = new(4,graphic)
  res@gsnLeftString   = "PHIS"
  plot(0) = gsn_csm_contour_map_ce(wks,phis,res)
  res@gsnLeftString   = "SGH"
  plot(1) = gsn_csm_contour_map_ce(wks,sgh,res)
  res@gsnLeftString   = "SGH30"
  plot(2) = gsn_csm_contour_map_ce(wks,sgh30,res)
  res@gsnLeftString   = "LANDFRAC"
  plot(3) = gsn_csm_contour_map_ce(wks,landfrac,res)

    txresCopyright               = True                      ; text mods desired
    txresCopyright@txJust        = "CenterLeft"              ; Default is "CenterCenter".
    txresCopyright@txFontHeightF = 0.01                     ; text font height
    gsn_text_ndc(wks,"~F135~c"+"~F30~Lauritzen(pel@ucar.edu,NCAR)",.1,.01,txresCopyright)

  gsn_panel(wks,plot,(/2,2/),True)


end
