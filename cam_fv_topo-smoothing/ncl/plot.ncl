load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
begin
;******************************************************
;
;
;
;******************************************************

  dataFile1 = "../fv-gmted2010-modis-0.9x1.25.nc"
  dataFile2 = "../fv-gtopo30-0.9x1.25.nc"

  format     = "pdf" 
  outputFile = "out"

  f          = addfile(dataFile1,"r")
  f2         = addfile(dataFile2,"r")

  h        = f  ->PHIS
  h2       = f2 ->PHIS

  g = 9.8196D0

  diff = h
  diff = (h-h2)/g

;******************************************************

  res                     = True         ; plot modifications desired
  res@gsnSpreadColors     = True         ; Use full colormap, but start

  res@cnFillOn            = True         ; Turn on contour fill

  res@cnLinesOn           = False        ; Turn off contour lines
  res@cnLineLabelsOn      = False        ; Turn off contour line labels

  res@lbLabelAutoStride   = True         ; Clean up labelbar labels.

  res@gsnDraw              = False        ; do not draw picture
  res@gsnFrame             = False        ; do not advance frame

  wks = gsn_open_wks(format,"cam-fv-smooth-elevation")               ; open workstation
  gsn_define_colormap(wks,"BlAqGrYeOrReVi200")   ; choose colormap

  plot = new(2,graphic)

  res@tiMainString = "GMTED2010 mean elevation (0.9x1.25); smoothed using CAM-FV algorithm"
  res@gsnLeftString   = "elevation [h]"
  res@gsnRightString   = "meters"

  tmp=h
  tmp=h/g
  plot(0) = gsn_csm_contour_map_ce(wks,tmp,res)

  res@tiMainString = "GMTED2010-GTOPO30 smoothed elevation difference"
  res@gsnLeftString   = "elevation [h]"
  res@gsnRightString   = "meters"

  plot(1) = gsn_csm_contour_map_ce(wks,diff,res)

  resP                  = True                   ; modify the panel plot
  resP@gsnFrame         = False                  ; don't advance panel plot
  resP@gsnPanelBottom   = 0.05                   ; add space at bottom
  resP@gsnPanelFigureStrings= (/"a)","b)"/)      ; add strings to panel
  resP@amJust   = "TopLeft"
  gsn_panel(wks,plot,(/2,1/),resP)    

  frame(wks)

end
