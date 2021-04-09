begin
;************************************************
; open netCDF file
;************************************************
  a = addfile("ne30pg3_nc3000_Co060_Fi001_MulG_PF_Nsw042.nc","r")
  ;
  ; SCRIP file
  ;
  sf =  addfile("../inputdata/grid-descriptor-file/ne30pg3.nc","r")
  cellfill = True
;************************************************
; Read in Regression Coef
;************************************************
  PHIS  = a->PHIS
;  PHIS = a->GBXAR
;************************************************
; create plot
;************************************************

  wks = gsn_open_wks("pdf","raster")         ; send graphics to PNG file
  gsn_define_colormap(wks,"BlAqGrYeOrReVi200")   ; choose colormap

  res                     = True         ; plot modifications desired
  res@gsnMaximize         = False         ; Maximize size of plot in frame
  res@cnFillOn            = True         ; Turn on contour fill
                                         ; use "CellFill" and "RasterFill"
  res@cnLinesOn           = False        ; Turn off contour lines
  res@cnLineLabelsOn      = False        ; Turn off contour line labels
  res@lbLabelAutoStride   = True         ; Clean up labelbar labels.

  if (cellfill) then
    lat1d   = sf->grid_center_lat
    lon1d   = sf->grid_center_lon
    latvert = sf->grid_corner_lat
    lonvert = sf->grid_corner_lon
	
    if (lonvert@units.eq."radians") then
      r2d  = get_r2d("double")
      latvert = r2d*latvert
      lonvert = r2d*lonvert
    end if

    res@cnFillMode = "CellFill"
    res@sfXCellBounds = lonvert
    res@sfYCellBounds = latvert

    delete([/sf,latvert,lonvert/])
  else
    lat1d = a->lat
    lon1d = a->lon
    res@cnFillMode          ="RasterFill"
  end if   

  res@sfXArray            = lon1d        ; Required to tell NCL where to
  res@sfYArray            = lat1d        ; overlay data on globe.

;===================================
; you can have the contour lines on in raster mode, but their thickness
; actually make the plot look like is was contoured normally.

;  res@cnFillMode       = "RasterFill"       ; Raster Mode
;================================
; these three resources make the continents look nice. The first two
; make them color, and the later adds continental outlines when
; raster mode is used.



  res@mpCenterLonF     = 180                ; set map center at 180
                                            ; must be in colormap  
  plot = gsn_csm_contour_map_ce(wks,PHIS, res) ; create plot

end
