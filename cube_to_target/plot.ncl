begin
;************************************************
; open netCDF file
;************************************************
  a = addfile(topoFile,"r")
;  b = addfile("../regression-test-data/ne30pg3_gmted2010_bedmachine_nc0540_Co012_20220307.nc","r")
;  PHIS_ref = b->PHIS
  ;
  ; SCRIP file
  ;
  sf =  addfile(scripFile,"r")
  cellfill = True

  if (isfilevar(a, "PHIS")) then
    PHIS      = a->PHIS
  end if
  if (isfilevar(a, "SGH")) then
    SGH       = a->SGH
  end if
  if (isfilevar(a, "SGH30")) then
    SGH30     = a->SGH30
  end if

  if (isfilevar(a, "LANDFRAC")) then
    LANDFRAC_ON_FILE=True
    LANDFRAC = a->LANDFRAC
  else
    LANDFRAC_ON_FILE=False
  end if
  if (isfilevar(a, "rrfac")) then
    RRFAC_ON_FILE=True
    RRFAC = a->rrfac
  else
    RRFAC_ON_FILE=False
  end if

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

;  res@cnFillMode       = "RasterFill"       ; Raster Mode

  res@mpCenterLonF     = 180                ; set map center at 180
                                            ; must be in colormap  
;  plot = gsn_csm_contour_map_ce(wks,PHIS, res) ; create plot
;  if (LANDFRAC_ON_FILE) then
;    plot = gsn_csm_contour_map_ce(wks,LANDFRAC, res) ; create plot
;  end if
  print(RRFAC_ON_FILE)
  if (RRFAC_ON_FILE) then
    plot = gsn_csm_contour_map_ce(wks,RRFAC, res) ; create plot
  end if
;  plot = gsn_csm_contour_map_ce(wks,SGH, res) ; create plot
;  plot = gsn_csm_contour_map_ce(wks,SGH30, res) ; create plot
end
