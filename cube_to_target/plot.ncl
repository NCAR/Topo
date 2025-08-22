begin
;************************************************
; open netCDF file
;************************************************
  a = addfile(topoFile,"r")
;  b = addfile("../regression-test-data/ne30pg3_gmted2010_modis_bedmachine_nc0540_Laplace0100_noleak_20250319.nc","r")
;b = addfile("/glade/campaign/cgd/amp/pel/topo/cesm3/ne30pg3_gmted2010_modis_bedmachine_nc3000_Laplace0100_noleak_greenlndantarcsgh30fac2.50_20250814.nc","r")
;  b = addfile("/glade/campaign/cesm/cesmdata/inputdata/atm/cam/topo/se/ne30pg3_gmted2010_modis_bedmachine_nc3000_Laplace0100_noleak_20240720.nc","r")
;  b = addfile("/glade/campaign/cgd/amp/aherring/TopoCESM3/cube_to_target/output/ne30pg3_gmted2010_modis_bedmachine_nc3000_Laplace0100_noleak_20240117_GRISrough.nc","r")
  b = addfile("/glade/campaign/cesm/cesmdata/inputdata/atm/cam/topo/se/ne30np4_nc3000_Co060_Fi001_PF_nullRR_Nsw042_20171020.nc","r")
  if (isvar("b")) then
    print("Reference data provided - plotting differences")
  end if
;
  ; SCRIP file
  ;
  sf =  addfile(scripFile,"r")
  cellfill = True

  if (isfilevar(a, "PHIS")) then
    PHIS      = a->PHIS
    if (isvar("b")) then    
      PHIS_ref  = b->PHIS    
      PHIS = PHIS-PHIS_ref
    end if
  end if
  if (isfilevar(a, "SGH")) then
    SGH       = a->SGH
    if (isvar("b")) then    
      SGH_ref   = b->SGH
      SGH = SGH-SGH_ref
    end if	
  end if
  if (isfilevar(a, "SGH30")) then
    SGH30     = a->SGH30
    if (isvar("b")) then
      SGH30_ref     = b->SGH30
      SGH30 = SGH30-SGH30_ref
    end if
  end if

  if (isfilevar(a, "LANDFRAC")) then
    LANDFRAC_ON_FILE=True
    LANDFRAC = a->LANDFRAC
    if (isvar("b")) then
      LANDFRAC_ref = b->LANDFRAC
      LANDFRAC = LANDFRAC-LANDFRAC_ref
    end if
  else
    LANDFRAC_ON_FILE=False
  end if
  if (isfilevar(a, "rrfac")) then
    RRFAC_ON_FILE=Truex
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
    ;
    ; SCRIP format
    ;
    if (isfilevar(sf, "grid_center_lat")) then
      lat1d   = sf->grid_center_lat
      lon1d   = sf->grid_center_lon
      latvert = sf->grid_corner_lat
      lonvert = sf->grid_corner_lon
    end if
    ;
    ; ESMF format
    ;
    if (isfilevar(sf, "centerCoords")) then 
      centerCoords   = sf->centerCoords
      nodeCoords     = sf->nodeCoords
      numElementConn = sf->numElementConn
      elementConn    = sf->elementConn
;      printVarSummary(nodeCoords)
      ;
      ; convert to SCRIP format
      ;
      lon1d = centerCoords(:,0)
      lat1d = centerCoords(:,1)

     dSizes = getfiledimsizes(sf)  
     dNames = getfiledimnames(sf) 
     do i=0,dimsizes(dNames)-1
       if (dNames(i).eq."maxNodePElement") then
         maxNodePElement = dSizes(i)
       end if
       if (dNames(i).eq."nodeCount") then
         nodeCount = dSizes(i)
       end if
       if (dNames(i).eq."elementCount") then
         elementCount = dSizes(i)
       end if
     end do
     lonvert = new((/elementCount,maxNodePElement/),double)
     latvert = new((/elementCount,maxNodePElement/),double)
     do element=0,elementCount-1
       numNodes = numElementConn(element)
       do node=0,numNodes-1
         j = elementConn(element,node)-1
         tmp_lon = nodeCoords(j,0)
         tmp_lat = nodeCoords(j,1)
         lonvert(element,node) = tmp_lon
         latvert(element,node) = tmp_lat
       end do
       if (node-1.ne.maxNodePElement-1) then
         lonvert(element,node:maxNodePElement-1) = tmp_lon
         latvert(element,node:maxNodePElement-1) = tmp_lat
       end if
     end do
  end if
	
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

  res@mpCenterLonF     = 180                ; set map center at 180
                                            ; must be in colormap  
  plot = gsn_csm_contour_map_ce(wks,PHIS, res) ; create plot
  if (LANDFRAC_ON_FILE) then
    plot = gsn_csm_contour_map_ce(wks,LANDFRAC, res) ; create plot
  end if
  if (RRFAC_ON_FILE) then
    plot = gsn_csm_contour_map_ce(wks,RRFAC, res) ; create plot
  end if
;  res@cnLevelSelectionMode = "ManualLevels"	; manually set the contour levels with the following 3 resources
;  res@cnMinLevelValF  = 0.			; set the minimum contour level
;  res@cnMaxLevelValF  = 300.			; set the maximum contour level
;  res@cnLevelSpacingF = 25.			; 
  plot = gsn_csm_contour_map_ce(wks,SGH, res) ; create plot
  plot = gsn_csm_contour_map_ce(wks,SGH30, res) ; create plot
end
