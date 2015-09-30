;
; This NCL script is a modified version of script provided by Peter Lawrence (NCAR)
;
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

begin

    surfacedataname = "../gmted2010_elevation_and_landfrac_modis_sft.nc"
;    surfacedataname = "../gmted2010_elevation_and_landfrac_modis_sft_fix_inland_water_elevation.nc"
;    surfacedataname = "../gtopo30/gtopo30-rawdata.nc"
    psname = "plotdem"
    modeltitle = "gmted2010 - Caspian Sea"

    in_file = addfile(surfacedataname,"r")
    
;    lllat = -25.0
;    lllon = -75.0
;    urlat = -15.0
;    urlon = -65.0
; 
; Caspian Sea
;   
    lllat = 35.0
    lllon = 44.0
    urlat = 48.0
    urlon = 70.0
;
; Great Lakes

;    lllat = 40.0
;    lllon = -95.0+360.0
;    urlat = 50.0
;    urlon = -75.0+360.0
;
;    lllat = 50.0
;    lllon = 100.0
;    urlat = 60.0
;    urlon = 140.0

;    lllat =  80.0
;    lllon = 337.0
;    urlat = 81.0
;    urlon = 339.0

    
    
    maplat = in_file->lat
    maplatdims = dimsizes(maplat)
    maplatdim = maplatdims(0)
    maplon = in_file->lon
    maplondims = dimsizes(maplon)
    maplondim = maplondims(0)
    
    lllatxy = 0
    urlatxy = 0
    do latcount = 0,maplatdim-1
        foundlat = maplat(latcount)
        if (lllat .gt. foundlat) then
	    lllatxy = latcount
	end if
        foundlat = maplat(latcount)
        if (urlat .gt. foundlat) then
	    urlatxy = latcount
	end if
    end do
    
    lllonxy = 0
    urlonxy = 0
    do loncount = 0,maplondim-1
        foundlon = maplon(loncount)
        if (lllon .gt. foundlon) then
	    lllonxy = loncount
	end if
        foundlon = maplon(loncount)
        if (urlon .gt. foundlon) then
	    urlonxy = loncount
	end if
    end do
    
    demout = in_file->htopo(lllatxy:urlatxy,lllonxy:urlonxy) 
;    demout = in_file->landfract(lllatxy:urlatxy,lllonxy:urlonxy) 
    demout@long_name = modeltitle
    demout@units = "meters"
    demout&lat@units="degrees_north"
    demout&lon@units="degrees_east"
        
    wks = gsn_open_wks("ps",psname)
;    gsn_define_colormap(wks,"wh-bl-gr-ye-re")
;    gsn_define_colormap(wks,"rainbow+white")
    
    res                      = True               ; plot mods desired
    res@tiMainString         = "" ; main title
    res@cnFillOn             = True               ; turn on color fill
    
;    print(res)
;    res@cnLevelSelectionMode = "ExplicitLevels"
;    res@cnLevels = (/1.0,10.0,20.0,30.0,40.0,50.0,60.0,70.0,80.0,90.0/)
;    res@cnLevels = (/1.0,2.5,5.0,10.0,15.0,20.0,30.0,40.0,50.0,75.0/)
    res@cnLinesOn = False
    res@cnLineLabelsOn = False
    res@cnRasterModeOn = True

    res@gsnSpreadColors = True    

    res@gsnDraw  = False                          ; don't draw
    res@gsnFrame = False                          ; don't advance frame
    res@lbLabelBarOn = False

    res@mpLimitMode                 = "Corners"           ; method to zoom
    res@mpLeftCornerLatF            = lllat
    res@mpLeftCornerLonF            = lllon
    res@mpRightCornerLatF           = urlat
    res@mpRightCornerLonF           = urlon
    res@gsnAddCyclic                = False

    plot1 = gsn_csm_contour_map_ce(wks,demout,res)

    pres = True
    pres@gsnPanelLabelBar = True
    pres@pmLabelBarWidthF = 0.8
    
    gsn_panel(wks,plot1,(/1,1/),pres)
        
end
