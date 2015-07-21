;
; define regional area attributes
;
; author: Peter Hjort Lauritzen, NCAR, pel@ucar.edu
; date  : July, 2014
;
;
undef("setRegionAttributes")
procedure setRegionAttributes (res:logical, area:integer, area_string:string)
;
; xy_lon = True : x-axis in line plot is longitude
;
; area:
;
; area=-1       : Global
; area=0 	: India+Nepal
; area=1  	: Equatorial
; area=2 	: India
; area=3  	: US
; area=4  	: South America
;
;
;local yBot, xLeft, xRight, txres, TimeStamp
begin
  if (area.eq.-1) then
    area_string = "global"
    MinLatF     = -90.                   
    MaxLatF     =  90.                    
    MinLonF     =   0.                    
    MaxLonF     = 360.  
  end if
  if (area.eq.0) then
    area_string = "himalayas"
    MinLatF     = 0.                  
    MaxLatF     = 40.                 
    MinLonF     =  35.                 
    MaxLonF     = 100.  
  end if

  if (area.eq.1) then
    area_string = "equator"
    MinLatF     = -30.   
    MaxLatF     = 30.    
  end if

  if (area.eq.2) then
    area_string = "india"
    MinLatF     = 0.
    MaxLatF     = 36.
    MinLonF     =  40.
    MaxLonF     = 90.
  end if

  if (area.eq.3) then
    area_string = "usa"
    MinLatF     = 0.
    MaxLatF     = 60.
    MinLonF     = 230.0
    MaxLonF     = 300.0
  end if

  if (area.eq.4) then
    area_string = "south_america"
    MinLatF     = -70.              
    MaxLatF     = 15.               
    MinLonF     =  250.0            
    MaxLonF     = 330.0
  end if
  ;
  ; for contour plots
  ;
  res@mpMinLatF    = MinLatF
  res@mpMaxLatF    = MaxLatF
  res@mpMinLonF    = MinLonF
  res@mpMaxLonF    = MaxLonF  
  res@mpCenterLonF = (MaxLonF + MinLonF)/2

  return(area_string)
end
