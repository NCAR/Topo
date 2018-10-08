set_plot, 'ps'
Device, /Helvetica, /Color, Bits_per_Pixel=16, File='image.eps'
device, xsize=5.0,xoff=2.0, ysize=5.1 ,yoff=2.0,/inches
p0lat=  35.265
p0lon=  60.0
rot  =  0.0
red = FSC_COLOR("Red", !D.Table_Size-1)
 gray = FSC_COLOR("Dark Gray", !D.Table_Size-3)
 yellow = FSC_COLOR("Yellow", !D.Table_Size-4)
 blue = FSC_COLOR("Blue", !D.Table_Size-2)
 green = FSC_COLOR("Green", !D.Table_Size-4)
map_set, p0lat, p0lon,rot, $
/horizon, /ortho , E_CONTINENTS={FILL:1},/NOBORDER;,$;/CONTINENT,$
;/horizon, /ortho , /CONTINENT, /NOBORDER;,$
;/horizon, /ortho , /CONTINENT, CON_COLOR=5,/NOBORDER;,$
; /horizon, /ortho , title = 'Y!I16!E32!N analytic initial condition',$
;POSITION=[0.15, 0.15, 0.95, 0.95]


Id = NCDF_OPEN('ne16np4_110512_pentagons.nc')
NCDF_VARGET, Id, 'grid_corner_lat', grid_lat
NCDF_VARGET, Id, 'grid_corner_lon', grid_lon
pi=3.14159265
;grid_lon=grid_lon*180./pi
;grid_lat=grid_lat*180./pi
n=13826
j=0

maxEdges=5

x=fltarr(maxEdges)
y=fltarr(maxEdges)


;FOR i=0L,1 DO BEGIN&$
;   FOR j=1L,3 DO BEGIN&$
;      print,i+j&$
;      print,i&$
;   ENDFOR&$
;ENDFOR


for i = 0,n-1 DO BEGIN&$
  for k = 0,maxEdges-1 DO BEGIN&$
     x(k) = grid_lon(k,i)&$
     y(k) = grid_lat(k,i)&$
;     print,j,x(k),y(k)&$
     j=j+1&$
  ENDFOR&$
  
  oplot,x,y, thick =3.8, color=blue&$
endfor 

NCDF_CLOSE, Id
Device, /Close_File
;end
