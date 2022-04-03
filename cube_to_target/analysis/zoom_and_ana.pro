pro zoom_and_ana,cu=cu,xl=x,panel=p,xr=xr,yr=yr,terr=t

;Problem areas (based on 3000x3000x6)
;
; S. Andes  xr=[100,220],  yr=[1950,2050],  panel=5


if not keyword_set(xr) then xr = [100,220]
if not keyword_set(yr) then yr = [1950,2050]
if not keyword_set(p)  then p  = 5

ox = where( x.panel eq p and $
            (x.xs ge xr(0) and x.xs le xr(1) ) and $
            (x.ys ge yr(0) and x.ys le yr(1) ) )


s=size(cu.dev)&nc=s(1)
uixy = cu.uniqi( xr(0):xr(1) , yr(0):yr(1) , p-1 )
ui = cu.uniqi( xr(0):xr(1) , yr(0):yr(1) , p-1 )
ui = ui(where(ui gt 0) )

; collect distinct values of ui

ui0 = long( min( ui ) )
ui1 = long( max( ui ) )

uivs = lonarr( ui1 - ui0 + 1)
uils = lonarr( ui1 - ui0 + 1)

for l=ui0,ui1 do begin

    oo=where( ui eq l)
    if max(oo) gt -1 then uivs(l-ui0)=l
    if max(oo) gt -1 then uils(l-ui0)= n_elements(oo)

endfor

uivs = uivs( where(uivs gt 0))
uils = uils( where(uils gt 0))

idx  = uivs*0
for l=0,n_elements( idx )-1 do begin
   oo=where( long(x.uniqid) eq uivs(l))
   idx(l) = oo(0)
endfor

xv=findgen(nc)+1
yv=findgen(nc)+1

lev=[-1,0,1,10, 20, 50, 100, 200, 500, 1000, 2000, 3000 ]
window,re=2,1,xs=1100,ys=900
amwgct
contour,cu.mxdis(*,*,p-1),xv,yv,xr=xr,yr=yr,/xst,/yst,thick=2,lev=lev,c_colo=indgen(16)
;contour,cu.super(*,*,p-1),xv,yv,xr=xr,yr=yr,/xst,/yst,thick=2,lev=lev,c_colo=indgen(16)
contour,cu.dev(*,*,p-1),xv,yv,xr=xr,yr=yr,/xst,/yst,lev=[-500,-200,-100,0,100,200,500,1000],/noer,c_line=[2,2,2,0,0,0,0,0],c_thick=[1,1,1,1,1,1,2,2],/fol
contour,cu.block(*,*,p-1),xv,yv,xr=xr,yr=yr,/xst,/yst,lev=[-500,-200,-100,0,100,200,500,1000],/noer,c_line=[2,2,2,0,0,0,0,0],c_thick=[1,1,1,1,1,1,2,2],/fol,c_colo=12

circlesym
stdcolo
oplot,x.xspk(idx),x.yspk(idx),ps=8,syms=3
for l=0,n_elements( idx )-1 do begin
   id$ = strtrim(  string(    long(x.uniqid( idx(l) ) )),2) 
   idx$ = strtrim(  string(    long(idx(l)) ),2)
   xyouts,/data, x.xspk(idx(l)),x.yspk(idx(l)) , '  '+idx$,size=2, colo=1
endfor
circlesym,nv=4
oplot,x.xs(idx),x.ys(idx),ps=8,syms=3
for l=0,n_elements( idx )-1 do begin
   id$ = strtrim(  string(    long(x.uniqid( idx(l) ) )),2) 
   idx$ = strtrim(  string(    long(idx(l)) ),2) 
   xyouts,/data, x.xs(idx(l)),x.ys(idx(l)) , '  '+idx$,size=2, colo=1
endfor

STOP

return
end
