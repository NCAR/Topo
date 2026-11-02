pro southeast_us,cu=cu,lont=lont,latt=latt,w0=w0

if not keyword_set(w0) then w0=0
rex
window,re=2,xs=1100,ys=900,1 + w0

ploo=where( lont gt -105+360. and lont lt -80+360. and latt gt 25 and latt lt 43 )
lev=[1,100,200,300,400,500+indgen(6)*100., 1200.+indgen(5)*200. ]
wset,1+w0&amwgct&contour,cu.raw(ploo),lont(ploo),latt(ploo),/irr,lev=lev,c_colo=indgen(16),/fill,/xst,/yst

;circlesym 
;oplot,[-105.1+360.],[40.16],ps=8,syms=2
;oplot,[-105.52+360.],[40.38],ps=8,syms=2

towns

lev=(findgen(16)-7.99)*50.
window,re=2,xs=1100,ys=900,2 + w0
wset,2+w0&amwgct&contour,cu.block(ploo),lont(ploo),latt(ploo),/irr,lev=lev,c_colo=indgen(16),/fill,/xst,/yst
towns
window,re=2,xs=1100,ys=900,3 + w0
wset,3+w0&amwgct&contour,cu.dev(ploo),lont(ploo),latt(ploo),/irr,lev=lev,c_colo=indgen(16),/fill,/xst,/yst
towns

STOP

return
end

