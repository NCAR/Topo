pro namerica,cu=cu,lont=lont,latt=latt,w0=w0

if not keyword_set(w0) then w0=0
rex
window,re=2,xs=1100,ys=900,1 + w0

ploo=where( lont gt -130+360. and lont lt -80+360. and latt gt 15 and latt lt 55 )
lev=[1,100,200,500,800,1100+indgen(11)*300.]
wset,1+w0&amwgct&contour,cu.raw(ploo),lont(ploo),latt(ploo),/irr,lev=lev,c_colo=indgen(16),/fill,/xst,/yst
xyouts,/norm,.5,.95,align=0.5,"Raw Topo",size=2
towns,/colorado


lev=(findgen(16)-7.99)*200.
window,re=2,xs=1100,ys=900,2 + w0
wset,2+w0&amwgct&contour,cu.block(ploo),lont(ploo),latt(ploo),/irr,lev=lev,c_colo=indgen(16),/fill,/xst,/yst
xyouts,/norm,.5,.95,align=0.5,"Blocks",size=2
towns,/colorado
window,re=2,xs=1100,ys=900,3 + w0
wset,3+w0&amwgct&contour,cu.dev(ploo),lont(ploo),latt(ploo),/irr,lev=lev,c_colo=indgen(16),/fill,/xst,/yst
xyouts,/norm,.5,.95,align=0.5,"Topo Dev",size=2
towns,/colorado
window,re=2,xs=1100,ys=900,4 + w0
wset,4+w0&amwgct&contour,cu.profi(ploo),lont(ploo),latt(ploo),/irr,lev=lev,c_colo=indgen(16),/fill,/xst,/yst
xyouts,/norm,.5,.95,align=0.5,"Blocks 2",size=2
towns,/colorado
window,re=2,xs=1100,ys=900,5 + w0
wset,5+w0&amwgct&contour,cu.mxdis(ploo),lont(ploo),latt(ploo),/irr,lev=lev,c_colo=indgen(16),/fill,/xst,/yst
wset,5+w0&amwgct&contour,cu.mxdis(ploo),lont(ploo),latt(ploo),/irr,lev=lev,c_colo=indgen(16),/foll,/noer,/xst,/yst,c_thick=3
xyouts,/norm,.5,.95,align=0.5,"Mxdis",size=2
towns,/colorado

STOP

return
end

