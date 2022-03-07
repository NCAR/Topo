pro topoplots,cu=cu,ploo=ploo,lont=lont,latt=latt

ploo=where( lont gt -100+360. and lont lt -40+360. and latt gt -75 and latt lt -45 )  

!p.charsize=1.75
lev=(findgen(16)-7.99999)*200.
window,re=2,xs=1200,ys=900,3
wset,3
amwgct
contour,cu.dev(ploo),lont(ploo),latt(ploo),/irr,lev=lev,c_colo=indgen(16)+1,/fill,/xst,/yst,pos=[.075,.1,.8,.9], xtit='Longitude',ytit='Latitude'
xcolorbar,pos=[.83,.2,.85,.8],clev=lev,unit='meters',labsz=1.5,uns=1.5
xyouts,/norm,align=.5,.4,.92,"(Raw 3km Topo)-(SmoothTopo), i.e., unresolved topo",size=3.

lev=(findgen(16)-7.99999)*200.
window,re=2,xs=1200,ys=900,2
wset,2
amwgct
contour,cu.smooth(ploo),lont(ploo),latt(ploo),/irr,lev=lev,c_colo=indgen(16)+1,/fill,/xst,/yst,pos=[.075,.1,.8,.9], xtit='Longitude',ytit='Latitude'
xcolorbar,pos=[.83,.2,.85,.8],clev=lev,unit='meters',labsz=1.5,uns=1.5
xyouts,/norm,align=.5,.4,.92,"SmoothTopo for 1 degree (fv_09) WACCM ",size=3.

lev=(findgen(16)-7.999999)*300.
window,re=2,xs=1200,ys=900,1
wset,1
amwgct
contour,cu.raw(ploo),lont(ploo),latt(ploo),/irr,lev=lev,c_colo=indgen(16)+1,/fill,/xst,/yst,pos=[.075,.1,.8,.9], xtit='Longitude',ytit='Latitude'
xcolorbar,pos=[.83,.2,.85,.8],clev=lev,unit='meters',labsz=1.5,uns=1.5
xyouts,/norm,align=.5,.4,.92," Raw 3km Topo  ",size=3.

capture,w=1,n='RawTopo'
capture,w=2,n='SmoothTopo'
capture,w=3,n='UnresTopo'



return
end
