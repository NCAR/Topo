pro panelplots,cu=cu,ipanel=p,lont=lont,latt=latt,w0=w0 $
              ,mxdis=mxdis,block=block,dev=dev,smooth=smooth,raw=raw,profi=profi

if not keyword_set(w0) then w0 =0 

rex
!p.charsize=1.75

if keyword_set(profi) then begin
lev=(findgen(16)-7.99999)*200.
window,re=2,xs=1200,ys=900,w0
wset,w0
amwgct
contour,cu.profi(*,*,p-1),lev=lev,c_colo=indgen(16)+1,/fill,/xst,/yst,pos=[.075,.1,.8,.9], xtit='Longitude',ytit='Latitude'
xcolorbar,pos=[.83,.2,.85,.8],clev=lev,unit='meters',labsz=1.5,uns=1.5
xyouts,/norm,align=.5,.4,.92," 'Profi' ",size=3.
xyouts,/norm,align=.5,.4,.02, cu.rf,size=1.7
endif

if keyword_set(block) then begin
lev=(findgen(16)-7.99999)*200.
window,re=2,xs=1200,ys=900,w0
wset,w0
amwgct
contour,cu.block(*,*,p-1),lev=lev,c_colo=indgen(16)+1,/fill,/xst,/yst,pos=[.075,.1,.8,.9], xtit='Longitude',ytit='Latitude'
xcolorbar,pos=[.83,.2,.85,.8],clev=lev,unit='meters',labsz=1.5,uns=1.5
xyouts,/norm,align=.5,.4,.92," 'Blocks' ",size=3.
xyouts,/norm,align=.5,.4,.02, cu.rf,size=1.7
endif

if keyword_set(mxdis) then begin
lev=(findgen(16)-7.99999)*200.
window,re=2,xs=1200,ys=900,w0
wset,w0
amwgct
contour,cu.mxdis(*,*,p-1),lev=lev,c_colo=indgen(16)+1,/fill,/xst,/yst,pos=[.075,.1,.8,.9], xtit='Longitude',ytit='Latitude'
xcolorbar,pos=[.83,.2,.85,.8],clev=lev,unit='meters',labsz=1.5,uns=1.5
xyouts,/norm,align=.5,.4,.92," MXDIS 'Skeleton' ",size=3.
xyouts,/norm,align=.5,.4,.02, cu.rf,size=1.7
endif


if keyword_set(dev) then begin
lev=(findgen(16)-7.99999)*200.
window,re=2,xs=1200,ys=900,w0
wset,w0
amwgct
contour,cu.dev(*,*,p-1),lev=lev,c_colo=indgen(16)+1,/fill,/xst,/yst,pos=[.075,.1,.8,.9], xtit='Longitude',ytit='Latitude'
xcolorbar,pos=[.83,.2,.85,.8],clev=lev,unit='meters',labsz=1.5,uns=1.5
xyouts,/norm,align=.5,.4,.92,"(Raw 3km Topo)-(SmoothTopo), i.e., unresolved topo",size=3.
xyouts,/norm,align=.5,.4,.02, cu.rf,size=1.7
endif

if keyword_set(smooth) then begin
lev=(findgen(16)-7.99999)*200.
window,re=2,xs=1200,ys=900,w0
wset,w0
amwgct
contour,cu.smooth(*,*,p-1),lev=lev,c_colo=indgen(16)+1,/fill,/xst,/yst,pos=[.075,.1,.8,.9], xtit='Longitude',ytit='Latitude'
xcolorbar,pos=[.83,.2,.85,.8],clev=lev,unit='meters',labsz=1.5,uns=1.5
xyouts,/norm,align=.5,.4,.92,"SmoothTopo for 1 degree (fv_09) WACCM ",size=3.
xyouts,/norm,align=.5,.4,.02, cu.rf,size=1.7
endif

if keyword_set(raw) then begin
lev=(findgen(16)-7.999999)*300.
window,re=2,xs=1200,ys=900,w0
wset,w0
amwgct
contour,cu.raw(*,*,p-1),lev=lev,c_colo=indgen(16)+1,/fill,/xst,/yst,pos=[.075,.1,.8,.9], xtit='Longitude',ytit='Latitude'
xcolorbar,pos=[.83,.2,.85,.8],clev=lev,unit='meters',labsz=1.5,uns=1.5
xyouts,/norm,align=.5,.4,.92," Raw 3km Topo  ",size=3.
xyouts,/norm,align=.5,.4,.02, cu.rf,size=1.7
endif



return
end
