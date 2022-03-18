pro panelplots,cu=cu,ipanel=p,lont=lont,latt=latt,w0=w0,zoom=zoom,swcorner=swc,size=size,xran=xran,yran=yran $
              ,mxdis=mxdis,block=block,dev=dev,smooth=smooth,raw=raw,profi=profi,xcuqi=cuqi $
              ,dprofi=dprofi,corr=corr,aniso=aniso,pdev=pdev,sdev=sdev

if not keyword_set(w0) then w0 =1

if keyword_set(xran) then begin
   xr=xran
endif
if keyword_set(yran) then begin
   yr=yran
endif
if keyword_set(zoom) then begin
   xr=[zoom(0) , zoom(2) ]
   yr=[zoom(1) , zoom(3) ]
endif
if keyword_set(zoom) then begin
   xr=[zoom(0) , zoom(2) ]
   yr=[zoom(1) , zoom(3) ]
endif
if keyword_set(swc) then begin
   xr=[swc(0),swc(0)+1.2*size]
   yr=[swc(1),swc(1)+size]
endif


rex
!p.charsize=1.75

if keyword_set(aniso) then begin
lev=(findgen(16)-1)/16.
window,re=2,xs=1200,ys=900,w0
wset,w0
amwgct
contour,cu.aniso(*,*,p-1),lev=lev,c_colo=indgen(16),/fill,/xst,/yst,pos=[.075,.1,.8,.9], xtit='Cell #',ytit='Cell #',xr=xr,yr=yr
contour,cu.aniso(*,*,p-1),lev=lev,c_colo=indgen(16),/noer,/xst,/yst,pos=[.075,.1,.8,.9],xr=xr,yr=yr,c_thick=4
xcolorbar,pos=[.83,.2,.85,.8],clev=lev,unit='meters',labsz=1.5,uns=1.5
xyouts,/norm,align=.5,.4,.92," Aniso ",size=3.
xyouts,/norm,align=.5,.5,.02, cu.rf,size=1.3
endif

if keyword_set(corr) then begin
lev=0.4+(0.6/16)*findgen(16)
window,re=2,xs=1200,ys=900,w0
wset,w0
amwgct
;tvlct ,indgen(16)*16,intarr(16),intarr(16)
contour,cu.corr(*,*,p-1),min=-1,lev=lev,c_colo=indgen(16),/cell,/xst,/yst,pos=[.075,.1,.8,.9], xtit='Cell #',ytit='Cell #',xr=xr,yr=yr
contour,cu.dev(*,*,p-1),lev=[1,1000],/foll,/xst,/yst,pos=[.075,.1,.8,.9], xtit='Cell #',ytit='Cell #',xr=xr,yr=yr ,/noer,c_line=2
xcolorbar,pos=[.83,.2,.85,.8],clev=lev,unit='meters',labsz=1.5,uns=1.5
xyouts,/norm,align=.5,.4,.92," 'Profi' ",size=3.
xyouts,/norm,align=.5,.5,.02, cu.rf,size=1.3
endif

if keyword_set(sdev) then begin
lev=(2./16)*findgen(16)
window,re=2,xs=1200,ys=900,w0
wset,w0
amwgct
;tvlct ,indgen(16)*16,intarr(16),intarr(16)
contour,cu.sdev(*,*,p-1),min=-1,lev=lev,c_colo=indgen(16),/cell,/xst,/yst,pos=[.075,.1,.8,.9], xtit='Cell #',ytit='Cell #',xr=xr,yr=yr
contour,cu.dev(*,*,p-1),lev=[1,1000],/foll,/xst,/yst,pos=[.075,.1,.8,.9], xtit='Cell #',ytit='Cell #',xr=xr,yr=yr ,/noer,c_line=2
xcolorbar,pos=[.83,.2,.85,.8],clev=lev,unit='meters',labsz=1.5,uns=1.5
xyouts,/norm,align=.5,.4,.92," 'Profi' ",size=3.
xyouts,/norm,align=.5,.5,.02, cu.rf,size=1.3
endif

if keyword_set(cuqi) then begin
lev=findgen(16)-1
window,re=2,xs=1200,ys=900,w0
wset,w0
amwgct
contour,cu.cuqi(*,*,p-1),lev=lev,c_colo=indgen(16),/fill,/xst,/yst,pos=[.075,.1,.8,.9], xtit='Cell #',ytit='Cell #',xr=xr,yr=yr
contour,cu.cuqi(*,*,p-1),lev=lev,c_colo=indgen(16),/foll,/xst,/yst,pos=[.075,.1,.8,.9], xtit='Cell #',ytit='Cell #',xr=xr,yr=yr ,/noer,c_thick=2
xcolorbar,pos=[.83,.2,.85,.8],clev=lev,unit='meters',labsz=1.5,uns=1.5
xyouts,/norm,align=.5,.4,.92," 'Profi' ",size=3.
xyouts,/norm,align=.5,.5,.02, cu.rf,size=1.3
endif

if keyword_set(dprofi) then begin
lev=(findgen(16)-7.99999)*200.
window,re=2,xs=1200,ys=900,w0
wset,w0
amwgct
contour,cu.profi(*,*,p-1) - cu.dev(*,*,p-1),lev=lev,c_colo=indgen(16),/fill,/xst,/yst,pos=[.075,.1,.8,.9], xtit='Cell #',ytit='Cell #',xr=xr,yr=yr
xcolorbar,pos=[.83,.2,.85,.8],clev=lev,unit='meters',labsz=1.5,uns=1.5
xyouts,/norm,align=.5,.4,.92," 'D(Profi,Dev)' ",size=3.
xyouts,/norm,align=.5,.5,.02, cu.rf,size=1.3
endif

if keyword_set(profi) then begin
lev=(findgen(16)-7.99999)*200.
window,re=2,xs=1200,ys=900,w0
wset,w0
amwgct
contour,cu.profi(*,*,p-1),lev=lev,c_colo=indgen(16),/fill,/xst,/yst,pos=[.075,.1,.8,.9], xtit='Cell #',ytit='Cell #',xr=xr,yr=yr
xcolorbar,pos=[.83,.2,.85,.8],clev=lev,unit='meters',labsz=1.5,uns=1.5
xyouts,/norm,align=.5,.4,.92," 'Profi' ",size=3.
xyouts,/norm,align=.5,.5,.02, cu.rf,size=1.3
endif

if keyword_set(block) then begin
lev=(findgen(16)-7.99999)*200.
window,re=2,xs=1200,ys=900,w0
wset,w0
amwgct
contour,cu.block(*,*,p-1),lev=lev,c_colo=indgen(16),/fill,/xst,/yst,pos=[.075,.1,.8,.9], xtit='Cell #',ytit='Cell #',xr=xr,yr=yr
xcolorbar,pos=[.83,.2,.85,.8],clev=lev,unit='meters',labsz=1.5,uns=1.5
xyouts,/norm,align=.5,.4,.92," 'Blocks' ",size=3.
xyouts,/norm,align=.5,.5,.02, cu.rf,size=1.3
endif

if keyword_set(mxdis) then begin
lev=(findgen(16)-7.99999)*200.
window,re=2,xs=1200,ys=900,w0
wset,w0
amwgct
contour,cu.mxdis(*,*,p-1),lev=lev,c_colo=indgen(16),/fill,/xst,/yst,pos=[.075,.1,.8,.9], xtit='Cell #',ytit='Cell #',xr=xr,yr=yr
contour,cu.mxdis(*,*,p-1),lev=lev,c_colo=indgen(16),/noer,/xst,/yst,pos=[.075,.1,.8,.9],xr=xr,yr=yr,c_thick=4
xcolorbar,pos=[.83,.2,.85,.8],clev=lev,unit='meters',labsz=1.5,uns=1.5
xyouts,/norm,align=.5,.4,.92," MXDIS 'Skeleton' ",size=3.
xyouts,/norm,align=.5,.5,.02, cu.rf,size=1.3
endif


if keyword_set(pdev) then begin
lev=(findgen(16)-7.99999)*200.
window,re=2,xs=1200,ys=900,w0
wset,w0
amwgct
contour,cu.pdev(*,*,p-1),lev=lev,c_colo=indgen(16),/fill,/xst,/yst,pos=[.075,.1,.8,.9], xtit='Cell #',ytit='Cell #',xr=xr,yr=yr
xcolorbar,pos=[.83,.2,.85,.8],clev=lev,unit='meters',labsz=1.5,uns=1.5
xyouts,/norm,align=.5,.4,.92,"'Pdev' i.e. (Raw Topo)-(SmoothTopo)>0",size=3.
xyouts,/norm,align=.5,.5,.02, cu.rf,size=1.3
endif

if keyword_set(dev) then begin
lev=(findgen(16)-7.99999)*200.
window,re=2,xs=1200,ys=900,w0
wset,w0
amwgct
contour,cu.dev(*,*,p-1),lev=lev,c_colo=indgen(16),/fill,/xst,/yst,pos=[.075,.1,.8,.9], xtit='Cell #',ytit='Cell #',xr=xr,yr=yr
xcolorbar,pos=[.83,.2,.85,.8],clev=lev,unit='meters',labsz=1.5,uns=1.5
xyouts,/norm,align=.5,.4,.92,"(Raw Topo)-(SmoothTopo), i.e., unresolved topo",size=3.
xyouts,/norm,align=.5,.5,.02, cu.rf,size=1.3
endif

if keyword_set(smooth) then begin
lev=(findgen(16)-7.99999)*200.
window,re=2,xs=1200,ys=900,w0
wset,w0
amwgct
contour,cu.smooth(*,*,p-1),lev=lev,c_colo=indgen(16),/fill,/xst,/yst,pos=[.075,.1,.8,.9], xtit='Cell #',ytit='Cell #',xr=xr,yr=yr
xcolorbar,pos=[.83,.2,.85,.8],clev=lev,unit='meters',labsz=1.5,uns=1.5
xyouts,/norm,align=.5,.4,.92,"SmoothTopo for 1 degree (fv_09) WACCM ",size=3.
xyouts,/norm,align=.5,.5,.02, cu.rf,size=1.3
endif

if keyword_set(raw) then begin
lev=(findgen(16)-7.999999)*300.
window,re=2,xs=1200,ys=900,w0
wset,w0
amwgct
contour,cu.raw(*,*,p-1),lev=lev,c_colo=indgen(16),/fill,/xst,/yst,pos=[.075,.1,.8,.9], xtit='Cell #',ytit='Cell #',xr=xr,yr=yr
xcolorbar,pos=[.83,.2,.85,.8],clev=lev,unit='meters',labsz=1.5,uns=1.5
xyouts,/norm,align=.5,.4,.92," Raw 3km Topo  ",size=3.
xyouts,/norm,align=.5,.5,.02, cu.rf,size=1.3
endif



return
end
