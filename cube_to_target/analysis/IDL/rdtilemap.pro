pro rdtilemap,tile=tile,r=r

close,1
openr,1,/f77,tile

ntarget=0L&nalloc=0L&mxobj=0L&npeaks=0L

readu,1,ntarget,nalloc,mxobj,npeaks

idxmap=lonarr(nalloc,ntarget)&idcoun=lonarr(ntarget)

numobjects=lonarr(ntarget)
myobject = lonarr(ntarget,mxobj)
wtobject = dblarr(ntarget,mxobj)
Lnobject = dblarr(ntarget,mxobj)
xwoid    = dblarr(ntarget,mxobj)
ywoid    = dblarr(ntarget,mxobj)
xloid    = dblarr(ntarget,mxobj)
yloid    = dblarr(ntarget,mxobj)
latc     = dblarr(ntarget,mxobj)
lonc     = dblarr(ntarget,mxobj)
latw     = dblarr(ntarget,mxobj)
lonw     = dblarr(ntarget,mxobj)
panel    = lonarr(ntarget,mxobj)
clext    = dblarr(ntarget,mxobj)
clngt    = dblarr(ntarget,mxobj)
aniso    = dblarr(ntarget,mxobj)
anglx    = dblarr(ntarget,mxobj)
mxdis    = dblarr(ntarget,mxobj)
hwdth    = dblarr(ntarget,mxobj)
clngt2   = fltarr(npeaks)


readu,1,idxmap,idcoun
readu,1,NumObjects
readu,1,MyObject
readu,1,WtObject
readu,1,LnObject
readu,1,xwoid
readu,1,ywoid
readu,1,xloid
readu,1,yloid
readu,1,panel

readu,1,lonc
readu,1,latc
readu,1,lonw
readu,1,latw

readu,1,aniso
readu,1,anglx
readu,1,mxdis
readu,1,hwdth
readu,1,clngt
readu,1,clext
readu,1,clngt2


stop
close,1

r={file:tile, idxmap:idxmap , idcoun:idcoun $
  ,MyObject:MyObject , LnObject:LnObject , WtObject:WtObject   $ 
  ,NumObjects:NumObjects $
  ,clext:clext,clngt:clngt,hwdth:hwdth,mxdis:mxdis $
  ,aniso:aniso,anglx:anglx,clngt2:clngt2}



return
end
