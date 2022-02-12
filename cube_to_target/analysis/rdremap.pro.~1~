pro rdremap,remapfile=fn0,topofile=ftopo0,cube=cube,basedir=basedir,cesm2=cesm2 $
           ,fvgrid=fvgrid,stop=stop

if not keyword_set(basedir) then begin
   ;rewondir$='/project/amp/juliob/Topo-generate-devel/Topo/cube_to_target_devel/output/'
   dir$=''
endif else begin
   dir$=basedir
endelse

;dir$='/project/amp/juliob/topo_git/Topo/cube_to_target/output/'
ftopo=dir$+ftopo0

nc=0L&npeaks=0L
close,1
openr,1,/f77,ftopo
readu,1,nc
terr=dblarr(nc,nc,6)
terr_sm=dblarr(nc,nc,6)
terr_dev=dblarr(nc,nc,6)
rrfac=dblarr(nc,nc,6)
readu,1,terr
readu,1,terr_sm
readu,1,terr_dev
;readu,1,rrfac


f=dir$+fn0

rf$ = fn0
tf$ = ftopo0

close,1
openr,1,/f77,f
readu,1,nc,npeaks



mxdis=dblarr(nc,nc,6)
block=dblarr(nc,nc,6)
profi=dblarr(nc,nc,6)
uniqi=dblarr(nc,nc,6)

;++ more and more
isoht=dblarr(nc,nc,6)
bumps=dblarr(nc,nc,6)


;anglx=dblarr(nc,nc,6)
;hwdth=dblarr(nc,nc,6)
;clngt=dblarr(nc,nc,6)
;cwght=dblarr(nc,nc,6)


dum=0.d
readu,1,mxdis
readu,1,block
readu,1,profi
readu,1,uniqi
readu,1,isoht
readu,1,bumps


xs=fltarr(npeaks)
ys=fltarr(npeaks)
xspk=fltarr(npeaks)
yspk=fltarr(npeaks)
ipks=lonarr(npeaks)
jpks=lonarr(npeaks)

readu,1,xs,ys,xspk,yspk,ipks,jpks

if not eof(1) then begin
   anglx=dblarr(nc,nc,6)
   hwdth=dblarr(nc,nc,6)
   clngt=dblarr(nc,nc,6)
   cwght=dblarr(nc,nc,6)
   isowd=dblarr(nc,nc,6)
   extension=1
   readu,1,anglx
   readu,1,hwdth
   readu,1,cwght
   readu,1,clngt
   readu,1,isowd
   print,' Extended remap file '
endif



close,1
print," read from"
print,f

pdev=terr_dev
pdev(where(pdev lt 0))=0.

if not keyword_set(extension) then begin
cube={raw:terr,dev:terr_dev,smooth:terr_sm,pdev:pdev,mxdis:mxdis  $ 
     ,block:block,profi:profi,uniqi:uniqi,bumps:bumps  $ 
     ,rf:fn0,tf:ftopo0}
endif else begin
cube={raw:terr,dev:terr_dev,smooth:terr_sm,pdev:pdev,mxdis:mxdis  $ 
     ,block:block,profi:profi,uniqi:uniqi,bumps:bumps  $ 
     ,anglx:anglx,hwdth:hwdth,clngt:clngt  $ 
     ,rf:fn0,tf:ftopo0}
endelse


if keyword_set(fvgrid) then begin
   nx=fvgrid.nx & ny=fvgrid.ny
   jxx = long( (itrgt - 1.0 ) / ny )
   ixx = long( (itrgt - 1.0 ) - jxx*ny )
endif


if keyword_set(stop) then STOP

return
end
