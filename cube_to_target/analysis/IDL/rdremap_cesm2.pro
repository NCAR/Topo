pro rdremap_cesm2,remapfile=fn0,cube=cube,basedir=basedir,cesm2=cesm2 $
           ,fvgrid=fvgrid,stop=stop,only_topo=only_topo

if not keyword_set(basedir) then begin
   ;rewondir$='/project/amp/juliob/Topo-generate-devel/Topo/cube_to_target_devel/output/'
   dir$=''
endif else begin
   dir$=basedir
endelse


f=dir$+fn0
rf$ = fn0

nc=0l & npeaks=0l
close,1
openr,1,/f77,f
readu,1,nc,npeaks



mxdis=dblarr(nc,nc,6)
block=dblarr(nc,nc,6)
profi=dblarr(nc,nc,6)
uniqi=dblarr(nc,nc,6)
aniso=dblarr(nc,nc,6)
nodes=dblarr(nc,nc,6)
wedge=dblarr(nc,nc,6)
nodos=dblarr(nc,nc,6)
wedgo=dblarr(nc,nc,6)
wedgi=dblarr(nc,nc,6)
anglx=dblarr(nc,nc,6)
hwdth=dblarr(nc,nc,6)
clngt=dblarr(nc,nc,6)
riseq=dblarr(nc,nc,6)
fallq=dblarr(nc,nc,6)
uniqw=dblarr(nc,nc,6)

xs=fltarr(npeaks)
ys=fltarr(npeaks)
xspk=fltarr(npeaks)
yspk=fltarr(npeaks)
ipks=lonarr(npeaks)
jpks=lonarr(npeaks)

dum=0.d



readu,1,mxdis


close,1
print," read from"
print,f


cube={mxdis:mxdis  $ 
     ,block:block,profi:profi,uniqi:uniqi  $ 
     ,anglx:anglx,hwdth:hwdth,clngt:clngt,aniso:aniso  $ 
     ,nodes:nodes,wedge:wedge,riseq:riseq,fallq:fallq  $
     ,nodos:nodos,wedgo:wedgo,uniqw:uniqw,wedgi:wedgi  $
     ,rf:fn0}


if keyword_set(stop) then STOP

return
end
