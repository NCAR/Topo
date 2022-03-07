pro gridjeck,xcase=xcase,fi=fi,co=co,nsw=nsw,nc=nc,ogrid=ogrid,quick=quick,stops=stops $
            ,rema=rema,fcam=fcam,topo=topo,grem=grem,tg=tg,list=list $
            ,cu=cu,latt=latt,lont=lont,itrgt=itrgt,xlist=xlist,xy=xy,terroutput=terroutput

 ;d='/project/amp/juliob/Topo-generate-devel/Topo/Ridge-Finding.git/output/'

if not keyword_set(xcase) then begin
   print,'Need xcase'
   stop
endif 
if not keyword_set(nc) then begin
   nc=3000L
endif 
if not keyword_set(ogrid) then begin
   ogrid='ne30pg3'
endif 

if keyword_set(quick) then begin
   nc=3000L
   ogrid='fv_0.9x1.25'
   Co=60
   Fi=8
   Nsw=42
endif

;topo_smooth_gmted2010_bedmachine_nc0540_Co012_Fi001.nc

fnames,xc=xcase,co=co,fi=fi,ns=nsw,og=ogrid,fcam=fcam,grem=grem,rema=rema,topo=topo,tg=tg,list=list,nc=nc,trxy=trxy


rdgrid,grem=grem,itrgt=itrgt
rncvar,f=tg,get='lon',dat=lont
rncvar,f=tg,get='lat',dat=latt
rncvar,f=tg,get='var30',dat=var30
rncvar,f=tg,get='terr',dat=raw
rncvar,f=topo,get='terr_dev',dat=dev
rncvar,f=topo,get='terr_sm',dat=smooth

rdremap,rem=rema,cube=cu
rdglist_e02,list=list,xlist=xlist

if keyword_set(trxy) and keyword_set(terroutput) then terrxy,xy=xy,trxy=trxy

raw     = reform( raw , nc , nc, 6)
dev     = reform( dev , nc , nc, 6)
smooth  = reform( smooth, nc , nc, 6)
var30   = reform( var30 , nc , nc, 6)
lont    = reform(  lont , nc , nc, 6)
latt    = reform(  latt , nc , nc, 6)

pdev=dev
pdev(where(pdev lt 0))=0.

cu=create_struct( cu, 'raw',   raw )
cu=create_struct( cu, 'dev',   dev )
cu=create_struct( cu, 'pdev',  pdev )
cu=create_struct( cu, 'smooth',smooth )
cu=create_struct( cu, 'lon',lont )
cu=create_struct( cu, 'lat',latt )

        if keyword_set(STOPS) then STOP

return
end
