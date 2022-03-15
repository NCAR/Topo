pro gridjeck,xcase=xcase,xtag=xtag,repo=repo,fi=fi,co=co,nsw0=nsw0,nc=nc,ogrid=ogrid,quick=quick,stops=stops $
            ,rema=rema,fcam=fcam,topo=topo,grem=grem,tg=tg,list=list $
            ,cu=cu,latt=latt,lont=lont,itrgt=itrgt,xlist=xlist,xy=xy,terroutput=terroutput,xloutput=xloutput $
            ,rr=rr

 ;d='/project/amp/juliob/Topo-generate-devel/Topo/Ridge-Finding.git/output/'

if not keyword_set(nc) then begin
   nc=3000L
endif 
nc=long(nc)
if not keyword_set(ogrid) then begin
   ogrid='ne30pg3'
endif 

if keyword_set(nsw0) then begin
    nsw=0
endif else begin
    nsw=fix( co/SQRT(2.) )
endelse

if not keyword_set(xcase) then begin
   if nc eq 3000 then xcase=ogrid+'_co'+strtrim(string(co),2)+'_fi'+strtrim(string(fi),2)+'_'+xtag
   if nc eq  540 then xcase='Reg_'+ogrid+'_co'+strtrim(string(co),2)+'_fi'+strtrim(string(fi),2)+'_'+xtag
   print,xcase
endif 

;topo_smooth_gmted2010_bedmachine_nc0540_Co012_Fi001.nc

fnames,xc=xcase,repo=repo,co=co,fi=fi,ns=nsw,og=ogrid,fcam=fcam,grem=grem,rema=rema,topo=topo,tg=tg,list=list,nc=nc,trxy=trxy


rdgrid,grem=grem,itrgt=itrgt
rncvar,f=tg,get='lon',dat=lont
rncvar,f=tg,get='lat',dat=latt
rncvar,f=tg,get='var30',dat=var30
rncvar,f=tg,get='terr',dat=raw
rncvar,f=topo,get='terr_dev',dat=dev
rncvar,f=topo,get='terr_sm',dat=smooth
if keyword_set(rr) then rncvar,f=topo,get='rr_fac',dat=rrfac

rdremap,rem=rema,cube=cu
if keyword_set(xloutput) then rdglist_e02,list=list,xlist=xlist

if keyword_set(trxy) and keyword_set(terroutput) then terrxy,xy=xy,trxy=trxy

raw     = reform( raw , nc , nc, 6)
dev     = reform( dev , nc , nc, 6)
smooth  = reform( smooth, nc , nc, 6)
var30   = reform( var30 , nc , nc, 6)
lont    = reform(  lont , nc , nc, 6)
latt    = reform(  latt , nc , nc, 6)
if keyword_set(rr) then rrfac  = reform(  rrfac , nc , nc, 6)
pdev=dev
pdev(where(pdev lt 0))=0.

cu=create_struct( cu, 'raw',   raw )
cu=create_struct( cu, 'dev',   dev )
cu=create_struct( cu, 'pdev',  pdev )
cu=create_struct( cu, 'smooth',smooth )
cu=create_struct( cu, 'lon',lont )
cu=create_struct( cu, 'lat',latt )
if keyword_set(rr) then cu=create_struct( cu, 'rrfac',rrfac )

        if keyword_set(STOPS) then STOP

return
end
