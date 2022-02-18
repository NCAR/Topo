pro gridjeck,xcase=xcase,fi=fi,co=co,nsw=nsw,nc=nc,ogrid=ogrid,quick=quick,stops=stops $
            ,rema=rema,fcam=fcam,topo=topo,grem=grem,tg=tg,list=list $
            ,cu=cu,latt=latt,lont=lont,itrgt=itrgt,xlist=xlist

 ;d='/project/amp/juliob/Topo-generate-devel/Topo/Ridge-Finding.git/output/'

if not keyword_set(xcase) then begin
   print,'Need xcase'
   stop
endif 
if not keyword_set(nc) then begin
   nc=3000L
endif 
if not keyword_set(ogrid) then begin
   ogrid='fv_0.9x1.25'
endif 

if keyword_set(quick) then begin
   nc=3000L
   ogrid='fv_0.9x1.25'
   Co=60
   Fi=8
   Nsw=42
endif


fnames,xc=xcase,co=co,fi=fi,ns=nsw,og=ogrid,fcam=fcam,grem=grem,rema=rema,topo=topo,tg=tg,list=list,nc=nc

rdgrid,grem=grem,itrgt=itrgt
rncvar,f=tg,get='lon',dat=lont
rncvar,f=tg,get='lat',dat=latt
rncvar,f=tg,get='var30',dat=var30
rdremap,rem=rema,top=topo,cube=cu
rdglist_e02,list=list,xlist=xlist

var30 = reform( var30 , nc , nc, 6)
lont  = reform(  lont , nc , nc, 6)
latt  = reform(  latt , nc , nc, 6)




        if keyword_set(STOPS) then STOP

return
end
