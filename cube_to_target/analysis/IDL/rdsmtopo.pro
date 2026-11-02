pro rdsmtopo,topofile=ftopo0,cube=cube,basedir=basedir,varres=varres,rr=rr


if not keyword_set(basedir) then begin
   dir$='../output/'
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
readu,1,terr
readu,1,terr_sm
readu,1,terr_dev

if keyword_set(varres) then begin
   rr=dblarr(nc,nc,6)
   readu,1,rr
endif



cube={raw:terr,dev:terr_dev,smooth:terr_sm}

return
end
