pro rdglist_e02,list=fn0,terr=terr,basedir=basedir$ , $
                xlist=xlist


if not keyword_set(basedir$) then basedir$ = '' ;'/project/amp/juliob/Topo-generate-devel/Topo/cube_to_target_devel/output/'

fn=basedir$ + fn0

close,1
 openr,1,/f77_u,fn


;nc=0L & nch=0L & grid_length_scale=0.D
;readu,1,nch , grid_length_scale
;t1=fltarr(nch,nch,6)
;t1d=fltarr(nch,nch,6)
;dtg=dblarr(nch,nch)
;xv=fltarr(nch)&yv=fltarr(nch)
; readu,1,xv,yv
; readu,1,t1
; readu,1,t1d ;pr,dtg

 
npks=0L & m=0L & NSW=0L & PSW=0L
 readu,1,npks  , NSW, PSW

  pki=lonarr(npks) & pkj=lonarr(npks)&pkip=lonarr(npks)
 
  xs=  fltarr(npks) &ys=fltarr(npks)&xspk=fltarr(npks)&yspk=fltarr(npks)
  mypanel = lonarr( npks )

  readu,1,xs,ys   , mypanel

  mxdis=fltarr(npks) & clngt=fltarr(npks) & hwdth=fltarr(npks) & anglx=fltarr(npks)
  aniso=fltarr(npks) & pkhts=fltarr(npks) & vldps=fltarr(npks) & npeak=fltarr(npks)
  angll=fltarr(npks) & rwpks=fltarr(npks) & rwvls=fltarr(npks)
  isoht=fltarr(npks) & isowd=fltarr(npks) & isobs=fltarr(npks) 
  RefFac=fltarr(npks)

  uqrid=fltarr(npks)

  xspk= fltarr(npks) & yspk= fltarr(npks)

 rdg_profiles = fltarr( Psw+1,npks )
 crst_profiles = fltarr( Psw+1,npks )
 crst_silhous = fltarr( Psw+1,npks )
 rt_diag = fltarr( 2*nsw+1, 2*nsw+1, npks )
 rtx_diag = fltarr( nsw+1, nsw+1, npks )


readu, 1 ;, mxvrx
readu, 1 ;, bsvar
readu, 1, mxdis
readu, 1, anglx
readu, 1, aniso
readu, 1 ;, mnslp
readu, 1, angll
readu, 1, xspk
readu, 1, yspk
readu, 1 ;, mxds0
readu, 1 ;, mxds1
readu, 1 ;, sft0
readu, 1 ;, sft1
readu, 1, hwdth
readu, 1, npeak
readu, 1 ;, mxvry

readu, 1 ;, nvls
readu, 1, pkhts
readu, 1, vldps
readu, 1, rwpks
readu, 1, rwvls

readu, 1 ;, lon0
readu, 1 ;, lon1
readu, 1 ;, lat0
readu, 1 ;, lat1

readu, 1, uqrid
readu, 1 ;, riseq
readu, 1 ;, fallq
readu, 1, clngt

readu, 1 ;, mxds2

;++11/1/21
readu, 1, rdg_profiles
readu, 1, crst_profiles
readu, 1, crst_silhous

;readu, 1 ;, rt_diag
;readu, 1 , rtx_diag

readu, 1, isoht
readu, 1, isowd
readu, 1, isobs
;readu, 1, RefFac

close,1



if keyword_set(stop) then STOP

xlist = { xs:xs, ys:ys, xspk:xspk, yspk:yspk, panel:mypanel, mxdis:mxdis, hwdth:hwdth, clngt:clngt, anglx:anglx, aniso:aniso, uniqid:uqrid, rdg_profiles:rdg_profiles }

return
end
