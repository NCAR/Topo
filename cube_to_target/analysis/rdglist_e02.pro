pro rdglist_e02,list=fn0,terr=terr,basedir=basedir$ , $
                xlist=xlist,xterr=xterr


if not keyword_set(basedir$) then basedir$ = '' ;'/project/amp/juliob/Topo-generate-devel/Topo/cube_to_target_devel/output/'

fn=basedir$ + fn0

close,1
 openr,1,/f77_u,fn


nc=0L & nch=0L & grid_length_scale=0.D
readu,1,nch , grid_length_scale


t1=fltarr(nch,nch,6)
t1d=fltarr(nch,nch,6)
dtg=dblarr(nch,nch)
xv=fltarr(nch)&yv=fltarr(nch)

 readu,1,xv,yv
 readu,1,t1
 readu,1,t1d ;pr,dtg

 npks=0L & m=0L & NSW=0L
 readu,1,npks  , NSW

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

 rdg_profiles = fltarr( nsw+1,npks )
 crst_profiles = fltarr( nsw+1,npks )
 crst_silhous = fltarr( nsw+1,npks )
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




ipo1=where( mypanel eq 1 )
ipo2=where( mypanel eq 2 )
ipo3=where( mypanel eq 3 )
ipo4=where( mypanel eq 4 )
ipo5=where( mypanel eq 5 )
ipo6=where( mypanel eq 6 )

mxdis0=700.
;Locations in Colorado Rockies
 rock=where( ys gt 2700. and xs gt 900 and xs lt 1050 and mxdis gt mxdis0 and mypanel eq 4 )                    
 sang=where( ys gt 2780. and ys lt 2810 and xs gt 950 and xs lt 1020 and mxdis gt mxdis0  and mypanel eq 4 )  
 loco=where( ys gt 2850. and ys lt 2890 and xs gt 940 and xs lt 1000 and mxdis gt mxdis0  and mypanel eq 4 )  

; This is Longs peak/Indian Peaks
;IDL> print,loco(12:13)
;      188921      189103

;Locations in southern Andes
pata=where( ys gt 1850 and ys lt 2200 and xs gt 0 and xs lt 450 and mxdis gt mxdis0  and mypanel eq 5 )  
paine=where( ys gt 1950 and ys lt 1970 and xs gt 150 and xs lt 200 and mxdis gt mxdis0  and mypanel eq 5 )  

cantp = where( ys gt 1810 and ys lt 1860 and xs gt 750 and xs lt 800 and mxdis gt mxdis0  and mypanel eq 5 )  

sgeorgia =  where( ys gt 2400 and ys lt 2600 and xs gt 600 and xs lt 900 and mxdis gt 10.  and mypanel eq 5 )  

peru=where( xs gt 2000 and xs lt 2200 and ys gt 1100 and ys lt 1300 and mxdis gt mxdis0 and  mypanel eq 4)

; locations in Europe ..

norw=where( xs gt 1550 and xs lt 1650 and ys gt 450 and ys lt 550 and mxdis gt 500. and  mypanel eq 6)


if keyword_set(stop) then STOP

xlist = { xs:xs, ys:ys, xspk:xspk, yspk:yspk, panel:mypanel, mxdis:mxdis, hwdth:hwdth, clngt:clngt, anglx:anglx, aniso:aniso, uniqid:uqrid, rdg_profiles:rdg_profiles }

return
end
