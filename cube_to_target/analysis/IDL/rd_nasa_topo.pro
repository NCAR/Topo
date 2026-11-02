pro rd_nasa_topo,n=n,raw=raw,smooth=smooth,topo=topo

b$='/project/amp/juliob/NCAR_TOPO_GMTED_UFS_SMOOTHING/'

dim$=strtrim( string(n),2)+'x'+strtrim( string(6*n),2)
dir$=b$+'c'+strtrim( string(n),2)

if keyword_set(raw)    then dd='unsmoothed'
if keyword_set(smooth) then dd='smoothed'

file=dir$+'/'+dd+'/'+'gmted_DYN_ave_'+dim$+'.nc4' 

rncvar,f=file,get='z',dat=zz


topo=fltarr(n,n,6)

p=1 & topo(*,*,p-1) = zz(*,  (p-1)*n : p*n-1)
p=2 & topo(*,*,p-1) = zz(*,  (p-1)*n : p*n-1)

p=3 & topo(*,*,p-1) = reverse(transpose( zz(*,(p-1)*n:p*n-1) ),1)
p=4 & topo(*,*,p-1) = reverse(transpose( zz(*,(p-1)*n:p*n-1) ),2)
p=5 & topo(*,*,p-1) = reverse(transpose( zz(*,(p-1)*n:p*n-1) ),2)

p=6 & topo(*,*,p-1) = zz(*,  (p-1)*n : p*n-1)


; Good matches by eye ---
;-----------------------------------------------------------------------
;  rdsmtopo,topo='../output/topo_smooth_nc3000_Co008_Fi001.dat',c=c                    
;  wset,1&contour,c.smooth(*,*,1),/xst,/yst,lev=[1,30,100,300,1000,2000,3000,5000],/fil

;  rd_nasa_topo,n=720,/smo,topo=stopo 
;   wset,2&contour,stopo(*,*,1),/xst,/yst,lev=[1,30,100,300,1000,2000,3000,5000],/fil




;IDL> rdsmtopo,topo='../output/topo_smooth_nc3000_Co016_Fi001.dat',c=c                    
;IDL> rd_nasa_topo,n=360,/smo,topo=stopo                                                  
 
;IDL> wset,2&contour,stopo(*,*,1),/xst,/yst,lev=[1,30,100,300,1000,2000,3000,5000],/fil   
;IDL> wset,1&contour,c.smooth(*,*,1),/xst,/yst,lev=[1,30,100,300,1000,2000,3000,5000],/fil


;-------------------------------------
; visual matches for smooth topo
;
;   c1440 --> Co004
;   c1080 -->  
;   c896  --> 
;   c720  --> Co008
;   c360  --> Co016
;   c180  --> Co032,Co030
;   c90   --> Co046 looks better than Co060
;   c48   -->
;   c24   --> 




return
end
