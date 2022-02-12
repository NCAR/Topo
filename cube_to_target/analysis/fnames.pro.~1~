pro fnames,xcase=xcase,co=co,fi=fi,nsw=nsw,ogrid=ogrid,nc=nc $
          ,rema=rema,fcam=fcam,topo=topo,grem=grem,tg=tg $
          ,list=list

 ;d='/project/amp/juliob/Topo-generate-devel/Topo/Ridge-Finding.git/output/'

if not keyword_set(xcase) then begin
   d='../output/'
endif else begin
   d='../../'+xcase+'/output/'
endelse 

case nc of 
3000 : tg='/project/amp/juliob/Topo-generate-devel/Topo/inputdata/cubed-sphere-topo/gmted2010_modis-ncube3000-stitch.nc'
540  : tg='/project/amp/juliob/Topo-generate-devel/Topo/inputdata/cubed-sphere-topo/gmted2010_bedmachine-ncube0540.nc'
endcase

Co$  = 'Co'+padstring(Co,/e2)
Fi$  = 'Fi'+padstring(Fi,/e2)
Nsw$ = 'Nsw'+padstring(Nsw,/e2)
nc$  = 'nc'+padstring(nc,/e3)

;fn = 'remap_nc3000_Nsw042_Nrs008_Co060_Fi008_vX_'
fn = 'remap_'+nc$+'_' + Nsw$ +'_Nrs000_' + Co$ + '_'+ Fi$ +'_vX_'
soo=file_search( d+fn+'*.dat')
nsoo=n_elements(soo)
rema=soo( nsoo -1 )

;fn = 'topo_smooth_nc3000_Co060_Fi008'
fn = 'topo_smooth_'+nc$+'_' + Co$ + '_' + Fi$
soo=file_search( d+fn+'*.dat')
nsoo=n_elements(soo)
topo=soo( nsoo-1 )

grem =d+'grid_remap_'+nc$+'_' + ogrid + '.dat'

; fv_0.9x1.25_nc3000_Nsw042_Nrs000_Co060_Fi008_20211222.nc
fn = ogrid + '_'+nc$+'_'+Nsw$+'_Nrs000_'+Co$+'_'+Fi$+'_'

soo=file_search( d+fn+'*.nc')
nsoo=n_elements(soo)
fcam =soo( nsoo-1 )

fn = 'Ridge_list_'+nc$+'_' + Nsw$ +'_' + Co$ + '_'+ Fi$
soo=file_search( d+fn+'*.dat')
nsoo=n_elements(soo)
list=soo( nsoo -1 )




print,tg
print,topo
print,rema
print,grem
print,list
print,fcam


return
end
