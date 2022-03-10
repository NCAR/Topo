pro fnames,xcase=xcase,repo=repo $ 
          ,co=co,fi=fi,nsw=nsw,ogrid=ogrid,nc=nc $
          ,rema=rema,fcam=fcam,topo=topo,grem=grem,tg=tg $
          ,list=list,trxy=trxy

 ;d='/project/amp/juliob/Topo-generate-devel/Topo/Ridge-Finding.git/output/'

if not keyword_set(repo) then begin
   repo$ = "/project/amp/juliob/Topo-generate-devel/Topo/Topo.git/"
endif else begin
   repo$ = "/project/amp/juliob/Topo-generate-devel/Topo/"+repo
endelse 

d=repo$+'/cases/'+xcase+'/output/'

print," Getting data from "+d

srct='gmted2010_bedmachine'
case nc of 
3000 : srctnc='gmted2010_bedmachine-ncube3000' ;-ncube3000-stitch'
540  : srctnc='gmted2010_bedmachine-ncube0540'
endcase
fn='/project/amp/juliob/Topo-generate-devel/Topo/inputdata/cubed-sphere-topo/'  + srctnc
soo=file_search( fn+'*.nc')
nsoo=n_elements(soo)
tg=soo( nsoo -1 )

Co$  = 'Co'+padstring(Co,/e2)
Fi$  = 'Fi'+padstring(Fi,/e2)
Nsw$ = 'Nsw'+padstring(Nsw,/e2)
nc$  = 'nc'+padstring(nc,/e3)



fn = 'remap_'+nc$  ;+'_' + Nsw$ +'_Nrs000_' + Co$ + '_'+ Fi$ +'_vX_'
soo=file_search( d+fn+'*.dat')
nsoo=n_elements(soo)
rema=soo( nsoo -1 )

fn = 'topo_smooth'  ; + srct +'_'+ nc$ + '_' + Co$  ;+ '_' + Fi$
soo=file_search( d+fn+'*.nc')
nsoo=n_elements(soo)
topo=soo( nsoo-1 )

grem =d+'grid_remap_'+nc$+'_' + ogrid + '.dat'

fn = ogrid + '_'+ srct  ;+'_'+nc$+'_'+Co$+'_'+Fi$+'_'

soo=file_search( d+fn+'*.nc')
nsoo=n_elements(soo)
fcam =soo( nsoo-1 )

fn = 'Ridge_list_'+nc$   ;+'_' + Nsw$ +'_' + Co$ + '_'+ Fi$
soo=file_search( d+fn+'*.dat')
nsoo=n_elements(soo)
list=soo( nsoo -1 )

fn = 'TerrXY_list_'    ;+nc$+'_' + Nsw$ +'_' + Co$ + '_'+ Fi$
soo=file_search( d+fn+'*.dat')
nsoo=n_elements(soo)
trxy=soo( nsoo -1 )




print,'tg =   ',tg
print,'topo = ',topo
print,'rema = ',rema
print,'grem = ',grem
print,'list = ',list
print,'fcam = ',fcam
print,'trxy = ',trxy



return
end
