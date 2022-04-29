pro blockcorr,cu=cu,nb=nb,corr=corr,block=block,profi=profi,nodes=nodes,fnodes=fnodes,wedgo=wedgo,fwedge=fwedge

s=size(cu.dev)
nc=s(1)

cube_tensors,cu=cu,gg=gg

corr=cu.profi*0.
sdev=cu.profi*0.-999999.
nvar=cu.profi*0.-999999.

if keyword_set(profi) then begin
   f1=cu.profi
   f2=cu.dev
endif

if keyword_set(block) then begin
   f1=cu.block
   f2=cu.pdev
endif

if keyword_set(nodes) then begin
   f1=cu.nodes
   f2=cu.dev
endif

if keyword_set(wedgo) then begin
   f1=cu.wedgo
   f2=cu.dev
endif

if keyword_set(fnodes) then begin
   f1=cu.nodes
   f2=cu.dev
   oo=where( f2 lt 0. and abs(f1) lt 1. )
   f1(oo)=f2(oo)
endif

for p=0,5 do begin
for j=0,nc/nb-1 do begin
for i=0,nc/nb-1 do begin
    c=correlate( f1 ( i*nb :(i+1)*nb-1, j*nb :(j+1)*nb-1, p ), f2 ( i*nb :(i+1)*nb-1, j*nb :(j+1)*nb-1, p ) )
    corr ( i*nb :(i+1)*nb-1, j*nb :(j+1)*nb-1, p ) =c
endfor
endfor
endfor

for p=0,5 do begin
for j=0,nc/nb-1 do begin
for i=0,nc/nb-1 do begin
    e = total (  gg( i*nb :(i+1)*nb-1, j*nb :(j+1)*nb-1 )  * ( f1 ( i*nb :(i+1)*nb-1, j*nb :(j+1)*nb-1, p ) - f2 ( i*nb :(i+1)*nb-1, j*nb :(j+1)*nb-1, p ) )^2   ) $
       /total (  gg( i*nb :(i+1)*nb-1, j*nb :(j+1)*nb-1 ) ) 
    d = total (  gg( i*nb :(i+1)*nb-1, j*nb :(j+1)*nb-1 )  * f2 ( i*nb :(i+1)*nb-1, j*nb :(j+1)*nb-1, p ) ^2 ) $
       /total (  gg( i*nb :(i+1)*nb-1, j*nb :(j+1)*nb-1 ) )  
    if ( d gt 10. ) then nvar ( i*nb :(i+1)*nb-1, j*nb :(j+1)*nb-1, p ) = e / d
    sdev ( i*nb :(i+1)*nb-1, j*nb :(j+1)*nb-1, p ) = SQRT( e  )
endfor
endfor
endfor

for p=0,5 do begin
for j=0,nc/nb-1 do begin
for i=0,nc/nb-1 do begin
    e = total (  gg( i*nb :(i+1)*nb-1, j*nb :(j+1)*nb-1 )  * ( f1 ( i*nb :(i+1)*nb-1, j*nb :(j+1)*nb-1, p )  )^2   ) $
       /total (  gg( i*nb :(i+1)*nb-1, j*nb :(j+1)*nb-1 ) ) 
    nvar ( i*nb :(i+1)*nb-1, j*nb :(j+1)*nb-1, p ) = SQRT( e  )
endfor
endfor
endfor

if not tagset(cu,'corr') then begin 
   cu = create_struct( cu , 'corr' , corr )
endif else begin
   cu.corr=corr
endelse
if not tagset(cu,'sdev') then begin 
   cu = create_struct( cu , 'sdev' , sdev )
endif else begin
   cu.sdev=sdev
endelse
if not tagset(cu,'nvar') then begin 
   cu = create_struct( cu , 'nvar' , nvar )
endif else begin
   cu.nvar=nvar
endelse

return
end
