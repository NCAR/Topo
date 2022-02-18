pro resort,f=f,mxdis=doox,  anglx=axoox,  wght=woox  $
              ,omxdis=doo,  oanglx=axoo , owght=woo 



rncvar,f=f,get='WGHTS',dat=woo 
rncvar,f=f,get='MXDIS',dat=doo 
rncvar,f=f,get='ANGLX',dat=axoo 
ixoo=fix( 16.*axoo/180. )

s = size( doo )
doox  = doo*0.
axoox = axoo*0.
woox  = woo*0.

flat = where( doo lt 0.1 )
;ixoo(flat)=0

for j=0,s(2)-1 do begin
for i=0,s(1)-1 do begin

  for n=0,15 do begin 
    if ixoo(i,j,n) ge 0 then begin
       doox(i,j,  ixoo(i,j,n)  )  = doo( i,j,n )
       woox(i,j,  ixoo(i,j,n)  )  = woo( i,j,n )
       axoox(i,j, ixoo(i,j,n)  )  = axoo( i,j,n )
    endif
  endfor

endfor
endfor


return
end
