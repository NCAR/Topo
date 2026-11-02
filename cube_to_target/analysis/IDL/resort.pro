pro resort,f=f,mxdis=doox, anglx=axoox, wght=woox, clngt=cloox, hwdth=hwoox $
              ,omxdis=doo, oanglx=axoo, owght=woo 



;rncvar,f=f,get='WGHTS',dat=woo 
rncvar,f=f,get='MXDIS',dat=doo 
rncvar,f=f,get='ANGLX',dat=axoo 
rncvar,f=f,get='CLNGT',dat=cloo 
rncvar,f=f,get='HWDTH',dat=hwoo 

ixoo=fix( 16.*axoo/180. )

s = size( doo )
doox  = doo*0.
axoox = axoo*0.
hwoox = hwoo*0.
cloox = cloo*0.
;woox  = woo*0.

flat = where( doo lt 0.1 )
;ixoo(flat)=0

if s(0) eq 3 then begin
for j=0,s(2)-1 do begin
for i=0,s(1)-1 do begin

  for n=0,15 do begin 
    if ixoo(i,j,n) ge 0 then begin
       doox(i,j,  ixoo(i,j,n)  )  = doo( i,j,n )
       ;woox(i,j,  ixoo(i,j,n)  )  = woo( i,j,n )
       axoox(i,j, ixoo(i,j,n)  )  = axoo( i,j,n )
       cloox(i,j, ixoo(i,j,n)  )  = cloo( i,j,n )
       hwoox(i,j, ixoo(i,j,n)  )  = hwoo( i,j,n )
    endif
  endfor

endfor
endfor
endif

if s(0) eq 2 then begin
for i=0,s(1)-1 do begin

  for n=0,15 do begin 
    if ixoo(i,n) ge 0 then begin
       doox(i,  ixoo(i,n)  )  = doo( i,n )
       ;woox(i,  ixoo(i,n)  )  = woo( i,n )
       axoox(i, ixoo(i,n)  )  = axoo( i,n )
       cloox(i, ixoo(i,n)  )  = cloo( i,n )
       hwoox(i, ixoo(i,n)  )  = hwoo( i,n )
    endif
  endfor
endfor
endif


return
end
