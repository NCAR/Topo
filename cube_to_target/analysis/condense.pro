pro condense,cu=cu

s=size( cu.mxdis )
nc=s(1)

doo=where( cu.mxdis gt 1. )

duqi = cu.uniqi(doo)
coqi = fltarr(  long(max(duqi)) + 1 )

for i=0,long(max(duqi)) do begin
    oo = where( duqi eq i*1.d )
    if max(oo) gt -1 then coqi(i) = n_elements( oo )
endfor

print,"Finished gathering unique IDs "

cuqi = fltarr( nc , nc , 6 )

for p=0,6 -1 do begin
for j=0,nc-1 do begin
for i=0,nc-1 do begin

    uqid         = cu.uniqi( i,j,p)
    cuqi(i,j,p)  = coqi( uqid )

endfor
endfor
print,"Panel = ",p
endfor

cu = create_struct( cu, 'cuqi', cuqi )
cu = create_struct( cu, 'coqi', coqi )




return
end
