pro condense,cu=cu,l=l

s=size( cu.mxdis )
nc=s(1)

doo=where( cu.mxdis gt 1. )

duqi = cu.uniqi(doo)

nuqi = n_elements( l.mxdis )

;coqi = fltarr(  long(max(duqi)) + 1 )
coqi = fltarr(  nuqi  )


;for i=0,long(max(duqi)) do begin
for i=1,nuqi-1 do begin
    oo = where( duqi eq i*1.d )
    if max(oo) gt -1 then coqi(i-1) = n_elements( oo )
endfor

print,"Finished gathering unique IDs "

cuqi = fltarr( nc , nc , 6 )

for p=0,6 -1 do begin
for j=0,nc-1 do begin
for i=0,nc-1 do begin

    uqid         = cu.uniqi( i,j,p)
    cuqi(i,j,p)  = coqi( uqid-1 )

endfor
endfor
print,"Panel = ",p
endfor

if tagset(cu,'cuqi') then begin
   cu.cuqi=cuqi
endif else begin
   cu = create_struct( cu, 'cuqi', cuqi )
endelse
if tagset(cu,'coqi') then begin
   cu.coqi=coqi
endif else begin
   cu = create_struct( cu, 'coqi', coqi )
endelse


return
end
