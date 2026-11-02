pro repaint,cu=cu

s=size( cu.mxdis )
nc=s(1)



repa=cu.mxdis*0.
nuqi=n_elements( cu.coqi )
coq2=cu.coqi*0
print,nuqi

for i=1,nuqi-1 do begin
    noo = cu.coqi( i )
    if noo gt 0 and noo le 7 then begin
       soo = where( cu.uniqi eq i*1.d )
       coq2(i) = n_elements(soo)
       repa(soo)=cu.mxdis(soo)
    endif
    if (i mod 1000) eq 0 then print," at i=",i
endfor

openw,1,/f77,'repa.dat'
writeu,1,repa

cu=create_struct( cu, 'repa' , repa )


return
end
