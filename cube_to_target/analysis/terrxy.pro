pro terrxy,trxy=trxy,xy=xy

close,1
openr,1,trxy,/f77_u

in1=0L & in2=0L
readu,1,in1,in2
aa=fltarr( 2*in2+1 , 2*in2+1 )
rt_diag = fltarr( 2*in2+1 , 2*in2+1 , in1 )
suba_diag = fltarr( 2*in2+1 , 2*in2+1 , in1 )
xs=fltarr( in1+1) 
ys=fltarr( in1+1) 
panel=lonarr( in1+1) 
a1=0.&a2=0.&i3=long(0)

for ipk=0,in1-1 do begin
    readu,1,a1,a2,i3
    xs(ipk)=a1&ys(ipk)=a2&panel(ipk)=i3
    readu,1,aa
    rt_diag(*,*,ipk) = aa
    readu,1,aa
    suba_diag(*,*,ipk) = aa
    if ipk mod 1000 eq 0 then print," Read to ",ipk
endfor

xy={rot:rt_diag,suba:suba_diag,xs:xs,ys:ys,panel:panel}

close,1

return
end
