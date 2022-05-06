function xdelev_sum,h=h,x=x,y=y

    s=size(h)
    nc=s(1)

    if keyword_set(x) then begin
       d=fltarr(nc-1,nc,6)
       for p=0,5 do begin
           d(*,*,p)=h(1:nc-1,*,p)-h(0:nc-2,*,p)
       endfor
       sdh=total( abs(d) , 1 )
    endif

    if keyword_set(y) then begin
       d=fltarr(nc,nc-1,6)
       for p=0,5 do begin
           d(*,*,p)=h(*,1:nc-1,p)-h(*,0:nc-2,p)
       endfor
       sdh=total( abs(d) , 2 )
    endif



return,sdh
end
