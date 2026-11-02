function rotby,aa,n,theta

n2=fix(n/2)
THETRAD = -theta*(!PI/180.)
xx=fltarr(n)
yy=fltarr(n)
xp=fltarr(n,n)
yp=fltarr(n,n)


  for i=1,n do begin
     xx(i-1) = -n2+i*1.   ;- 0.5
  endfor
  for i=1,n do begin
     yy(i-1) = -n2+i*1.   ;- 0.5
  endfor

  for j=0,n-1 do begin
  for i=0,n-1 do begin

     xp(i,j)=xx(i)*cos(thetrad) - yy(j)*sin(thetrad)
     yp(i,j)=yy(j)*cos(thetrad) + xx(i)*sin(thetrad)
     
  endfor
  endfor

 
STOP

ar=0.
return,ar
end
