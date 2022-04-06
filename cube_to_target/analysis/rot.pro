function rot,aa,n,theta

  THETRAD = -theta*(!PI/180.)

  x=fltarr(n)
  y=fltarr(n)
  xp=fltarr(n,n)
  yp=fltarr(n,n)

  n2=fix(n/2)
  for i=0,n-1 do begin
     x(i)=-n2+i*1.
  endfor
  for i=0,n-1  do begin
     y(i)=-n2+i*1.
  endfor

  for j=0,n-1  do begin
  for i=0,n-1  do begin
     xp(i,j)=x(i)*cos(thetrad) - y(j)*sin(thetrad)
     yp(i,j)=y(j)*cos(thetrad) + x(i)*sin(thetrad)
  endfor
  endfor

  for j=0,n-1 do begin
  for i=0,n-1 do begin
     if ( (xp(i,j)<x(0)).or.(xp(i,j)>x(n-1)).or.(yp(i,j)<y(0)).or.(yp(i,j)>y(n-1)) ) then begin
        ar(i,j)=-9999999.9
     endif else begin
     ir = FIX( xp(i,j) + n2+0.5 )
     jr = FIX( yp(i,j) + n2+0.5 )
          ar(i,j) = aa(ir,jr)
     endelse
  endfor
  endfor






return,ar
end




