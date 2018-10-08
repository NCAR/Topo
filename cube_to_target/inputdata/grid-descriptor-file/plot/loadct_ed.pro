pro shape,array,type,xoffset,xlen,ynorm

shape = findgen(xlen)/(xlen-1)
case abs(type) of
0: shape = 1.                   ; --- const
1:                                 ; --- linear
2: shape = sin(!PI*shape)          ; --- hump
3: shape = sqrt(shape)
4: shape = shape^2
5: shape = sin(!PI/2.*shape)^2     ; --- smooth transitions
6: shape = sqrt(sin(!PI/2.*shape)) ; --- rapid rise and level off

endcase

if (type lt 0) then shape = reverse(shape)
array[xoffset:xoffset+xlen-1] += fix(ynorm*shape)

return
end

; ------------------------------------------
pro loadct_ed,r,g,b,scheme=scheme
; by Edwin Sirko
r = bytarr(256)
g = bytarr(256)
b = bytarr(256)

if not keyword_set(scheme) then begin
; --- 2003-12-25: a new good colormap.
shape,r,5,75,90,255
shape,r,0,165,91,255

shape,g,5,20,70,255
shape,g,0,90,60,255
shape,g,-3,150,60,255
shape,g,4,210,46,255

shape,b,3,0,50,255
shape,b,0,50,15,255
shape,b,-5,65,85,255
shape,b,3,210,46,255

endif else begin
;  --- 2005-2-22: scheme 1: want black in the middle
shape,b,-1,0,128,255
shape,g,1,128,40,255
shape,g,-5,168,50,255
shape,g,5,218,38,255
shape,b,5,192,64,255
shape,r,5,140,60,255
shape,r,0,200,56,255

endelse

tvlct,r,g,b


return
end

