function len_in_square,tht
d=0.
if tht le !pi/4. and tht gt -!pi/4. then d=1./ cos(tht) 
if tht gt !pi/4. and tht le !pi*3./4. then d=1./ cos(tht-!pi/2) 
if tht gt !pi*3/4. and tht le !pi*5./4. then d=1./ cos(tht-!pi) 
if tht gt !pi*5/4. and tht le !pi*7./4. then d=-1./ cos(tht-!pi/2) 
if tht gt !pi*7/4. and tht le !pi*9./4. then d=1./ cos(tht) 


return,d
end
