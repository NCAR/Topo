pro towns,southeast=southeast,colorado=colorado


if keyword_set(southeast) then begin
   circlesym
   oplot,[-95.68+360.],[39.05],ps=8,syms=2 ; Topeka
   oplot,[-97.52+360.],[35.47],ps=8,syms=2 ; Oklahoma City

   circlesym,nv=3
   oplot,[-97.49+360.],[36.61],ps=8,syms=2 ; Lamont, Oklahoma ARM-SGP


   circlesym,nv=4
   ;oplot,[-108.12+360.],[39.44],ps=8,syms=2 ; Mt. Callahan
endif

if keyword_set(colorado) then begin

   circlesym

   oplot,[-105.1+360.],[40.16],ps=8,syms=2 ; Longmont
   oplot,[-105.52+360.],[40.38],ps=8,syms=2 ; Estes Park
   oplot,[-104.91+360.],[38.86],ps=8,syms=2 ; Manitou Springs
   oplot,[-106.00+360.],[39.22],ps=8,syms=2 ; Fairplay
   oplot,[-107.88+360.],[37.28],ps=8,syms=2 ; Durango
   oplot,[-105.42+360.],[37.20],ps=8,syms=2 ; San Luis

   circlesym,nv=4
   oplot,[-108.12+360.],[39.44],ps=8,syms=2 ; Mt. Callahan
endif

return
end
