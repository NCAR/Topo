pro cube_topo_eval,cu=cu,err1=err1,nsm=sm

cube_tensors,cu=cu,gg=gg


profi=cu.profi
nodes=cu.nodes
dev=cu.dev

if keyword_set(sm) then begin
   profi = smoothxyp( profi, sm, sm)
   nodes = smoothxyp( nodes, sm, sm)
   dev   = smoothxyp( dev,   sm, sm)
endif


var=fltarr(6)
for p=0,5 do begin

   var(p) = total( (dev(*,*,p))^2 * gg )/total(gg)

endfor
print,cu.rf
print,var,' : ',total(var)

err1=fltarr(6)
for p=0,5 do begin

   err1(p) = total( (nodes(*,*,p) - dev(*,*,p))^2 * gg )/total(gg)

endfor

print," Nodes-Dev "
print,err1,' : ',total(err1)

err2=fltarr(6)
for p=0,5 do begin

   err2(p) = total( (profi(*,*,p) - dev(*,*,p))^2 * gg )/total(gg)

endfor

print," Profi-Dev "
print,err2,' : ',total(err2)


return
end
