pro cube_topo_eval,cu=cu,err1=err1,nsm=sm, $
    profi=profi_eval,dev=dev_eval,nodes=nodes_eval,wedgo=wedgo_eval,pwedgo=pwedgo_eval

cube_tensors,cu=cu,gg=gg


profi=cu.profi
nodes=cu.nodes
wedgo=cu.wedgo
dev=cu.dev
pdev=cu.pdev

pwedgo=wedgo
poo=where( wedgo lt 0 )
pwedgo(poo)=0.


if keyword_set(sm) then begin
   profi = smoothxyp( profi, sm, sm)
   nodes = smoothxyp( nodes, sm, sm)
   wedgo = smoothxyp( wedgo, sm, sm)
   pwedgo = smoothxyp( pwedgo, sm, sm)
   ;dev   = smoothxyp( dev,   sm, sm)
endif

if keyword_set(dev_eval) then begin
var=fltarr(6)
for p=0,5 do begin

   var(p) = total( (dev(*,*,p))^2 * gg )/total(gg)

endfor
print,cu.rf
print,var,' : ',total(var)
endif

; Nodes
if keyword_set(nodes_eval) then begin
err1=fltarr(6)
for p=0,5 do begin

   err1(p) = total( (nodes(*,*,p) - dev(*,*,p))^2 * gg )/total(gg)

endfor
print," Nodes-Dev "
print,err1,' : ',total(err1)
endif

; Profi
if keyword_set(profi_eval) then begin
err2=fltarr(6)
for p=0,5 do begin

   err2(p) = total( (profi(*,*,p) - dev(*,*,p))^2 * gg )/total(gg)

endfor
print," Profi-Dev "
print,err2,' : ',total(err2)
endif

; Wedgo
if keyword_set(wedgo_eval) then begin
err2=fltarr(6)
for p=0,5 do begin

   err2(p) = total( (wedgo(*,*,p) - dev(*,*,p))^2 * gg )/total(gg)

endfor
print," Wedgo-Dev "

print,err2,' : ',total(err2)
endif
; P-Wedgo
if keyword_set(pwedgo_eval) then begin
err2=fltarr(6)
for p=0,5 do begin

   err2(p) = total( (pwedgo(*,*,p) - pdev(*,*,p))^2 * gg )/total(gg)

endfor
print," PWedgo-PDev "
print,err2,' : ',total(err2)
endif

return
end
