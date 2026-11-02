pro ridge_prominence,h=h,nsw=nsw,dcenter=dcenter,stop=stop

ns2=nsw/2
hmin=min( h )
hmax=max( h )
n=n_elements( h )
nh=100
dh=(hmax-hmin)/nh
hvec = hmin + findgen( nh )*dh

terr=intarr( n , nh )

xv=findgen(nsw+1)



kinks=intarr(n)
peaks=intarr(n)
vails=intarr(n)

for i=1,nsw-2 do begin

    ldh = h(i)   - h(i-1)
    rdh = h(i+1) - h(i)

    if ldh gt 0 and rdh lt 0 then peaks(i)=1
    if ldh lt 0 and rdh gt 0 then vails(i)=1
    if abs(ldh) gt 0 and rdh eq 0 then kinks(i)=1
    if abs(rdh) gt 0 and ldh eq 0 then kinks(i)=1

 endfor

if h(0) lt h(1) then vails(0)=1
if h(nsw-1) lt h(nsw-2) then vails(nsw-1)=1

nvail=0L
npeak=0L

oo=where(vails eq 1)
if min(oo) gt -1 then xvail= oo
if min(oo) gt -1 then nvail=n_elements( xvail )

oo=where(peaks eq 1)
if min(oo) gt -1 then xpeak= oo
if min(oo) gt -1 then npeak=n_elements( xpeak )


if npeak gt 0 then begin 
   dcenter =  min(abs(xpeak-ns2))
   ii=where( abs(xpeak-ns2) eq dcenter)
   if n_elements(ii) eq 2 then begin
      if h(ii(0)) ge h(ii(1)) then ii=ii(0)
      if h(ii(0)) lt h(ii(1)) then ii=ii(1)
   endif else begin
      ii=ii(0)
   endelse
endif

ipeak = xpeak(ii)
nodes=[xvail,xpeak]
nodes=nodes(sort(nodes))

if npeak eq 0 then dcenter =  -999

nnodes=n_elements(nodes)
iprom=intarr(nnodes)+1
for i=0,nnodes-1 do begin
    if h(nodes(i)) gt h(ipeak) then iprom(i)=0 
endfor


if keyword_set(stop) then STOP
return
end
