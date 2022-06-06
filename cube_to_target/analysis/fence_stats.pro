pro fence_stats,coqi=coqi,mxdis=mxdis,fences=fences



maxl = max(coqi)

fences=fltarr(maxl+1)


for i=1,maxl-1 do begin
    oo=where( coqi eq i )
    if min(oo) ne -1 then  fences(i) = total( mxdis(oo)*coqi(oo) )
endfor 

return
end
