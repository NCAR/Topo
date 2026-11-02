pro delev_sums,cu=cu,pdev10=pdev10,pdev01=pdev01

    s=size(cu.dev)
    nc=s(1)

    x=fltarr(nc-1,nc,6)
    for p=0,5 do begin
        x(*,*,p)=cu.dev(1:nc-1,*,p)-cu.dev(0:nc-2,*,p)
    endfor
    dev10=total( abs(x) , 1 )
    for p=0,5 do begin
        x(*,*,p)=cu.pdev(1:nc-1,*,p)-cu.pdev(0:nc-2,*,p)
    endfor
    pdev10=total( abs(x) , 1 )
    for p=0,5 do begin
        x(*,*,p)=cu.mxdis(1:nc-1,*,p)-cu.mxdis(0:nc-2,*,p)
    endfor
    mxd10=total( abs(x) , 1 )

stop

return
end
