pro rdgrid,grem=fn,itrgt=itrgt


;write(911) ncube,npeaks
;write(911) itrgtC
;write(911) itrgxC
;write(911) xs,ys,xspk,yspk,peaks%i,peaks%j


nc=0L & npks=0L
close,1
openr,1,/f77_u,fn

readu,1,nc,npks

itrgt=dblarr(nc,nc,6)
itrgx=dblarr(nc,nc,6)

readu,1,itrgt
readu,1,itrgx
close,1

return
end
