pro cube_tensors,cu=cu,nc=nn, $
    gg=gg,g_aa=g_aa,g_bb=g_bb,g_ab=g_ab

common misc_parms, alhl,alhs,cp,rgas,airmw,h2omw,grav,rkap,EARTH_RADIUs,tice
misc_parms

if keyword_set(cu) then begin
   mxdis = cu.mxdis
   s=size( mxdis )
   nn=s(1)
endif

rnn=double(nn)
nn2=nn/2


lambdx=dblarr( nn , nn , 6 )
theta =dblarr( nn , nn , 6 )
bet=dblarr( nn , nn)
alp=dblarr( nn, nn )

ca = ((dindgen( nn )- (nn2-.5) ) / (0.5*rnn))  *!pi/4.  
for i=0,nn-1 do bet(*,i)=ca(i)
for i=0,nn-1 do alp(*,i)=ca(*)


rr = 1./sqrt(3.d)

xc = rr * tan(alp)
yc = rr * tan(bet)

RR = 1.d  ;earth_radius
aa = RR/sqrt(3.d)

x  = aa * tan( alp )
y  = aa * tan( bet )

rho = SQRT ( 1.0 + tan(alp)^2 + tan(bet)^2 )

; sqrt( det(g_ij) )
gg   = RR^2 / ( rho^3 * cos(alp)^2 * cos(bet)^2 ) 

; 4 components of metric tensor
c0   =  RR^2 / ( rho^4 * cos(alp)^2 * cos(bet)^2 ) 
g_aa =  c0 * (1+tan(alp)^2)
g_ab = -c0 * tan(alp)*tan(bet)
g_ba =  g_ab 
g_bb =  c0 * (1+tan(bet)^2)


return
end
