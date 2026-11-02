module ridge_utils

use rotation, only : rotbyx => rotby4
USE reconstruct
use shr_kind_mod, only: r8  => shr_kind_r8
use shr_kind_mod, only: rpx => shr_kind_r8

IMPLICIT NONE
private

public ridgeshift
public ridgescales
public rnodes
public rnodes_v2
public rebuild_nodes
public length_in_square
public repaint

    REAL(KIND=dbl_kind), PARAMETER :: pi        = 3.14159265358979323846264338327
    REAL(KIND=dbl_kind), PARAMETER :: earth_radius        = 6371.0

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!====================================
!====================================

   subroutine ridgeshift( nsw, ridge, crest, anglx0 , &
                           xs0 , ys0, xspk0 , yspk0 , &
                           lyshift )

    integer ,          intent(in   )  :: nsw
    real(RPX),              intent(in   )  :: ridge(nsw+1), crest(nsw+1)
    real(RPX),              intent(in   )  :: xs0,ys0,anglx0
    real(RPX),              intent(inout)  :: xspk0,yspk0
    logical, optional, intent(in   )  :: lyshift
    ! local vars
    real(RPX)   :: xr(nsw+1),xmn 
    real(RPX)   :: ang00,xshft,yshft,pcrest(nsw+1),pridge(nsw+1),gridge(nsw+1),cran,hran
    real(RPX)   :: hwd1,hwd2,isox
    real(RPX)   :: rt( 2*nsw+1, 2*nsw+1),rt2d(nsw+1 , nsw+1),iso2d(nsw+1 , nsw+1)
    integer :: ipkh(1),ns0,ns1,j,i
    logical :: Lcount(nsw+1) , shift_along_y

    if (present(lyshift)) then
       shift_along_y = lyshift
    else
       shift_along_y = .FALSE.
    end if

    ns0=nsw/2+1
    ns1=ns0+nsw+1
    do i=1,nsw+1
       xr(i)=1.*(i-1) ! i?
    end do
    xmn=0.5*(xr(1)+xr(nsw+1))

    ! XR is just [1,..,nsw+1]. Fine coord for both NORMAL(X) and ALONG(Y) ridge direction

    !gridge  = ridge * exp( -1.*( (xr-xmn)/(nsw/1) )**2 )


    ipkh    = MAXLOC( ridge ) ! index of MAX peak height in rotated topo avg cross-section
    xshft   = XR( ipkh(1) ) - xmn

    ang00   = anglx0 * (PI/180.)

    ! Shift peak/crest location NORMAL to ridge - "X"
    xspk0   = xs0  + xshft * cos( ang00 )
    yspk0   = ys0  - xshft * sin( ang00 )

    cran      = MAXVAL(crest) - MINVAL(crest)
    hran      = MAXVAL(ridge) - MINVAL(ridge)
 
    pcrest(:) = crest(:)  -  MINVAL(crest)
    where (pcrest < 0.1*cran)
       pcrest = 0.
    end where

    if ( shift_along_y ) then
       ! Shut off along crest shifts
       ! Now shift peak/crest location ALONG crest - "Y"
       !  1) Calculate centroid in Y, diff w/ resp center 
       yshft  = sum( xr * pcrest )/( sum( pcrest )+ 0.1 ) - xmn
       !  2) Shift in cubed sphere coords. Two +'s not a sign
       !     error.  Combination of conventions for ridges lead 
       !     to this: 0 <= ang00 <= +180. w/ ang00=0 eq N-S ridge
       xspk0  = xspk0  + yshft *sin( ang00 )
       yspk0  = yspk0  + yshft *cos( ang00 )
    end if
   
 end subroutine ridgeshift

!====================================================================
   subroutine ridgescales( nsw, ipk, suba, & 
              ridge,    crest,  &
              xnodes,   hnodes, nnode0, &
              xwedge,   hwedge, &
              xspk0,    yspk0,  &
              dcenter0, anglx0, &
              clngt0,   hwdth0, &
              aniso0,   mxdis0,  &
              rnpks0,   pkhts0, & 
              riseq0,   fallq0, &
              ridge_x )

    integer , intent(in   )  :: nsw,ipk
    real(RPX),     intent(inout)  :: ridge(nsw+1), crest(nsw+1)
    integer,  intent(  out)  :: xnodes(nsw+1) , xwedge(3) , dcenter0, nnode0
    real(RPX),     intent(  out)  :: hnodes(nsw+1) , hwedge(3)
    real(RPX),     intent(inout)  :: xspk0, yspk0
    real(RPX),     intent(in   )  :: anglx0
    real(RPX),     intent(  out)  :: clngt0,hwdth0,aniso0,mxdis0,pkhts0,rnpks0
    real(RPX),     intent(  out)  :: riseq0,fallq0
    real(RPX),     intent(in   )  :: suba( 2*nsw+1 , 2*nsw+1 )
    real(RPX),     intent(inout)  :: ridge_x( 2*nsw+1 )

    ! local vars
    real(RPX)   :: xr(nsw+1),xmn, yr(nsw+1),ymn,yshft,ycrst,ycvar
    real(RPX)   :: ang00,xshft,pcrest(nsw+1),pridge(nsw+1),gridge(nsw+1),cran,hran
    real(RPX)   :: hwd1,hwd2,isox,mnt,rotvar,var,rotmn,qual_upd,mxdis_upd
    real(RPX)   :: rt( 2*nsw+1, 2*nsw+1),rt2d(nsw+1 , nsw+1),iso2d(nsw+1 , nsw+1)
    integer :: ipkh(1),ns0,ns1,j,i,nvls0,npks0
    logical :: Lcount(nsw+1)

    ns0=nsw/2+1
    ns1=ns0+nsw+1
    do i=1,nsw+1
       xr(i)=1.*(i-1) ! i?
    end do
    xmn=0.5*(xr(1)+xr(nsw+1))
    do i=1,nsw+1
       yr(i)=1.*(i-1) ! i?
    end do
    ymn=0.5*(yr(1)+yr(nsw+1))

    !gridge  = ridge * exp( -1.*( (xr-xmn)/(nsw/1) )**2 )


    ! Notes
    !    ridge here ==> rtx in ANISO_ANA
    !----------------------------------------
    rt  = rotbyx( suba, 2*nsw+1 , anglx0 )
    ridge_x(1:2*nsw+1) = sum( rt( : , ns0:ns1-1) , 2 ) /(ns1-ns0)  
    ridge = sum( rt(ns0:ns1-1,ns0:ns1-1) , 2 ) /( ns1-ns0 ) ! Y-average 
           
    call rnodes (nsw , ridge , & 
         xnodes , &
         hnodes , & 
         dcenter0 , &             
         xwedge , &
         hwedge,  &
         npks0,nvls0,nnode0 )

    ! Note that rt is 2*nsw+1 coords so xwedge has to shifted
    ! by ns0.
    !--------------------------------------------------------
    do j=ns0,ns1-1
       crest(j-ns0+1) = maxval(  rt( (ns0+xwedge(1)):(ns0+xwedge(3)), j) )
    end do                   
 
    ang00   = anglx0 * (PI/180.)
    pcrest = crest - minval( crest )
    ycrst = SUM( pcrest * yr ) / (SUM( pcrest )+0.1)
    yshft = (ycrst - ymn)


    if (abs(yshft) > 1) then
       xspk0  = xspk0  + yshft *sin( ang00 )
       yspk0  = yspk0  + yshft *cos( ang00 )
    end if


    ycvar  = SUM( pcrest * (yr-ycrst)**2 ) / (SUM( pcrest )+0.1)   
    rnpks0 = 1.*npks0
    pkhts0 = maxval( hnodes )


    ! Notes
    !    rtx in ANISO_ANA  <==> ridge here
    !    rt in ANISO_ANA   <==> rt here     
    !----------------------------------------
    mxdis0  = maxval( hwedge) - minval(hwedge)
    riseq0  = hwedge(2)-hwedge(1)
    fallq0  = hwedge(3)-hwedge(2)
    hwdth0  = xwedge(3)-xwedge(1)

    mnt = sum( ridge )/( ns1-ns0 ) 
    var = sum( ( ridge - mnt )**2 )/( ns1-ns0 )    
    rotmn  =  sum( sum( rt(ns0:ns1-1,ns0:ns1-1) , 1 ), 1) /(( ns1-ns0 )*(ns1-ns0))
    rotvar =  sum( sum( (rt(ns0:ns1-1,ns0:ns1-1)-rotmn)**2 , 1 ), 1) /(( ns1-ns0 )*(ns1-ns0))
    aniso0 =  0.
    if (rotvar>0.) aniso0 = var/rotvar
    clngt0  = min( 1.*(nsw+1) , aniso0*(nsw+1) )


 end subroutine ridgescales

 subroutine rnodes (nsw , ridge, xnodes, hnodes, dcenter, xwedge,hwedge, npeak, nvail, nnode )
    integer , intent(in   )  :: nsw
    real(RPX),     intent(in   )  :: ridge(nsw+1)
    integer,  intent(  out)  :: xnodes(nsw+1) , xwedge(3)
    real(RPX),     intent(  out)  :: hnodes(nsw+1) , hwedge(3)
    integer,  intent(  out)  :: dcenter
    integer , intent(  out)  :: npeak,nvail,nnode
    

    ! local
    integer   :: picos(nsw+1),vails(nsw+1),kinks(nsw+1),mesas(nsw+1)
    integer   :: xpico(nsw+1),xvail(nsw+1),xnode(nsw+1),xkink(nsw+1)
    integer   :: ncntrpk,xcntrpk(nsw+1),xlim(2)
    real(RPX) :: ldh,rdh,hq1,tol,ahq,ihq,dhn,rms
    integer   :: i,j,k,nkink,ns2
    logical   :: lok,lnew_wedge_calc

    !real(RPX)   :: hwedge(3)
    !integer :: xwedge(3)
    integer   :: xwdg0,xwdg1,xwdg2,inode_c

    lnew_wedge_calc = .true.
    ns2=nsw/2

    hnodes(:)=0.
    xnodes(:)=-1

    picos(:)=0
    vails(:)=0
    kinks(:)=0
    mesas(:)=0

    xpico(:)=-1
    xvail(:)=-1
    xkink(:)=-1
    xnode(:)=-1
    xcntrpk(:)=-1

    do i=2,nsw

       ldh = ridge(i)   - ridge(i-1)
       rdh = ridge(i+1) - ridge(i)

       if ( (ldh > 0.) .and. (rdh < 0) )  picos(i)=1
       if ( (ldh < 0)  .and. (rdh > 0) )  vails(i)=1
       if ( (abs(ldh) > 0) .and. (rdh == 0) ) mesas(i)=1
       if ( (abs(rdh) > 0) .and. (ldh == 0) ) mesas(i)=1
    end do

    if ( ridge(1) < ridge(2) ) vails(1)=1
    if ( ridge(nsw+1) < ridge(nsw) ) vails(nsw+1)=1

    npeak=0
    nvail=0
    nkink=0
    nnode=0
    ncntrpk=0
    inode_c=0

    kinks = picos  + vails + mesas

    ! Always put left and right edges on list of kinks
    kinks(1)     = 1
    kinks(nsw+1) = 1

    do i=1,nsw+1
       if (picos(i) == 1) then 
           npeak=npeak+1
           xpico(npeak)=i
       end if
       if (vails(i) == 1) then 
           nvail=nvail+1
           xvail(nvail)=i
       end if
       if (kinks(i) == 1) then 
           nkink=nkink+1
           xkink(nkink)=i
       end if
    end do

    nnode  = nkink
    xnodes = xkink
    do i=1,nnode
       hnodes(i) = ridge( xnodes(i) )
    end do
   
    if ( npeak == 0 ) then 
       dcenter=-999
    else 
       dcenter=minval( abs( xpico(1:npeak)-ns2 ) )
    end if

    ! Find highest peak closest to NS2
    !---------------------------------
    if ( npeak > 1 ) then
       do i=1,npeak
          if (abs( xpico(i)-ns2) == dcenter ) then
             ncntrpk          = ncntrpk+1
             xcntrpk(ncntrpk) = xpico(i)
          end if
       end do
    else
       ncntrpk          = 1
       xcntrpk(ncntrpk) = xpico(1)
    end if

    if (ncntrpk == 2) then
       if ( ridge(xcntrpk(2)) > ridge(xcntrpk(1)) ) xcntrpk(1)=xcntrpk(2) 
    end if

    ! xcntrpk(1) is the location of highest peak closest to NS2 the nominal
    ! "center" of the ridge finding domain
    !-----------------------------------------
 
    ! Now work outwards from xcntrpk(1) to create a 3-point "wedge" of 
    ! topography. Use xnodes and hnodes.
    !------------
    xwdg0=-999
    xwdg1=-999
    xwdg2=-999
    tol  = 0.25*(maxval(hnodes)-minval(hnodes))
    do i=1,nnode
       if (xnodes(i) == xcntrpk(1)) inode_c=i
    end do
    if (lnew_wedge_calc) then
       call rnodes_v2( nsw, ridge, xwedge, hwedge, xlim, rms, lok )
    else
       if ((inode_c > 1).AND.(inode_c < nnode) ) then
          xwdg1 = xnodes( inode_c )
          do i=inode_c,2,-1
             ahq = sum(  hnodes(1:i-1) ) / ( i -1 )
             dhn = hnodes(i)-hnodes(i-1)
             !if (hnodes(i) < ahq ) then
             if ((hnodes(i) < ahq ).and.( dhn < tol ))  then
                xwdg0 = xnodes(i)
                exit
             end if
          end do
          if (xwdg0<0) xwdg0=1

          do i=inode_c,nnode-1
             ahq = sum(  hnodes(i+1:nnode) ) / ( nnode - i  )
             dhn = hnodes(i)-hnodes(i+1)
             !if (hnodes(i) < ahq ) then
             if ((hnodes(i) < ahq ).and.( dhn < tol ))  then
                xwdg2 = xnodes(i)
                exit
             end if
          end do
          if (xwdg2<0) xwdg2=nsw+1

          xwedge = (/ xwdg0,         xwdg1,         xwdg2        /)
          hwedge = (/ ridge(xwdg0),  ridge(xwdg1),  ridge(xwdg2) /)

       else
          xwedge = 0
          hwedge = 0. 
       end if
    end if
       
end subroutine rnodes
!==================================================
!==================================================
subroutine rnodes_v2( nsw, ridge, xwedge, hwedge, xlim, rms, lok )
!--------------------------------------------------------------------
! Version-2 wedge: best-fit 5-point wedge on the ridge profile itself,
! returned as the 3-point wedge [x_0, x_c, x_1] (+ limits [x_L, x_R]).
!
! Algorithm (J. Bacmeister, Nov 2026; Python reference rnodes_v2.py
! with xnodes = 1..nsw+1, hnodes = ridge, bound_at_higher = .TRUE.):
!
!  1) x_c = local maximum of ridge closest to the window centre ns2=nsw/2
!     (strict: higher than both neighbours; tie on distance -> higher).
!  2) Active window: from x_c step outward on each side while the point
!     is lower than h_c and not the edge.  The first point NOT lower than
!     h_c, or the edge, is the limit x_L (x_R).
!  3) For every x_0 in (x_L,x_c) and x_1 in (x_c,x_R) build the polyline
!     [x_L,h_L]-[x_0,h_0]-[x_c,h_c]-[x_1,h_1]-[x_R,h_R], evaluate it at
!     every integer x in [x_L,x_R], and take the pair with minimum rms
!     against ridge.  A side with no interior point uses x_0=x_L (x_1=x_R).
!
! Positions are 1-based indices into ridge(1:nsw+1), like rnodes.
! lok = .FALSE. (and everything 0, rms=-1) when there is no interior peak;
! callers may then keep the existing (0,0,0) / -444 handling.
!--------------------------------------------------------------------
    integer,   intent(in )  :: nsw
    real(RPX), intent(in )  :: ridge(nsw+1)
    integer,   intent(out)  :: xwedge(3), xlim(2)
    real(RPX), intent(out)  :: hwedge(3), rms
    logical,   intent(out)  :: lok

    integer   :: n, ns2, i, j, ic, iL, iR, i0, i1, dmin, d
    integer   :: xp(5)
    real(RPX) :: hc, hp(5), fit, err, best
    logical   :: first

    n   = nsw + 1
    ns2 = nsw / 2

    xwedge = 0 ; hwedge = 0._RPX ; xlim = 0 ; rms = -1._RPX ; lok = .FALSE.

    !---- 1) central peak -----------------------------------------------
    ic   = 0
    dmin = huge(1)
    do i = 2, n-1
       if ( ridge(i) > ridge(i-1) .and. ridge(i) > ridge(i+1) ) then
          d = abs( i - ns2 )
          if ( d < dmin ) then
             dmin = d ; ic = i
          else if ( d == dmin .and. ridge(i) > ridge(ic) ) then
             ic = i
          end if
       end if
    end do
    if ( ic == 0 ) return
    hc = ridge(ic)

    !---- 2) active window ----------------------------------------------
    i = ic
    do
       if ( i == 1 ) then
          iL = 1 ; exit
       end if
       j = i - 1
       if ( ridge(j) < hc ) then
          i = j ; cycle
       end if
       iL = j ; exit                ! first point not lower than h_c
    end do

    i = ic
    do
       if ( i == n ) then
          iR = n ; exit
       end if
       j = i + 1
       if ( ridge(j) < hc ) then
          i = j ; cycle
       end if
       iR = j ; exit
    end do

    !---- 3) best 5-point wedge -----------------------------------------
    best  = huge(1._RPX)
    first = .TRUE.
    do i0 = merge( iL, iL+1, iL+1 > ic-1 ), merge( iL, ic-1, iL+1 > ic-1 )
       do i1 = merge( iR, ic+1, ic+1 > iR-1 ), merge( iR, iR-1, ic+1 > iR-1 )
          xp = (/ iL, i0, ic, i1, iR /)
          hp = (/ ridge(iL), ridge(i0), ridge(ic), ridge(i1), ridge(iR) /)
          err = 0._RPX
          do i = iL, iR
             fit = polyline5( xp, hp, i )
             err = err + ( fit - ridge(i) )**2
          end do
          err = sqrt( err / real( iR-iL+1, RPX ) )
          if ( first .or. err < best ) then
             best = err ; first = .FALSE.
             xwedge = (/ i0, ic, i1 /)
             hwedge = (/ ridge(i0), ridge(ic), ridge(i1) /)
          end if
       end do
    end do

    xlim = (/ iL, iR /)
    rms  = best
    lok  = .TRUE.

contains

    pure function polyline5( xp, hp, x ) result( y )
    ! piecewise-linear interpolation of the 5-point polyline at integer x.
    ! Duplicate abscissae (x_0 = x_L or x_1 = x_R) carry equal heights, so
    ! they are simply skipped.
       integer,   intent(in) :: xp(5), x
       real(RPX), intent(in) :: hp(5)
       real(RPX)             :: y
       integer :: k
       y = hp(1)
       do k = 1, 4
          if ( xp(k+1) == xp(k) ) cycle
          if ( x >= xp(k) .and. x <= xp(k+1) ) then
             y = hp(k) + ( hp(k+1) - hp(k) ) * real( x - xp(k), RPX ) / real( xp(k+1) - xp(k), RPX )
             return
          end if
       end do
       if ( x >= xp(5) ) y = hp(5)
    end function polyline5

end subroutine rnodes_v2
!==================================================
subroutine rebuild_nodes (nsw , psw, nnodes, xnodes, hnodes, ridge, lextend_profiles )
    integer ,  intent(in   )  :: nsw,psw,nnodes
    real(RPX), intent(  out)  :: ridge(2*psw+1)
    integer,   intent(in   )  :: xnodes(nnodes) 
    real(RPX), intent(in   )  :: hnodes(nnodes) 
!--------- optional arguments
    logical, optional, intent(in   )  :: lextend_profiles

!--------- local variables
    integer :: xridge( 2*psw + 1),xnodes_s(nnodes)
    integer :: i,j,i0,ishft
    logical :: do_extend_profiles

    !---------------------------------------
    ! Default is to extend rebuilt profiles
    ! to range x := [1,2*psw+1].
    ! Set false in call to limit rebuild to
    ! range of xnodes input.
    !---------------------------------------- 
    if ( PRESENT(lextend_profiles)) then
       do_extend_profiles = lextend_profiles
    else
       do_extend_profiles = .TRUE.
    end if

    i0    = psw - (nsw+1)/2
    ishft = i0 !- 1
    xnodes_s = xnodes + ishft

    do i=1,2*psw+1
       xridge(i) = i
       ridge(i)  = 0.
    end do

    do j=1,nnodes-1
       do i=xnodes_s(j),xnodes_s(j+1)
          if (ABS(xnodes(j+1) - xnodes(j))>1E-12) then
             ridge(i) = hnodes(j) + &
                  ( i - xnodes_s(j))* &
                  ( hnodes(j+1) - hnodes(j) ) / &
                  ( xnodes(j+1) - xnodes(j) )
          else
             ridge(i)=0.
          end if
       end do
    end do


    if (do_extend_profiles) then
    j=0
       do i=1,xnodes_s(j+1)
          ridge(i) = 0. + &
                     ( i - 1 )* &
                     ( hnodes(j+1) - 0. ) / &
                     ( xnodes_s(j+1) - 1 ) 
       end do
    j=nnodes
       do i=xnodes_s(j),2*psw+1
          ridge(i) = hnodes(j) + &
                     ( i - xnodes_s(j))* &
                     ( 0. - hnodes(j) ) / &
                     ( 2*psw + 1 - xnodes_s(j) ) 
       end do
    end if

end subroutine rebuild_nodes
!==================================================
function length_in_square( anglx ) result(d)
real(r8),intent(in) :: anglx 
real(r8) :: d
real(r8) :: angrad

    angrad = anglx*PI/180._r8

    if ( ( angrad > -pi/4.  ) .and. ( angrad <= pi/4.    ) ) d = 1./ cos(angrad) 
    if ( ( angrad > pi/4.   ) .and. ( angrad <= pi*3./4. ) ) d = 1./ cos(angrad-pi/2) 
    if ( ( angrad > pi*3./4.) .and. ( angrad <= pi*5./4. ) ) d = 1./ cos(angrad-pi)  
    if ( ( angrad > pi*5./4.) .and. ( angrad <= pi*7./4. ) ) d =-1./ cos(angrad-pi/2) 
    if ( ( angrad > pi*7./4.) .and. ( angrad <= pi*9./4. ) ) d = 1./ cos(angrad) 

end function length_in_square
!==================================================

subroutine repaint( ncube, mxdis, uniqi, repnt )
integer,  intent(in)   :: ncube
real(r8), intent(in)   :: mxdis(ncube*ncube*6),uniqi(ncube*ncube*6)
real(r8),  intent(out) :: repnt(ncube*ncube*6)
!---- local-----
integer,allocatable :: duqi(:),coqi(:),duqc(:)
integer :: npack,maxid,i
integer :: irepnt(ncube*ncube*6)


duqi  = INT( pack( uniqi, (mxdis>1._r8) ) )
npack = size( duqi )
maxid = maxval( duqi )
allocate( coqi(maxid) )

write(*,*) " repaint - 1 "

do i = 1,maxid
   coqi(i)=count( duqi==i ) 
end do

write(*,*) " repaint - 2 "

allocate( duqc(npack) )
do i = 1,maxid
   where( duqi==i )
      duqc = coqi(i)
   end where
end do

write(*,*) " repaint - 3 "

irepnt(:)=0

irepnt = unpack( duqc , (mxdis>1._r8) , irepnt )

repnt  = DBLE( irepnt )

write(*,*) " repaint - 4 "

end subroutine repaint

end module ridge_utils

