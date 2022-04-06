module ridge_utils

use rotation, only : rotbyx => rotby4
USE reconstruct
use shr_kind_mod, only: r8 => shr_kind_r8

IMPLICIT NONE
private

public ridgeshift
public ridgescales
public rnodes
public rebuild_nodes

    REAL (KIND=dbl_kind), PARAMETER :: pi        = 3.14159265358979323846264338327
    REAL (KIND=dbl_kind), PARAMETER :: earth_radius        = 6371.0

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!====================================
!====================================

   subroutine ridgeshift( nsw, ridge, crest, anglx0 , &
                           xs0 , ys0, xspk0 , yspk0 , &
                           lyshift )

    integer ,          intent(in   )  :: nsw
    real,              intent(in   )  :: ridge(nsw+1), crest(nsw+1)
    real,              intent(in   )  :: xs0,ys0,anglx0
    real,              intent(inout)  :: xspk0,yspk0
    logical, optional, intent(in   )  :: lyshift
    ! local vars
    real    :: xr(nsw+1),xmn 
    real    :: ang00,xshft,yshft,pcrest(nsw+1),pridge(nsw+1),gridge(nsw+1),cran,hran
    real    :: hwd1,hwd2,isox
    real    :: rt( 2*nsw+1, 2*nsw+1),rt2d(nsw+1 , nsw+1),iso2d(nsw+1 , nsw+1)
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
   subroutine ridgescales( nsw, suba, & 
              ridge,    crest,  &
              xnodes,   hnodes, nnode0, &
              xwedge,   hwedge, &
              xspk0,    yspk0,  &
              dcenter0, anglx0, &
              clngt0,   hwdth0, &
              aniso0,   mxdis0,  &
              rnpks0,   pkhts0, & 
              ridge_x )

    integer , intent(in   )  :: nsw
    real,     intent(inout)  :: ridge(nsw+1), crest(nsw+1)
    integer,  intent(  out)  :: xnodes(nsw+1) , xwedge(3) , dcenter0, nnode0
    real,     intent(  out)  :: hnodes(nsw+1) , hwedge(3)
    real,     intent(inout)  :: xspk0, yspk0
    real,     intent(in   )  :: anglx0
    real,     intent(  out)  :: clngt0,hwdth0,aniso0,mxdis0,pkhts0,rnpks0
    real,     intent(in   )  :: suba( 2*nsw+1 , 2*nsw+1 )
    real,     intent(inout)  :: ridge_x( 2*nsw+1 )

    ! local vars
    real    :: xr(nsw+1),xmn, yr(nsw+1),ymn,yshft,ycrst,ycvar
    real    :: ang00,xshft,pcrest(nsw+1),pridge(nsw+1),gridge(nsw+1),cran,hran
    real    :: hwd1,hwd2,isox,mnt,rotvar,var,rotmn,qual_upd,mxdis_upd
    real    :: rt( 2*nsw+1, 2*nsw+1),rt2d(nsw+1 , nsw+1),iso2d(nsw+1 , nsw+1)
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

    do j=ns0,ns1-1
       crest(j-ns0+1) = maxval(  rt( xwedge(1):xwedge(3), j) )
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
    mxdis_upd  = MAXVAL(ridge)-MINVAL(ridge)

    mnt = sum( ridge )/( ns1-ns0 ) 

    var = sum( ( ridge - mnt )**2 )/( ns1-ns0 ) 

    
    rotmn  =  sum( sum( rt(ns0:ns1-1,ns0:ns1-1) , 1 ), 1) /(( ns1-ns0 )*(ns1-ns0))
    rotvar =  sum( sum( (rt(ns0:ns1-1,ns0:ns1-1)-rotmn)**2 , 1 ), 1) /(( ns1-ns0 )*(ns1-ns0))
    qual_upd=0.
    if (rotvar>0.) qual_upd = var/rotvar

    aniso0 = qual_upd
    mxdis0 = mxdis_upd 


    hwdth0 = xwedge(3)-xwedge(1)
    mxdis0 = maxval( hwedge) - minval(hwedge)
    clngt0 = min( 1.*(nsw+1) , aniso0*(nsw+1) )


 end subroutine ridgescales

 subroutine rnodes (nsw , ridge, xnodes, hnodes, dcenter, xwedge,hwedge, npeak, nvail, nnode )
    integer , intent(in   )  :: nsw
    real,     intent(in   )  :: ridge(nsw+1)
    integer,  intent(  out)  :: xnodes(nsw+1) , xwedge(3)
    real,     intent(  out)  :: hnodes(nsw+1) , hwedge(3)
    integer,  intent(  out)  :: dcenter
    integer , intent(  out)  :: npeak,nvail,nnode
    

    ! local
    integer :: picos(nsw+1),vails(nsw+1),kinks(nsw+1),mesas(nsw+1)
    integer :: xpico(nsw+1),xvail(nsw+1),xnode(nsw+1),xkink(nsw+1)
    integer :: ncntrpk,xcntrpk(nsw+1)
    real    :: ldh,rdh,hq1,tol,ahq
    integer :: i,j,k,nkink,ns2

    !real    :: hwedge(3)
    !integer :: xwedge(3)
    integer :: xwdg0,xwdg1,xwdg2,inode_c

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
    do i=1,nnode
       if (xnodes(i) == xcntrpk(1)) inode_c=i
    end do
    if ((inode_c > 1).AND.(inode_c < nnode) ) then
       xwdg1 = xnodes( inode_c )
       do i=inode_c,2,-1
       ahq = sum(  hnodes(1:i-1) ) / ( i -1 )
       !!hq1 = minval( hnodes(1:i-1))
       !!tol = 0.5*(hnodes(inode_c)-hnodes(i) )
       if (hnodes(i) < ahq ) then
       !!if (hnodes(i) < hq1 ) then
       !!if (hnodes(i) < hnodes(i-1) ) then
          xwdg0 = xnodes(i)
          exit
       end if
       end do
       if (xwdg0<0) xwdg0=1
       do i=inode_c,nnode-1
       ahq = sum(  hnodes(i+1:nnode) ) / ( nnode - i  )
       !!hq1 = minval( hnodes(i+1:nnode))
       !!tol = 0.5*(hnodes(i)-hnodes(inode_c) )
       if (hnodes(i) < ahq ) then
       !!if (hnodes(i) < hq1 ) then
       !!if (hnodes(i) < hnodes(i+1) ) then
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

end subroutine rnodes
!==================================================
subroutine rebuild_nodes (nsw , psw, nnodes, xnodes, hnodes, ridge, lextend_profiles )
    integer , intent(in   )  :: nsw,psw,nnodes
    real,     intent(  out)  :: ridge(2*psw+1)
    integer,  intent(in   )  :: xnodes(nnodes) 
    real,     intent(in   )  :: hnodes(nnodes) 
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
          ridge(i) = hnodes(j) + &
                     ( i - xnodes_s(j))* &
                     ( hnodes(j+1) - hnodes(j) ) / &
                     ( xnodes(j+1) - xnodes(j) ) 
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
end module ridge_utils

