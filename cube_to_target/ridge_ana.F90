module ridge_ana

use rotation 
USE reconstruct

IMPLICIT NONE
private


public find_ridges




  REAL, allocatable  ::  MXVRX(:,:),MXDIS(:,:),MNSLP(:,:),ANGLX(:,:),ANISO(:,:),XS(:),YS(:), &
  XSPK(:,:),YSPK(:,:),MXDS0(:,:),MXDS1(:,:),SFT0(:,:),SFT1(:,:), BSVAR(:,:),HWDTH(:,:),NPKS(:,:),NVLS(:,:),MXVRY(:,:), &
  PKHTS(:,:),VLDPS(:,:),RWPKS(:,:),RWVLS(:,:)


    REAL(KIND=dbl_kind), allocatable ::  ALP0(:,:),BET0(:,:),LAT0(:,:,:),LON0(:,:,:)
    REAL(KIND=dbl_kind), allocatable ::  ALP1(:,:),BET1(:,:),LAT1(:,:,:),LON1(:,:,:)

    REAL, allocatable ::  BSVAR6(:,:,:),MXVRX6(:,:,:),MXDIS6(:,:,:),ANGLX6(:,:,:),ANISO6(:,:,:),MNSLP6(:,:,:),&
         ANGLL6(:,:,:),XSPK6(:,:,:),YSPK6(:,:,:),MXVRY6(:,:,:)
    REAL, allocatable ::  MXDS06(:,:,:),MXDS16(:,:,:),SFT06(:,:,:),SFT16(:,:,:),HWDTH6(:,:,:),NPKS6(:,:,:)
    REAL, allocatable ::  NVLS6(:,:,:),PKHTS6(:,:,:),VLDPS6(:,:,:),RWPKS6(:,:,:),RWVLS6(:,:,:)

    INTEGER (KIND=int_kind),allocatable :: UQRID6(:,:,:) 



    REAL, allocatable ::  TMPx6(:,:,:),wt1p(:,:)



contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine find_ridges ( terr_dev, terr_raw, ncube, nhalo, nsb, nsw )


    REAL (KIND=dbl_kind), &
            DIMENSION(ncube-1,ncube-1,6), INTENT(IN) :: terr_dev
    REAL (KIND=dbl_kind), &
            DIMENSION(ncube-1,ncube-1,6), INTENT(IN) :: terr_raw


       INTEGER (KIND=int_kind), INTENT(IN) :: ncube, nhalo, nsb, nsw
       INTEGER (KIND=int_kind) :: i,j,np

    REAL (KIND=dbl_kind), PARAMETER :: pi        = 3.14159265358979323846264338327


    REAL (KIND=dbl_kind),                                            &
         DIMENSION(1-nhalo:ncube+nhalo, 1-nhalo:ncube+nhalo, 6) :: terr_halo
    REAL (KIND=dbl_kind),                                            &
         DIMENSION(1-nhalo:ncube+nhalo, 1-nhalo:ncube+nhalo, 6) :: terr_dev_halo

    REAL  ,                                                          &
         DIMENSION(1-nhalo:ncube+nhalo )                          :: xv,yv,alph,beta

    DO np = 1, 6
     CALL CubedSphereFillHalo_Linear_extended(terr_raw, terr_halo(:,:,np), np, ncube+1,nhalo)  
     CALL CubedSphereFillHalo_Linear_extended(terr_dev, terr_dev_halo(:,:,np), np, ncube+1,nhalo)  
    END DO


   
    DO i=1-nhalo,ncube+nhalo
       xv(i)=1.*i
       yv(i)=1.*i
    END DO
    DO i=1-nhalo,ncube+nhalo
       alph(i)=(pi/4.)*(1.*i - 0.5 + nhalo - (ncube+2.*nhalo-1)/2.) / ((ncube+2.*nhalo-1)/2.)
       beta(i)=(pi/4.)*(1.*i - 0.5 + nhalo - (ncube+2.*nhalo-1)/2.) / ((ncube+2.*nhalo-1)/2.)
    END DO

      write(811) size(terr_halo,1), size(terr_halo,2) ,size(terr_halo,3)
      write(811) xv,yv,alph,beta
      write(811) terr_halo,terr_dev_halo




end subroutine find_ridges
!----------------------------------------------------------

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine ANISO_ANA( AA,AARAW,X,Y,N,NSB,NSW,IPANEL)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  INTEGER,            intent(IN)  ::  N,NSB,NSW,IPANEL
  REAL,               intent(IN)  ::  AA(N,N),X(N),Y(N),AARAW(N,N)

  integer, parameter             ::   NANG=16

  real, allocatable:: SUBA(:,:), SUBARW(:,:),SUBX(:),SUBY(:),RT(:,:),RTX(:),XRT(:),RTXMN(:),RTXSLP(:) &
                     , PKLC(:), RTY(:),RTRW(:,:),RTRWX(:)

  logical,allocatable :: lhgts(:)

  real :: THETRAD,PI,swt,ang,rotmn,rotvar,mnt,var,xmn,xvr,basmn,basvar,mn2,var2
  integer :: i,j,l,m,n2,mini,maxi,minj,maxj,ns0,ns1,iorn(1),jj,ipkh(1),ift0(1),ift1(1),i2

  real :: vvaa(NANG),qual(NANG),dex(NANG),beta(NANG),alph,xpkh(NANG),ang00,dex0(nang),dex1(nang),xft0(NANG),xft1(NANG),HWDX(NANG) &
         , NPKX(NANG),NVLX(NANG),vva2(NANG),pkht(NANG),vldp(NANG),rwpk(NANG),rwvl(NANG)

  

  ns0=nsw/2+1
  ns1=ns0+nsw

  

  PI = 2*ACOS(0.0)

! Allocate work arrays for ridge analysis
!-----------------------------------------
  allocate( suba( 2*nsw, 2*nsw ) )
  allocate( subarw( 2*nsw, 2*nsw ) )
  allocate( rt( 2*nsw, 2*nsw ) )
  allocate( rtrw( 2*nsw, 2*nsw ) )
  allocate( subx( 2*nsw  ) )
  allocate( suby( 2*nsw  ) )
  allocate( rtx( nsw  ) )
  allocate( rty( nsw  ) )
  allocate( rtrwx( nsw  ) )
  allocate( lhgts( nsw  ) )
  allocate( rtxslp( nsw-1  ) )
  allocate( pklc( 2:nsw-1  ) )
  allocate( rtxmn( nsw  ) )
  allocate( xrt( nsw  ) )



! Allocate arrays to hold results on reduced Cubed-sphere grid
!--------------------------------------------------------------
  allocate( xs(N/nsb) )
  allocate( ys(N/nsb) )
  ys=-999.
  xs=-999.

  allocate( bsvar( N/nsb , N/nsb ) )
  bsvar=0. !-999.

  allocate( mxvrx( N/nsb , N/nsb ) )
   mxvrx=0.
  allocate( mxvry( N/nsb , N/nsb ) )
   mxvry=0.
  allocate( mxdis( N/nsb , N/nsb ) )
   mxdis=0.
  allocate( anglx( N/nsb , N/nsb ) )
   anglx=-999.
  allocate( aniso( N/nsb , N/nsb ) )
   aniso=0.
  allocate( mnslp( N/nsb , N/nsb ) )
   mnslp=0.
  allocate( xspk( N/nsb , N/nsb ) )
   xspk=0.
  allocate( yspk( N/nsb , N/nsb ) )
   yspk=0.
  allocate( mxds0( N/nsb , N/nsb ) )
   mxds0=0.
  allocate( mxds1( N/nsb , N/nsb ) )
   mxds1=0.
  allocate( hwdth( N/nsb , N/nsb ) )
   hwdth=0.
  allocate( npks( N/nsb , N/nsb ) )
   npks=0.
  allocate( nvls( N/nsb , N/nsb ) )
   nvls=0.
  allocate( sft0( N/nsb , N/nsb ) )
   sft0=0.
  allocate( sft1( N/nsb , N/nsb ) )
   sft1=0.
  allocate( vldps( N/nsb , N/nsb ) )
   vldps=0.
  allocate( pkhts( N/nsb , N/nsb ) )
   pkhts=0.
  allocate( rwvls( N/nsb , N/nsb ) )
   rwvls=0.
  allocate( rwpks( N/nsb , N/nsb ) )
   rwpks=0.




end subroutine ANISO_ANA


end module ridge_ana
