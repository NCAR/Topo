#define ROTATEBRUSH
#define DETGDEP
#undef DEBUGOUTPUT
module ridge_ana

use rotation, only : rotbyx => rotby4
USE reconstruct
use shr_kind_mod, only: rpx => shr_kind_r8
use shr_kind_mod, only: r8 => shr_kind_r8
use ridge_utils

IMPLICIT NONE
private


public find_local_maxes
public find_ridges
public remapridge2target
public remapridge2cube
public remapridge2tiles

public anglx_target,aniso_target,mxdis_target,hwdth_target
public mxvrx_target,mxvry_target,bsvar_target,wghts_target,riseq_target
public ang22_target,anixy_target,clngt_target,cwght_target,count_target
public nsubr,grid_length_scale,fallq_target,isoht_target,isowd_target
public isovar_target

public peak_type

!===============================================================================
!These quantities will be on ridge-based "list"
  REAL(RPX), allocatable  :: MXVRX(:),MXDIS(:),MNSLP(:),ANGLX(:),ANISO(:),XS(:),YS(:) 
  REAL(RPX), allocatable  :: XSPK(:),YSPK(:),MXDS0(:),MXDS1(:),SFT0(:),SFT1(:)
  REAL(RPX), allocatable  :: PKHTS(:),VLDPS(:),RWPKS(:),RWVLS(:),ANGLL(:)
  REAL(RPX), allocatable  :: BSVAR(:),HWDTH(:),NPKS(:),NVLS(:),MXVRY(:),CLNGT2(:)
  REAL(RPX), allocatable  :: RISEQ(:),FALLQ(:),ANIXY(:),MXDSP(:),CLNGTH(:),MXDS2(:)

  REAL(RPX), allocatable  :: rdg_profiles(:,:) , crst_profiles(:,:), crst_silhous(:,:)
  INTEGER, allocatable ::  MyPanel(:), NSWx_diag(:)
  REAL(RPX), allocatable  :: rt_diag(:,:,:), suba_diag(:,:,:),rdg_profiles_x(:,:)

  REAL(RPX), allocatable  :: UNIQID(:),ISOHT(:),ISOWD(:),ISOBS(:),xs01(:),ys01(:)
  REAL(RPX), allocatable  :: RefFac(:)
  REAL(RPX), allocatable  :: hnodes_list(:,:)
  INTEGER, allocatable  :: xnodes_list(:,:), dcenter_list(:), nnodes_list(:)
  INTEGER, allocatable  :: xwedge_list(:,:)
  REAL(RPX), allocatable  :: hwedge_list(:,:)


  REAL(RPX), allocatable  :: hwedge_o(:,:),hwedge_x(:,:),hnodes_x(:,:),hnodes_o(:,:)

!================================================================================


    real(r8), allocatable ::  ALP0(:),BET0(:),LAT0(:),LON0(:)
    real(r8), allocatable ::  ALP1(:),BET1(:),LAT1(:),LON1(:)

  real(r8), allocatable, dimension(:,:) :: anglx_target,aniso_target,mxdis_target,hwdth_target
  real(r8), allocatable, dimension(:,:) :: mxvrx_target,mxvry_target,bsvar_target,wghts_target 
  real(r8), allocatable, dimension(:,:) :: ang22_target,anixy_target,clngt_target,cwght_target
  real(r8), allocatable, dimension(:,:) :: count_target,riseq_target,fallq_target
  real(r8), allocatable, dimension(:,:) :: isoht_target,isowd_target
  real(r8), allocatable, dimension(:)   :: isovar_target
  !!,rwpks_target

    INTEGER (KIND=int_kind),allocatable :: UQRID(:) 

  real(r8), allocatable, dimension(:,:) :: anglx_tiles,aniso_tiles,mxdis_tiles,hwdth_tiles
  real(r8), allocatable, dimension(:,:) :: clngt_tiles

  integer :: PSW  ! NSW/PSW extremely clever analogy to ncols/pcols 

    REAL(RPX), allocatable ::  wt1p(:,:)

    REAL(RPX), allocatable :: agnom(:),bgnom(:)

    REAL(r8) :: grid_length_scale

    REAL(KIND=dbl_kind), PARAMETER :: pi        = 3.14159265358979323846264338327
    REAL(KIND=dbl_kind), PARAMETER :: earth_radius        = 6371.0

    integer, parameter             ::   NANG=16
    integer, parameter             ::   NSUBR = NANG
    integer, parameter             ::   NTILES = 250


  !-----------------------------------
  ! Tunable parameters for Aniso Ana
  real(rpx), parameter  :: CC_L1=0.5   ! y-slop/max(x_slop) for length (0.2 for cesm2.0)
  real(rpx), parameter  :: CC_L2=0.5   ! y/max(y) for length
  real(rpx), parameter  :: CC_W1=0.5   ! y/max(y) for length



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  TYPE peak_type
    REAL(RPX) :: maxht = -999.
    INTEGER :: i  = -99
    INTEGER :: j  = -99
    INTEGER :: ip = -99
    INTEGER :: idpk = -99
 end type peak_type

 type (peak_type), allocatable, dimension(:)  ::  peaks

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine find_local_maxes ( terr_dev, ncube, nhalo, nsw, iopt_ridge_seed )
!------------------------------------------------
!  INPUTS.
!      NSW = size of window used for ridge analysis
!      

    !type (peak_type), allocatable, dimension(:), intent(out) ::  peaks


    REAL(KIND=dbl_kind), &
            DIMENSION(ncube,ncube,6), INTENT(IN) :: terr_dev

       INTEGER (KIND=int_kind), INTENT(IN)  :: ncube, nhalo, nsw
       INTEGER (KIND=int_kind), INTENT(IN)  :: iopt_ridge_seed
       INTEGER (KIND=int_kind) :: i,j,np,ncube_halo,ipanel,N,norx,nory,ip,nhigher,npeaks
       INTEGER (KIND=int_kind) :: ipk,nblock,ijaa(2),im,jm,bloc,ivar1,ivar2,ii,jj

    REAL(KIND=dbl_kind), &
            DIMENSION(ncube,ncube,6)  :: terr_max , terr_sm

    REAL(KIND=dbl_kind),                                            &
         DIMENSION(1-nhalo:ncube+nhalo, 1-nhalo:ncube+nhalo, 6) :: terr_max_halo

    REAL(KIND=dbl_kind),                                            &
         DIMENSION(1-nhalo:ncube+nhalo, 1-nhalo:ncube+nhalo, 6) :: terr_dev_halo, terr_sm_halo, terr_dev_halox

    REAL(RPX) ,                                                          &
         DIMENSION(1-nhalo:ncube+nhalo )                          :: xv,yv,alph,beta

    real(KIND=dbl_kind), allocatable :: aa(:,:)


    !real(r8)  :: lon_r8, lat_r8, cosll, dx, dy, dcube2, ampfsm,dbet,dalp,diss,diss00
    real(r8)  :: thsh

    CHARACTER(len=1024) :: ofile,ve

!---------------------------------------------------------------------------------------
!  B E G I N   C A L C U L A T I O N S
!---------------------------------------------------------------------------------------


    DO np = 1, 6
     CALL CubedSphereFillHalo_Linear_extended(terr_dev, terr_dev_halo(:,:,np), np, ncube+1,nhalo)  
    END DO



    DO np = 1, 6
    DO j=1-nhalo,ncube+nhalo
    DO i=1-nhalo,ncube+nhalo
       terr_max_halo(i,j,np) = 0._r8
    END DO
    END DO
    END DO

    terr_sm = terr_dev*0._r8
 
 
    if( iopt_ridge_seed == 3 ) then
    thsh  = 0.
    bloc  = 1 
    ivar2 = INT( (2*bloc+1)**2   )-1 ! INT( (2*bloc+1)**2 /2  ) 
    allocate( aa(-bloc:bloc,-bloc:bloc) )
    write(*,*) " in find_local_max iopt_ridge_seed=3 "
    write(*,*) " ---> NSW , bloc , thresh count ",nsw,bloc,ivar2

    DO np = 1, 6
    DO j=1-nhalo+bloc,ncube+nhalo-bloc
    DO i=1-nhalo+bloc,ncube+nhalo-bloc
       aa   = terr_dev_halo(i-bloc:i+bloc,j-bloc:j+bloc,np)
       ivar1=0
       do jj=-bloc,bloc
       do ii=-bloc,bloc
          if (aa(0,0) > aa(ii,jj) ) ivar1=ivar1+1
       end do
       end do
       if ( (ivar1 >=ivar2) .AND. ( terr_dev_halo(i,j,np) > thsh )) &
           terr_max_halo(i,j,np)= terr_dev_halo(i,j,np)
    END DO
    END DO
             write(*,*) " FACE = ",np
    END DO
    deallocate(aa)
    end if

    if( iopt_ridge_seed == 2 ) then
    thsh = 10.
    bloc = MAX( 4 ,  NINT(nsw/8.) ) ! Think about floor of 4
    !bloc = MAX( 2 ,  NINT(nsw/16.) ) ! Think about floor of 4
    allocate( aa(bloc+1,bloc+1) )
    write(*,*) " in find_local_max iopt_ridge_seed=2 "
    write(*,*) " ---> NSW , bloc ",nsw,bloc

    DO np = 1, 6
    DO j=1-nhalo+1,ncube+nhalo-1,bloc
    DO i=1-nhalo+1,ncube+nhalo-1,bloc
       aa   = terr_dev_halo(i:i+bloc,j:j+bloc,np)
       ijaa = MAXLOC( aa )
       im   = ijaa(1)-1 + i
       jm   = ijaa(2)-1 + j
       if ( terr_dev_halo(im,jm,np) >= thsh ) terr_max_halo(im,jm,np)= terr_dev_halo(im,jm,np)
    END DO
    END DO
             write(*,*) " FACE = ",np
    END DO
    deallocate(aa)
    end if

    if( iopt_ridge_seed == 1 ) then
    write(*,*) "performing a diff with 3x3 to find 'peaks' "

    DO np = 1, 6
    DO j=1-nhalo+1,ncube+nhalo-1
    DO i=1-nhalo+1,ncube+nhalo-1

      terr_dev_halox(i,j,np) =       &
         terr_dev_halo(i+1,j,np) -   &
         (  terr_dev_halo(i+1,j,np)+ &
            terr_dev_halo(i-1,j,np)+ &
            terr_dev_halo(i,j+1,np)+ &
            terr_dev_halo(i,j-1,np)+ & 
            terr_dev_halo(i+1,j-1,np)+ & 
            terr_dev_halo(i-1,j-1,np)+ & 
            terr_dev_halo(i+1,j+1,np)+ & 
            terr_dev_halo(i-1,j+1,np)+ & 
            terr_dev_halo(i,j,np) ) /9.

    END DO
    END DO
             write(*,*) " FACE = ",np
    END DO

    terr_dev_halo = terr_dev_halox
    end if

    if( ( iopt_ridge_seed == 1 ).OR. ( iopt_ridge_seed == 0 )) then
    thsh = 0.
    DO np = 1, 6
    DO j=1-nhalo+1,ncube+nhalo-1
    DO i=1-nhalo+1,ncube+nhalo-1

      if ( ( terr_dev_halo(i,j,np) > terr_dev_halo(i+1,j,np)   ) .and. &
           ( terr_dev_halo(i,j,np) > terr_dev_halo(i-1,j,np)   ) .and. &
           ( terr_dev_halo(i,j,np) > terr_dev_halo(i,j+1,np)   ) .and. &
           ( terr_dev_halo(i,j,np) > terr_dev_halo(i,j-1,np)   ) .and. & 
           ( terr_dev_halo(i,j,np) > terr_dev_halo(i+1,j-1,np)   ) .and. & 
           ( terr_dev_halo(i,j,np) > terr_dev_halo(i-1,j-1,np)   ) .and. & 
           ( terr_dev_halo(i,j,np) > terr_dev_halo(i+1,j+1,np)   ) .and. & 
           ( terr_dev_halo(i,j,np) > terr_dev_halo(i-1,j+1,np)   ) .and. & 
           ( terr_dev_halo(i,j,np) > 0. )  ) then 

                  terr_max_halo(i,j,np) = terr_dev_halo(i,j,np)
       END IF

    END DO
    END DO
             write(*,*) " FACE = ",np
    END DO
    end if


    do np=1,6
       terr_max(1:ncube,1:ncube,np) = terr_max_halo(1:ncube,1:ncube,np )
    end do

     

    npeaks = count(  (terr_max > thsh) )

    allocate( peaks( npeaks ) )

    ipk=1
    DO np = 1, 6
    DO j=1,ncube
    DO i=1,ncube
       if (terr_max(i,j,np) > thsh ) then
          peaks( ipk )%i  = i
          peaks( ipk )%j  = j
          peaks( ipk )%ip = np
          peaks( ipk )%idpk  = ipk
          peaks( ipk )%maxht = terr_max(i,j,np)
          ipk=ipk+1
       endif
    end do
    end do
write(*,*) ' PANEL = ',NP
    end do

write(*,*) " two sizes of peaks ", npeaks, ipk-1
write(*,*) " SHAPE ", shape( peaks%i )


 end subroutine find_local_maxes

!===================================================================================================

subroutine find_ridges ( terr_dev, terr_raw, ncube, nhalo, nsw,     & 
!                        ++ following used only for file name construction -11/8/21
                         ncube_sph_smooth_coarse, ncube_sph_smooth_fine, &
                         ldevelopment_diags, lregional_refinement, rr_factor)
!---------------------------------------------------------------------
!  Key INPUTS.
!      NSW: = HALF-size of square window used for ridge analysis. Subsquares of
!             topo are created and fed to ridge analysis scheme ANISO_ANA, e.g, 
!                    suba    = terr_dev_halo_r4( i-nsw:i+nsw , j-nsw:j+nsw, np )
!             NSW winds up in file names, e.g., here where nsw=42
!                    fv_0.9x1.25_nc3000_Nsw042_Nrs008_Co060_Fi001_20211102.nc
!             The current thought (11/2021) is that NSW should be about 
!             SQRT(2)*coarse_smoothing_radius, i.e., inscribed square (Note units
!             are ~3km pixel-lengths). This is refected in example filename 'Co60'.
!----------------------------------------------------------------------


    !type (peak_type), dimension(npeaks), intent(inout) ::  peaks
    REAL(KIND=dbl_kind), &
            DIMENSION(ncube,ncube,6),           INTENT(IN) :: terr_dev
    REAL(KIND=dbl_kind), &
            DIMENSION(ncube,ncube,6),           INTENT(IN) :: terr_raw
    INTEGER (KIND=int_kind),                    INTENT(IN) :: ncube, nhalo, nsw !, npeaks
    INTEGER (KIND=int_kind),                    INTENT(IN) :: ncube_sph_smooth_coarse
    INTEGER (KIND=int_kind),                    INTENT(IN) :: ncube_sph_smooth_fine
    LOGICAL,                                    INTENT(IN) :: ldevelopment_diags
    LOGICAL, OPTIONAL,                          INTENT(IN) :: lregional_refinement
    REAL(KIND=dbl_kind), &
            DIMENSION(ncube,ncube,6), optional, INTENT(IN) :: rr_factor
    !
    ! Local variables
    ! 
    INTEGER (KIND=int_kind) :: i,j,np,ncube_halo,ipanel,N,norx,nory,ip,ipk,npeaks,nswx
    INTEGER (KIND=int_kind) :: ispk,jspk
    INTEGER (KIND=int_kind) :: num_iter_ridge,iter_ridge,ns0,ns1


    REAL(KIND=dbl_kind),                                            &
         DIMENSION(1-nhalo:ncube+nhalo, 1-nhalo:ncube+nhalo, 6) :: terr_halo
    REAL(KIND=dbl_kind),                                            &
         DIMENSION(1-nhalo:ncube+nhalo, 1-nhalo:ncube+nhalo, 6) :: terr_dev_halo
    REAL(RPX) ,                                            &
         DIMENSION(1-nhalo:ncube+nhalo, 1-nhalo:ncube+nhalo, 6) :: terr_halo_r4
    REAL(RPX) ,                                            &
         DIMENSION(1-nhalo:ncube+nhalo, 1-nhalo:ncube+nhalo, 6) :: terr_dev_halo_r4

    REAL(RPX) ,                                                          &
         DIMENSION(1-nhalo:ncube+nhalo )                          :: xv,yv,alph,beta

    !! real(RPX) :: SUBA(2*nsw+1, 2*nsw+1 ), SUBARW(2*nsw+1, 2*nsw+1 ), SUBX(2*nsw+1), SUBY(2*nsw+1)
    real(RPX),allocatable ::SUBA(: , : ), SUBARW( : , : ), SUBX(:), SUBY(:),subrot(:,:)

    real(r8)  :: lon_r8, lat_r8, cosll, dx, dy, dcube2, ampfsm,dbet,dalp,diss,diss00
    real(r8)  :: ggaa,ggbb,ggab,irho
    !! Square root of DET(metric tensor), i.e., area 

    REAL(KIND=dbl_kind),                                            &
         DIMENSION(1-nhalo:ncube+nhalo, 1-nhalo:ncube+nhalo ) :: rdtg

    CHARACTER(len=1024) :: ofile,ve
    character(len=8)    :: date
    character(len=10)   :: time
    logical :: do_refine

!----------------------------------------------------------------------------------------------------
    do_refine = .FALSE.
    if(present(lregional_refinement)) do_refine = lregional_refinement

    if(do_refine) then
      write(*,*) "regional refinement not merged - ABORT"
      !STOP
    end if

    npeaks = size( peaks% i )
   
write(*,*) " size of peaks in find_ridge ", npeaks
write(*,*) " SHAPE ", shape( peaks%i )



!---------------------------------------------------------------------------------------
!  B E G I N   C A L C U L A T I O N S
!---------------------------------------------------------------------------------------

    DO np = 1, 6
     CALL CubedSphereFillHalo_Linear_extended(terr_raw, terr_halo(:,:,np), np, ncube+1,nhalo)  
     CALL CubedSphereFillHalo_Linear_extended(terr_dev, terr_dev_halo(:,:,np), np, ncube+1,nhalo)  
    END DO


#if 1
! Original definitions  
    DO i=1-nhalo,ncube+nhalo
       xv(i)=1.*i   !  xv,yv are 'SW' corners
       yv(i)=1.*i   
    END DO
    DO i=1-nhalo,ncube+nhalo
       alph(i) =  ( xv(i) - 0.5 - ncube/2 )*(pi/2.)/ncube
       beta(i) =  ( yv(i) - 0.5 - ncube/2 )*(pi/2.)/ncube
    END DO
#else
    DO i=1-nhalo,ncube+nhalo
       xv(i)=1.*i - 0.5   !  xv,yv are cell centers
       yv(i)=1.*i - 0.5
    END DO
    DO i=1-nhalo,ncube+nhalo
       alph(i) =  ( xv(i) - ncube/2 )*(pi/2.)/ncube
       beta(i) =  ( yv(i) - ncube/2 )*(pi/2.)/ncube
    END DO
#endif

    !! Calculate Square root of DET(metric tensor), i.e., area 
    DO j=1-nhalo,ncube+nhalo
    DO i=1-nhalo,ncube+nhalo
       irho = ( 1. + (tan(alph(i))**2) + (tan(beta(j))**2 ) )**2   
       irho = 1. / ( ( cos(alph(i))**2 ) * (cos(beta(j))**2) * irho )  
       !irho = 1./ ( ( cos(alph(i))**2)*(cos(beta(j))**2)* ( ( 1. + (tan(alph(i))**2) + (tan(beta(j))**2 ) )**2  ))   ???
       ggaa = irho * ( 1. + ( tan( alph(i) ) )**2 )
       ggbb = irho * ( 1. + ( tan( beta(j) ) )**2 )
       ggab = -irho *( tan( beta(j) ) ) * ( tan( alph(i) ) )
       rdtg(i,j) = SQRT( ggaa*ggbb - ggab**2 )
    END DO
    END DO

    grid_length_scale = ( alph(1)-alph(0) )*earth_radius


    write(*,*) "ALPHA(1:NCUBE) ",alph(1)*180./pi,alph(ncube)*180./pi

    write(*,*) " GRID scale ",grid_length_scale

    allocate( agnom(ncube) )
    allocate( bgnom(ncube) )
    agnom(1:ncube) = alph(1:ncube)
    bgnom(1:ncube) = beta(1:ncube)

      terr_dev_halo_r4 = terr_dev_halo
      terr_halo_r4     = terr_halo


      ncube_halo = size( terr_halo_r4, 1)

   call alloc_ridge_qs(npeaks , NSW)
 
   do ipk = 1,npeaks
        i  = peaks(ipk)%i
        j  = peaks(ipk)%j
        np = peaks(ipk)%ip
        MyPanel( ipk ) = np  ! could just write peaks%ip but WTF        



#ifndef DETGDEP
        ! Fixed ridge-finding window 
        nswx=  nsw 
#else
        ! det(g) dependent ridge-finding window 
        nswx=  NINT( 1.*nsw / rdtg(i,j) )
#endif
        if(do_refine) nswx = NINT( nswx / rr_factor(i,j,np) )
        if(do_refine) RefFac(ipk) = rr_factor(i,j,np) 

        !++jtb 05/26/24: Protection against too-small nswx
        nswx = MAX( 4 , nswx )

        NSWx_diag( ipk ) = nswx

             allocate( suba( 2*nswx+1 ,  2*nswx+1 ) )
             allocate( subarw( 2*nswx+1 ,  2*nswx+1 ) )
             allocate( subx( 2*nswx+1  ) )
             allocate( suby( 2*nswx+1  ) )

        suba    = terr_dev_halo_r4( i-nswx:i+nswx , j-nswx:j+nswx, np )
        subarw  = terr_halo_r4( i-nswx:i+nswx , j-nswx:j+nswx, np )
        subx    = xv(i-nswx:i+nswx )
        suby    = yv(j-nswx:j+nswx )
        call ANISO_ANA( suba , subarw , subX , subY ,NSWx, ipk )

        ispk = NINT(1.*xspk(ipk) )
        jspk = NINT(1.*yspk(ipk) )
        suba    = terr_dev_halo_r4( ispk-nswx:ispk+nswx , jspk-nswx:jspk+nswx, np )
        call ANISO_ANA_2( SUBA,NSWx,IPK )
  
        deallocate( suba, subarw, subx, suby )
#if 0
        allocate( suba( 2*PSW+1 , 2*PSW+1 ) )
        allocate( subrot( 2*PSW+1 , 2*PSW+1 ) )
        suba    = terr_dev_halo_r4( ispk-PSW:ispk+PSW , jspk-PSW:jspk+PSW, np )
        subrot  = rotbyx( suba, 2*PSW+1 , anglx(ipk) )
        where(subrot < -8000.) subrot=0.
        rdg_profiles_x(:,ipk) = sum( subrot( : , PSW-nsw/2:PSW+nsw/2 ) , 2 ) /(nsw+1) 
        deallocate( suba, subrot )
#endif
        write(*,903,advance='no') achar(13) , ipk, npeaks,nswx , mxdis(ipk)
 
   end do

    write(*,*)

    write(*,*) " Done with anisotropy analysis "

900 format( a1, "  Analyzed Ridges (original) ",i8," out of ",i8,"  NSWx=",i3," mxdis=",f8.2 )
902 format( a1, "  Analyzed Ridges (iterative): N="i2," Ridge ",i8," out of ",i8,"  NSWx=",i3," mxdis=",f8.2 )
903 format( a1, "  Analyzed Ridges (two-Phase) ",i8," out of ",i8,"  NSWx=",i3," mxdis=",f8.2 )
901 format(" Ridge coords ", i6,i6,i3 )
!++tune 
    !call thinout_list( ncube, npeaks, nsw )

    if (ldevelopment_diags) then
      write( ofile, &
           "('./output/Ridge_list_nc',i0.4, '_Nsw',i0.3,  &
           '_Co',i0.3,'_Fi',i0.3 )" ) & 
           ncube, nsw , ncube_sph_smooth_coarse, ncube_sph_smooth_fine 
      
      !--- get time stamp for output filename
      !----------------------------------------------------------------------
      call DATE_AND_TIME( DATE=date,TIME=time)
      
      ofile  = trim(ofile)//'_'//date//'_'//time(1:4)//'.dat'
      
      OPEN (unit = 31, file= trim(ofile) ,form="UNFORMATTED" )

      write(31) npeaks  , NSW, PSW
      write(31) xs,ys  , MyPanel, NSWx_diag
      write(31) mxvrx
      write(31) bsvar
      write(31) mxdis
      write(31) anglx
      write(31) aniso
      write(31) mnslp
      write(31) angll
      write(31) xspk , xs01
      write(31) yspk , ys01
      write(31) mxds0
      write(31) mxds1
      write(31) sft0
      write(31) sft1
      write(31) hwdth
      write(31) npks
      write(31) mxvry
      
      write(31) nvls
      write(31) pkhts
      write(31) vldps
      write(31) rwpks
      write(31) rwvls
      
      write(31) lon0
      write(31) lon1
      write(31) lat0
      write(31) lat1
      
      write(31) uniqid
      write(31) riseq
      write(31) fallq
      write(31) clngth
      
      write(31) mxds2
      
      
      write(31) rdg_profiles
      write(31) crst_profiles
      write(31) crst_silhous
      write(31) isoht
      write(31) isowd
      write(31) isobs
      write(31) RefFac
      write(31) rdg_profiles_x

      write(31) xnodes_list
      write(31) hnodes_list
      write(31) dcenter_list
      write(31) xwedge_list,hwedge_list

      write(31) nnodes_list
      write(31) hwedge_x
      write(31) hnodes_x
      write(31) hwedge_o
      write(31) hnodes_o

      CLOSE(31)

#ifdef DEBUGOUTPUT
      write( ofile , &
           "('./output/TerrXY_list_nc',i0.4, '_Nsw',i0.3,  &
           '_Co',i0.3,'_Fi',i0.3 )" ) & 
           ncube, nsw , ncube_sph_smooth_coarse, ncube_sph_smooth_fine 
      
      !--- get time stamp for output filename
      !----------------------------------------------------------------------
      call DATE_AND_TIME( DATE=date,TIME=time)
      
      ofile  = trim(ofile)//'_'//date//'_'//time(1:4)//'.dat'
      
      OPEN (unit = 32, file= trim(ofile) ,form="UNFORMATTED" )
      
      write(32) npeaks  , PSW
      do ipk=1,npeaks
        write(32) xs(ipk),ys(ipk)  , MyPanel(ipk),xspk(ipk),yspk(ipk)
        write(32) rt_diag(:,:,ipk)
        write(32) suba_diag(:,:,ipk)
      end do
      
      close(32)
#endif
 
    end if
end subroutine find_ridges
!----------------------------------------------------------

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine ANISO_ANA( SUBA,SUBARW,SUBX,SUBY,NSW,IPK )
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  INTEGER,            intent(IN)  ::  NSW,IPK
  REAL(RPX),               intent(IN)  ::  SUBA(2*nsw+1,2*nsw+1),SUBX( 2*nsw+1),SUBY(2*nsw+1)
  REAL(RPX),               intent(IN)  ::  SUBARW( 2*nsw+1, 2*nsw+1)

  real(rpx), allocatable :: RT(:,:),RTX(:),XRT(:),RTXMN(:),RTXSLP(:),rtx_dt(:)
  real(rpx), allocatable :: PKLC(:), RTY(:),RTRW(:,:),RTRWX(:),DERTX(:),DERTY(:),CUSP(:),face(:)
  real(rpx), allocatable :: silux(:), sillx(:), siluy(:), silly(:)

  real(rpx), allocatable :: rdg_profile(:,:),crst_profile(:,:)
  real(rpx), allocatable :: crst_silhouette(:,:)
  real(rpx), allocatable :: rdg_profile_bk(:),crst_silhouette_bk(:)

  logical,allocatable :: lhgts(:),lflats(:),lsides(:)
  logical :: Keep_Cuestas=.true.
  logical :: reposition_ridge

  real(RPX) :: THETRAD,PI,swt,ang,rotmn,rotvar,mnt,var,xmn,xvr,basmn,basvar,mn2,var2
  real(RPX) :: dyr_crest
  real(RPX) :: xspk0 , yspk0
  integer :: i,j,l,m,n2,mini,maxi,minj,maxj,ns0,ns1,iorn(1),jj
  integer :: ipkh(1),ivld(1),ift0(1),ift1(1),i2,ii,ipksv(1),nswx
  integer :: ibad_left,ibad_rght
  integer :: phase

  real(RPX) :: vvaa(NANG),qual(NANG),dex(NANG),beta(NANG),alph,xpkh(NANG),ang00
  real(RPX) :: dex0(nang),dex1(nang),xft0(NANG),xft1(NANG),xvld(NANG)
        
  real(RPX) :: NPKX(NANG),NVLX(NANG),vva2(NANG),pkht(NANG),vldp(NANG),rwpk(NANG)
  real(RPX) :: rwvl(NANG),RISEX(NANG),FALLX(NANG)
  real(RPX) :: dex_dt(NANG)


  !-----------------------------------------
  ! Indices for most ridge scheme subarrays.
  ! Note ns1-ns0 = nsw+1 always. Appears 
  ! safe for any value of nsw.
  !-----------------------------------------
  ns0=nsw/2+1
  ns1=ns0+nsw+1
 

  PI = 2*ACOS(0.0)


! Allocate work arrays for ridge analysis
!-----------------------------------------
  allocate( rt( 2*nsw+1, 2*nsw+1 ) )
  allocate( rtrw( 2*nsw+1, 2*nsw+1 ) )
  allocate( rtx( nsw+1  ) )
  allocate( rtx_dt( nsw+1  ) )
  allocate( rty( nsw+1  ) )
  allocate( cusp( nsw+1  ) )
  allocate( dertx( nsw+1-1  ) )
  allocate( derty( nsw+1-1  ) )
  allocate( rtrwx( nsw+1  ) )
  allocate( rtxslp( nsw+1-1  ) )
  allocate( pklc( 2:nsw+1-1  ) )
  allocate( rtxmn( nsw+1  ) )
  allocate( xrt( nsw+1  ) )
  allocate( face( nsw+1  ) )

  allocate( silux( nsw+1  ) )
  allocate( sillx( nsw+1  ) )
  allocate( siluy( nsw+1  ) )
  allocate( silly( nsw+1  ) )

  allocate( Lhgts( nsw+1  ) )
  allocate( Lflats( nsw+1  ) )
  allocate( Lsides( nsw+1  ) )

  allocate( rdg_profile(nsw+1,NANG) )
  allocate( crst_profile(nsw+1,NANG) )
  allocate( crst_silhouette(nsw+1,NANG) )
  allocate( rdg_profile_bk(nsw+1) )
  allocate( crst_silhouette_bk(nsw+1) )



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


  do i=1,nsw+1 
     xrt(i) = 1.*(i-1)
  end do
  xmn=0.5*(xrt(1)+xrt(nsw+1))
  xvr=sum( (xrt-xmn)**2 )/nsw+1

     ys(ipk)= sum( suby )/size(suby,1)
     xs(ipk)= sum( subx )/size(subx,1)

        basmn  =  sum( sum( suba(ns0:ns1-1,ns0:ns1-1) , 1 ), 1) /(( ns1-ns0 )*(ns1-ns0))
        basvar =  sum( sum( (suba(ns0:ns1-1,ns0:ns1-1)-basmn)**2 , 1 ), 1) /(( ns1-ns0 )*(ns1-ns0))
        bsvar(ipk) = basvar

        vvaa(:)=0
        qual(:)=0.
        npkx(:)=0.
        do L=1,nang
                                           ! Rotate 2D topography by ANG
           ang = (L-1)*(180./nang)
           rt  = rotbyx( suba, 2*nsw+1 , ang )
           rtrw  = rotbyx( subarw, 2*nsw+1 , ang )  ! Raw topo rotation

                                           ! Take "Y" (and "X")-average of rotated topography.
                                           ! Yields topo profile in X ==> RTX
           rtx = sum( rt(ns0:ns1-1,ns0:ns1-1) , 2 ) /( ns1-ns0 ) ! Y-average 
           rty = sum( rt(ns0:ns1-1,ns0:ns1-1) , 1 ) /( ns1-ns0 ) ! X-average
           rtrwx = sum( rtrw(ns0:ns1-1,ns0:ns1-1) , 2 ) /( ns1-ns0 ) ! Y-average of Raw topo

                                          ! "Silhouettes" in x and y
           do m=ns0,ns1-1
              siluy(m-ns0+1) = maxval(  rt( ns0:ns1-1, m) )
           end do
                          
                                           ! Mean elevation
           mnt = sum( rtx )/( ns1-ns0 ) 
           mn2 = sum( rty )/( ns1-ns0 ) 


                 ! count actual peaks and valleys in RTX cross section
           pklc = 0.
           do i2=2,nsw
              if ( ( rtx(i2-1)<rtx(i2) ).and.( rtx(i2+1)<rtx(i2) ) ) pklc(i2)=1.
           end do
           npkx(L)=sum(pklc)



           ! Accumulate rising and falling segments
           risex(L)=0.
           fallx(L)=0.


           ! Record actual max and min elevations in RTX and Raw topo profile (RTRWX)
           pkht(L)=maxval(RTX)


           var = sum( (rtx-mnt)**2 )/( ns1-ns0 ) 
           vvaa(L) = var
           dex(L)  = MAXVAL(RTX)-MINVAL(RTX)


           var2 = sum( (rty-mn2)**2 )/( ns1-ns0 ) 
           vva2(L) = var2


           !================ Dec 2021  =========================================
           ! Ideally ipkh=nsw/2, i.e, center of ridge profile. If ipkh=1 or nsw
           ! this feature could just be sloping terrain.  In practice it appears
           ! redundancy saves our a-- in paintridge2cube.  Here we provide some 
           ! more protection against id'ing sloping terrain as a ridge:
           !
           !      1 -- | ---       nsw/2      --- | -- nsw
           !      //////                          //////
           !           ibad_left                  ibad_rght
           !
           ! If ipkh is /// region then we flag it as "bad".  Maybe this code 
           ! should be removed altogether ... 
           !====================================================================
           ipkh    = MAXLOC( RTX ) ! index of MAX peak height in rotated topo avg cross-section
           if (nsw>=8) then
              ibad_left  = 1    
              ibad_rght  = nsw 
           else
              ibad_left  = 1
              ibad_rght  = nsw
           endif
           if( ( ipkh(1) <= ibad_left ).or.( ipkh(1) >= ibad_rght ) ) then
               npkx(L)=0.0
           end if
           
 
           rotmn  =  sum( sum( rt(ns0:ns1-1,ns0:ns1-1) , 1 ), 1) /(( ns1-ns0 )*(ns1-ns0))
           rotvar =  sum( sum( (rt(ns0:ns1-1,ns0:ns1-1)-rotmn)**2 , 1 ), 1) /(( ns1-ns0 )*(ns1-ns0))
           if (rotvar>0.) qual(L) = var/rotvar
           if (rotvar>0.) vvaa(L) = vvaa(L)*(basvar/rotvar)
           if (rotvar>0.) vva2(L) = vva2(L)*(basvar/rotvar)


           rdg_profile( : , L )  = rtx( : )
           crst_profile( : , L ) = rty( : )
           crst_silhouette( : , L ) = siluy( : )


        end do ! LOOP over angles - index=L

        iorn       = MAXLOC( vvaa )

        uniqid(ipk) = (1.0d+0) * ipk
        aniso(ipk) = qual( iorn(1) )
        anglx(ipk) = (iorn(1)-1)*(180./nang)

        mxdis(ipk) = dex( iorn(1) )

        mxvrx(ipk) = vvaa( iorn(1) ) !MAXVAL( vvaa )
        mxvry(ipk) = vva2( iorn(1) ) !MAXVAL( vvaa )
        npks(ipk)  = npkx( iorn(1) )

        pkhts(ipk) = pkht( iorn(1) )
 
        rdg_profiles(1:nsw+1,ipk)  = rdg_profile( : , iorn(1) )
        crst_profiles(1:nsw+1,ipk) = crst_profile( : , iorn(1) )
        crst_silhous(1:nsw+1,ipk)  = crst_silhouette( : , iorn(1) )


!===============================================================
!  Could be more direct and intuitive to relocate xspk and yspk,
!  and to estimate ridge width and crest length here, from saved 
!  ridge profiles in X and Y.  In fact lots of simplification 
!  could follow.
!
!  Note, we have access to the full 2D topo block in this subr
!      Deviations: SUBA(2*nsw+1,2*nsw+1)
!      Raw:        SUBARW( 2*nsw+1, 2*nsw+1)
!===============================================================

        call ridgeshift( nsw, rdg_profiles(:,ipk), crst_silhous(:,ipk), &
             anglx(ipk), xs(ipk) , ys(ipk), & 
             xspk(ipk) , yspk(ipk)  )

  xs01(ipk) =  xspk(ipk)
  ys01(ipk) =  yspk(ipk)

  deallocate( rt)
  deallocate( rtrw)
  deallocate( rtx)
  deallocate( rty)
  deallocate( cusp)
  deallocate( dertx)
  deallocate( derty)
  deallocate( rtrwx)
  deallocate( lhgts)
  deallocate( rtxslp)
  deallocate( pklc)
  deallocate( rtxmn)
  deallocate( xrt)
  deallocate( face)
  deallocate( lflats)
  deallocate( lsides)
  deallocate( silux)
  deallocate( siluy)
  deallocate( sillx)
  deallocate( silly)

!++11/1/21
  deallocate( rdg_profile )
  deallocate( crst_profile )
  deallocate( crst_silhouette )
  deallocate( rdg_profile_bk )
  deallocate( crst_silhouette_bk )

end subroutine ANISO_ANA

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine ANISO_ANA_2( SUBA,NSW,IPK )
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  INTEGER,   intent(IN)  ::  NSW,IPK
  REAL(RPX), intent(IN)  ::  SUBA(2*nsw+1,2*nsw+1)
  real(RPX) :: rt( 2*nsw+1, 2*nsw+1)
  real(RPX) :: ridge_x( 2*nsw+1 )  ,hnodes(nsw+1),pkht0,npk0
  integer   :: ns0,ns1, xnodes(nsw+1)
  logical :: lredo
  !-----------------------------------------
  ! Indices for most ridge scheme subarrays.
  ! Note ns1-ns0 = nsw+1 always. Appears 
  ! safe for any value of nsw.
  !-----------------------------------------
  ns0=nsw/2+1
  ns1=ns0+nsw+1

     call ridgescales( nsw, ipk, suba, & 
          rdg_profiles(:,ipk), &
          crst_silhous(:,ipk), &
          xnodes_list(:,ipk), &
          hnodes_list(:,ipk), & 
          nnodes_list(ipk),   &
          xwedge_list(:,ipk), &
          hwedge_list(:,ipk), &
          xspk(ipk), yspk(ipk), &
          dcenter_list(ipk) , &             
          anglx(ipk) , &
          clngth(ipk), hwdth(ipk), & 
          aniso(ipk),  mxdis(ipk), & 
          npks(ipk),   pkhts(ipk), &
          riseq(ipk),  fallq(ipk), &
          ridge_x )




          call rebuild_nodes( nsw , PSW, &
               3 , &
               xwedge_list(:,ipk), &
               hwedge_list(:,ipk), & 
               hwedge_o(:,ipk) ,  lextend_profiles=.FALSE.)

          call rebuild_nodes( nsw , PSW, &
               nnodes_list(ipk) , &
               xnodes_list(:,ipk), &
               hnodes_list(:,ipk), & 
               hnodes_o(:,ipk),  lextend_profiles=.FALSE. )

          call rebuild_nodes( nsw , PSW, &
               3 , &
               xwedge_list(:,ipk), &
               hwedge_list(:,ipk), & 
               hwedge_x(:,ipk) )

          call rebuild_nodes( nsw , PSW, &
               nnodes_list(ipk) , &
               xnodes_list(:,ipk), &
               hnodes_list(:,ipk), & 
               hnodes_x(:,ipk) )

          rdg_profiles_x(PSW+1-nsw:PSW+1+nsw,ipk) = ridge_x(1:2*nsw+1)


#ifdef DEBUGOUTPUT  
             rt  = rotbyx( suba, 2*nsw+1 , anglx(ipk) )
             rt_diag( PSW+1-nsw:PSW+1+nsw, PSW+1-nsw:PSW+1+nsw, ipk)   = rt(:,:)
             suba_diag( PSW+1-nsw:PSW+1+nsw, PSW+1-nsw:PSW+1+nsw, ipk) = suba(:,:)
#endif

end subroutine ANISO_ANA_2


subroutine thinout_list( ncube, npeaks,NSW )
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  INTEGER,   intent(IN)  ::  ncube,NSW,npeaks
  integer :: i,j,ipk,bloc,np,ipkx,nalloc,ipk0,ib0,ib1,jb0,jb1
  real(rpx), allocatable     :: anglx_sv(:),quali_sv(:)
  real(RPX)  :: max_quali
  integer, allocatable  :: ipk_sv(:)


  bloc   =  NINT( 1.*NSW/ 4. ) ! 4
  nalloc =  2*(bloc+1)*(bloc+1)

  allocate( anglx_sv( nalloc ) , quali_sv( nalloc ), ipk_sv( nalloc ) )

#if 0
  !------------------------------------------------------------
  ! This form of thinning gives good metrics and is intuitively
  ! clear, but is extremely slow when 'bloc' is small and 
  ! npeaks is large, e.g., nc=3000, co=15 ...
  !
  ! Keep it in code base for now, maybe an be sped up
  !-------------------------------------------------------------
  do np= 1,6
  do j = 1-bloc,ncube+bloc,bloc
  do i = 1-bloc,ncube+bloc,bloc
     
     quali_sv(:) = 0.
     anglx_sv(:) = 0.
     ipk_sv(:)   = 0
     ipkx        = 0

     do ipk=1,npeaks
        if ( (xspk(ipk) >= i).AND.(xspk(ipk) < i+bloc) .AND. & 
             (yspk(ipk) >= j).AND.(yspk(ipk) < j+bloc) .AND. & 
             (MyPanel(ipk) == np ) ) then
             ipkx=ipkx+1
             ipk_sv(ipkx)   = ipk
             quali_sv(ipkx) = mxdis(ipk)*aniso(ipk)
             anglx_sv(ipkx) = anglx(ipk)
        end if
     end do
 
     if (ipkx >=1 ) then
        max_quali = maxval( quali_sv(1:ipkx) )
        do ipk=1,ipkx
           if (quali_sv(ipk) < max_quali ) & 
             mxdis( ipk_sv(ipk) )= -999.0
        end do
     end if
                       
  end do
  end do
        write(*,*) "Thinning list Panel=",np," with BLOC=",bloc
  end do
#else
  !----------------------------------------------------
  ! This form of thinning seems to give marginally better
  ! metrics than other forms, or than no thinning, and is 
  ! also much faster when 'bloc' is small
  !----------------------------------------------------
  do ipk0=1,npeaks
     quali_sv(:) = 0.
     anglx_sv(:) = 0.
     ipk_sv(:)   = 0
     ipkx        = 0

     !!i  = NINT( 1.* xspk(ipk0) )
     !!j  = NINT( 1.* yspk(ipk0) )
     i  = INT( xspk(ipk0) )
     j  = INT( yspk(ipk0) )
     np = MyPanel(ipk0)

     ! ib0=i-bloc/2
     ! ib1=i+bloc/2
     ! jb0=j-bloc/2
     ! jb1=j+bloc/2


     ib0=bloc*INT(i/bloc) + 1
     ib1=ib0 + bloc
     jb0=bloc*INT(j/bloc) + 1
     jb1=jb0 + bloc

     do ipk=1,npeaks
        if ( ( INT(xspk(ipk)) >= ib0 ).AND.( INT(xspk(ipk)) <= ib1 ) .AND. & 
             ( INT(yspk(ipk)) >= jb0 ).AND.( INT(yspk(ipk)) <= jb1 ) .AND. & 
             (MyPanel(ipk) == np ) ) then
             ipkx=ipkx+1
             if (ipkx >nalloc) STOP "not enough space in thinout bloc"
             ipk_sv(ipkx)   = ipk
             quali_sv(ipkx) = mxdis(ipk)*aniso(ipk)
             anglx_sv(ipkx) = anglx(ipk)
        end if
     end do
     if (ipkx >=1 ) then
        max_quali = maxval( quali_sv(1:ipkx) )
        do ipk=1,ipkx
           if (quali_sv(ipk) < max_quali ) & 
             mxdis( ipk_sv(ipk) )= -999.0
        end do
     end if
    
       write(*,900,advance='no') achar(13) , ipk0,npeaks,bloc
    end do


900 format( a1, "  Thinning peaks ",i8," out of ",i8 , " bloc=" ,i4)
#endif

    deallocate( anglx_sv , quali_sv , ipk_sv )
 
      write(*,*) " will ZERO out negative peaks and 'Cuestas' in thinout "

      do ipk=1,npeaks
         if( (pkhts(ipk)<0.1*mxdis(ipk)).or.(npks(ipk)<1.0) ) then
           mxdis(ipk)  = -222.0
         endif
      end do
      do ipk=1,npeaks
          if( ( minval( hwedge_list(:,ipk))<0.).AND.(hwedge_list(2,ipk)>0.).AND. &
             ( minval( hwedge_list(:,ipk)) < -4*hwedge_list(2,ipk)) ) then
           mxdis(ipk)  = -333.0
         endif
      end do
     do ipk=1,npeaks
         if( hwedge_list(2,ipk)<=1. ) then
           mxdis(ipk)  = -444.0
         endif
     end do
 
end subroutine THINOUT_LIST

!====================================
   subroutine remapridge2cube(ncube,nhalo,nsw,nsmcoarse,nsmfine,lzerovalley, & 
         ldevelopment_diags,lregional_refinement,rr_factor, &
         uniqidC,uniqwgC,anisoC,anglxC,mxdisC,hwdthC,clngtC,riseqC, &
         fallqC,mxvrxC,mxvryC,nodesC,cwghtC,wedgoC )
!==========================================
! Some key inputs
!      NSW:  = 'nwindow_halfwidth' which comes from topo namelist, but should always be 
!              about SQRT(2)*coarse_smoothing_radius (in # of 3km pixels), i.e., size of
!              inscribed square for rotation etc.. Winds up in file names, e.g., 
!                      fv_0.9x1.25_nc3000_Nsw042_Nrs008_Co060_Fi001_20211102.nc
!===========================================

      use shr_kind_mod, only: r8 => shr_kind_r8
      use remap
      use reconstruct !, only : EquiangularAllAreas
      implicit none
      integer ,           intent(in) :: ncube,nhalo,nsw,nsmcoarse,nsmfine
      logical,            intent(in) :: lzerovalley
      logical,            intent(in) :: ldevelopment_diags
      REAL(KIND=dbl_kind), &
                          intent(in) :: rr_factor(ncube,ncube,6)
      LOGICAL,            intent(in) :: lregional_refinement

      real(KIND=dbl_kind), & 
           dimension(ncube*ncube*6),            INTENT(out) :: uniqidC, uniqwgC, mxdisC
      real(KIND=dbl_kind), & 
           dimension(ncube*ncube*6),            INTENT(out) :: anglxC,  anisoC,  hwdthC, clngtC
      real(KIND=dbl_kind), & 
           dimension(ncube*ncube*6),            INTENT(out) :: riseqC,  fallqC,  mxvrxC,  mxvryC
      real(KIND=dbl_kind), & 
           dimension(ncube*ncube*6),            INTENT(out) :: nodesC,  cwghtC, wedgoC
  
      REAL(RPX) ,                                                          &
         DIMENSION(1-nhalo:ncube+nhalo )                          :: xv,yv,alph,beta
      
      integer :: alloc_error

      integer :: i,ix,iy,ip,ii,counti,norx,nory,i_last,isubr,iip,j,ipk,npeaks
      integer :: nswx,nrs_junk
      real(r8):: wt
      real(KIND=dbl_kind), dimension(1-nhalo:ncube+nhalo,1-nhalo:ncube+nhalo ,6) :: tmpx6
      real(KIND=dbl_kind), dimension(ncube*ncube*6) :: wedgeC, nodosC , wedgiC
      real(KIND=dbl_kind), dimension(ncube*ncube*6) :: profiC
      real(KIND=dbl_kind), dimension(ncube*ncube*6) :: bsvarC,  blockC
      real(KIND=dbl_kind), dimension(ncube*ncube*6) :: itrgtC,  rwpksC, itrgxC
      real(KIND=dbl_kind), dimension(ncube*ncube)   :: dA    
      real(KIND=dbl_kind), dimension(ncube*ncube*6) :: tempC,repntC

      CHARACTER(len=1024) :: ofile
      character(len=8)  :: date
      character(len=10) :: time

!----------------------------------------------------------------------------------------------------

    !!allocate ( dA(ncube,ncube),stat=alloc_error )
    CALL EquiangularAllAreas(ncube, dA)

    DO i=1-nhalo,ncube+nhalo
       xv(i)=1.*i
       yv(i)=1.*i
    END DO
 
     npeaks=size(mxdis)

     itrgtC = 0.

        write(*,*) " about to call paintridge2cube "

     ! "Paint" basic ridge quanitities back onto 3km cubed-sphere
     ! using default "skeleton" approach, i.e., onto notional 
     ! ridge lines 
     !---------------------------------------------------------------
     write(*,*) " painting UNIQID "
     tmpx6 = paintridge2cube ( uniqid ,  ncube,nhalo,nsw,lzerovalley )
     uniqidC = reshape( tmpx6(1:ncube, 1:ncube, 1:6 ) , (/ncube*ncube*6/) )

     write(*,*) " painting MXDIS "
     tmpx6 = paintridge2cube ( mxdis ,  ncube,nhalo,nsw,lzerovalley )
     mxdisC = reshape( tmpx6(1:ncube, 1:ncube, 1:6 ) , (/ncube*ncube*6/) )

     write(*,*) " painting HWDTH "
     tmpx6 = paintridge2cube ( hwdth ,  ncube,nhalo,nsw,lzerovalley )
     hwdthC = reshape( tmpx6(1:ncube, 1:ncube, 1:6 ) , (/ncube*ncube*6/) )

     write(*,*) " painting CLNGT "
     tmpx6 = paintridge2cube ( clngth ,  ncube,nhalo,nsw,lzerovalley )
     clngtC = reshape( tmpx6(1:ncube, 1:ncube, 1:6 ) , (/ncube*ncube*6/) )

     write(*,*) " painting ANGLX "
     tmpx6 = paintridge2cube ( anglx ,  ncube,nhalo,nsw,lzerovalley )
     anglxC = reshape( tmpx6(1:ncube, 1:ncube, 1:6 ) , (/ncube*ncube*6/) )

     write(*,*) " painting ANISO "
     tmpx6 = paintridge2cube ( aniso ,  ncube,nhalo,nsw,lzerovalley )
     anisoC = reshape( tmpx6(1:ncube, 1:ncube, 1:6 ) , (/ncube*ncube*6/) )

     write(*,*) " painting MXVRX "
     tmpx6 = paintridge2cube ( mxvrx ,  ncube,nhalo,nsw,lzerovalley )
     mxvrxC = reshape( tmpx6(1:ncube, 1:ncube, 1:6 ) , (/ncube*ncube*6/) )

     write(*,*) " painting MXVRY "
     tmpx6 = paintridge2cube ( mxvry ,  ncube,nhalo,nsw,lzerovalley )
     mxvryC = reshape( tmpx6(1:ncube, 1:ncube, 1:6 ) , (/ncube*ncube*6/) )

        ! The 'crest_weight' option simply forces paintridge2cube to paint 
        ! a value of 1 along the ridge.
     write(*,*) " painting CWGHT "
     tmpx6 = paintridge2cube ( clngth ,  ncube,nhalo,nsw,lzerovalley, crest_weight=.true. )
     cwghtC = reshape( tmpx6(1:ncube, 1:ncube, 1:6 ) , (/ncube*ncube*6/) )

     write(*,*) " painting FALLQ "
     tmpx6 = paintridge2cube ( fallq ,  ncube,nhalo,nsw,lzerovalley )
     fallqC = reshape( tmpx6(1:ncube, 1:ncube, 1:6 ) , (/ncube*ncube*6/) )

     write(*,*) " painting RISEQ "
     tmpx6 = paintridge2cube ( riseq ,  ncube,nhalo,nsw,lzerovalley )
     riseqC = reshape( tmpx6(1:ncube, 1:ncube, 1:6 ) , (/ncube*ncube*6/) )

     write(*,*) " painting WEDGI "
     tmpx6 = paintridge2cube ( hwedge_list(2,:) ,  ncube,nhalo,nsw,lzerovalley )
     wedgiC = reshape( tmpx6(1:ncube, 1:ncube, 1:6 ) , (/ncube*ncube*6/) )


     !----------------------------------------------
     ! New approach to add volume ...
     !----------------------------------------------
     write(*,*) " fleshing out BLOCK "
     tmpx6  = fleshout_block ( ncube,nhalo,nsw,mxdisC,hwdthC,anglxC, rr_factor )
     blockC = reshape( tmpx6(1:ncube, 1:ncube, 1:6 ) , (/ncube*ncube*6/) )

     write(*,*) " fleshing out PROFI "
     tmpx6  = fleshout_profi ( ncube,nhalo,PSW,mxdisC,anglxC,uniqidC, rr_factor, rdg_profiles_x )
     profiC = reshape( tmpx6(1:ncube, 1:ncube, 1:6 ) , (/ncube*ncube*6/) )

     write(*,*) " fleshing out NODES "
     tmpx6  = fleshout_profi ( ncube,nhalo,PSW,mxdisC,anglxC,uniqidC, rr_factor, hnodes_x )
     nodesC = reshape( tmpx6(1:ncube, 1:ncube, 1:6 ) , (/ncube*ncube*6/) )

     write(*,*) " fleshing out WEDGES "
     tmpx6  = fleshout_profi ( ncube,nhalo,PSW,mxdisC,anglxC,uniqidC, rr_factor, hwedge_x )
     wedgeC = reshape( tmpx6(1:ncube, 1:ncube, 1:6 ) , (/ncube*ncube*6/) )

     write(*,*) " fleshing out NODOS "
     tmpx6  = fleshout_profi ( ncube,nhalo,PSW,mxdisC,anglxC,uniqidC, rr_factor, hnodes_o )
     nodosC = reshape( tmpx6(1:ncube, 1:ncube, 1:6 ) , (/ncube*ncube*6/) )

     write(*,*) " fleshing out WEDGO "
     tmpx6  = fleshout_profi ( ncube,nhalo,PSW,mxdisC,anglxC,uniqidC, rr_factor, hwedge_o )
     wedgoC = reshape( tmpx6(1:ncube, 1:ncube, 1:6 ) , (/ncube*ncube*6/) )

     write(*,*) " Painting IDs on top of WEDGO "
     tmpx6  = color_on_profi ( ncube,nhalo,PSW,mxdisC,anglxC,uniqidC, rr_factor, hwedge_o, uniqid )
     uniqwgC = reshape( tmpx6(1:ncube, 1:ncube, 1:6 ) , (/ncube*ncube*6/) )
                   
    i_last = -9999

    ! call repaint( ncube, mxdisC, uniqidC, repntC )
    repntC = mxdisC * 0._r8
    
! Previous calculation of wghts_target was bad because "flat" areas had ANGLX=0.
! Fix by setting ANGLX to "bad" value in "flats" before remap
     where( mxdisC < 0.1 )
       anglxC = -999.
     end where


     if (ldevelopment_diags) then
       nrs_junk=0
       call DATE_AND_TIME( DATE=date,TIME=time)
       
       write( ofile , &
            "('./output/remap_nc',i0.4, '_Nsw',i0.3,'_Nrs',i0.3  &
            '_Co',i0.3,'_Fi',i0.3)" ) & 
            ncube, nsw, nrs_junk, nsmcoarse, nsmfine
       ofile= trim(ofile)//'_vX_'//date//'_'//time(1:4)//'.dat'
       
       OPEN (unit = 911, file= trim(ofile) ,form="UNFORMATTED" )
       
       write(911) ncube,npeaks
       write(911) uniqidC
       write(911) anisoC
       write(911) anglxC

       write(911) mxdisC
       write(911) hwdthC
       write(911) clngtC

       write(911) blockC
       write(911) profiC

       write(911) nodesC
       write(911) wedgeC

       write(911) nodosC
       write(911) wedgoC


       write(911) xs,ys,xspk,yspk,peaks%i,peaks%j
              
       write(911) riseqC
       write(911) fallqC
       write(911) uniqwgC
       write(911) wedgiC
       write(911) repntC

       close(911)
              
       write(*,*) " GOT OUT OF remapridge2cube "
     end if
       
  end subroutine remapridge2cube
     
!====================================
   subroutine remapridge2target(area_target,target_center_lon,target_center_lat,  &
         weights_eul_index_all,weights_lgr_index_all,weights_all,ncube,jall, &
         nreconstruction,ntarget, & 
         output_grid,ldevelopment_diags,terr_dev, &
         uniqidC,uniqwgC,anisoC,anglxC,mxdisC,hwdthC,clngtC, & 
         riseqC,fallqC,mxvrxC,mxvryC,nodesC,cwghtC, &
         itrgtC )
!==========================================
! Some key inputs
!      NSW:  = 'nwindow_halfwidth' which comes from topo namelist, but should always be 
!              about SQRT(2)*coarse_smoothing_radius (in # of 3km pixels), i.e., size of
!              inscribed square for rotation etc.. Winds up in file names, e.g., 
!                      fv_0.9x1.25_nc3000_Nsw042_Nrs008_Co060_Fi001_20211102.nc
!===========================================

      use shr_kind_mod, only: r8 => shr_kind_r8
      use remap
      use reconstruct !, only : EquiangularAllAreas
      implicit none
      real(r8),           intent(in) :: weights_all(jall,nreconstruction)
      integer ,           intent(in) :: weights_eul_index_all(jall,3),weights_lgr_index_all(jall)
      integer ,           intent(in) :: ncube,jall,nreconstruction,ntarget
      real(r8),           intent(in) :: area_target(ntarget),target_center_lon(ntarget),target_center_lat(ntarget)
      character(len=1024),intent(in) :: output_grid
      logical,            intent(in) :: ldevelopment_diags

      integer,dimension(ncube*ncube*6),intent(out)  :: itrgtC

      REAL(KIND=dbl_kind), &
            DIMENSION(ncube*ncube*6),           INTENT(IN)  :: terr_dev
      real(KIND=dbl_kind), & 
           dimension(ncube*ncube*6),            INTENT(IN)  :: uniqidC, uniqwgC, mxdisC
      real(KIND=dbl_kind), & 
           dimension(ncube*ncube*6),            INTENT(IN)  :: anglxC,  anisoC,  hwdthC, clngtC
      real(KIND=dbl_kind), & 
           dimension(ncube*ncube*6),            INTENT(IN)  :: riseqC,  fallqC,  mxvrxC,  mxvryC
      real(KIND=dbl_kind), & 
           dimension(ncube*ncube*6),            INTENT(IN)  :: nodesC,  cwghtC

      real(r8):: f(ntarget)
        
      integer :: alloc_error

      integer :: i,ix,iy,ip,ii,counti,norx,nory,i_last,isubr,iip,j,ipk,npeaks
      integer :: nswx,nrs_junk
      real(r8):: wt
      !!real(KIND=dbl_kind), dimension(ncube*ncube*6) :: itrgtC, itrgxC
      integer,            dimension(ncube*ncube*6) :: itrgxC
      real(KIND=dbl_kind), dimension(ncube*ncube)   :: dA    
      real(KIND=dbl_kind), dimension(ncube*ncube*6) :: tempC

      CHARACTER(len=1024) :: ofile
      character(len=8)  :: date
      character(len=10) :: time

!----------------------------------------------------------------------------------------------------

    !!allocate ( dA(ncube,ncube),stat=alloc_error )
    CALL EquiangularAllAreas(ncube, dA)

    allocate (wghts_target(ntarget,nsubr),stat=alloc_error )
    if( alloc_error /= 0 ) then; print*,'Program could not allocate space for wghts_target'; stop; endif
    wghts_target = 0.
    allocate (mxdis_target(ntarget,nsubr),stat=alloc_error )
    if( alloc_error /= 0 ) then; print*,'Program could not allocate space for mxdis_target'; stop; endif
    mxdis_target = 0.
    allocate (anglx_target(ntarget,nsubr),stat=alloc_error )
    if( alloc_error /= 0 ) then; print*,'Program could not allocate space for anglx_target'; stop; endif
    anglx_target = 0.
    allocate (aniso_target(ntarget,nsubr),stat=alloc_error )
    if( alloc_error /= 0 ) then; print*,'Program could not allocate space for aniso_target'; stop; endif
    aniso_target = 0.
    allocate (anixy_target(ntarget,nsubr),stat=alloc_error )
    if( alloc_error /= 0 ) then; print*,'Program could not allocate space for anixy_target'; stop; endif
    anixy_target = 0.
    allocate (mxvrx_target(ntarget,nsubr),stat=alloc_error )
    if( alloc_error /= 0 ) then; print*,'Program could not allocate space for mxvrx_target'; stop; endif
    mxvrx_target = 0.
    allocate (mxvry_target(ntarget,nsubr),stat=alloc_error )
    if( alloc_error /= 0 ) then; print*,'Program could not allocate space for mxvry_target'; stop; endif
    mxvry_target = 0.
    allocate (hwdth_target(ntarget,nsubr),stat=alloc_error )
    if( alloc_error /= 0 ) then; print*,'Program could not allocate space for hwdth_target'; stop; endif
    hwdth_target = 0.
    allocate (clngt_target(ntarget,nsubr),stat=alloc_error )
    if( alloc_error /= 0 ) then; print*,'Program could not allocate space for clngt_target'; stop; endif
    clngt_target = 0.
    allocate (cwght_target(ntarget,nsubr),stat=alloc_error )
    if( alloc_error /= 0 ) then; print*,'Program could not allocate space for cwght_target'; stop; endif
    cwght_target = 0.
    allocate (count_target(ntarget,nsubr),stat=alloc_error )
    if( alloc_error /= 0 ) then; print*,'Program could not allocate space for count_target'; stop; endif
    count_target = 0.
    allocate (riseq_target(ntarget,nsubr),stat=alloc_error )
    if( alloc_error /= 0 ) then; print*,'Program could not allocate space for riseq_target'; stop; endif
    riseq_target = 0.
    allocate (fallq_target(ntarget,nsubr),stat=alloc_error )
    if( alloc_error /= 0 ) then; print*,'Program could not allocate space for fallq_target'; stop; endif
    fallq_target = 0.
    allocate (bsvar_target(ntarget,nsubr),stat=alloc_error )
    if( alloc_error /= 0 ) then; print*,'Program could not allocate space for bsvar_target'; stop; endif
    bsvar_target = 0.
    allocate (isovar_target(ntarget),stat=alloc_error )
    if( alloc_error /= 0 ) then; print*,'Program could not allocate space for isovar_target'; stop; endif
    isovar_target = 0.

    itrgtC = 0.
    i_last = -9999
    itrgxC = -1


! 
!      In the following loop "counti" is the index of a piece of 
!      the "exchange grid" - created by cutting the cubed-sphere topo
!      and target grid into each other.  
    do counti=1,jall
     
      i    = weights_lgr_index_all(counti)

      ix  = weights_eul_index_all(counti,1)
      iy  = weights_eul_index_all(counti,2)
      ip  = weights_eul_index_all(counti,3)
      !
      ! convert to 1D indexing of cubed-sphere
      !
      ii = (ip-1)*ncube*ncube+(iy-1)*ncube+ix
      wt = weights_all(counti,1)

      iip=(iy-1)*ncube+ix

      itrgtC(ii) = i
      if (mxdisC(ii) > 0.1)  itrgxC(ii) = i
      isubr = INT( anglxC(ii) * nsubr/180. ) + 1
      if ( (isubr >= 1).and.(isubr <= nsubr) ) then
      wghts_target( i , isubr ) = wghts_target( i , isubr ) + wt
      hwdth_target( i , isubr ) = hwdth_target( i , isubr ) + wt*hwdthC(ii)
      mxvrx_target( i , isubr ) = mxvrx_target( i , isubr ) + wt*mxvrxC(ii)
      mxvry_target( i , isubr ) = mxvry_target( i , isubr ) + wt*mxvryC(ii)
      mxdis_target( i , isubr ) = mxdis_target( i , isubr ) + wt*mxdisC(ii)
      aniso_target( i , isubr ) = aniso_target( i , isubr ) + wt*anisoC(ii)
      anglx_target( i , isubr ) = anglx_target( i , isubr ) + wt*anglxC(ii)
      clngt_target( i , isubr ) = clngt_target( i , isubr ) + wt*cwghtC(ii)/dA(iip)
      cwght_target( i , isubr ) = cwght_target( i , isubr ) + wt*cwghtC(ii) 
      count_target( i , isubr ) = count_target( i , isubr ) + wt/dA(iip)
      fallq_target( i , isubr ) = fallq_target( i , isubr ) + wt*fallqC(ii)
      riseq_target( i , isubr ) = riseq_target( i , isubr ) + wt*riseqC(ii)
      endif

      i_last = i
    end do       

    ! change width (and length) to km
    hwdth_target = hwdth_target * grid_length_scale
    clngt_target = clngt_target * grid_length_scale


     !==========================================
     ! Note: Weighting by CWGHT saved your 
     ! ass in the presence of the WGHTS bug.
     !==========================================
     where( wghts_target > 1.e-15 )
        mxdis_target = mxdis_target / wghts_target
        fallq_target = fallq_target / wghts_target
        riseq_target = riseq_target / wghts_target
        aniso_target = aniso_target / wghts_target
        anglx_target = anglx_target / wghts_target
        hwdth_target = hwdth_target / wghts_target
        mxvrx_target = mxvrx_target / wghts_target
        mxvry_target = mxvry_target / wghts_target
     elsewhere      
        clngt_target = 0.
        mxdis_target = 0.
        fallq_target = 0.
        riseq_target = 0.
        aniso_target = 0.
        anglx_target = -9000.
        hwdth_target = 0.
        mxvrx_target = 0.
        mxvry_target = 0.
     end where

     do isubr=1,nsubr
     do i=1,ntarget 
        if (anglx_target(i,isubr) > -9000. ) &
        clngt_target(i,isubr) = clngt_target(i,isubr)*length_in_square( anglx_target(i,isubr) )
     end do
     end do

     anixy_target = mxvrx_target /( mxvrx_target + mxvry_target + 0.0001 )
      
     call importancesort (ntarget)

     call latlonangles (target_center_lon,target_center_lat,ntarget)

     tempC = nodesC
     where( (terr_dev < 0.) .AND. (abs(nodesC)<1.0) ) tempC = terr_dev
!--------------------------------------------------------------------------
!      In the following loop "counti" is the index of a piece of 
!      the "exchange grid" - created by cutting the cubed-sphere topo
!      and target grid into each other.  
    do counti=1,jall
       i    = weights_lgr_index_all(counti)

       ix  = weights_eul_index_all(counti,1)
       iy  = weights_eul_index_all(counti,2)
       ip  = weights_eul_index_all(counti,3)
       !
       ! convert to 1D indexing of cubed-sphere
       !
       ii = (ip-1)*ncube*ncube+(iy-1)*ncube+ix
       wt = weights_all(counti,1)
       isovar_target( i ) = isovar_target( i ) + wt*( (tempC(ii)-terr_dev(ii))**2 )/area_target(i)
    end do
    isovar_target = SQRT( isovar_target )

      write(*,*) " remap--target "
      write(*,*) "Min Max ITRGT  " , minval(itrgtC),maxval(itrgtC)

    if (ldevelopment_diags) then
       write( ofile , &
            "('./output/grid_remap_nc',i0.4 )" ) ncube
       ofile= trim(ofile)//'_'//trim(output_grid)//'.dat'
       
       OPEN (unit = 911, file= trim(ofile) ,form="UNFORMATTED" )
       write(911) ncube,npeaks
       write(911) dble(itrgtC)
       write(911) dble(itrgxC)
       write(911) xs,ys,xspk,yspk,peaks%i,peaks%j
       close(911)
       
       
       write(*,*) " GOT OUT OF remapridge2target "
     end if

       
  end subroutine remapridge2target
   

!====================================
   subroutine remapridge2tiles ( ntarget,ncube,jall,nreconstruction,     &
              area_target,target_center_lon,target_center_lat,         &
              weights_eul_index_all,weights_lgr_index_all,weights_all, &
              uniqidC,uniqwgC,itrgtC,wedgoC )

      use shr_kind_mod, only: r8 => shr_kind_r8
      use remap
      use reconstruct

      implicit none
      real(r8),           intent(in) :: weights_all(jall,nreconstruction)
      integer ,           intent(in) :: weights_eul_index_all(jall,3),weights_lgr_index_all(jall)
      integer ,           intent(in) :: ncube,jall,nreconstruction,ntarget
      real(r8),           intent(in) :: area_target(ntarget),target_center_lon(ntarget),target_center_lat(ntarget)
      !character(len=1024),intent(in) :: output_grid
      !logical,            intent(in) :: ldevelopment_diags

      real(KIND=dbl_kind), & 
           dimension(ncube*ncube*6), INTENT(IN)  :: uniqidC, uniqwgC, wedgoC
      integer, & 
           dimension(ncube*ncube*6), INTENT(IN)  :: itrgtC

      real(r8):: f(ntarget)
        
      integer :: alloc_error
      integer :: npack,NobMin,NobMax,iir,iic,maxtiles,npeaks
      integer :: i,ix,iy,ip,ii,counti,norx,nory,i_last,isubr,iip,j,ipk,ir
      integer :: nswx,nrs_junk,ig,nalloc,n,ird,ThisRidge,k,nf1,nf2,IdxMin,IdxMax
      real(r8):: wt,wght
      integer,             dimension(ncube*ncube*6) :: xcoord,ycoord,pcoord
      real(KIND=dbl_kind), dimension(ncube*ncube)   :: dA     
      real(KIND=dbl_kind), dimension(ncube,ncube,6) :: tempC
      integer,             dimension(ntarget)       :: ncells

      integer, allocatable, dimension(:)             :: gxv,gyv,gyp,gid,idcoun,NumRidges,NumCrests,idx1,idx2
      integer, allocatable, dimension(:)             :: NumObjects,idxU,IdxOc,IdxPack,IdxPackRdg,IdxPackCst
      integer, allocatable, dimension(:,:)           :: idxmap,MyRidges,MyCrests,MyObject,panel_tiles
      real(r8),allocatable, dimension(:,:)           :: WtRidges,LnCrests,WtObject,LnObject
      integer, allocatable, dimension(:)             :: xpack,ypack
      real(r8),allocatable, dimension(:)             :: hpack
      real(r8),allocatable, dimension(:,:)           :: xlext_tiles,ylext_tiles,clext_tiles
      real(r8),allocatable, dimension(:,:)           :: xwoid_tiles,ywoid_tiles,xloid_tiles,yloid_tiles
      real(r8),allocatable, dimension(:,:)           :: lonc_tiles,latc_tiles
      real(r8),allocatable, dimension(:,:)           :: lonw_tiles,latw_tiles

      integer, allocatable, dimension(:)             :: uniqwgMap,uniqidMap,xcoordMap,ycoordMap
      real(r8),allocatable, dimension(:)             :: wedgoMap

      real(r8) :: alph,beta,lono,lato,xobj,yobj
      integer  :: pobj       

      CHARACTER(len=1024) :: ofile
      character(len=8)  :: date
      character(len=10) :: time

      CALL EquiangularAllAreas(ncube, dA)
      write(*,*) "Max target grid area   ",maxval( area_target )
      write(*,*) "Min cubetopo grid area ",maxval( dA )
      write(*,*) "                     Ratio: ",  maxval( area_target ) / minval( dA )
      nalloc = 5*NINT(  maxval( area_target ) / minval( dA ) )

      write(*,*) " remap--xxx "
      write(*,*) " nalloc     ",nalloc
      write(*,*) "Min Max ITRGT  " , minval(itrgtC),maxval(itrgtC)
      write(*,*)

      allocate( WtRidges(ntarget, nalloc) )
      allocate( MyRidges(ntarget, nalloc) )
      allocate( LnCrests(ntarget, nalloc) )
      allocate( MyCrests(ntarget, nalloc) )
      allocate( NumRidges(ntarget) )
      allocate( NumCrests(ntarget) )

      allocate( NumObjects(ntarget) )
      allocate( MyObject(ntarget, nalloc) )
      allocate( WtObject(ntarget, nalloc) , LnObject(ntarget, nalloc)  )

      allocate( idxmap(nalloc, ntarget) )
      allocate( idcoun(ntarget) )
      allocate( idx1(nalloc) , idx2(nalloc) )
      allocate( IdxOc(nalloc) )
      idxmap = -1      
      idcoun = 0   
      NumRidges = 0
      MyRidges  = 0
      WtRidges  = 0._r8
      NumCrests = 0
      MyCrests  = 0
      LnCrests  = 0._r8

      do counti=1,jall
         i   = weights_lgr_index_all(counti)
         ix  = weights_eul_index_all(counti,1)
         iy  = weights_eul_index_all(counti,2)
         ip  = weights_eul_index_all(counti,3)
         !
         ! convert to 1D indexing of cubed-sphere
         !
         ii = (ip-1)*ncube*ncube+(iy-1)*ncube+ix
         if (itrgtC(ii)==i) then
            idcoun(i)   = idcoun(i)+1
            idxmap(idcoun(i),i)= ii
         end if
      write(*,900,advance='no') achar(13) , counti, jall
      end do
      write(*,*)" "
      !--------------------------------------------
      ! idxmap stores the 1D index for cube arrays 
      ! in each target cell: idxmap ( [cube] , [target] ). 
      ! idcoun stores the number of cube cells in each
      ! target cell
      !--------------------------------------------

      do j=1,ntarget
         ird=0
         IdxOc(:) = 0.
         do i=1,idcoun(j)
            IdxOc(i) = uniqwgC( idxmap(i,j) )
         end do
         IdxPack = pack( IdxOc, (IdxOc>0) )
         npack   = size( IdxPack )
         if (npack > 0 ) then
            IdxMin  = minval(IdxPack)
            IdxMax  = maxval(IdxPack)
            do ii   = IdxMin,IdxMax
               wght  = count( IdxPack == ii )
               if (wght > 0) then 
                  ird=ird+1
                  MyRidges(j,ird) =  ii
                  WtRidges(j,ird) =  wght
               end if
            end do
         end if
         NumRidges(j)=ird
         !deallocate( IdxPack )
         write(*,901,advance='no') achar(13), 1 ,j,ntarget,ird
      end do
      write(*,*)" "
 
      ! Crests
      do j=1,ntarget
         ird=0
         IdxOc(:) = 0.
         do i=1,idcoun(j)
            IdxOc(i) = uniqidC( idxmap(i,j) )
         end do
         IdxPack = pack( IdxOc, (IdxOc>0) )
         npack   = size( IdxPack )
         if (npack > 0 ) then
            IdxMin  = minval(IdxPack)
            IdxMax  = maxval(IdxPack)
            do ii   = IdxMin,IdxMax
               wght  = count( IdxPack == ii )
               if (wght > 0) then 
                  ird=ird+1
                  MyCrests(j,ird) =  ii
                  LnCrests(j,ird) =  wght
               end if
            end do
         end if
         NumCrests(j)=ird
         !deallocate( IdxPack )
         write(*,901,advance='no') achar(13), 2 ,j,ntarget,ird
      end do
      write(*,*) ' '

      ! Merge to general objects
      do i=1,ntarget
         IdxOc = MyRidges(i,: )
         IdxPackRdg  = pack( IdxOc, (IdxOc>0) )
         IdxOc = MyCrests(i,: )
         IdxPackCst = pack( IdxOc, (IdxOc>0) )
         NobMin   = min( minval(IdxPackRdg),minval(IdxPackCst) )
         NobMax   = max( maxval(IdxPackRdg),maxval(IdxPackCst) )
         ird = 0          

         do j=NobMin,NobMax
            if ( (any(IdxPackRdg==j)) .and. (any(IdxPackCst==j)) ) then ! both ridge and crest
               ird = ird+1 
               MyObject(i,ird)=j   
               do ii=1,numridges(i)
                  if (MyRidges(i,ii)==j) iir=ii
               end do
               do ii=1,numcrests(i)
                  if (MyCrests(i,ii)==j) iic=ii
               end do
               WtObject(i,ird) = WtRidges(i,iir)
               LnObject(i,ird) = LnCrests(i,iic)
            end if
            if ( (any(IdxPackRdg==j)) .and. .not.(any(IdxPackCst==j)) ) then ! just ridge NOT crest
               ird = ird+1 
               MyObject(i,ird)=j   
               do ii=1,numridges(i)
                  if (MyRidges(i,ii)==j) iir=ii
               end do
               WtObject(i,ird) = WtRidges(i,iir)
               LnObject(i,ird) = 0._r8
            end if
            if ( .not.(any(IdxPackRdg==j)) .and. (any(IdxPackCst==j)) ) then ! just crest NOT ridge. Shouldn't happen much, ...
               ird = ird+1 
               MyObject(i,ird)=j   
               do ii=1,numcrests(i)
                  if (MyCrests(i,ii)==j) iic=ii
               end do
               WtObject(i,ird) = 0._r8
               LnObject(i,ird) = LnCrests(i,iic)
            end if
         end do
         NumObjects(i)=ird
         !deallocate( IdxPack )
         write(*,901,advance='no') achar(13), 3 ,i,ntarget,ird
      end do
      maxtiles = maxval( NumObjects )


!------------------------------------------------
! AT this you have 4 arrays on the target grid
!
! NumObjects( ntarget ) - the number of objects
!    in each target cell
! MyObject( ntarget ,: ) - the unique ID of each
!    object derived in remapridge2cube
! WtObject( ntarget, : ) - the number of topo cells
!    associated with the 3pt 'WEDGE' of each object
! LnObject( ntarget, : ) - the number of topo cells
!    associated with the 'crest' of each object
!
! Note the 2D arrays only really need to go to 
! maxtiles in the 2nd dimension
!--------------------------------------------------
 
      do ip=1,6
      do iy=1,ncube
      do ix=1,ncube
         ! convert to 1D indexing of cubed-sphere
         !---------------------------------------
         ii = (ip-1)*ncube*ncube+(iy-1)*ncube+ix
         xcoord(ii) = 1.*ix
         ycoord(ii) = 1.*iy
         pcoord(ii) = 1.*ip
      end do
      end do
      end do
  
    
      write(*,*) " "
      allocate( xwoid_tiles(ntarget,maxtiles ) )
      allocate( ywoid_tiles(ntarget,maxtiles ) )
      xwoid_tiles(:,:)=-9999._r8  
      ywoid_tiles(:,:)=-9999._r8
      allocate( xloid_tiles(ntarget,maxtiles ) )
      allocate( yloid_tiles(ntarget,maxtiles ) )
      xloid_tiles(:,:)=-9999._r8  
      yloid_tiles(:,:)=-9999._r8
      allocate( panel_tiles(ntarget,maxtiles ) )
      panel_tiles(:,:)=-1
      allocate( clext_tiles(ntarget,maxtiles ) )
      clext_tiles(:,:)=0._r8

      allocate( uniqwgMap(nalloc),uniqidMap(nalloc),xcoordMap(nalloc),ycoordMap(nalloc), &
                wedgoMap(nalloc)  )

      do j=1,ntarget
         n=NumObjects(j)
         uniqwgMap(:) = -1 
         uniqidMap(:) = -1
         xcoordMap(:) = -1
         ycoordMap(:) = -1
         wedgoMap(:)  = -1._r8
         do i=1,idcoun(j)
            uniqwgMap(i) = uniqwgC( idxmap(i,j) )
            uniqidMap(i) = uniqidC( idxmap(i,j) )
            xcoordMap(i) = xcoord ( idxmap(i,j) )
            ycoordMap(i) = ycoord ( idxmap(i,j) )
            wedgoMap(i)  = wedgoC ( idxmap(i,j) )
         end do
         do ir=1,n
            ThisRidge = MyObject(j,ir)  !
            xpack = pack( xcoordMap, (uniqwgMap == ThisRidge) ) 
            ypack = pack( ycoordMap, (uniqwgMap == ThisRidge) )
            hpack = pack( wedgoMap,  (uniqwgMap == ThisRidge) )
            npack = size(xpack)
            if (npack > 0) then 
               xwoid_tiles(j,ir) = sum( xpack *hpack )/ sum(hpack) !npack
               ywoid_tiles(j,ir) = sum( ypack *hpack )/ sum(hpack) !npack
            end if
            xpack = pack( xcoordMap, (uniqidMap == ThisRidge) )
            ypack = pack( ycoordMap, (uniqidMap == ThisRidge) )
            npack = size(xpack)
            if (npack > 0) then 
               xloid_tiles(j,ir) = sum( xpack )/ npack
               yloid_tiles(j,ir) = sum( ypack )/ npack
               clext_tiles(j,ir) = sqrt( 1.*(maxval(xpack)-minval(xpack))**2 &
                                       + 1.*(maxval(ypack)-minval(ypack))**2 )
            end if
          end do
          write(*,902,advance='no') achar(13), j,ntarget
       end do

      allocate( mxdis_tiles(ntarget,maxtiles ), aniso_tiles(ntarget,maxtiles ), & 
                anglx_tiles(ntarget,maxtiles ), hwdth_tiles(ntarget,maxtiles ), &  
                clngt_tiles(ntarget,maxtiles ), lonc_tiles(ntarget,maxtiles ) , &
                latc_tiles(ntarget,maxtiles ) , lonw_tiles(ntarget,maxtiles ) , &
                latw_tiles(ntarget,maxtiles ) )

      latc_tiles(:,:)  =-9999._r8 
      lonc_tiles(:,:)  =-9999._r8  
      latw_tiles(:,:)  =-9999._r8 
      lonw_tiles(:,:)  =-9999._r8  
      aniso_tiles(:,:) =-9999._r8  
      anglx_tiles(:,:) =-9999._r8  
      mxdis_tiles(:,:) =-9999._r8  
      hwdth_tiles(:,:) =-9999._r8  
      clngt_tiles(:,:) =-9999._r8  

      write(*,*) ' '


      do i=1,ntarget
         n=NumObjects(i)
         do ir=1,n
            ThisRidge = MyObject(i,ir)
            write(*,903,advance='no') achar(13), i,ir,ThisRidge
            panel_tiles(i,ir) = MyPanel(ThisRidge)
            mxdis_tiles(i,ir) = mxdis(ThisRidge)
            aniso_tiles(i,ir) = aniso(ThisRidge)
            anglx_tiles(i,ir) = anglx(ThisRidge)
            hwdth_tiles(i,ir) = hwdth(ThisRidge)
            clngt_tiles(i,ir) = clngth(ThisRidge)
            clngt2(ThisRidge) = clngt2(ThisRidge)+clext_tiles(i,ir)
         end do
         !write(*,903,advance='no') achar(13), i,ntarget
      end do
#if 1     
       do j=1,ntarget
          n=NumObjects(j)
          do ir=1,n
             xobj = xloid_tiles(j,ir)
             yobj = yloid_tiles(j,ir)
             pobj = panel_tiles(j,ir)
             if ((xobj>-999.).AND.(yobj>-999.).AND.(pobj>-1)) then
                alph =  ( xobj - 0.5 - ncube/2 )*(pi/2.)/ncube
                beta =  ( yobj - 0.5 - ncube/2 )*(pi/2.)/ncube
                call CubedSphereRLLFromABP(alph, beta, pobj, lono, lato)
                lonc_tiles(j,ir) = lono
                latc_tiles(j,ir) = lato
             end if
             xobj = xwoid_tiles(j,ir)
             yobj = ywoid_tiles(j,ir)
             pobj = panel_tiles(j,ir)
             if ((xobj>-999.).AND.(yobj>-999.).AND.(pobj>-1)) then
                alph =  ( xobj - 0.5 - ncube/2 )*(pi/2.)/ncube
                beta =  ( yobj - 0.5 - ncube/2 )*(pi/2.)/ncube
                call CubedSphereRLLFromABP(alph, beta, pobj, lono, lato)
                lonw_tiles(j,ir) = lono
                latw_tiles(j,ir) = lato
             end if
          end do
       end do
#endif

! Logging ....
      write(*,*)" "
      write(*,*)"  max (NumObjects) ",maxtiles
      write(*,*) " writing IDXMAP to file "
900   format( a1, "  Remapping -2- Tiles 01 ",i8," out of ",i8  )
901   format( a1, "  Remapping -2- Tiles Phase ",i2," ",i6," out of ",i8 ," # ridges ",i6 )
902   format( a1, "  More remapping stuff ",i8," out of ",i8  )
903   format( a1, "  Matching ID's stuff; i= ",i6," ir= ",i6," uniqid=",i8  )
!903   format( a1, "  Matching ID's stuff ",i8," out of ",i8  )
!902   format( a1, "  Doing more stuff; i=",i6," ir=",i6," npack=",i6  )
! -------------------------------------

      npeaks = size(clngt2 )

      ofile="output/Ridge_tile_map.dat"
      OPEN (unit = 911, file= trim(ofile) ,form="UNFORMATTED" )
      write(911) ntarget,nalloc,maxtiles,npeaks
      write(911) idxmap,idcoun
      write(911) NumObjects
      write(911) MyObject(:,1:maxtiles )
      write(911) WtObject(:,1:maxtiles )
      write(911) LnObject(:,1:maxtiles )
      write(911) xwoid_tiles
      write(911) ywoid_tiles
      write(911) xloid_tiles
      write(911) yloid_tiles
      write(911) panel_tiles
      write(911) lonc_tiles
      write(911) latc_tiles
      write(911) lonw_tiles
      write(911) latw_tiles
      write(911) aniso_tiles
      write(911) anglx_tiles
      write(911) mxdis_tiles
      write(911) hwdth_tiles
      write(911) clngt_tiles
      write(911) clext_tiles
      write(911) clngt2
              close(911)


      deallocate(idxmap,idcoun,MyRidges,WtRidges,NumRidges)
            write(*,*) " GOT OUT OF remapridge2tiles "

  end subroutine remapridge2tiles
!================================================================
  subroutine latlonangles (target_center_lon,target_center_lat,ntarget)
!-----------------------------------

      real(r8), intent(in) :: target_center_lon(ntarget),target_center_lat(ntarget)
      integer,  intent(in) :: ntarget

  real(r8) :: lat22,lon22,a22,b22,dx2,dy2,cosll
  real(r8) :: lat22s,lon22s,a22s,b22s
  real(r8) :: To_Radians

  integer  :: i,j,isubr,ipanel22
  integer  :: alloc_error

    if ( maxval( abs( target_center_lat )) > 2.0 ) then
       ! Center coords are in DEGREES. Rescale.
       To_Radians = PI/180.
    else
       ! Center coords are in RADIANS. Leave alone.
       To_Radians=  1.
    endif

    allocate (ang22_target(ntarget,nsubr),stat=alloc_error )
    if( alloc_error /= 0 ) then; print*,'Program could not allocate space for ang22_target'; stop; endif
    ang22_target = 0.

! Calculate angles in lat-lon system

  do i=1,ntarget
     lon22 = target_center_lon(i) * To_Radians
     lat22 = target_center_lat(i) * To_Radians
     call CubedSphereABPFromRLL(lon22, lat22, a22, b22, ipanel22 , .true. )

     do isubr=1,nsubr
        if ( ANGLX_TARGET(i,isubr) > -9000. ) then
          a22s = a22 + 0.01*SIN( ANGLX_TARGET(i,isubr)*PI/180. )   
          b22s = b22 + 0.01*COS( ANGLX_TARGET(i,isubr)*PI/180. )
          call CubedSphereRLLFromABP(a22s, b22s , ipanel22, lon22s, lat22s )
          dx2 = COS( lat22 )*(lon22s-lon22 )
          dy2 = ( lat22s-lat22 )
          if ( dx2 < 0.0 ) dy2  = -1.*dy2  ! 
          COSLL  = dy2 /sqrt( dx2**2 + dy2**2 )
          ANG22_TARGET(i,isubr) = ACOS( COSLL )*180./PI
        else
          ANG22_TARGET(i,isubr) = -9000.
        end if   
     end do
  end do

 end subroutine latlonangles


!================================================================
 subroutine importancesort (ntarget)
!-----------------------------------

   integer,  intent(in) :: ntarget


  real(r8), allocatable, dimension(:) :: imprtnc,tmp2
  integer,  allocatable, dimension(:) :: insrt

  
  real(r8) :: tmp
  integer  :: i,ii,jj,kk,itmp
  integer  :: alloc_error

 
    allocate( insrt(nsubr) , stat=alloc_error )
    if( alloc_error /= 0 ) then; print*,'Program could not allocate space for INSRT '; stop; endif
    allocate( imprtnc(nsubr) , stat=alloc_error )
    if( alloc_error /= 0 ) then; print*,'Program could not allocate space for IMPRTNC '; stop; endif
    allocate( tmp2(nsubr) , stat=alloc_error )
    if( alloc_error /= 0 ) then; print*,'Program could not allocate space for TMP2 '; stop; endif



  do i=1,ntarget

     do JJ = 1,nsubr
        insrt(JJ)   = JJ
        !imprtnc(JJ) = mxvrx_target(i,JJ) * wghts_target(i,JJ) * aniso_target(i,JJ)
        imprtnc(JJ) = mxdis_target(i,JJ) * clngt_target(i,JJ) 
     end do

     do JJ = 1,nsubr-1
        do KK = JJ+1,nsubr
           if ( imprtnc(JJ) < imprtnc(KK) ) then
              tmp = imprtnc(KK)
              imprtnc(KK) = imprtnc(JJ)
              imprtnc(JJ) = tmp
              itmp = insrt(KK)
              insrt(KK)   = insrt(JJ)
              insrt(JJ)   = itmp
           end if
        end do
     end do


     do JJ = 1,nsubr
        tmp2(JJ) =  mxvrx_target(i,JJ)
     end do
     do JJ = 1,nsubr
        mxvrx_target(i,JJ) = tmp2( insrt(JJ) )
     end do

     do JJ = 1,nsubr
        tmp2(JJ) =  mxvry_target(i,JJ)
     end do
     do JJ = 1,nsubr
        mxvry_target(i,JJ) = tmp2( insrt(JJ) )
     end do

     do JJ = 1,nsubr
        tmp2(JJ) =  mxdis_target(i,JJ)
     end do
     do JJ = 1,nsubr
        mxdis_target(i,JJ) = tmp2( insrt(JJ) )
     end do

     do JJ = 1,nsubr
        tmp2(JJ) =  riseq_target(i,JJ)
     end do
     do JJ = 1,nsubr
        riseq_target(i,JJ) = tmp2( insrt(JJ) )
     end do

     do JJ = 1,nsubr
        tmp2(JJ) =  fallq_target(i,JJ)
     end do
     do JJ = 1,nsubr
        fallq_target(i,JJ) = tmp2( insrt(JJ) )
     end do

     do JJ = 1,nsubr
        tmp2(JJ) =  bsvar_target(i,JJ)
     end do
     do JJ = 1,nsubr
        bsvar_target(i,JJ) = tmp2( insrt(JJ) )
     end do

     do JJ = 1,nsubr
        tmp2(JJ) =  aniso_target(i,JJ)
     end do
     do JJ = 1,nsubr
        aniso_target(i,JJ) = tmp2( insrt(JJ) )
     end do

     do JJ = 1,nsubr
        tmp2(JJ) =  anixy_target(i,JJ)
     end do
     do JJ = 1,nsubr
        anixy_target(i,JJ) = tmp2( insrt(JJ) )
     end do

     do JJ = 1,nsubr
        tmp2(JJ) =  anglx_target(i,JJ)
     end do
     do JJ = 1,nsubr
        anglx_target(i,JJ) = tmp2( insrt(JJ) )
     end do

     do JJ = 1,nsubr
        tmp2(JJ) =  hwdth_target(i,JJ)
     end do
     do JJ = 1,nsubr
        hwdth_target(i,JJ) = tmp2( insrt(JJ) )
     end do

     do JJ = 1,nsubr
        tmp2(JJ) =  wghts_target(i,JJ)
     end do
     do JJ = 1,nsubr
        wghts_target(i,JJ) = tmp2( insrt(JJ) )
     end do

     do JJ = 1,nsubr
        tmp2(JJ) =  cwght_target(i,JJ)
     end do
     do JJ = 1,nsubr
        cwght_target(i,JJ) = tmp2( insrt(JJ) )
     end do

     do JJ = 1,nsubr
        tmp2(JJ) =  clngt_target(i,JJ)
     end do
     do JJ = 1,nsubr
        clngt_target(i,JJ) = tmp2( insrt(JJ) )
     end do

!++ 11/2/21
! Never sorted count variable!!
     do JJ = 1,nsubr
        tmp2(JJ) =  count_target(i,JJ)
     end do
     do JJ = 1,nsubr
        count_target(i,JJ) = tmp2( insrt(JJ) )
     end do
  end do


end subroutine importancesort

!======================================

!++1/22/22 Added 
function fleshout_block ( ncube,nhalo,nsw,mxdisC,hwdthC,anglxC,rrfac ) result( axc )
   
       integer, intent(in) :: ncube,nhalo,nsw
       real(KIND=dbl_kind), intent(in), dimension( ncube, ncube, 6 ) :: mxdisC
       real(KIND=dbl_kind), intent(in), dimension( ncube, ncube, 6 ) :: hwdthC
       real(KIND=dbl_kind), intent(in), dimension( ncube, ncube, 6 ) :: anglxC
       real(KIND=dbl_kind), intent(in), dimension( ncube, ncube, 6 ) :: rrfac

       real(KIND=dbl_kind), dimension(1-nhalo:ncube+nhalo,1-nhalo:ncube+nhalo ,6) :: axc
       real(rpx), dimension(-nsw:nsw,-nsw:nsw) :: suba,sub1,sub11
       real(rpx), dimension(-nsw:nsw,-nsw:nsw) :: subr,subq,subdis
       real(rpx), dimension(-nsw:nsw)          :: xq,yq
       real(RPX) :: rotangl,dsq,ssq
       integer :: i,j,x0,x1,y0,y1,ip,ns0,ns1,ii,jj,norx,nory,nql,ncl,nhw,ipk,npeaks,jw,iw,nswx
!---------------------------------------------------


write(*,*) " in fleshout_block "
!===============================
! Initialize cube sphere "canvas"
! for "painting"
!================================
  axc = 0.

  do ip=1,6
  do j=1,ncube
  do i=1,ncube
     if(mxdisC(i,j,ip)>=1.0) then
       nswx = NINT( nsw / rrfac(i,j,ip) )
       suba(:,:) = 0.
       subr(:,:) = 0.
       nhw  = MIN( INT(hwdthC(i,j,ip)/2) , nswx/2 )
       do jw=-nhw,nhw
          suba( jw , -1:1 ) = 1.-1.0*abs(jw)/nhw
       end do
       rotangl = - anglxC(i,j,ip) 
       subr = rotbyx( suba , 2*nsw+1, rotangl )
       subdis = subr  *    mxdisC(i,j,ip)
       where(abs(subdis)>=8000.)
           subdis = 0.
       end where

                ! Reconstruct 
                !------------------------
                do jj = -NSWx/2,NSWx/2
                do ii = -NSWx/2,NSWx/2
                    x0 = i ! INT( xspk(ipk) ) + 1
                    y0 = j ! INT( yspk(ipk) ) + 1
                    if ( (x0+ii>=1-nhalo).and.(x0+ii<=ncube+nhalo).AND.(Y0+ii>=1-nhalo).and.(Y0+ii<=ncube+nhalo) ) then
                       if ( subdis(ii,jj) >=  AXC( x0+ii, y0+jj, ip ) )  AXC( x0+ii, y0+jj, ip ) = subdis(ii,jj)
                    end if
                end do
                end do
     end if
  end do
  end do
  write(*,*)" finished panel=",ip
  end do

end function fleshout_block
!======================================
!++1/25/22 Added 
function fleshout_profi ( ncube,nhalo,nsw,mxdisC,anglxC,uniqidC,rrfac,shape_x ) & 
                          result( axc )
   
       integer, intent(in) :: ncube,nhalo,nsw
       real(KIND=dbl_kind), intent(in), dimension( ncube, ncube, 6 ) :: mxdisC
       real(KIND=dbl_kind), intent(in), dimension( ncube, ncube, 6 ) :: anglxC
       real(KIND=dbl_kind), intent(in), dimension( ncube, ncube, 6 ) :: uniqidC
       real(KIND=dbl_kind), intent(in), dimension( ncube, ncube, 6 ) :: rrfac
       real(rpx),                intent(in), dimension( 2*PSW+1, size(xs) ) :: shape_x

       real(KIND=dbl_kind), dimension(1-nhalo:ncube+nhalo,1-nhalo:ncube+nhalo ,6) :: axc
       real(rpx), dimension(-nsw:nsw,-nsw:nsw) :: suba,sub1,sub11
       real(rpx), dimension(-nsw:nsw,-nsw:nsw) :: subr,subq,subdis
       real(rpx), dimension(-nsw:nsw)          :: xq,yq
       real(RPX) :: rotangl,dsq,ssq
       integer :: i,j,x0,x1,y0,y1,ip,ns0,ns1,ii,jj,norx,nory,nql,ncl,nhw,ipk,npeaks,jw,iw,idx1,nswx
!---------------------------------------------------

!===============================
! Initialize cube sphere "canvas"
! for "painting"
!================================
  axc  =  0.

  do ip=1,6
  do j=1,ncube
  do i=1,ncube
     if(mxdisC(i,j,ip)>=1.0) then
       nswx = NINT( nsw / rrfac(i,j,ip) )
       ipk  = INT( uniqidC ( i,j,ip ) )
       suba(:,:) = 0.
       subr(:,:) = 0.
       do jw=-1,1
          !!suba(: , jw  ) = rdg_profiles_x(PSW+1-nsw:PSW+1+nsw,ipk) 
          suba(: , jw  ) = shape_x(PSW+1-nsw:PSW+1+nsw,ipk) 
       end do
       rotangl = - anglxC(i,j,ip) 
       subr = rotbyx( suba , 2*nsw+1, rotangl )
       subdis = subr
       where(abs(subdis)>=8000.)
           subdis = 0.
       end where
                ! Reconstruct 
                !------------------------
                do jj = -NSWx/2,NSWx/2
                do ii = -NSWx/2,NSWx/2
                    x0 = i ! INT( xspk(ipk) ) + 1
                    y0 = j ! INT( yspk(ipk) ) + 1
                    if ( (x0+ii>=1-nhalo).and.(x0+ii<=ncube+nhalo).AND.(Y0+ii>=1-nhalo).and.(Y0+ii<=ncube+nhalo) ) then
                       if ( (subdis(ii,jj) <= 0.).and. (AXC( x0+ii, y0+jj, ip )<=0. ) ) then
                          if ( subdis(ii,jj) <=  AXC( x0+ii, y0+jj, ip ) )  AXC( x0+ii, y0+jj, ip ) = subdis(ii,jj)
                       else
                          if ( subdis(ii,jj) >=  AXC( x0+ii, y0+jj, ip ) )  AXC( x0+ii, y0+jj, ip ) = subdis(ii,jj)
                       end if
                    end if
                end do
                end do
     end if
  end do
  end do
  write(*,*)" finished panel=",ip
  end do

end function fleshout_profi

!======================================
function color_on_profi ( ncube,nhalo,nsw,mxdisC,anglxC,uniqidC,rrfac,shape_x,colors ) & 
                          result( bxc )
   
       integer, intent(in) :: ncube,nhalo,nsw
       real(KIND=dbl_kind), intent(in), dimension( ncube, ncube, 6 )   :: mxdisC
       real(KIND=dbl_kind), intent(in), dimension( ncube, ncube, 6 )   :: anglxC
       real(KIND=dbl_kind), intent(in), dimension( ncube, ncube, 6 )   :: uniqidC
       real(KIND=dbl_kind), intent(in), dimension( ncube, ncube, 6 )   :: rrfac
       real(rpx),                intent(in), dimension( 2*PSW+1, size(xs) ) :: shape_x
       real(rpx),                intent(in), dimension( size(xs) )          :: colors

       real(KIND=dbl_kind), dimension(1-nhalo:ncube+nhalo,1-nhalo:ncube+nhalo ,6) :: axc
       real(KIND=dbl_kind), dimension(1-nhalo:ncube+nhalo,1-nhalo:ncube+nhalo ,6) :: bxc
       real(rpx), dimension(-nsw:nsw,-nsw:nsw) :: suba,sub1,sub11
       real(rpx), dimension(-nsw:nsw,-nsw:nsw) :: subr,subq,subdis,subcolo
       real(rpx), dimension(-nsw:nsw)          :: xq,yq
       real(RPX) :: rotangl,dsq,ssq
       integer :: i,j,x0,x1,y0,y1,ip,ns0,ns1,ii,jj,norx,nory,nql,ncl,nhw,ipk,npeaks,jw,iw,idx1,nswx
!---------------------------------------------------

 
!write(*,*) " in fleshout_profi "
!===============================
! Initialize cube sphere "canvas"
! for "painting"
!================================
  axc  =  0.

  do ip=1,6
  do j=1,ncube
  do i=1,ncube
     if(mxdisC(i,j,ip)>=1.0) then
       nswx = NINT( nsw / rrfac(i,j,ip) )
       ipk  = INT( uniqidC ( i,j,ip ) )
       suba(:,:) = 0.
       subr(:,:) = 0.
       subcolo(:,:) = 0.
       do jw=-1,1
          !!suba(: , jw  ) = rdg_profiles_x(PSW+1-nsw:PSW+1+nsw,ipk) 
          suba(: , jw  ) = shape_x(PSW+1-nsw:PSW+1+nsw,ipk) 
       end do
       rotangl = - anglxC(i,j,ip) 
       subr = rotbyx( suba , 2*nsw+1, rotangl )
       subdis = subr
       where(abs(subdis)>=8000.)
           subdis = 0.
       end where
       where((subdis)>0.001)
           subcolo = colors(ipk)
       end where
                ! Reconstruct 
                !------------------------
                do jj = -NSWx/2,NSWx/2
                do ii = -NSWx/2,NSWx/2
                    x0 = i ! INT( xspk(ipk) ) + 1
                    y0 = j ! INT( yspk(ipk) ) + 1
                    if ( (x0+ii>=1-nhalo).and.(x0+ii<=ncube+nhalo).AND.(Y0+ii>=1-nhalo).and.(Y0+ii<=ncube+nhalo) ) then
                       if ( (subdis(ii,jj) <= 0.).and. (AXC( x0+ii, y0+jj, ip )<=0. ) ) then
                          if ( subdis(ii,jj) <=  AXC( x0+ii, y0+jj, ip ) ) then
                             AXC( x0+ii, y0+jj, ip ) = subdis(ii,jj)
                             BXC( x0+ii, y0+jj, ip ) = subcolo(ii,jj)
                          end if
                       else
                          if ( subdis(ii,jj) >=  AXC( x0+ii, y0+jj, ip ) )  then 
                             AXC( x0+ii, y0+jj, ip ) = subdis(ii,jj)
                             BXC( x0+ii, y0+jj, ip ) = subcolo(ii,jj)
                          end if
                       end if
                    end if
                end do
                end do
     end if
  end do
  end do
  write(*,*)" finished panel=",ip
  end do

end function color_on_profi


!======================================
function paintridge2cube ( axr, ncube,nhalo,nsw, lzerovalley, crest_length, crest_weight, all_pixels ) & 
                           result( axc )
   
       integer, intent(in) :: ncube,nhalo,nsw
       real(rpx), intent(in), dimension( size(xs) ) :: axr
       logical, intent(in) :: lzerovalley
       logical, optional, intent(in) :: crest_length
       logical, optional, intent(in) :: crest_weight
       logical, optional, intent(in) :: all_pixels

       real(KIND=dbl_kind), dimension(1-nhalo:ncube+nhalo,1-nhalo:ncube+nhalo ,6) :: axc
       real(KIND=dbl_kind), dimension(1-nhalo:ncube+nhalo,1-nhalo:ncube+nhalo ,6) :: qc
       real(rpx), dimension(-nsw:nsw,-nsw:nsw) :: suba,sub1,sub11
       real(rpx), dimension(-nsw:nsw,-nsw:nsw) :: subr,subq,subdis
       real(rpx), dimension(-nsw:nsw,-nsw:nsw) :: subblk,subblk0
       real(rpx), dimension(-nsw:nsw)          :: xq,yq
       real(RPX) :: rotangl,dsq,ssq
       integer :: i,j,x0,x1,y0,y1,ip,ns0,ns1,ii,jj,norx,nory,nql,ncl,nhw,ipk,npeaks,jw,iw,nswx
       logical :: lcrestln,lcrestwt,lblockfl,lprofifl,lbumpfl,allpixels
!---------------------------------------------------


       write(*,*) " in paintridge "

    if(present(crest_length)) then
      lcrestln = crest_length
    else
       lcrestln = .false.
    endif
    if(present(crest_weight)) then
      lcrestwt = crest_weight
    else
       lcrestwt = .false.
    endif
    if(present(all_pixels)) then
       allpixels = all_pixels
    else
       allpixels = .false.
    endif


    DO i=-nsw,nsw
       xq(i)=i
       yq(i)=i
    END DO
 

  ns0=nsw/2+1
  ns1=ns0+nsw

     npeaks=size(mxdis)


!===============================================================
! Set-up 2*NSW+1 sub1 square "brush" for future reconcilaition/annealing.
! The array SUB1 is supposed to be =1.0 for all points within NSW 
! of center, and =0.0 for points further out.
!===============================================================
  sub1(:,:)=0.
  nql = INT( nsw/1.0 )

    DO j=-nsw,nsw
    DO i=-nsw,nsw
       ssq= SQRT(xq(i)**2+yq(j)**2)
       if (SSQ < nql) sub1(i,j)=1.0
    END DO
    END DO
  
    sub11(:,:)=0.
    DO j=-nsw/2,nsw/2
    DO i=-nsw/4,nsw/4
          sub11(i,j)=1.0
    END DO
    END DO
#if 0
    DO j=-nsw/2,nsw/2
    DO i=-nsw/2,nsw/2
          sub11(i,j)=1.0
    END DO
    END DO
#endif
#ifdef ROTATEBRUSH
    write(*,*) "Using nsw/2 x nsw/2 ROTATED brush in paintridge"
#endif
!=======================================
! Initialize cube sphere "canvas" arrays 
! for "painting"
!=======================================
  axc = 0.
  qc  = 0.


!==================================================================
! Notes following 11/3/2021
!  In the following we create arrays suba(-nsw:nsw, -nsw:nsw)  
!  with normalized magnitudes 0.0-1.0. Here X (dim1) is the 
!  cross-ridge direction while Y (dim2) is the long-crest 
!  direction.  The suba array is then rotated into the correct
!  orientation for the current peak/feature, resulting in subr. The 
!  starting suba array can take on different shapes depending on task. 
!  suba is always ZERO for Y indices further than ncl=CLNGTH/2 from
!  center.  For all tasks except block_fill (and maybe a future
!  profile_fill), suba is 0 everywhere except for suba(0,-ncl:ncl)=1.0,
!  i.e., a one-pixel line along the ridge crest.  
!
!  After rotation of suba to subr, subr (still =0.0-1.0) may be 
!  multiplied by a peak/feature-list quantity passed in arguments as 
!  axr( size(xs)(npks?) ) to create the array subdis(-nsw:nsw,-nsw:nsw), 
!  which may now have physical units, e.g., meters if axr=mxdis.
!
!  After subdis has been created it is "painted" onto a "haloed" 
!  (ncube+halo,ncube+halo,6) output array AXC. Multiple paints 
!  are possible due to many redundant features. The "best" painting 
!  is selected based the closeness of the feature center (xs,ys) to
!  its diagnosed peak-ridgecrest location (xspk,yspk)
!=======================================================================
  do ipk=1,npeaks
        if(mxdis(ipk)>=1.0) then
 
            if(Lcrestwt) then
               suba(:,:) = 0.
               ncl  = MIN( INT(clngth(ipk)/2) , nsw/2 )
               suba( 0 , -ncl:ncl ) = 1.        
               rotangl = - anglx(ipk) 
               subr = rotbyx( suba , 2*nsw+1, rotangl )
               subdis = subr 
             else if(Lcrestln) then
               suba(:,:) = 0.
               ncl  = MIN( INT(clngth(ipk)/2) , nsw/2 )
               suba( 0 , -ncl:ncl ) = 1.        
               rotangl = - anglx(ipk) 
               subr = rotbyx( suba , 2*nsw+1, rotangl )
               subdis = subr * axr(ipk)
             else            
               suba(:,:) = 0.
               ncl  = MIN( INT(clngth(ipk)/2) , nsw/2 )
               suba( 0 , -ncl:ncl ) = 1.        
               rotangl = - anglx(ipk) 
               subr = rotbyx( suba , 2*nsw+1, rotangl )
               subdis =  subr * axr(ipk)
             end if


#ifdef ROTATEBRUSH
             ! rotated "brush"
             rotangl = - anglx(ipk) 
             sub1 = rotbyx( sub11 , 2*nsw+1, rotangl )
#endif

             
             
             NSWx = NSW / RefFac(ipk)
             !++jtb 05/26/24: Protection against too-small nswx
             NSWx = MAX( 4 , NSWx )

             if (.NOT.(allpixels)) then
#if 0
             !-------------------------------------------------------
             ! original reconciliation based on {xs,ys} vs {xspk,yspk} 
             !-------------------------------------------------------
             !======================================================
             ! Scale sub1 by 1 minus normalized distance 
             ! from current feature location (xs,ys) to diagnosed 
             ! peak/ridge location (xspk,yspk).  Idea here is
             ! that we want features that are also close to actual
             ! peaks to "win" when creating skeletons.
             !======================================================
             dsq    = 1.0 - SQRT( (xs(ipk)-xspk(ipk))**2 + (ys(ipk)-yspk(ipk))**2 )/nsw
             subq   = sub1 * dsq
             do jj = -NSWx/2,NSWx/2
             do ii = -NSWx/2,NSWx/2
                ip = peaks(ipk)%ip
                x0 = INT( xspk(ipk) ) + 1  ! original, original has +1
                y0 = INT( yspk(ipk) ) + 1  ! original, original has +1
                !x0 = INT( xspk(ipk) )      ! why do we need +1 
                !y0 = INT( yspk(ipk) )
                if ( (x0+ii>=1-nhalo).and.(x0+ii<=ncube+nhalo).AND.(Y0+ii>=1-nhalo).and.(Y0+ii<=ncube+nhalo) ) then
                       if ( QC( x0+ii, y0+jj, ip ) <= subq(ii,jj) )  AXC( x0+ii, y0+jj, ip ) = subdis(ii,jj)
                       if ( QC( x0+ii, y0+jj, ip ) <= subq(ii,jj) )  QC( x0+ii, y0+jj, ip )  = subq(ii,jj)
                endif
             end do
             end do
#else
             !------------------------------------------------------------
             ! reconstruction/reconciliation based on a quality/amplitude
             ! measure in rotated rectangle (subblk)
             !------------------------------------------------------------
             rotangl = - anglx(ipk) 
             subblk0(:,:)=0.
             ncl  = MIN( INT(clngth(ipk)/2) , nsw/2 )

!++tune
!   For now make opt='_h2' the default. Compromise
              !nhw  = MIN( INT(hwdth(ipk)/1) , nsw/2 )  ! _h0 
              !nhw  = MIN( INT(hwdth(ipk)/2) , nsw/2 )  ! _h1 
             nhw  = MIN( INT(hwdth(ipk)/4) , nsw/2 )  ! _h2
              !nhw  = MIN( INT(hwdth(ipk)/8) , nsw/2 )  ! _h3 
              !nhw  = MIN( INT(hwdth(ipk)/16) , nsw/2 ) ! _h4 
#if 1
             do jw=-nhw,nhw
               subblk0( jw , -ncl:ncl ) = 1.
             end do
#else
             if (ncl>0) then
             do jw=-ncl,ncl
             do iw=-nhw,nhw
               subblk0( iw , jw ) = 1. * (1. - abs(jw)/ncl )
             end do
             end do
             else
             do jw=-ncl,ncl
             do iw=-nhw,nhw
               subblk0( iw , jw ) = 1. 
             end do
             end do
             end if
#endif
             subblk0 = rotbyx( subblk0 , 2*nsw+1, rotangl )
             subblk  = subblk0 * mxdis(ipk) 

             do jj = -NSWx/2,NSWx/2
             do ii = -NSWx/2,NSWx/2
                ip = peaks(ipk)%ip
                !x0 = INT( xspk(ipk) )      ! do we need +1 
                !y0 = INT( yspk(ipk) )  
                x0 = NINT( 1.*xspk(ipk) )      ! do we need +1 
                y0 = NINT( 1.*yspk(ipk) )  
                if ( (x0+ii>=1-nhalo).and.(x0+ii<=ncube+nhalo).AND.(Y0+ii>=1-nhalo).and.(Y0+ii<=ncube+nhalo) ) then
                       if (subblk(ii,jj) >= QC( x0+ii, y0+jj, ip ))  AXC( x0+ii, y0+jj, ip ) = subdis(ii,jj)
                       if (subblk(ii,jj) >= QC( x0+ii, y0+jj, ip ))   QC( x0+ii, y0+jj, ip ) = subblk(ii,jj)
                 endif
             end do
             end do
#endif
             else
             ! allpixels reconstruction/reconcilaition
             !----------------------------------------
             do jj = -NSWx/2,NSWx/2
             do ii = -NSWx/2,NSWx/2
                ip = peaks(ipk)%ip
                x0 = INT( xspk(ipk) )
                y0 = INT( yspk(ipk) )
                if ( (x0+ii>=1-nhalo).and.(x0+ii<=ncube+nhalo).AND.(Y0+ii>=1-nhalo).and.(Y0+ii<=ncube+nhalo) ) then
                       if (subdis(ii,jj) >= AXC( x0+ii, y0+jj, ip ))  AXC( x0+ii, y0+jj, ip ) = subdis(ii,jj)
                 endif
             end do
             end do
             end if
          end if
      end do

     write(*,*) " finished paintridge2cube "
     
  end function paintridge2cube
 !==================================================================

 subroutine alloc_ridge_qs (npeaks , NSW )

  integer, intent(in) :: npeaks
  integer, intent(in) :: NSW

  allocate( xs(npeaks) )
  allocate( ys(npeaks) )
  ys=-999.
  xs=-999.

  allocate( bsvar(npeaks) )
  bsvar=0. !-999.

  allocate( mxvrx(npeaks) )
   mxvrx=0.
  allocate( mxvry(npeaks) )
   mxvry=0.
  allocate( mxdis(npeaks) )
   mxdis=0.
  allocate( mxds2(npeaks) )
   mxds2=0.
  allocate( anglx(npeaks) )
   anglx=-999.
  allocate( angll(npeaks) )
   angll=-999.
  allocate( aniso(npeaks) )
   aniso=0.
  allocate( anixy(npeaks) )
   anixy=0.
  allocate( mnslp(npeaks) )
   mnslp=0.
  allocate( xspk(npeaks) )
   xspk=0.
  allocate( yspk(npeaks) )
   yspk=0.
  allocate( xs01(npeaks) )
   xs01=0.
  allocate( ys01(npeaks) )
   ys01=0.
  allocate( mxdsp(npeaks) )
   mxdsp=0.
  allocate( mxds0(npeaks) )
   mxds0=0.
  allocate( mxds1(npeaks) )
   mxds1=0.
  allocate( hwdth(npeaks) )
   hwdth=0.
  allocate( npks(npeaks) )
   npks=0.
  allocate( nvls(npeaks) )
   nvls=0.
  allocate( sft0(npeaks) )
   sft0=0.
  allocate( sft1(npeaks) )
   sft1=0.
  allocate( vldps(npeaks) )
   vldps=0.
  allocate( pkhts(npeaks) )
   pkhts=0.
  allocate( rwvls(npeaks) )
   rwvls=0.
  allocate( rwpks(npeaks) )
   rwpks=0.
  allocate( uqrid(npeaks) )
   uqrid=-1
  allocate( riseq(npeaks) )
   riseq=-1.
  allocate( fallq(npeaks) )
   fallq=0.
  allocate( clngth(npeaks) )
   clngth=0.
  allocate( clngt2(npeaks) )
   clngt2=0.

          allocate( ALP0(npeaks))
            ALP0  = -9999.d+0
          allocate( BET0(npeaks))
            BET0  = -9999.d+0
          allocate( LON0(npeaks))
              LON0  = -9999.d+0
          allocate( LAT0(npeaks))
              LAT0  = -9999.d+0
          allocate( ALP1(npeaks))
            ALP1  = -9999.d+0
          allocate( BET1(npeaks))
            BET1  = -9999.d+0
          allocate( LON1(npeaks))
             LON1  = -9999.d+0
          allocate( LAT1(npeaks))
              LAT1  = -9999.d+0

  ! Set for original fixed NSW ridge-finding 
  !PSW = nsw
  ! Set for ridge-finding w/ det(g) dependent window
  PSW = NINT ( 3.*nsw / 2.)

  allocate( rdg_profiles( PSW+1, npeaks ) )
       rdg_profiles(:,:)=0.d+0
  allocate( crst_profiles( PSW+1, npeaks ) )
       crst_profiles(:,:)=0.d+0
  allocate( crst_silhous( PSW+1, npeaks ) )
       crst_silhous(:,:)=0.d+0
   allocate( MyPanel( npeaks ) )
       MyPanel = -1
   allocate( NSWx_diag( npeaks ) )
       NSWx_diag = -1


  allocate( UNIQID( npeaks ) )
       UNIQID(:)=-9999.d+0


  allocate( isoht(npeaks) )
   isoht=0.
  allocate( isowd(npeaks) )
   isowd=0.
  allocate( isobs(npeaks) )
   isobs=0.
  allocate( RefFac(npeaks) )
   RefFac=1.0
 
  allocate( hnodes_list( PSW+1, npeaks ) )
       hnodes_list(:,:)=0.d+0
  allocate( xnodes_list( PSW+1, npeaks ) )
       xnodes_list(:,:)=0
  allocate( nnodes_list( npeaks ) )
       nnodes_list(:)=0
  allocate( dcenter_list( npeaks ) )
       dcenter_list=-999

  allocate( xwedge_list( 3 , npeaks ) )
       xwedge_list(:,:)=0
  allocate( hwedge_list( 3 , npeaks ) )
       hwedge_list(:,:)=0


  allocate( rdg_profiles_x( 2*PSW+1, npeaks ) )
       rdg_profiles_x(:,:)=0.d+0
  allocate( hnodes_x( 2*PSW+1, npeaks ) )
       hnodes_x(:,:)=0.d+0
  allocate( hnodes_o( 2*PSW+1, npeaks ) )
       hnodes_o(:,:)=0.d+0
  allocate( hwedge_x( 2*PSW+1, npeaks ) )
       hwedge_x(:,:)=0.d+0
  allocate( hwedge_o( 2*PSW+1, npeaks ) )
       hwedge_o(:,:)=0.d+0




#ifdef DEBUGOUTPUT
  allocate( rt_diag( 2*PSW+1, 2*PSW+1, npeaks ) )
       rt_diag(:,:,:)=0.d+0
  allocate( suba_diag( 2*PSW+1, 2*PSW+1, npeaks ) )
       suba_diag(:,:,:)=0.d+0
#endif

end subroutine alloc_ridge_qs

!==================================================================



end module ridge_ana
