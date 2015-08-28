module ridge_ana

use rotation 
USE reconstruct
use shr_kind_mod, only: r8 => shr_kind_r8

IMPLICIT NONE
private


public find_ridges
public remapridge2target




  REAL, allocatable  :: MXVRX(:,:,:),MXDIS(:,:,:),MNSLP(:,:,:),ANGLX(:,:,:),ANISO(:,:,:),XS(:),YS(:)
  REAL, allocatable  :: XSPK(:,:,:),YSPK(:,:,:),MXDS0(:,:,:),MXDS1(:,:,:),SFT0(:,:,:),SFT1(:,:,:)
  REAL, allocatable  :: PKHTS(:,:,:),VLDPS(:,:,:),RWPKS(:,:,:),RWVLS(:,:,:),ANGLL(:,:,:)
  REAL, allocatable  :: BSVAR(:,:,:),HWDTH(:,:,:),NPKS(:,:,:),NVLS(:,:,:),MXVRY(:,:,:)

    REAL(KIND=dbl_kind), allocatable ::  ALP0(:,:),BET0(:,:),LAT0(:,:,:),LON0(:,:,:)
    REAL(KIND=dbl_kind), allocatable ::  ALP1(:,:),BET1(:,:),LAT1(:,:,:),LON1(:,:,:)

  real(r8), allocatable, dimension(:,:) :: anglx_target,aniso_target,mxdis_target,hwdth_target
  real(r8), allocatable, dimension(:,:) :: mxvrx_target,mxvry_target,bsvar_target,wghts_target,ang22_target

    INTEGER (KIND=int_kind),allocatable :: UQRID(:,:,:) 



    REAL, allocatable ::  wt1p(:,:)

    REAL (KIND=dbl_kind), PARAMETER :: pi        = 3.14159265358979323846264338327

    integer, parameter             ::   NANG=16
    integer, parameter             ::   NSUBR = NANG

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine find_ridges ( terr_dev, terr_raw, ncube, nhalo, nsb, nsw )
!------------------------------------------------
!  INPUTS.
!      NSW = size of window used for ridge analysis
!      




    REAL (KIND=dbl_kind), &
            DIMENSION(ncube-1,ncube-1,6), INTENT(IN) :: terr_dev
    REAL (KIND=dbl_kind), &
            DIMENSION(ncube-1,ncube-1,6), INTENT(IN) :: terr_raw


       INTEGER (KIND=int_kind), INTENT(IN) :: ncube, nhalo, nsb, nsw
       INTEGER (KIND=int_kind) :: i,j,np,ncube_halo,ipanel,N,norx,nory,ip



    REAL (KIND=dbl_kind),                                            &
         DIMENSION(1-nhalo:ncube+nhalo, 1-nhalo:ncube+nhalo, 6) :: terr_halo
    REAL (KIND=dbl_kind),                                            &
         DIMENSION(1-nhalo:ncube+nhalo, 1-nhalo:ncube+nhalo, 6) :: terr_dev_halo
    REAL ,                                            &
         DIMENSION(1-nhalo:ncube+nhalo, 1-nhalo:ncube+nhalo, 6) :: terr_halo_r4
    REAL ,                                            &
         DIMENSION(1-nhalo:ncube+nhalo, 1-nhalo:ncube+nhalo, 6) :: terr_dev_halo_r4

    REAL  ,                                                          &
         DIMENSION(1-nhalo:ncube+nhalo )                          :: xv,yv,alph,beta


    REAL(KIND=dbl_kind)  :: lon_r8, lat_r8, cosll, dx, dy, dcube2, ampfsm,dbet,dalp,diss,diss00

    CHARACTER(len=1024) :: ofile$,ve$

!---------------------------------------------------------------------------------------
!  B E G I N   C A L C U L A T I O N S
!---------------------------------------------------------------------------------------

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

      terr_dev_halo_r4 = terr_dev_halo
      terr_halo_r4     = terr_halo


      ncube_halo = size( terr_halo_r4, 1)




! Allocate arrays to hold results on reduced Cubed-sphere grid
!--------------------------------------------------------------

  N = ncube_halo
  allocate( xs(N/nsb) )
  allocate( ys(N/nsb) )
  ys=-999.
  xs=-999.

  allocate( bsvar( N/nsb , N/nsb , 6) )
  bsvar=0. !-999.

  allocate( mxvrx( N/nsb , N/nsb , 6) )
   mxvrx=0.
  allocate( mxvry( N/nsb , N/nsb , 6) )
   mxvry=0.
  allocate( mxdis( N/nsb , N/nsb , 6) )
   mxdis=0.
  allocate( anglx( N/nsb , N/nsb , 6) )
   anglx=-999.
  allocate( angll( N/nsb , N/nsb , 6) )
   angll=-999.
  allocate( aniso( N/nsb , N/nsb , 6) )
   aniso=0.
  allocate( mnslp( N/nsb , N/nsb , 6) )
   mnslp=0.
  allocate( xspk( N/nsb , N/nsb , 6) )
   xspk=0.
  allocate( yspk( N/nsb , N/nsb , 6) )
   yspk=0.
  allocate( mxds0( N/nsb , N/nsb , 6) )
   mxds0=0.
  allocate( mxds1( N/nsb , N/nsb , 6) )
   mxds1=0.
  allocate( hwdth( N/nsb , N/nsb , 6) )
   hwdth=0.
  allocate( npks( N/nsb , N/nsb , 6) )
   npks=0.
  allocate( nvls( N/nsb , N/nsb , 6) )
   nvls=0.
  allocate( sft0( N/nsb , N/nsb , 6) )
   sft0=0.
  allocate( sft1( N/nsb , N/nsb , 6) )
   sft1=0.
  allocate( vldps( N/nsb , N/nsb , 6) )
   vldps=0.
  allocate( pkhts( N/nsb , N/nsb , 6) )
   pkhts=0.
  allocate( rwvls( N/nsb , N/nsb , 6) )
   rwvls=0.
  allocate( rwpks( N/nsb , N/nsb , 6) )
   rwpks=0.
  allocate( uqrid( N/nsb , N/nsb , 6) )
   uqrid=-1

       norx= N/nsb !size(anglx,1)
       nory= N/nsb !size(anglx,1)


          allocate( ALP0(norx,nory))
            ALP0  = -9999.d+0
          allocate( BET0(norx,nory))
            BET0  = -9999.d+0
          allocate( LON0(norx,nory,6))
              LON0  = -9999.d+0
          allocate( LAT0(norx,nory,6))
              LAT0  = -9999.d+0
          allocate( ALP1(norx,nory))
            ALP1  = -9999.d+0
          allocate( BET1(norx,nory))
            BET1  = -9999.d+0
          allocate( LON1(norx,nory,6))
             LON1  = -9999.d+0
          allocate( LAT1(norx,nory,6))
              LAT1  = -9999.d+0




    dcube2 = 0.5 * ncube 


    do ipanel=1,6   !1, 6
       call ANISO_ANA( terr_dev_halo_r4(:,:,ipanel) , terr_halo_r4(:,:,ipanel) , XV , YV , ncube_halo ,NSB,NSW, ipanel )


       
       do j=1,nory
       do i=1,norx
          uqrid( i,j, ipanel ) = (ipanel-1) * norx * nory + (j-1)*norx + i
       end do
       end do

       do j=1,nory
       do i=1,norx
          alp0(i,j) = (PI/4.)*(xs(i)-dcube2)/dcube2
          bet0(i,j) = (PI/4.)*(ys(j)-dcube2)/dcube2
       end do
       end do
       do j=1,nory
       do i=1,norx
          alp1(i,j) =  alp0(i,j) + 0.01*SIN( ANGLX(i,j,ipanel)*PI/180. )   
          bet1(i,j) =  bet0(i,j) + 0.01*COS( ANGLX(i,j,ipanel)*PI/180. )   
       end do
       end do

       do j=1,nory
       do i=1,norx
          call CubedSphereRLLFromABP(alp0(i,j), bet0(i,j) , ipanel, lon_r8, lat_r8)
          lon0(i,j,ipanel) = lon_r8
          lat0(i,j,ipanel) = lat_r8
          call CubedSphereRLLFromABP(alp1(i,j), bet1(i,j) , ipanel, lon_r8, lat_r8)
          lon1(i,j,ipanel) = lon_r8
          lat1(i,j,ipanel) = lat_r8
       end do
       end do


      do j=1,nory
       do i=1,norx
          dx = COS( lat0(i,j,ipanel))*(lon1(i,j,ipanel)-lon0(i,j,ipanel))
          dy = (lat1(i,j,ipanel)-lat0(i,j,ipanel))
          if ( dx < 0.0 ) dy  = -1.*dy  ! 
          COSLL  = dy /sqrt( dx**2 + dy**2 )
          ANGLL(i,j,ipanel) = ACOS( COSLL )*180./PI
       end do
       end do



      write(*,*) " Finished with Panel = ",   ipanel

    end do




#if 1

    ofile$ = "TEST.dat"
    OPEN (unit = 31, file= trim(ofile$) ,form="UNFORMATTED" )

    write(31) ncube_halo
    write(31) xv,yv
    write(31) terr_halo_r4
    write(31) terr_dev_halo_r4
   


write(31) size(anglx,1),size(anglx,2)
write(31) xs,ys
write(31) mxvrx
write(31) bsvar
write(31) mxdis
write(31) anglx
write(31) aniso
write(31) mnslp
write(31) angll
write(31) xspk
write(31) yspk
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

write(31) uqrid

   CLOSE(31)
!!!STOP
#endif

 





end subroutine find_ridges
!----------------------------------------------------------

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine ANISO_ANA( AA,AARAW,X,Y,N,NSB,NSW,IP)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  INTEGER,            intent(IN)  ::  N,NSB,NSW,IP
  REAL,               intent(IN)  ::  AA(N,N),X(N),Y(N),AARAW(N,N)


  real, allocatable:: SUBA(:,:), SUBARW(:,:),SUBX(:),SUBY(:),RT(:,:),RTX(:),XRT(:),RTXMN(:),RTXSLP(:) &
                     , PKLC(:), RTY(:),RTRW(:,:),RTRWX(:)

  logical,allocatable :: lhgts(:)
  logical :: Keep_Cuestas=.true.
  

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



#if 1

  n2=n/2
  mini=nsw/nsb+1
  maxi=N/nsb-nsw/nsb   !+1
  minj=nsw/nsb+1
  maxj=N/nsb-nsw/nsb   !+1

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! check ranges for orientation analysis
  !***************************************************
     write(*,*) "CHECKING RANGES in topoana ", nsb,nsw
     write(*,*) "  N,nsb,nsw ", N,nsb,nsw


    if (mini*nsb-nsw+1<1) then 
       write(*,*) "fixing bad mini ", mini*nsb-nsw+1, mini
       mini=mini+1
       if (mini*nsb-nsw+1<1) then 
          write(*,*) "irreparable mini ", mini*nsb-nsw+1, mini
          STOP
       endif
    endif
    if (maxi*nsb+nsw>N) then 
       write(*,*) "fixing bad maxi ", maxi*nsb+nsw , maxi
       maxi=maxi-1
       if (maxi*nsb+nsw>N) then 
          write(*,*) "irrep. maxi ", maxi*nsb+nsw , maxi
          STOP 
      endif
    endif
    if (minj*nsb-nsw+1<1) then 
       write(*,*) "fixing bad minj ", minj*nsb-nsw+1
       minj=minj+1
       if (minj*nsb-nsw+1<1) then 
          write(*,*) "irrep. minj ", minj*nsb-nsw+1
          STOP
       endif
    endif
    if (maxj*nsb+nsw>N) then 
       write(*,*) "fixing bad maxj ", maxj*nsb+nsw , maxi
       maxj=maxj-1
       if (maxj*nsb+nsw>N) then 
          write(*,*) "irrep. maxj ", maxj*nsb+nsw , maxi
          STOP
       endif
    endif

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


  do i=1,nsw
     xrt(i) = 1.*(i-1)
  end do
  xmn=0.5*(xrt(1)+xrt(nsw))
  xvr=sum( (xrt-xmn)**2 )/nsw

  do j=minj,maxj
     suby = Y(j*nsb-nsw+1:j*nsb+nsw )
     ys(j)= sum( suby )/size(suby,1)
     do i=mini,maxi
        subx = x(i*nsb-nsw+1:i*nsb+nsw )
        xs(i)= sum( subx )/size(subx,1)
        suba = AA( i*nsb-nsw+1:i*nsb+nsw ,  j*nsb-nsw+1:j*nsb+nsw )

        subarw = AARAW( i*nsb-nsw+1:i*nsb+nsw ,  j*nsb-nsw+1:j*nsb+nsw )

        basmn  =  sum( sum( suba(ns0:ns1-1,ns0:ns1-1) , 1 ), 1) /(( ns1-ns0 )*(ns1-ns0))
        basvar =  sum( sum( (suba(ns0:ns1-1,ns0:ns1-1)-basmn)**2 , 1 ), 1) /(( ns1-ns0 )*(ns1-ns0))
        bsvar(i,j,ip) = basvar

        vvaa(:)=0
        qual(:)=0.
        npkx(:)=0.
        do l=1,nang
                                           ! Rotate 2D topography by ANG
           ang = (L-1)*(180./nang)
           rt  = rotby2( suba, 2*nsw , ang )
           rtrw  = rotby2( subarw, 2*nsw , ang )  ! Raw topo rotation

                                           ! Take "Y" (and "X")-average of rotated topography.
                                           ! Yields topo profile in X ==> RTX
           rtx = sum( rt(ns0:ns1-1,ns0:ns1-1) , 2 ) /( ns1-ns0 ) ! Y-average 
           rty = sum( rt(ns0:ns1-1,ns0:ns1-1) , 1 ) /( ns1-ns0 ) ! X-average
           rtrwx = sum( rtrw(ns0:ns1-1,ns0:ns1-1) , 2 ) /( ns1-ns0 ) ! Y-average of Raw topo
                                           
                                           ! Mean elevation
           mnt = sum( rtx )/( ns1-ns0 ) 
           mn2 = sum( rty )/( ns1-ns0 ) 

           rtxslp(1:nsw-1)=abs(rtx(2:nsw)-rtx(1:nsw-1) )                         
  

                 ! count actual peaks and valleys in RTX cross section
           pklc = 0.
           do i2=2,nsw-1
              if ( ( rtx(i2-1)<rtx(i2) ).and.( rtx(i2+1)<rtx(i2) ) ) pklc(i2)=1.
           end do
           npkx(L)=sum(pklc)

           pklc = 0.
           do i2=2,nsw-1
              if ( ( rtx(i2-1)>rtx(i2) ).and.( rtx(i2+1)>rtx(i2) ) ) pklc(i2)=1.
           end do
           nvlx(L)=sum(pklc)


                ! Record actual max and min elevations in RTX and Raw topo profile (RTRWX)
           pkht(L)=maxval(RTX)
           vldp(L)=minval(RTX)
           rwpk(L)=maxval(RTRWX)
           rwvl(L)=minval(RTRWX)



                  ! Mean slope BETA (and intercept ALPH) of RTX
           beta(L) = sum( (rtx-mnt)*(xrt-xmn) )/(nsw*xvr)           
           alph    = mnt - beta(L)*xmn



           var = sum( (rtx-mnt)**2 )/( ns1-ns0 ) 
           vvaa(L) = var
           dex(L)  = MAXVAL(RTX)-MINVAL(RTX)


           var2 = sum( (rty-mn2)**2 )/( ns1-ns0 ) 
           vva2(L) = var2

           Lhgts = ( ( rtx - minval(rtx) ) > 0.5*maxval( rtx - minval(rtx) ) )

           hwdx(L) = 1.0*count( Lhgts )

#if 0
           if (dex(L) > 1.0) then 
              !hwdx(L) = sum( rtxslp**2  , 1) / (dex(L)**2)            
              hwdx(L) = ( dex(L)**2 ) / sum( rtxslp**2  , 1)            
           else
              hwdx(L) = -1.
           endif
#endif

           ipkh    = MAXLOC( RTX ) ! index of peak height in rotated topo avg cross-section
           xpkh(L) = XRT( ipkh(1) )-xmn


           ift0    = MINLOC( RTX( 1 : ipkh(1) ) )
           ift1    = MINLOC( RTX( ipkh(1) :  ) )+ipkh(1)-1

           dex0(L)  = MAXVAL(RTX)-MINVAL( RTX( 1 : ipkh(1) ) )
           dex1(L)  = MAXVAL(RTX)-MINVAL( RTX( ipkh(1) :  ) )

           xft0(L)  =  XRT( ift0(1) ) -XRT( ipkh(1) )
           xft1(L)  =  XRT( ift1(1) ) -XRT( ipkh(1) )

           rotmn  =  sum( sum( rt(ns0:ns1-1,ns0:ns1-1) , 1 ), 1) /(( ns1-ns0 )*(ns1-ns0))
           rotvar =  sum( sum( (rt(ns0:ns1-1,ns0:ns1-1)-rotmn)**2 , 1 ), 1) /(( ns1-ns0 )*(ns1-ns0))
           if (rotvar>0.) qual(L) = var/rotvar
           if (rotvar>0.) vvaa(L) = vvaa(L)*(basvar/rotvar)
           if (rotvar>0.) vva2(L) = vva2(L)*(basvar/rotvar)


          ! rtdx2(ns0:ns1-1,ns0:ns1-1)  =  4*rt(ns0:ns1-1,ns0:ns1-1) - ( rt(ns0-1:ns1-2,ns0:ns1-1)+  rt(ns0+1:ns1,ns0:ns1-1) ) &
          !           - ( rt(ns0:ns1-1,ns0-1:ns1-2) + rt(ns0:ns1-1,ns0+1:ns1) )

#if 1
           if ( dex(L)>500.  )  then
                
                rotvar = rotvar * 1.00
                
           end if
#endif



        end do ! LOOP over angles



        iorn       = MAXLOC( vvaa )

        mxvrx(i,j,ip) = vvaa( iorn(1) ) !MAXVAL( vvaa )
        mxvry(i,j,ip) = vva2( iorn(1) ) !MAXVAL( vvaa )
        mxdis(i,j,ip) = dex( iorn(1) )
        hwdth(i,j,ip) = hwdx( iorn(1) )
        npks(i,j,ip)  = npkx( iorn(1) )
        nvls(i,j,ip)  = nvlx( iorn(1) )
        aniso(i,j,ip) = qual( iorn(1) )
        anglx(i,j,ip) = (iorn(1)-1)*(180./nang)
        mnslp(i,j,ip) = beta( iorn(1) )

        pkhts(i,j,ip) = pkht( iorn(1) )
        vldps(i,j,ip) = vldp( iorn(1) )
 
        rwpks(i,j,ip) = rwpk( iorn(1) )
        rwvls(i,j,ip) = rwvl( iorn(1) )

        ang00      =  (iorn(1)-1)*(180./nang)*(PI/180.)

        xspk(i,j,ip)  = xs(i)  + xpkh(iorn(1))*cos( ang00 )
        yspk(i,j,ip)  = ys(j)  - xpkh(iorn(1))*sin( ang00 )

        mxds0(i,j,ip) = dex0( iorn(1) )
        mxds1(i,j,ip) = dex1( iorn(1) )
        sft0(i,j,ip)  = xft0( iorn(1) )
        sft1(i,j,ip)  = xft1( iorn(1) )




     end do
         write(*,*) " J", j," out of ", N/nsb
  end do

if ( .not.(KEEP_CUESTAS)) then
! Clean up "goofy" results for inclined planes
  where ( (nvls == 0) .and. (npks == 0) )
        mxdis = 0.
        aniso = 0.
        mxvrx = 0.
        mxvry = 0.
  endwhere
else
  write(*,*) " Keeping goofy 'Cuestas' "
endif




  deallocate( suba )
  deallocate( rt )
  deallocate( subx )
  deallocate( suby )
  deallocate( rtx )
  deallocate( rty )

#endif

aniso(:,:,ip)=ip*1.0

write(*,*) "Got though ANISO ANA"

end subroutine ANISO_ANA

!====================================
   subroutine remapridge2target(area_target,weights_eul_index_all,weights_lgr_index_all,weights_all,ncube,jall,&
         nreconstruction,ntarget,nhalo,nsb)
      use shr_kind_mod, only: r8 => shr_kind_r8
      use remap
      implicit none
      real(r8), intent(in) :: weights_all(jall,nreconstruction)
      integer , intent(in) :: weights_eul_index_all(jall,3),weights_lgr_index_all(jall)
      integer , intent(in) :: ncube,jall,nreconstruction,ntarget,nhalo,nsb
      real(r8), intent(in) :: area_target(ntarget)
      real(r8):: f(ntarget)
  
      REAL  ,                                                          &
         DIMENSION(1-nhalo:ncube+nhalo )                          :: xv,yv,alph,beta
      
      integer :: alloc_error

      integer :: i,ix,iy,ip,ii,counti,norx,nory,i_last,isubr
      real(r8):: wt
      real(KIND=dbl_kind), dimension(1-nhalo:ncube+nhalo,1-nhalo:ncube+nhalo ,6) :: tmpx6
      real(KIND=dbl_kind), dimension(ncube*ncube*6) :: mxdisC , anglxC

!----------------------------------------------------------------------------------------------------

    DO i=1-nhalo,ncube+nhalo
       xv(i)=1.*i
       yv(i)=1.*i
    END DO
    DO i=1-nhalo,ncube+nhalo
       alph(i)=(pi/4.)*(1.*i - 0.5 + nhalo - (ncube+2.*nhalo-1)/2.) / ((ncube+2.*nhalo-1)/2.)
       beta(i)=(pi/4.)*(1.*i - 0.5 + nhalo - (ncube+2.*nhalo-1)/2.) / ((ncube+2.*nhalo-1)/2.)
    END DO
      
    write(*,*) "check weights INSIDE ridge ana " 
    write(*,*) weights_lgr_index_all(781202)
    write(*,*) weights_eul_index_all(781202,:)


    allocate (wghts_target(ntarget,nsubr),stat=alloc_error )
    if( alloc_error /= 0 ) then; print*,'Program could not allocate space for wghts_target'; stop; endif
    wghts_target = 0.
    allocate (mxdis_target(ntarget,nsubr),stat=alloc_error )
    if( alloc_error /= 0 ) then; print*,'Program could not allocate space for mxdis_target'; stop; endif
    mxdis_target = 0.
    allocate (anglx_target(ntarget,nsubr),stat=alloc_error )
    if( alloc_error /= 0 ) then; print*,'Program could not allocate space for anglx_target'; stop; endif
    anglx_target = 0.

     norx=size(mxdis,1)
     nory=size(mxdis,2)

     tmpx6 = mapridge2cube ( mxdis , norx, nory,xs,ys,xv,yv,ncube,nhalo,nsb )
     mxdisC = reshape( tmpx6(1:ncube, 1:ncube, 1:6 ) , (/ncube*ncube*6/) )
     tmpx6 = mapridge2cube ( anglx , norx, nory,xs,ys,xv,yv,ncube,nhalo,nsb )
     anglxC = reshape( tmpx6(1:ncube, 1:ncube, 1:6 ) , (/ncube*ncube*6/) )


       !mxdis_target = remap_field(tmp,area_target,weights_eul_index_all(1:jall,:),weights_lgr_index_all(1:jall),&
       !weights_all(1:jall,:),ncube,jall,nreconstruction,ntarget)





    i_last = -9999
!++jtb 
! 
!      In the following loop "count" is the index of a piece of 
!      the "exchange grid" - created by cutting the cubed-sphere topo
!      and target grid into each other.  
    do counti=1,jall
     
      i    = weights_lgr_index_all(counti)
      !if( itracker_target(i) == 0 ) then ! If true this is first time target cell i has been visited
      !   itracker_target(i)=1
      !end if

      ix  = weights_eul_index_all(counti,1)
      iy  = weights_eul_index_all(counti,2)
      ip  = weights_eul_index_all(counti,3)
      !
      ! convert to 1D indexing of cubed-sphere
      !
      ii = (ip-1)*ncube*ncube+(iy-1)*ncube+ix
      wt = weights_all(counti,1)

      isubr = INT( anglxC(ii) * nsubr/180. ) + 1

      if ( (isubr >= 1).and.(isubr <= nsubr) ) then
      wghts_target( i , isubr ) = wghts_target( i , isubr ) + wt
      !!hwdth_target( i , isubr ) = hwdth_target( i , isubr ) + wt*hwdth(ii)
      !!mxvrx_target( i , isubr ) = mxvrx_target( i , isubr ) + wt*mxvrx(ii)
      !!mxvry_target( i , isubr ) = mxvry_target( i , isubr ) + wt*mxvry(ii)
      !!bsvar_target( i , isubr ) = bsvar_target( i , isubr ) + wt*bsvar(ii)
      mxdis_target( i , isubr ) = mxdis_target( i , isubr ) + wt*mxdisC(ii)
      !!aniso_target( i , isubr ) = aniso_target( i , isubr ) + wt*aniso(ii)
      anglx_target( i , isubr ) = anglx_target( i , isubr ) + wt*anglxC(ii)
      endif

      i_last = i
    end do       





    where( wghts_target > 1.e-15 )
        !aniso_target = aniso_target / wghts_target
        anglx_target = anglx_target / wghts_target
        mxdis_target = mxdis_target / wghts_target
        !hwdth_target = hwdth_target / wghts_target
        !mxvrx_target = mxvrx_target / wghts_target
        !mxvry_target = mxvry_target / wghts_target
        !bsvar_target = bsvar_target / wghts_target
     elsewhere
        !aniso_target = 0.
        anglx_target = -9000.
        mxdis_target = -999.
        !hwdth_target = 0.
        !mxvrx_target = 0.
        !mxvry_target = 0.
        !bsvar_target = 0.
     end where





      !real(r8), intent(in) :: weights_all(jall,nreconstruction)
      !integer , intent(in) :: weights_eul_index_all(jall,3),weights_lgr_index_all(jall)
write(911) jall,nreconstruction
write(911)   weights_eul_index_all, weights_lgr_index_all
write(911)  weights_all

write(911) ncube
write(911) mxdisC,anglxC

write(911) ntarget,nsubr
write(911) mxdis_target
write(911) anglx_target
write(911) wghts_target


      write(*,*) " GOT OUT OF remapridge2target "

 
   end subroutine remapridge2target
!======================================

function mapridge2cube ( a, norx, nory,xs,ys,xv,yv,ncube,nhalo,nsb ) result( axc )
   
       integer, intent(in) :: norx,nory,ncube,nhalo,nsb
       real, intent(in), dimension(norx,nory,6) :: a
       real, intent(in), dimension(norx) :: xs
       real, intent(in), dimension(nory) :: ys
       real, intent(in), DIMENSION(1-nhalo:ncube+nhalo ) :: xv,yv

       real(KIND=dbl_kind), dimension(1-nhalo:ncube+nhalo,1-nhalo:ncube+nhalo ,6) :: axc
       integer:: i,j,x0,x1,y0,y1,ip
!---------------------------------------------------

     do ip=1,6
       do j=1,nory
          y0 = int( ys(j) )+1 - NSB/2. 
          y1 = int( ys(j) )  +  NSB/2. 
          do i=1,norx
             x0 = int( xs(i) )+1 - NSB/2. 
             x1 = int( xs(i) )  +  NSB/2.
             if( (x0>-900).and.(x1>-900).and.(y0>-900).and.(y1>-900) ) then
                AXC( x0:x1, y0:y1, ip ) = A( i , j , ip )
             endif
          end do
       end do
     end do

end function mapridge2cube



end module ridge_ana
