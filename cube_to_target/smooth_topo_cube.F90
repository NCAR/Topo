!#define idealized_test
!-----------------------------------------------------------------------------
! MODULE subgrid_topo_ana
!
! Purpose:
!
!      Date       Programmer       Affiliation          Description of change
!      ====       ==========       ===========          =====================
!
!-----------------------------------------------------------------------------
MODULE smooth_topo_cube_sph
  USE reconstruct
  use shr_kind_mod, only: r8 => shr_kind_r8
IMPLICIT NONE
PRIVATE

PUBLIC smooth_intermediate_topo_wrap

CONTAINS

!=============================================================================
!=============================================================================
  SUBROUTINE smooth_intermediate_topo_wrap(terr, rrfac, da &
                                    , ncube,nhalo, NSCL_f,NSCL_c &
                                    , terr_sm,terr_dev,ofname  & 
                                    , lread_smooth_topofile  &
                                    , ldistance_weighted_smoother&
                                    , luse_prefilter &
                                    , lstop_after_smoothing & 
                                    , lregional_refinement &
                                    , rrfac_max &
                                    , ldevelopment_diags &
                                    , command_line_arguments&
                                    , str_dir, str_source, ogrid& 
                                    , nu_lap, smooth_phis_numcycle,landfrac&
                                    , lsmoothing_over_ocean,lsmooth_rrfac&
                                    , smooth_topo_fname)

    use shared_vars, only: progress_bar
    implicit none
    real(r8), PARAMETER :: pi        = 3.14159265358979323846264338327
    
    integer, INTENT(IN) :: ncube, nhalo,NSCL_f,NSCL_c
    
    real(r8), DIMENSION(ncube,ncube,6), INTENT(INOUT) :: terr
    real(r8), DIMENSION(ncube,ncube),   INTENT(IN)    :: da
    real(r8), DIMENSION(ncube,ncube,6), INTENT(OUT)   :: terr_dev
    real(r8), DIMENSION(ncube,ncube,6), INTENT(OUT)   :: terr_sm
    real(r8), DIMENSION(ncube,ncube,6), INTENT(INOUT) :: rrfac
    LOGICAL, INTENT(IN)  :: lread_smooth_topofile    ! , lsmooth_topo_cubesph
    LOGICAL, INTENT(IN)  :: luse_prefilter, lstop_after_smoothing, ldistance_weighted_smoother
    LOGICAL, INTENT(IN)  :: lregional_refinement, ldevelopment_diags
    CHARACTER(len=1024), INTENT(IN   )           :: str_dir, str_source, ogrid
    CHARACTER(len=1024), INTENT(INOUT)           :: ofname
    character(len=1024), INTENT(IN   )           :: command_line_arguments !for writing netCDF file
    real(r8), intent(in)             :: nu_lap
    integer, intent(in)                          :: smooth_phis_numcycle
    real(r8), DIMENSION(ncube,ncube,6), INTENT(IN) :: landfrac
    logical, intent(in)                          :: lsmoothing_over_ocean, lsmooth_rrfac
    CHARACTER(len=1024), INTENT(IN   ), optional :: smooth_topo_fname


    integer, INTENT(IN)  :: rrfac_max

    real(r8), DIMENSION(1-nhalo:ncube+nhalo, 1-nhalo:ncube+nhalo, 6) :: terr_halo
    real(r8), DIMENSION(1-nhalo:ncube+nhalo, 1-nhalo:ncube+nhalo, 6) :: terr_halo_sm, terr_halo_dev
    real(r8), DIMENSION(1-nhalo:ncube+nhalo, 1-nhalo:ncube+nhalo, 6) :: da_halo, rr_halo, rr_halo_sm
    real(r8), DIMENSION(ncube,ncube,6)                               :: daxx, rrfac_sm, lap
    real(r8), DIMENSION(ncube, ncube, 6)                             :: terr_sm00,terr_dev00,rr_updt

    integer  :: ncubex, nhalox,NSCL_fx,NSCL_cx,ip
    real(r8) :: volterr_in,volterr_sm
     

    integer  :: ncube_in_file, iter

    logical ::     read_in_precomputed, use_prefilter, stop_after_smoothing
    logical ::     smooth_topo_cubesph, do_refine
    logical ::     read_in_and_refine, new_smooth_topo

    real(r8), parameter :: rearth = 6.37122e6 !radius of Earth from CIME/CESM
    real(r8)            :: nu_lap_unit_sphere,dt
    real(r8)            :: min_terr, max_terr   !to check if Laplacian smoother is stable
    real(r8)            :: min_rrfac, max_rrfac !to check if Laplacian smoother is stable
    !read_in_precomputed = .FALSE.
    read_in_precomputed = lread_smooth_topofile  !.TRUE.
    use_prefilter = luse_prefilter 
    stop_after_smoothing = lstop_after_smoothing 
    smooth_topo_cubesph = .TRUE.  
    read_in_and_refine=.FALSE.

    IF (read_in_precomputed) then
      write(*,*) " Read precomputed filtered topography from ",trim(smooth_topo_fname)
      if (lregional_refinement) then
!check can not overwrite rrfac - I think we need merge from Julio
        !        call read_topo_smooth_data(smooth_topo_fname,ncube*ncube*6,terr_sm,terr_dev,rr_fac=rrfac)
        call read_topo_smooth_data(smooth_topo_fname,ncube*ncube*6,terr_sm,terr_dev)
      else
        call read_topo_smooth_data(smooth_topo_fname,ncube*ncube*6,terr_sm,terr_dev)
      end if
      ! return to main program after
      ! reading topography variables
      if (.NOT. lregional_refinement) then 
        RETURN
      else
        read_in_and_refine=.TRUE.
      end if   
      terr_sm00  = terr_sm
      terr_dev00 = terr_dev
    ELSE
      terr_sm00  = 0.
      terr_dev00 = 0.
    ENDIF

     ! If your are here and read_in_and_refine=.FALSE. then
     ! then you must want to generate a new smooth topo.  So 
     ! assign logical new_smooth_topo here. This could be 
     ! done in subr. smooth_intermediate_topo_halo, but code
     ! is more readable if done here and passed in.
     !---------------------------------------
     new_smooth_topo = .NOT.(read_in_and_refine)

     if ( ldevelopment_diags.AND.(NSCL_f>0) ) then
       write( ofname , &
            "('_nc',i0.4,'_Co',i0.3,'_Fi',i0.3)" ) & 
            ncube, NSCL_c/2, NSCL_f/2
       ofname = 'topo_smooth_'//trim(str_source)//trim(ofname)
     else
       write( ofname , &
            "('_nc',i0.4,'_Co',i0.3)" ) & 
            ncube, NSCL_c/2
       ofname = 'topo_smooth_'//trim(str_source)//trim(ofname)
     end if
     
     if (lregional_refinement) then
       ofname= TRIM(str_dir)//'/'//trim(ofname)//'_'//trim(ogrid)//'.nc'
     else
       ofname= TRIM(str_dir)//'/'//trim(ofname)//'.nc'
     end if
     
     write(*,*) " Will do smoothing of topo on cubed sphere "
     write(*,*) " Output will go to: ",trim(ofname)
     
    ! Smooth cubed sphere topography
    !--------------------------------------
    if (ldistance_weighted_smoother) then
      if (.not.lsmoothing_over_ocean) then
        write(*,*) "Not smoothing over ocean not supported for distance weighted smoother"
        write(*,*) "(add -m option)"
        write(*,*) "ABORT"
        stop
      end if
      write(*,*) " Smoothing parameters : ",NSCL_f,NSCL_c

      !terr_in = terr
      DO ip = 1, 6
        daxx(:,:,ip) = da
      end do

      terr_sm = 0.
      terr_dev = 0.

         DO ip = 1, 6
            CALL CubedSphereFillHalo_Linear_extended(terr,  terr_halo(:,:,ip), ip, ncube+1,nhalo)  
            CALL CubedSphereFillHalo_Linear_extended(daxx,  da_halo(:,:,ip),   ip, ncube+1,nhalo)
            CALL CubedSphereFillHalo_Linear_extended(rrfac, rr_halo(:,:,ip),   ip, ncube+1,nhalo)  
         end DO
                    write(*,*) " MINVAL(abs(rr_halo) ) , MAXVAL(abs(rr_halo) ) "
                    write(*,*) MINVAL(abs(rr_halo) ) , MAXVAL(abs(rr_halo) )
                    write(*,*) " MINVAL(abs(rrfac) ) , MAXVAL(abs(rrfac) ) "
                    write(*,*) MINVAL(abs(rrfac) ) , MAXVAL(abs(rrfac) )
 
         if (lregional_refinement) then
            call smooth_rrfac_halo(rr_halo, rrfac_max, ncube,nhalo, NSCL_c, rr_halo_sm  ) 
            rr_halo = rr_halo_sm
            rr_updt = rr_halo( 1:ncube , 1:ncube, :) 
            rrfac   = rr_updt
         end if

         if (use_prefilter) then
            call smooth_intermediate_topo_halo(terr_halo, da_halo,rr_halo, ncube,nhalo, 1,NSCL_f &
                                         , terr_halo_sm,  read_in_and_refine, new_smooth_topo  ) 
            terr_halo = terr_halo_sm
         else
            write(*,*) " No prefilter in smooth_topo !!!!!! "
         end if
         call smooth_intermediate_topo_halo(terr_halo, da_halo, rr_halo, ncube,nhalo, 1,NSCL_c &
                                     , terr_halo_sm,  read_in_and_refine, new_smooth_topo )

         
         terr_sm( 1:ncube , 1:ncube, :)  = terr_halo_sm( 1:ncube , 1:ncube, :) 
         terr_dev( 1:ncube , 1:ncube, :) = terr_halo( 1:ncube , 1:ncube, :) -  terr_halo_sm( 1:ncube , 1:ncube, :) 
    
         ! 
         if (read_in_and_refine) then
            where( rr_updt <= 1. )
               terr_sm   =  terr_sm00
               terr_dev  =  terr_dev00
            end where
         end if
       else
         !
         ! sanity check
         !
         if (nu_lap<0) then
           write(*,*) "nu_lap must be positive; nu_lap=",nu_lap
           stop
         end if
         if (smooth_phis_numcycle<0) then
           write(*,*) "smooth_phis_numcycle must be >=0; smooth_phis_numcycle=",smooth_phis_numcycle
           stop
         end if
         !
         ! Laplacian smoothing
         !
         nu_lap_unit_sphere = nu_lap/(rearth*rearth)

         if (lregional_refinement.and.lsmooth_rrfac) then
           max_rrfac = MAXVAL(rrfac)
           min_rrfac = MINVAL(rrfac)
           rrfac_sm = rrfac
           dt = 16.0/real(smooth_phis_numcycle)
           do iter = 1,smooth_phis_numcycle
             call progress_bar("# ", iter, DBLE(100*iter)/DBLE(smooth_phis_numcycle))
             call laplacian(rrfac_sm, ncube, lap, landfrac,.false.)
             rrfac_sm = rrfac_sm+lap*dt*nu_lap_unit_sphere
             if (MAXVAL(rrfac_sm)>1.2*max_rrfac.or.MINVAL(rrfac_sm)<min_rrfac-1.0) then
               write(*,*) "Laplace iteration seems to be unstable: MINVAL(rrfac_sm),MAXVAL(rrfac_sm)",&
                    MINVAL(rrfac_sm),MAXVAL(rrfac_sm)
               stop
             end if
           end do
           rrfac = rrfac_sm
         end if
         !
         ! smooth surface height
         !         
         rrfac_sm = (1.0/rrfac)**2 !scaling of smoothing coefficient
         max_terr = MAXVAL(terr)
         min_terr = MINVAL(terr)
         terr_sm = terr
         dt = 16.0/real(smooth_phis_numcycle)
         do iter = 1,smooth_phis_numcycle
           call progress_bar("# ", iter, DBLE(100*iter)/DBLE(smooth_phis_numcycle))
           call laplacian(terr_sm, ncube, lap, landfrac,lsmoothing_over_ocean)
           terr_sm = terr_sm+lap*dt*nu_lap_unit_sphere*rrfac_sm
           if (MAXVAL(terr_sm)>1.2*max_terr.or.MINVAL(terr_sm)<min_terr-500.0) then
             write(*,*) "Laplace iteration seems to be unstable: MINVAL(terr_sm),MAXVAL(terr_sm)",MINVAL(terr_sm),MAXVAL(terr_sm)
             stop
           end if
         end do
         terr_dev( 1:ncube , 1:ncube, :) = terr_halo( 1:ncube , 1:ncube, :) -  terr_sm( 1:ncube , 1:ncube, :) 
       endif
      volterr_in=0.
      volterr_sm=0.
      do ip=1,6 
         volterr_in =  volterr_in + sum( terr   (:,:,ip) * da )
         volterr_sm =  volterr_sm + sum( terr_sm(:,:,ip) * da )
      end do
      write(*,*) " Topo volume BEFORE smoother = ",volterr_in/(6*sum(da))
      write(*,*) " Topo volume  AFTER smoother = ",volterr_sm/(6*sum(da))
      write(*,*) "            Difference       = ",(volterr_in - volterr_sm)/(6*sum(da))

      if (stop_after_smoothing .OR. ldevelopment_diags) then
        if (lregional_refinement) then
          call wrtncdf_topo_smooth_data(ncube,ncube*ncube*6,terr_sm,terr_dev,ofname,command_line_arguments,rr_fac=rr_updt)
        else
          call wrtncdf_topo_smooth_data(ncube,ncube*ncube*6,terr_sm,terr_dev,ofname,command_line_arguments)
        end if
        if (stop_after_smoothing ) STOP
      end if
  end SUBROUTINE smooth_intermediate_topo_wrap

  subroutine laplacian(terr, ncube, lap, landfrac, lsmoothing_over_ocean)
    real(r8), dimension(ncube,ncube,6), intent(in)  :: terr
    integer,                                        intent(in)  :: ncube
    real(r8), dimension(ncube,ncube,6), intent(out) :: lap
    real(r8), dimension(ncube,ncube,6), intent(in)  :: landfrac
    logical,                                        intent(in)  :: lsmoothing_over_ocean

    integer :: i,j,ip

    integer, parameter :: nhalo=2

    real(r8), dimension(ncube,ncube,6)         :: landfrac_local
    real(r8), dimension(0:ncube+1,0:ncube+1)   :: ggaa, ggbb, ggab, ggba, sqgg !metric terms cell centers
    real(r8), dimension(0:ncube+1,0:ncube+1)   :: ggaa_e, ggbb_e, ggab_e, ggba_e, sqgg_e!metric terms edge
    real(r8), dimension(1-nhalo:ncube+nhalo,1-nhalo:ncube+nhalo,6) :: terr_halo
    real(r8), dimension(0:ncube+2,0:ncube+2)   :: terr_halo_edgeX, terr_halo_edgeY
    real(r8)                                                       :: alph, beta
    real(r8)                                                       :: da,irho,irhosq,piq,pi
    real(r8)                                                       :: inv_da,factor
    real(r8)                                                       :: lap_x,lap_y,lap_xy,lap_yx


    real(r8), dimension(0:ncube+2,0:ncube+2) :: xterm, yterm, xterm_edgeX, xterm_edgeY
    real(r8) :: aa,bb,cc,dd,det
#ifdef idealized_test
    real(r8), dimension(ncube,ncube,6)  :: exact
    call idealized_lap(exact,ncube)
#endif

    if (.not.lsmoothing_over_ocean) then
      landfrac_local = landfrac
    else
      landfrac_local = 1.0
    end if
#ifdef idealized_test
    landfrac_local = 1.0
#endif

    piq = DATAN(1.D0)
    pi  = 4.D0*piq
    da  = 0.5D0*pi/DBLE(ncube)
    inv_da = 1.0D0/da
    !
    ! metrics in center of cells: eq (C3) in Nair et al. 2005
    ! https://doi.org/10.1175/MWR2890.1
    !
    ! remember to use inverse metric!
    !
    DO j=0,ncube+1
      DO i=0,ncube+1
        alph = -piq+(DBLE(i)-0.5)*da
        beta = -piq+(DBLE(j)-0.5)*da
        
        irhosq = 1.0D0 + tan(alph)*tan(alph)+tan(beta)*tan(beta)
        irho   = sqrt(irhosq)
        
        factor = irhosq*irhosq*cos(alph)*cos(alph)*cos(beta)*cos(beta)
        factor = 1.0D0/factor
        
        aa =  factor*(1.0D0 + tan(alph)*tan(alph))
        dd =  factor*(1.0D0 + tan(beta)*tan(beta))
        bb = -factor*tan(alph)*tan(beta)
        cc = bb
        
        det = aa*dd-bb*cc
        sqgg(i,j) = sqrt(det)
        det = 1.0/det
        
        ggaa(i,j) =  det*dd 
        ggab(i,j) = -det*bb
        ggba(i,j) = -det*cc
        ggbb(i,j) =  det*aa
      END DO
    END DO
    !
    ! Same as above but for edge of cells
    !
    DO j=0,ncube+1
      DO i=0,ncube+1
        alph = -piq+DBLE((i-1))*da
        beta = -piq+DBLE((j-1))*da
        
        irhosq = 1.0D0 + tan(alph)*tan(alph)+tan(beta)*tan(beta)
        irho   = sqrt(irhosq)
        
        factor = irhosq*irhosq*cos(alph)*cos(alph)*cos(beta)*cos(beta)
        factor = 1.0D0/factor
        
        aa =  factor*(1.0D0 + tan(alph)*tan(alph))
        dd =  factor*(1.0D0 + tan(beta)*tan(beta))
        bb = -factor*tan(alph)*tan(beta)
        cc = bb

        det = aa*dd-bb*cc
        sqgg_e(i,j) = sqrt(det)
        det = 1.0/det

        ggaa_e(i,j) =  det*dd 
        ggab_e(i,j) = -det*bb
        ggba_e(i,j) = -det*cc
        ggbb_e(i,j) =  det*aa
      END DO
    END DO

    do ip=1,6
      call CubedSphereFillHalo_Cubic(terr, terr_halo, ip, ncube+1)
    end do
    !
    ! Laplacian
    !
    ! Equation (3) in
    ! 
    ! "Diffusion Experiments with a Global Discontinuous Galerkin Shallow-Water Model"
    ! Nair, MWR, 2009
    !
    do ip=1,6
      do j=1,ncube
        do i=1,ncube
          if (landfrac_local(i,j,ip) > 0) then
            
            lap_x  = (terr_halo(i+1,j,ip)-terr_halo(i  ,j,ip))*sqgg_e(i+1,j)*ggaa_e(i+1,j)  !x-derivative on edge i+1/2
            lap_x  = lap_x -&
                 (terr_halo(i  ,j,ip)-terr_halo(i-1,j,ip))*sqgg_e(i  ,j)*ggaa_e(i  ,j)  !x-derivative on edge i-1/2
            lap_x  = lap_x*inv_da*inv_da
            
            lap_xy = (terr_halo(i+1,j+1,ip)-terr_halo(i+1,j-1,ip))*sqgg(i+1,j)*ggab(i+1,j) !centered finite difference
            lap_xy = lap_xy - &
                 (terr_halo(i-1,j+1,ip)-terr_halo(i-1,j-1,ip))*sqgg(i-1,j)*ggab(i-1,j) !centered finite difference
            lap_xy = lap_xy*0.25*inv_da*inv_da
            
            lap_y  = (terr_halo(i,j+1,ip)-terr_halo(i,j  ,ip))*sqgg_e(i,j+1)*ggbb_e(i,j+1) !y-derivative on edge j+1/2
            lap_y  = lap_y -&
                 (terr_halo(i,j  ,ip)-terr_halo(i,j-1,ip))*sqgg_e(i,j  )*ggbb_e(i  ,j) !y-derivative on edge j-1/2
            lap_y  = lap_y*inv_da*inv_da
            
            lap_yx = (terr_halo(i+1,j+1,ip)-terr_halo(i-1,j+1,ip))*sqgg(i,j+1)*ggba(i,j+1) !centered finite difference
            lap_yx = lap_yx - &
                 (terr_halo(i+1,j-1,ip)-terr_halo(i-1,j-1,ip))*sqgg(i,j-1)*ggba(i,j-1) !centered finite difference
            lap_yx = lap_yx*0.25*inv_da*inv_da
            
            lap(i,j,ip) = (lap_x+lap_y+lap_xy+lap_yx)/sqgg(i,j)
          end if
        end do
      end do
    end do

end subroutine laplacian


  subroutine idealized_lap(lap_psi,ncube)
    use shr_kind_mod, only: r8 => shr_kind_r8
    real(r8), intent(out) :: lap_psi(ncube,ncube,6)
    integer, intent(in)   :: ncube

    real(r8) :: piq,da,alpha,beta,lon,lat,psi,grad_lat,grad_lon,xterm,yterm
    integer  :: i,j,ip
    piq = DATAN(1.D0)
    da = 2.0_r8*piq/DBLE(ncube)
    do ip=1,6
      do j=1,ncube
        do i=1,ncube
          alpha = -piq+(i-0.5)*da; beta = -piq+(j-0.5)*da
          call CubedSphereRLLFromABP(alpha, beta, ip, lon, lat)
          psi      = (2.0+cos(lat)*cos(lat)*cos(2.0*lon))!Y22
          grad_lon = -2.0*sin(2.0*lon)
          lap_psi(i,j,ip)   = 2.0*cos(2.0*lon)*(sin(lat)**2-2.0*cos(lat)**2)
          lap_psi(i,j,ip)   = cos(2.0*lon)*(4.0*sin(lat)*sin(lat)-2.0*cos(lat)*cos(lat))-4.0*cos(2.0*lon)
        end do
      end do
    end do
  end subroutine idealized_lap


!=============================================================================
  SUBROUTINE smooth_intermediate_topo_halo(terr_halo, da_halo, rr_halo &
       , ncube,nhalo, NSCL_f,NSCL_c &
       , terr_halo_sm & 
       , read_in_and_refine, new_smooth_topo  ) 
    
    real(r8), PARAMETER :: pi        = 3.14159265358979323846264338327

    integer, INTENT(IN) :: ncube, nhalo,NSCL_f,NSCL_c

    real(r8), DIMENSION(1-nhalo:ncube+nhalo, 1-nhalo:ncube+nhalo, 6), INTENT(IN)  :: terr_halo
    real(r8), DIMENSION(1-nhalo:ncube+nhalo, 1-nhalo:ncube+nhalo, 6), INTENT(IN)  :: da_halo
    real(r8), DIMENSION(1-nhalo:ncube+nhalo, 1-nhalo:ncube+nhalo, 6), INTENT(IN)  :: rr_halo
    real(r8), DIMENSION(1-nhalo:ncube+nhalo, 1-nhalo:ncube+nhalo, 6), INTENT(OUT) :: terr_halo_sm

    LOGICAL, INTENT(IN)  :: read_in_and_refine , new_smooth_topo

    !-----------------------------------------------------------------
    !Internal work arrays
    !-----------------------------------------------------------------
    !------------------------------
    real(r8), DIMENSION(1-nhalo:ncube+nhalo, 1-nhalo:ncube+nhalo):: smwt,ggaa,ggbb,ggab
    real(r8), DIMENSION(1-nhalo:ncube+nhalo )                    :: xv,yv,alph,beta

    integer:: np,i,j, ncube_halo,norx,nory,ipanel,x0,&
         x1,y0,y1,initd,ii0,ii1,jj0,jj1,nctest,NSM,NS2,ismi,NSB,ns2x


    real(r8), allocatable ::  wt1p(:,:),terr_patch(:,:)
    real(r8)  :: cosll, dx, dy ,dbet,dalp,diss,diss00,lon_ij,lat_ij,latfactor,diss0r

    INTEGER :: NOCTV , isx0, isx1, jsy0, jsy1,i2,j2,iix,jjx,i00,ncube_in_file

    real(r8) :: RSM_scl, smoo,irho,volt0,volt1,volume_after,volume_before,wt1ps,diss0e

    CHARACTER(len=1024) :: ofile

    LOGICAL :: do_smooth_ij

    write(*,*) " NCUBE !!! " , ncube

    ncube_halo = size( terr_halo(:,:,1), 1 )

    DO i=1-nhalo,ncube+nhalo
       xv(i)=1.*i
       yv(i)=1.*i
    END DO
    DO i=1-nhalo,ncube+nhalo
       alph(i)=(pi/4.)*(1.*i - 0.5 + nhalo - (ncube+2.*nhalo)/2.) / ((ncube+2.*nhalo)/2.)
       beta(i)=(pi/4.)*(1.*i - 0.5 + nhalo - (ncube+2.*nhalo)/2.) / ((ncube+2.*nhalo)/2.)
    END DO
    DO j=1-nhalo,ncube+nhalo
      DO i=1-nhalo,ncube+nhalo
        irho = ( 1. + (tan(alph(i))**2) + (tan(beta(j))**2 ) )**2   
        irho = 1. / ( ( cos(alph(i))**2 ) * (cos(beta(j))**2) * irho )  
        !irho = 1./ ( ( cos(alph(i))**2)*(cos(beta(j))**2)* ( ( 1. + (tan(alph(i))**2) + (tan(beta(j))**2 ) )**2  ))   ???
        ggaa(i,j) = irho * ( 1. + ( tan( alph(i) ) )**2 )
        ggbb(i,j) = irho * ( 1. + ( tan( beta(j) ) )**2 )
        ggab(i,j) = -irho *( tan( beta(j) ) ) * ( tan( alph(i) ) )
      END DO
    END DO

    terr_halo_sm = terr_halo

    if (NSCL_f  > 1 ) then
      write(*,*)" Fine scale pre-smoother no longer used "
      write(*,*) "Use prefilter instead"
      STOP       
    end if
    
    NSM=NSCL_c
    NS2=NSM/2
    i00 = ncube/2
    ! Set smoothing radius based on namelist param
    dalp   = alph(i00+ns2 )-alph(i00)
    diss00 = 1./sqrt(  ggaa(i00,i00)*dalp*dalp )
    diss0e = 1./sqrt(  ggaa(i00,  1)*dalp*dalp )
    
    write(*,*) " diss0e , diss00 diss0e/diss00 ",diss0e , diss00, diss0e/diss00 
    
    NS2=INT( 1.3 * NSM/2  ) ! Add 30% padding to acommodate compression
    ! of equi-angular grid at panel edges 
    allocate( wt1p(-ns2:ns2, -ns2:ns2 ) )
    allocate( terr_patch(-ns2:ns2, -ns2:ns2 ) )
    
    
    write(*,*) " ncube, nhalo, ns2 ",ncube, nhalo, ns2
    
    write(*,*) "LIMITS in smoother "
    write(*,*) 1-nhalo+ns2,ncube+nhalo-ns2
    
    if (NSCL_c > 1 ) then
      write(*,*) "Direct smoothing option "
      write(*,*) " new_smooth_topo , read_in_and_refine ", new_smooth_topo , read_in_and_refine
      write(*,*) " max RR in panels "
      write(*,*) maxval(rr_halo(:,:,1) ),maxval(rr_halo(:,:,2) ),maxval(rr_halo(:,:,3) ),  & 
           maxval(rr_halo(:,:,4) ),maxval(rr_halo(:,:,5) ),maxval(rr_halo(:,:,6) )
      !-----------------------------------------------------
      ! Smooth topography with Conical kernel.
      !   i,j    :: Central point in smoothing window
      !   i2,j2  :: Distal point in smoothing window
      !   wt1p   :: Is a weighting array dim(-ns2:ns2,-ns2:ns2).
      !   wt1ps  :: Is sum of weights wt1p for normalization
      !   diss00 :: Is distance SQRT(g_aa*da*da) spanned by ns2
      !             cells at center of panel. Sh/Could modify 
      !             to be at i,j?
      !   diss   :: Is distance from (i2,j2) to (i,j)
      !-----------------------------------------------------
      DO np=1,6
        DO j=1-nhalo+ns2,ncube+nhalo-ns2
          DO i=1-nhalo+ns2,ncube+nhalo-ns2

            do_smooth_ij = ( ( (rr_halo(i,j,np)>1.0).AND.read_in_and_refine ).OR. new_smooth_topo )
            
            if (do_smooth_ij) THEN
              !-----------------------------------
              !  Set up conical kernel
              !----------------------------------- 
              wt1p(:,:)=0.
              wt1ps=0.
              ns2x = ns2 /rr_halo( i,j,np )
              do j2=-ns2x,ns2x
                do i2=-ns2x,ns2x
                  jjx = j+j2
                  iix = i+i2
                  dalp = alph(iix)-alph(i)
                  dbet = beta(jjx)-beta(j)
                  diss = ggaa(i,j)*dalp*dalp + ggbb(i,j)*dbet*dbet + 2.*ggab(i,j)*dalp*dbet
                  
                  diss0r = diss00 * rr_halo( i,j,np )
                  wt1p(i2,j2) = ( 1. - diss0r * sqrt( diss ) )*da_halo(iix,jjx,np)
                  
                  if (wt1p(i2,j2)<0.) wt1p(i2,j2)=0.
                  wt1ps = wt1ps + wt1p(i2,j2)
                end do
              end do
              
              !------------------------------
              ! Now smooth over conical kernel
              !-------------------------------
              terr_halo_sm(i,j,np) = 0.
              do j2=-ns2x,ns2x
                do i2=-ns2x,ns2x
                  jjx = j+j2
                  iix = i+i2
                  terr_halo_sm(i,j,np) = terr_halo_sm(i,j,np) + terr_halo(iix,jjx,np)*wt1p(i2,j2)/wt1ps
                end do
              end do
              ! end of smoothing with conical kernel
              !---------------------------------------
              ! Block above is functionally like the one-line smoothing command here:
              !  terr_halo_sm(i,j,np) = SUM( terr_halo_fx( i-ns2:i+ns2  ,  j-ns2:j+ns2   ,np ) )/ ((2.*ns2+1)**2)        
              
            end if

          END DO  !  i-loop   
          if (mod(j,1) ==0 ) write(*,900,advance='no') achar(13), J, Ncube+2*Nhalo, Np
        END DO  ! j-loop
      END DO  ! panel loop
      
      write(*,*) " end / clear "
    end if
    
    
900 format( a1, " Smoothed Row ",i4," out of ",i4," Panel=",i2 )
    
    deallocate( wt1p )
    deallocate( terr_patch )
  END SUBROUTINE smooth_intermediate_topo_halo

!===========================================================


!=============================================================================
SUBROUTINE smooth_rrfac_halo(rr_halo &
                           , rrfac_max, ncube,nhalo, NSCL_c &
                           , rr_halo_sm  ) 

    real(r8), PARAMETER :: pi        = 3.14159265358979323846264338327

    integer, INTENT(IN) :: ncube, nhalo,NSCL_c,rrfac_max

    real(r8), &
            DIMENSION(1-nhalo:ncube+nhalo, 1-nhalo:ncube+nhalo, 6), INTENT(IN)  :: rr_halo
    real(r8), &
            DIMENSION(1-nhalo:ncube+nhalo, 1-nhalo:ncube+nhalo, 6), INTENT(OUT) :: rr_halo_sm


    !-----------------------------------------------------------------
    !Internal work arrays
    !-----------------------------------------------------------------
    !------------------------------
    real(r8), &
            DIMENSION(1-nhalo:ncube+nhalo, 1-nhalo:ncube+nhalo, 6)  :: rr_halo_xx

    
    INTEGER :: NSM,iter,i,j,np , ns
    real(r8) :: wt


      NSM=NSCL_c
      write(*,*) "1-2-1 smoothing of RRFAC "

      rr_halo_xx = rr_halo
      rr_halo_sm = rr_halo

      ns=1
      wt = 1./((2.*ns+1.)**2)
      DO iter=1,4*NSM/ns
      DO np=1,6
        DO j=1-nhalo+ns,ncube+nhalo-ns
        DO i=1-nhalo+ns,ncube+nhalo-ns

#if 1
           ! This is less flexible but is about 2x faster than
           ! using the SUM function
           rr_halo_xx(i,j,np) = (                          &
                                rr_halo_sm(i+1,j,  np)  +  &
                                rr_halo_sm(i-1,j,  np)  +  &
                                rr_halo_sm(i,  j+1,np)  +  &
                                rr_halo_sm(i,  j-1,np)  +  &
                                rr_halo_sm(i+1,j+1,np)  +  &
                                rr_halo_sm(i-1,j-1,np)  +  &
                                rr_halo_sm(i-1,j+1,np)  +  &
                                rr_halo_sm(i+1,j-1,np)  +  &
                                rr_halo_sm(i,  j,  np)  ) / 9.  
#else
           rr_halo_xx(i,j,np) = SUM( rr_halo_sm(i-ns:i+ns, j-ns:j+ns, np) ) * wt
#endif

        END DO  !  i-loop   
        END DO  ! j-loop
      END DO  ! panel loop

      rr_halo_sm = rr_halo_xx
      write(*,900,advance='no') achar(13), iter,4*nsm/ns
      END DO


#if 0
      rr_halo_sm = 1.0*NINT( rr_halo_sm )

      where(rr_halo_sm.lt.2.0) rr_halo_sm = 1.0
      where(rr_halo_sm.gt.2.0 .and. rr_halo_sm.le.3.0) rr_halo_sm = 2.0
      where(rr_halo_sm.gt.3.0 .and. rr_halo_sm.le.8.0) rr_halo_sm = 4.0 !hi-conn
      where(rr_halo_sm.gt.8.0 .and. rr_halo_sm.le.16.0) rr_halo_sm = 8.0
#endif


900   format( a1, " Smoothing iteration ",i4," out of ",i4 )

 
   END SUBROUTINE smooth_rrfac_halo
!======================================================================================

subroutine wrtncdf_topo_smooth_data(ncube,n,terr_sm,terr_dev,output_fname,command_line_arguments,rr_fac)
  use shr_kind_mod, only: r8 => shr_kind_r8
  use shared_vars, only : rad2deg
  implicit none
  
#     include         <netcdf.inc>
    
  !
  ! Dummy arguments
  !
  integer, intent(in) :: n, ncube
  real(r8),dimension(n),           intent(in) :: terr_sm,terr_dev
  character(len=1024),             intent(in) :: output_fname
  real(r8),dimension(n), optional, intent(in) :: rr_fac
  character(len=1024),             intent(in) :: command_line_arguments

  integer            :: foutid     ! Output file id
  integer            :: lonvid
  integer            :: latvid
  integer            :: terr_smid, terr_devid, rr_fac_id
  integer            :: status
  character (len=8)  :: datestring
  integer, dimension(2) :: nid
    
  real(r8), parameter :: fillvalue = 1.d36
  integer, dimension(1) :: latdim
  character(len=1024) :: str
  real(r8)            :: pi,piq,da,alph,beta,lon(ncube,ncube,6),lat(ncube,ncube,6)
  integer             :: i,j,ip
    
  !
  !  Create NetCDF file for output
  !
  print *,"Create NetCDF file for output: ",trim(output_fname)
  status = nf_create (trim(output_fname), NF_64BIT_OFFSET , foutid)
  if (status .ne. NF_NOERR) call handle_err(status)
  !
  ! Meta data for CESM compliance
  !
  !-data_summary		|	Short paragraph about the data.
  !-data_creator 		| 	Name and email of the person who created the dataset
  !-cesm_contact    	        |     	The liaison of the relevant WG
  !-creation_date    	        |     	Full date of dataset creation
  !-update_date    	        |     	Full date of most recent modification
  !-history    		        |     	Updates to changes made to the data.
  !-data_script    	        |     	script to generate data (will be available in the SVN repository ?)
  !-data_description_url 	|     	A web-page with a description if available  (this could be the climatedataguide webpage.)
  !-data_source_url    	        |     	The web page where the raw data can be downloaded
  !-data_reference    	        |     	Full reference for the dataset if available
  !-data_doi    		|     	If doi of data exists
  !-climo_years    	        |     	Year 1-year N of the climatological averaging period.
  !-data_mods    		|     	Any special substantive (non resolution) modifications that were made to the input data set purely for the purpose of using it in CESM. 
  !
  str = 'Smoothed topo data for quicker generation of topography data'
  status = nf_put_att_text (foutid,NF_GLOBAL,'data_summary',LEN(TRIM(str)), TRIM(str))
  if (status .ne. NF_NOERR) call handle_err(status)
  
  call DATE_AND_TIME(DATE=datestring)
  status = nf_put_att_text (foutid,NF_GLOBAL,'creation_date',8, TRIM(datestring) )
  if (status .ne. NF_NOERR) call handle_err(status)
  
  str = 'Peter Hjort Lauritzen and Julio Bacmeister'
  status = nf_put_att_text (foutid,NF_GLOBAL,'cesm_contact',LEN(TRIM(str)), TRIM(str))
  if (status .ne. NF_NOERR) call handle_err(status)
  
  str = 'https://github.com/NCAR/Topo.git'
  status = nf_put_att_text (foutid,NF_GLOBAL,'data_source',LEN(TRIM(str)), TRIM(str))
  if (status .ne. NF_NOERR) call handle_err(status)
  
  status = nf_put_att_text (foutid,NF_GLOBAL,'data_script',LEN(TRIM(command_line_arguments)), TRIM(command_line_arguments))
  if (status .ne. NF_NOERR) call handle_err(status)
  
  str = TRIM('Lauritzen, P. H. et al.: NCAR global model topography generation software for unstructured grids, '// &
       'Geosci. Model Dev., 8, 1-12, doi:10.5194/gmd-8-1-2015, 2015.')
  status = nf_put_att_text (foutid,NF_GLOBAL,'data_reference',LEN(TRIM(str)), TRIM(str))
  if (status .ne. NF_NOERR) call handle_err(status)
  
  
  
  !  status = nf_put_att_text (foutid,NF_GLOBAL,'data_script',LEN(TRIM(command_line_arguments)), TRIM(command_line_arguments))
  !  if (status .ne. NF_NOERR) call handle_err(status)
  !
  ! Create dimensions for output
  !
  status = nf_def_dim (foutid, 'ncol', n, nid(1))
  if (status .ne. NF_NOERR) call handle_err(status)
  
  print *,"Create variable for output: terr_sm"
  status = nf_def_var (foutid,'terr_sm', NF_DOUBLE, 1, nid(1), terr_smid)
  if (status .ne. NF_NOERR) call handle_err(status)

  status = nf_put_att_text (foutid,terr_smid,'long_name', 21, 'precomputed smoothed height field')
  status = nf_put_att_text (foutid,terr_smid,'units', 1, 'm')
  status = nf_put_att_double (foutid, terr_smid, 'missing_value', nf_double, 1, fillvalue)
  status = nf_put_att_double (foutid, terr_smid, '_FillValue'   , nf_double, 1, fillvalue)
  
  print *,"Create variable for output: terr_dev"
  status = nf_def_var (foutid,'terr_dev', NF_DOUBLE, 1, nid(1), terr_devid)
  if (status .ne. NF_NOERR) call handle_err(status)

  status = nf_put_att_text (foutid,terr_devid,'long_name', 82, &
       'precomputed difference between intermdiate cubed-sphere height and smoothed height')
  status = nf_put_att_text (foutid,terr_devid,'units', 1, 'm')
  status = nf_put_att_double (foutid, terr_devid, 'missing_value', nf_double, 1, fillvalue)
  status = nf_put_att_double (foutid, terr_devid, '_FillValue'   , nf_double, 1, fillvalue)

  if (present(rr_fac)) then
    print *,"Create variable for output: rr_fac"
    status = nf_def_var (foutid,'rr_fac', NF_DOUBLE, 1, nid(1), rr_fac_id)
    if (status .ne. NF_NOERR) call handle_err(status)    
  end if

  status = nf_def_var (foutid,'lat', NF_DOUBLE, 1, nid(1), latvid)
  if (status .ne. NF_NOERR) then
    call handle_err(status)
    write(*,*) "lat error"
  end if
  
  status = nf_def_var (foutid,'lon', NF_DOUBLE, 1, nid(1), lonvid)
  if (status .ne. NF_NOERR) then
    call handle_err(status)
    write(*,*) "lon error"
  end if

  status = nf_put_att_text (foutid,latvid,'long_name', 8, 'latitude')
  if (status .ne. NF_NOERR) call handle_err(status)
  status = nf_put_att_text (foutid,latvid,'units', 13, 'degrees_north')
  if (status .ne. NF_NOERR) call handle_err(status)
  
  status = nf_put_att_text (foutid,lonvid,'long_name', 9, 'longitude')
  if (status .ne. NF_NOERR) call handle_err(status)
  status = nf_put_att_text (foutid,lonvid,'units', 12, 'degrees_east')
  if (status .ne. NF_NOERR) call handle_err(status)
  !
  ! End define mode for output file
  !
  status = nf_enddef (foutid)
  if (status .ne. NF_NOERR) call handle_err(status)
  
  print*,"writing terr",MINVAL(terr_sm),MAXVAL(terr_sm)
  status = nf_put_var_double (foutid, terr_smid, terr_sm)
  if (status .ne. NF_NOERR) call handle_err(status)
  
  print*,"writing terr_dev",MINVAL(terr_dev),MAXVAL(terr_dev)
  status = nf_put_var_double (foutid, terr_devid, terr_dev)
  if (status .ne. NF_NOERR) call handle_err(status)

  if (present(rr_fac)) then
    print*,"writing rr_fac",MINVAL(rr_fac),MAXVAL(rr_fac)
    status = nf_put_var_double (foutid, rr_fac_id, rr_fac)
    if (status .ne. NF_NOERR) call handle_err(status)
  end if

  pi  = 4*DATAN(1.D0)
  piq = DATAN(1.D0)
  da  = 0.5*pi/DBLE(ncube)
  do ip=1,6
    DO j=1,ncube
      DO i=1,ncube
        alph = -piq+(i-0.5)*da
        beta = -piq+(j-0.5)*da
        call CubedSphereRLLFromABP(alph, beta, ip, lon(i,j,ip), lat(i,j,ip))
      end do
    end do
  end do

  status = nf_put_var_double (foutid, lonvid, lon*rad2deg)    
  status = nf_put_var_double (foutid, latvid, lat*rad2deg)    
  
  print *,"close file"
  status = nf_close (foutid)
  if (status .ne. NF_NOERR) call handle_err(status)
end subroutine wrtncdf_topo_smooth_data

subroutine read_topo_smooth_data(fname,ncol,terr_sm,terr_dev,rr_fac)
  use shr_kind_mod, only: r8 => shr_kind_r8
  implicit none
#     include         <netcdf.inc>
  character(len=1024),      intent(in)  :: fname
  integer,                  intent(in)  :: ncol
  real(r8),dimension(ncol), intent(out) :: terr_sm,terr_dev
  real(r8),dimension(ncol), intent(out), optional :: rr_fac

  integer :: ncid,status, dimid, alloc_error, terr_sm_id,terr_dev_id,ncol_file, rr_fac_id

  status = nf_open(TRIM(fname) , 0, ncid)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR(STATUS)
  
  status = NF_INQ_DIMID(ncid,'ncol', dimid)
  IF (status .NE. NF_NOERR) CALL HANDLE_ERR(status)

  status = NF_INQ_DIMLEN(ncid, dimid, ncol_file)
  IF (status .NE. NF_NOERR) CALL HANDLE_ERR(status)
  
  if (ncol.ne.ncol_file) then
    write(*,*) "Mismatch between specified smoothed topo data dimension: ",ncol_file,ncol
    stop
  end if

  status = NF_INQ_VARID(ncid, 'terr_sm', terr_sm_id)
  IF (status .NE. NF_NOERR) CALL HANDLE_ERR(status)
  
  status = NF_GET_VAR_DOUBLE(ncid, terr_sm_id,terr_sm)
  IF (status .NE. NF_NOERR) CALL HANDLE_ERR(status)

  status = NF_INQ_VARID(ncid, 'terr_dev', terr_dev_id)
  IF (status .NE. NF_NOERR) CALL HANDLE_ERR(status)
  
  status = NF_GET_VAR_DOUBLE(ncid, terr_dev_id,terr_dev)
  IF (status .NE. NF_NOERR) CALL HANDLE_ERR(status)

  if (present(rr_fac)) then
    status = NF_GET_VAR_DOUBLE(ncid, rr_fac_id,rr_fac)
    IF (status .NE. NF_NOERR) CALL HANDLE_ERR(status)
  end if

  status = nf_close (ncid)
  if (status .ne. NF_NOERR) call handle_err(status)  
end subroutine read_topo_smooth_data



END MODULE smooth_topo_cube_sph
