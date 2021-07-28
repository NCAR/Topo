!  DATE CODED:   2011 to 2015
!  DESCRIPTION:  Remap topo data from cubed-sphere grid to target grid using rigorous remapping
!                (Lauritzen, Nair and Ullrich, 2010, J. Comput. Phys.)
!
!  Author: Peter Hjort Lauritzen (pel@ucar.edu), AMP/CGD/NCAR 
!
program convterr
  use shr_kind_mod, only: r8 => shr_kind_r8
  use smooth_topo_cube_sph
  use ridge_ana
  use shared_vars
  use reconstruct
  implicit none
#     include         <netcdf.inc>

  !

  !
  !**********************************************************************
  !
  ! END OF USER SETTINS BELOW
  ! (do not edit beyond this point unless you know what you are doing!)
  ! (OR, you are Julio Bacmeister)
  !**********************************************************************
  !
  integer :: ncube !dimension of cubed-sphere grid
  integer :: ncube_rr ! dimension of refinement factor. 
                      ! Should be equal to ncube.
  

 
  integer :: alloc_error
  
  logical :: ldbg
  !
  ! constants
  !  

  !  
  real(r8) :: wt

  !
  ! for linear interpolation
  !
  !
  ! variable for regridding
  !
  integer :: counti
  !
  integer :: ntarget, ncorner, nrank

  integer :: ii,ip,jx,jy,jp,np
  integer, parameter :: ngauss = 3
!  integer, parameter :: ngauss = 5
  integer :: jmax_segments,jall,jall_anticipated
  real(r8) :: tmp
  
  real(r8), allocatable, dimension(:) :: tmp_var



  real(r8), allocatable, dimension(:,:) :: weights_all
  integer , allocatable, dimension(:,:) :: weights_eul_index_all
  integer , allocatable, dimension(:)   :: weights_lgr_index_all
  integer :: ix,iy  , i
  !
  ! volume of topography
  !
  real(r8) :: vol_target, vol_target_un, area_target_total,vol_source,area_source,mea_source
  integer :: nlon,nlat
  logical :: ltarget_latlon,lpole

  !
  ! for internal filtering
  !
!  real(r8), allocatable, dimension(:,:) :: weights_all_coarse
!  integer , allocatable, dimension(:,:) :: weights_eul_index_all_coarse
!  integer , allocatable, dimension(:)   :: weights_lgr_index_all_coarse
!  real(r8), allocatable, dimension(:)   :: area_target_coarse
!  real(r8), allocatable, dimension(:,:) :: da_coarse,da
  real(r8), allocatable, dimension(:,:) :: da, terr_2(:,:,:),rrfac(:,:,:),rrfac_tmp(:,:,:)
!  real(r8), allocatable, dimension(:,:) :: recons,centroids
  integer :: nreconstruction
  real(r8) :: da_min_ncube, da_min_target ! used to compute jmax_segments
  real(r8) :: volterr, volterr_sm  
!  
!  integer :: jmax_segments_coarse,jall_coarse,ncube_coarse

  !
  ! namelist variables
  !
  !
  ! if smoothed PHIS is available SGH needs to be recomputed  to account for the sub-grid-scale
  ! variability introduced by the smoothing
  !
  logical :: lsmooth_terr = .FALSE. 
  !
  ! PHIS is smoothed by other software/dynamical core
  !
  logical :: lexternal_smooth_terr = .TRUE. ! lexternal_smooth_terr = .FALSE. is NOT supported currently
  !
  ! set PHIS=0.0 if LANDFRAC<0.01
  !
  logical :: lzero_out_ocean_point_phis = .FALSE.
  !
  !++jtb
  logical :: lzero_negative_peaks = .TRUE.
  logical :: lread_smooth_topofile = .FALSE.
  logical :: luse_multigrid = .FALSE.
  logical :: luse_prefilter = .FALSE.
  logical :: lstop_after_smoothing = .FALSE.
  logical :: lb4b_with_cesm2 = .FALSE.
  !--jtb
  !
  ! Cubed sphere terr is band-pass filtered using circular kernels
  logical :: lsmooth_on_cubed_sphere = .FALSE.

  !                             *Radii* of smoothing circles
  integer :: ncube_sph_smooth_coarse = -1
  integer :: ncube_sph_smooth_fine   = -1
  integer :: ncube_sph_smooth_iter   = -1
  !
  ! namelist variables for detection of sub-grid scale orientation
  ! i.e., "ridge finding"
  !
  logical :: lfind_ridges = .FALSE.
  !                             Ridge analysis takes place on
  !                             squares of 2*NW+1
  integer :: nwindow_halfwidth = -1
  !                             
  !                             for backwards compat with CESM2.0
  integer :: nridge_subsample = -1
  !
  logical :: lridgetiles = .FALSE.
!+++ARH
  logical :: lregional_refinement = .FALSE.
  integer :: rrfac_max = 1
!---ARH
  !
  !
  ! For internal smoothing (experimental at this point)
  ! ===================================================
  !
  ! if smoothing is internal (lexternal_smooth_terr=.FALSE.) choose ncube_coarse
  !                                   
  integer :: ncube_coarse = 45
  integer :: norder = 3
  integer :: nmono  = 1
  integer :: npd    = 1
  !
  INTEGER :: UNIT

  INTEGER :: NSCL_f, NSCL_c, nhalo,nsb,nsw, i_in_sg, itarget
  integer, allocatable :: isg(:)

  character(len=1024) :: grid_descriptor_fname,intermediate_cubed_sphere_fname,output_fname,  externally_smoothed_topo_file
  character(len=1024) :: output_grid = 'none', ofile, proctag, smooth_fname, remap_fname, smoothprm, rdgwin, rdglist_fname
  character(len=1024) :: rrfactor_fname

  character(len=8)  :: date
  character(len=10) :: time

!+++ARH
  character(len=1024) :: cube_file
!+++ARH

#if 0  
    type (peak_type), allocatable, dimension(:) ::  peaks
    integer :: npeaks
#endif

  namelist /topoparams/ &
       grid_descriptor_fname,output_grid,intermediate_cubed_sphere_fname, &
       output_fname, externally_smoothed_topo_file,&
       lsmooth_terr, lexternal_smooth_terr,lzero_out_ocean_point_phis, & 
       lsmooth_on_cubed_sphere,lfind_ridges,lridgetiles,lzero_negative_peaks, &
       lregional_refinement,rrfac_max, &
       !
       ! variables for interal smoothing of topography
       !
       ncube_coarse,norder,nmono,npd,ncube_sph_smooth_coarse,ncube_sph_smooth_fine, &
       ncube_sph_smooth_iter,nwindow_halfwidth,nridge_subsample,lread_smooth_topofile, &
       luse_multigrid,luse_prefilter,lstop_after_smoothing, &
       lb4b_with_cesm2 
 
  UNIT=221
  OPEN( UNIT=UNIT, FILE="nlmain.nl" ) !, NML =  cntrls )
  READ( UNIT=UNIT, NML=topoparams)
  CLOSE(UNIT=UNIT)
  
  call  set_constants

  !
  ! turn extra debugging on/off
  !
  ldbg = .FALSE.

  ! Read in target grid
  !------------------------------------------------------------------------------------------------
  call read_target_grid(grid_descriptor_fname,lregional_refinement,ltarget_latlon,lpole,nlat,nlon,ntarget,ncorner,nrank)

  ! Read in topo data on cubed sphere grid
  !------------------------------------------------------------------------------------------------
  call read_intermediate_cubed_sphere_grid(intermediate_cubed_sphere_fname,ncube)
   
  allocate ( dA(ncube,ncube),stat=alloc_error )
  CALL EquiangularAllAreas(ncube, dA)

!+++ARH
  ! Compute overlap weights
  !------------------------------------------------------------------------------------------------

  ! On entry to overlap_weights 'jall' is a generous guess at the number of cells in
  ! in the 'exchange grid'

   jall_anticipated = ncube*ncube*12*10 !anticipated number of weights (can be tweaked)

   jmax_segments = 1000000   !can be tweaked xxx
   write(*,*) "jmax_segments",jmax_segments !,da_min_target,da_min_ncube   
   nreconstruction = 1
   allocate (weights_all(jall_anticipated,nreconstruction),stat=alloc_error )
   allocate (weights_eul_index_all(jall_anticipated,3),stat=alloc_error )
   allocate (weights_lgr_index_all(jall_anticipated),stat=alloc_error )
   jall=jall_anticipated

   if (.not.lstop_after_smoothing) then
     write(*,*) "Compute overlap weights: "
     CALL overlap_weights(weights_lgr_index_all,weights_eul_index_all,weights_all,&
          jall,ncube,ngauss,ntarget,ncorner,jmax_segments,target_corner_lon,target_corner_lat,nreconstruction)
   end if
   deallocate(target_corner_lon,target_corner_lat)

  ! On exit from overlap_weights 'jall' is the correct number of cells in the exchange grid. 
  !------------------------------------------------------------------------------------------------
            
  ! Set-up regional refinement control.
  !------------------------------------------
  ! Array rrfac is a refinement factor >= 1.0 
  ! Passed to smooth topo and ridge finder to
  ! control lengthscales used in algorithms. 
  ! RRfac is always used. If output_grid has no 
  ! regional refinement then rrfac(:,:,:)=1.

  allocate( rrfac(ncube,ncube,6)  )
  if (lregional_refinement) then
    !--- remap rrfac to cube
    !-----------------------------------------------------------------
    do counti=1,jall
      i    = weights_lgr_index_all(counti)!!
      !
      ix  = weights_eul_index_all(counti,1)
      iy  = weights_eul_index_all(counti,2)
      ip  = weights_eul_index_all(counti,3)
      !
      ! convert to 1D indexing of cubed-sphere
      !
      ii = (ip-1)*ncube*ncube+(iy-1)*ncube+ix!
      !
      wt = weights_all(counti,1)
      !
      rrfac(ix,iy,ip) = rrfac(ix,iy,ip) + wt*(target_rrfac(i))/dA(ix,iy)
    end do
  else
     rrfac(:,:,:) = 1.
     write(*,*) " NO refinement: RRFAC = 1. everywhere "
  endif
  write(*,*) "MINMAX RRFAC RAW MAPPED FIELD",minval(rrfac),maxval(rrfac)
!---ARH



         !--- Make output filenames
         !----------------------------------------------------------------------
         !  Final gridded nc file 
         !---------------------------
         call DATE_AND_TIME( DATE=date,TIME=time)

         proctag=' '
         if (lb4b_with_cesm2) then
            proctag = trim(proctag)//'_CESM2'
         end if
         if (luse_multigrid) then
            proctag = trim(proctag)//'_MulG'
         end if
         if (luse_prefilter) then
            proctag = trim(proctag)//'_PF'
         end if

         write( smoothprm , &
             "('_nc',i0.4,'_Co',i0.3,'_Fi',i0.3 )" ) & 
         ncube, ncube_sph_smooth_coarse, ncube_sph_smooth_fine

         if(lfind_ridges) then
           if (nwindow_halfwidth<1) then
             write(*,*) "nwindow_halfwidth must be >0",nwindow_halfwidth
             stop
           endif
         write( rdgwin , &
             "('_Nsw',i0.3 )" ) nwindow_halfwidth  
         else
           rdgwin = '_NoAniso'
         end if  
         
         write(*,*)" Smooth ", trim(smoothprm)
         write(*,*)" Procs  ", trim(proctag)
         write(*,*)" Ridge  ", trim(rdgwin)


         smooth_fname  = './inputdata/smooth_topo_cube/topo_smooth'//trim(smoothprm)//trim(proctag)//'_v02.dat'
         write(*,*) " smoothed topo file ::  ",trim(smooth_fname)

         rdglist_fname = './inputdata/RdgList'//trim(smoothprm)//trim(proctag)//trim(rdgwin)//'.dat'
         write(*,*) " rdglist fname::  ",trim(rdglist_fname)

         remap_fname   = './output/remap'//trim(smoothprm)//trim(proctag)//trim(rdgwin)//'.dat'
         write(*,*) " remap to cube file ::  ",trim(remap_fname)

         if (trim(output_grid) == 'none') then
             output_fname = 'none'
         else
             output_fname  = './output/'//trim(output_grid)//trim(smoothprm)//trim(proctag)//trim(rdgwin)//'.nc'
             write(*,*) " Final output file  ::  ",trim(output_fname)
         endif


!++jtb
   !!!!  if (lsmooth_on_cubed_sphere) then
      NSCL_c = 2*ncube_sph_smooth_coarse
      NSCL_f = 2*ncube_sph_smooth_fine
      nhalo  = NSCL_c  !*ncube_sph_smooth_iter ! 120      

      allocate( terr_sm(ncube,ncube,6)  )
      allocate( terr_dev(ncube,ncube,6) )
      allocate( terr_2(ncube,ncube,6)  )
      terr_2 = reshape( terr,    (/ncube,ncube,6/) )

      write(*,*) " SMOOTHING on CUBED SPHERE 10/7/15 "

      ! This routine writes out an f77 unf file containing terr,terr_sm, and terr_dev
      ! File also contains rr_factor for possible use by ridge finder

      if (NSCL_c > 0) then
!+++ARH
         if (lregional_refinement) then
           !Use 4X larger smoothing radius than that used for topography
           NSCL_c = 4*2*ncube_sph_smooth_coarse
           nhalo  = NSCL_c

           write(*,*) "rrfac_max",rrfac_max

           allocate( rrfac_tmp(ncube,ncube,6)  )
           rrfac_tmp(:,:,:) = rrfac(:,:,:)
           call  smooth_intermediate_topo_wrap (rrfac_tmp, da, ncube,nhalo, NSCL_f,NSCL_c, &
                                          rrfac, terr_dev , &
                                          smooth_fname, &
                                          lread_smooth_topofile, &
                                          lsmooth_on_cubed_sphere, &
                                          luse_multigrid, &
                                          luse_prefilter, &
                                          lstop_after_smoothing, &
                                          lb4b_with_cesm2 , &
                                          rrfac_tmp )
           write(*,*) "MINMAX RRFAC SMOOTHED",minval(rrfac),maxval(rrfac)
           !
           !---rrfac limiter
           rrfac = REAL(NINT(rrfac))
           where (rrfac.gt.rrfac_max) rrfac = rrfac_max

           write(*,*) "MINMAX RRFAC FINAL",minval(rrfac),maxval(rrfac)

           !!cube_file = 'rrfac_HMA.nc'
           !!CALL wrt_cube(ncube,terr,rrfac,cube_file)

         end if
         NSCL_c = 2*ncube_sph_smooth_coarse
         nhalo  = NSCL_c 
!---ARH
         call  smooth_intermediate_topo_wrap (terr_2, da, ncube,nhalo, NSCL_f,NSCL_c, & 
                                        terr_sm, terr_dev , &
                                        smooth_fname, &
                                        lread_smooth_topofile, & 
                                        lsmooth_on_cubed_sphere, &
                                        luse_multigrid, &
                                        luse_prefilter, &
                                        lstop_after_smoothing, &
                                        lb4b_with_cesm2 , & 
                                        rrfac )
           write(*,*)" Out we goooo !!"
      else
         terr_dev = terr_2
      endif

      volterr=0.
      volterr_sm=0.
      do np=1,6 
         volterr    =  volterr    + sum( terr_2(:,:,np) * da )
         volterr_sm =  volterr_sm + sum( terr_sm(:,:,np) * da )
      end do

      write(*,*) " Topo volume BEFORE smoother = ",volterr/(6*sum(da))
      write(*,*) " Topo volume  AFTER smoother = ",volterr_sm/(6*sum(da))
      write(*,*) "            Difference       = ",(volterr - volterr_sm)/(6*sum(da))

      terr_sm = (volterr/volterr_sm)*terr_sm
      volterr_sm=0.
      do np=1,6 
         volterr_sm =  volterr_sm + sum( terr_sm(:,:,np) * da )
      end do

      write(*,*) " Topo volume  AFTER smoother AND fixer = ",volterr_sm/(6*sum(da))

      if(lfind_ridges) then
        ! Guessing NSW should be ~1/SQRT(2.)
        ! of smoothing radius
        nsw = nwindow_halfwidth
        ! following formula for nsb ensures
        ! compatibilty with CESM2.0 Co60.  
        ! Temporary fix (3/31/2017)
        nsb = INT( nsw/6 )+1 !nridge_subsample
        nhalo=2*nsw

        call find_local_maxes ( terr_dev, ncube, nhalo, nsb, nsw ) 
        if(lregional_refinement) then
          call find_ridges ( terr_dev, terr, ncube, nhalo, nsb, nsw &
                           , rdglist_fname & 
                           , lregional_refinement=lregional_refinement &
                           , rr_factor = rrfac  )
        else
          call find_ridges ( terr_dev, terr, ncube, nhalo, nsb, nsw &
                           , rdglist_fname )
        end if

 
      endif

  !*********************************************************
  !
  ! Begin actual remapping calculations
  !
  !*********************************************************
 
   call allocate_target_vars(ntarget)  

  !*********************************************************************
  !      In the following loops "counti" is the index of a piece of 
  !      the "exchange grid"
  !********************************************************************
  
  !
  ! Sum exchange grid cells within each target
  ! grid cell
  !
  tmp = 0.0
  do counti=1,jall
    i    = weights_lgr_index_all(counti)
    wt = weights_all(counti,1)
    area_target        (i) = area_target(i) + wt
  end do
  write(*,*) "MIN/MAX area_target",MINVAL(area_target),MAXVAl(area_target)
  write(*,*) "MIN/MAX target_area",MINVAL(target_area),MAXVAl(target_area)

!+++ARH
  !write(*,*) "Remapping landfrac"
  !write(*,*) "MIN/MAX before remap:", MINVAL(landfrac), MAXVAL(landfrac)
  !landfrac_target = remap_field(landfrac,area_target,weights_eul_index_all(1:jall,:),weights_lgr_index_all(1:jall),&       
  !     weights_all(1:jall,:),ncube,jall,nreconstruction,ntarget)
  !write(*,*) "MIN/MAX after remap:", MINVAL(landfrac_target), MAXVAL(landfrac_target)
!---ARH`

  write(*,*) "Remapping terrain"
  terr_target = remap_field(terr,area_target,weights_eul_index_all(1:jall,:),weights_lgr_index_all(1:jall),&
       weights_all(1:jall,:),ncube,jall,nreconstruction,ntarget)
  write(*,*) "MIN/MAX:", MINVAL(terr_target), MAXVAL(terr_target)
  terr_uf_target = remap_field(terr,area_target,weights_eul_index_all(1:jall,:),weights_lgr_index_all(1:jall),&
       weights_all(1:jall,:),ncube,jall,nreconstruction,ntarget)
  write(*,*) "MIN/MAX:", MINVAL(terr_target), MAXVAL(terr_target)


  write(*,*) "Remapping landm_coslat"
  landm_coslat_target = remap_field(landm_coslat,area_target,weights_eul_index_all(1:jall,:),weights_lgr_index_all(1:jall),&
       weights_all(1:jall,:),ncube,jall,nreconstruction,ntarget)
  write(*,*) "MIN/MAX:", MINVAL(landm_coslat_target), MAXVAL(landm_coslat_target)

  write(*,*) "Remapping SGH30"
  sgh30_target = remap_field(var30,area_target,weights_eul_index_all(1:jall,:),weights_lgr_index_all(1:jall),&
       weights_all(1:jall,:),ncube,jall,nreconstruction,ntarget)
  write(*,*) "MIN/MAX:", MINVAL((sgh30_target)), MAXVAL(sqrt(sgh30_target))
  deallocate(var30)
  deallocate(landm_coslat)

  !deallocate(terr_smooth_internal)

  WRITE(*,*) "max difference between target grid area and remapping software area",&
              MAXVAL(target_area-area_target)
  
  !
  ! Consistency checks  
  !
  do counti=1,ntarget
    if (terr_target(counti)>8848.0) then
      !
      ! max height is higher than Mount Everest
      !
      write(*,*) "FATAL error: max height is higher than Mount Everest!"
      write(*,*) "terr_target",counti,terr_target(counti)
      write(*,*) "(lon,lat) locations of vertices of cell with excessive max height::"
      do i=1,ncorner
        write(*,*) target_corner_lon(i,counti),target_corner_lat(i,counti)
      end do
      STOP
    else if (terr_target(counti)<-423.0) then
      !
      ! min height is lower than Dead Sea
      !
      write(*,*) "FATAL error: min height is lower than Dead Sea!"
      write(*,*) "terr_target",counti,terr_target(counti)
      write(*,*) "(lon,lat) locations of vertices of cell with excessive min height::"
      do i=1,ncorner
        write(*,*) target_corner_lon(i,counti),target_corner_lat(i,counti)
      end do
      STOP
    else 
      
    end if
  end do
  WRITE(*,*) "Elevation data passed min/max consistency check!"
  WRITE(*,*) " "
  

  !
  ! compute mean height (globally) of topography about sea-level for target grid unfiltered elevation
  !
  vol_target_un     = 0.0D0
  area_target_total = 0.0D0
  DO i=1,ntarget
    area_target_total = area_target_total+area_target(i)
!    write(*,*) i,vol_target_un,terr_target(i),area_target(i)
    vol_target_un     = vol_target_un+terr_target(i)*area_target(i)
  END DO
  WRITE(*,*) "mean height (globally) of topography about sea-level for target grid unfiltered elevation",&
       vol_target_un/area_target_total,vol_target_un,area_target_total
  
  !
  ! diagnostics
  !
  vol_source     = 0.0D0
  mea_source     = 0.0D0
  area_source    = 0.0D0
!++jtb
!   The two lines below moved up before call
!   to smooth_intermediate_topo
!
  !allocate ( dA(ncube,ncube),stat=alloc_error )
  !CALL EquiangularAllAreas(ncube, dA)
!--jtb
  DO jp=1,6
    DO jy=1,ncube
      DO jx=1,ncube
        ii = (jp-1)*ncube*ncube+(jy-1)*ncube+jx
        vol_source = vol_source+terr(ii)*dA(jx,jy)
!+++ARH
        !if (landfrac(ii)>0.0D0) then
           mea_source   = mea_source  + terr(ii)*dA(jx,jy)
           area_source  = area_source +          dA(jx,jy)
        !else
        !end if
!---ARH
      END DO
    END DO
  END DO
  WRITE(*,*) "volume of input cubed-sphere terrain           :",vol_source
  WRITE(*,*) "average elevation of input cubed-sphere terrain:",vol_source/(4.0D0*pi)
  WRITE(*,*) "average elevation of input cubed-sphere terrain over land:",vol_source/area_source

  DEALLOCATE(dA)
!+++ARH
  !deallocate(landfrac)
!---ARH
  !
  ! compute variance with respect to cubed-sphere data
  !
  WRITE(*,*) "compute variance with respect to 3km cubed-sphere data: SGH"
  !  
  IF (lsmooth_terr) THEN
    call smooth_terrain(lexternal_smooth_terr,ltarget_latlon,terr_target,ntarget,externally_smoothed_topo_file,&
         nlon,nlat)
  END IF
  !
  ! compute mean height (globally) of topography about sea-level for target grid filtered elevation
  !
  vol_target = 0.0
  DO i=1,ntarget
    vol_target = vol_target+terr_target(i)*area_target(i)
  END DO
  WRITE(*,*) "mean height (globally) of topography about sea-level for target grid filtered elevation",&
       vol_target/area_target_total
  WRITE(*,*) "percentage change in mean height between filtered and unfiltered elevations",&
       100.0D0*(vol_target-vol_target_un)/vol_target_un
  WRITE(*,*) "percentage change in mean height between input cubed-sphere and unfiltered elevations",&
       100.0D0*(vol_source-vol_target_un)/vol_source    
  !
  ! Done internal smoothing
  !
  if (lzero_out_ocean_point_phis) then
    WRITE(*,*) "if ocean mask PHIS=0.0"
  end if

  !!! if(lsmooth_on_cubed_sphere ) terr_target=0.0 ???
  terr_target=0.0
  sgh_target=0.0
  sgh_uf_target=0.0
  do counti=1,jall

    i    = weights_lgr_index_all(counti)!!
    !
    ix  = weights_eul_index_all(counti,1)
    iy  = weights_eul_index_all(counti,2)
    ip  = weights_eul_index_all(counti,3)
    !
    ! convert to 1D indexing of cubed-sphere
    !
    ii = (ip-1)*ncube*ncube+(iy-1)*ncube+ix!
    
    wt = weights_all(counti,1)
   
    ! Get rid of this ridiculous if.not. crap !!
    !if( .not.(lsmooth_on_cubed_sphere) ) then
    !  if (lzero_out_ocean_point_phis.AND.landfrac_target(i).lt.0.01_r8) then
    !    terr_target(i) = 0.0_r8   !5*terr_target(i)
    !  end if
    !  sgh_target(i) = sgh_target(i)+wt*((terr_target(i)-terr(ii))**2)/area_target(i)
    !else
    !  sgh_target  (i) = sgh_target  (i) + wt*(terr_dev(ix,iy,ip))**2/area_target(i)
    !  terr_target (i) = terr_target (i) + wt*(terr_sm(ix,iy,ip))/area_target(i) 
    !endif

    sgh_target  (i) = sgh_target  (i) + wt*(terr_dev(ix,iy,ip))**2/area_target(i)
    terr_target (i) = terr_target (i) + wt*(terr_sm(ix,iy,ip))/area_target(i) 
    sgh_uf_target(i) = sgh_uf_target(i)+wt*((terr_uf_target(i)-terr(ii))**2)/area_target(i)

  end do

  if( (lfind_ridges) ) then

     if(lregional_refinement) then
     call remapridge2target(area_target,target_center_lon,target_center_lat, & 
         weights_eul_index_all(1:jall,:), & 
         weights_lgr_index_all(1:jall),weights_all(1:jall,:),ncube,jall,&
         nreconstruction,ntarget,nhalo,nsb,nsw, &
         ncube_sph_smooth_coarse,ncube_sph_smooth_fine,remap_fname, &
         lzero_negative_peaks,luse_multigrid,luse_prefilter,lb4b_with_cesm2 &
                          , lregional_refinement=lregional_refinement &
                          , rr_factor = rrfac  )
     else
     call remapridge2target(area_target,target_center_lon,target_center_lat, & 
         weights_eul_index_all(1:jall,:), & 
         weights_lgr_index_all(1:jall),weights_all(1:jall,:),ncube,jall,&
         nreconstruction,ntarget,nhalo,nsb,nsw, &
         ncube_sph_smooth_coarse,ncube_sph_smooth_fine,remap_fname, &
         lzero_negative_peaks,luse_multigrid,luse_prefilter,lb4b_with_cesm2)
     end if

        if (lridgetiles) then 
           call remapridge2tiles(area_target,target_center_lon,target_center_lat, & 
                weights_eul_index_all(1:jall,:), & 
                weights_lgr_index_all(1:jall),weights_all(1:jall,:),ncube,jall,&
                nreconstruction,ntarget,nhalo,nsb)
        endif

  endif

  write(*,*) " !!!!!!!!  ******* maxval terr_target " , maxval(terr_target)

       !!    if(lfind_ridges)  call paintridgeoncube ( ncube,nhalo,nsb,nsw , terr_dev )

  !
  ! zero out small values
  !
  DO i=1,ntarget
!+++ARH
    !IF (landfrac_target(i)<.001_r8)  landfrac_target(i) = 0.0D0
!---ARH
    IF (sgh_target(i)     <    0.5)  sgh_target(i)      = 0.0D0
    IF (sgh30_target(i)<       0.5D0) sgh30_target(i)    = 0.0D0
  END DO
  sgh30_target = SQRT(sgh30_target)
  sgh_target = SQRT(sgh_target)

  WRITE(*,*) "min/max of terr source                   : ",MINVAL(terr),MAXVAL(terr)
  WRITE(*,*) "min/max of terr_target                   : ",MINVAL(terr_target    ),MAXVAL(terr_target    )
!+++ARH
  !WRITE(*,*) "min/max of landfrac_target               : ",MINVAL(landfrac_target),MAXVAL(landfrac_target)
!---ARH
  WRITE(*,*) "min/max of landm_coslat_target           : ",&
       MINVAL(landm_coslat_target),MAXVAL(landm_coslat_target)
  WRITE(*,*) "min/max of var30_target                  : ",MINVAL(sgh30_target   ),MAXVAL(sgh30_target   )
  WRITE(*,*) "min/max of var_target                    : ",MINVAL(sgh_target   ),MAXVAL(sgh_target   )

  write(*,*) " Model topo output file ",trim(output_fname)
!+++ARH
  !IF (ltarget_latlon) THEN
  !  CALL wrtncdf_rll(nlon,nlat,lpole,ntarget,terr_target,landfrac_target,sgh_target,sgh30_target,&
  !       landm_coslat_target,target_center_lon,target_center_lat,.FALSE.,output_fname,lfind_ridges)
  !
  !ELSE
  !  CALL wrtncdf_unstructured(ntarget,terr_target,landfrac_target,sgh_target,sgh30_target,&
  !       landm_coslat_target,target_center_lon,target_center_lat,target_area,output_fname,lfind_ridges)
  !END IF
  IF (ltarget_latlon) THEN
    CALL wrtncdf_rll(nlon,nlat,lpole,ntarget,terr_target,sgh_target,sgh30_target,&
         landm_coslat_target,target_center_lon,target_center_lat,.FALSE.,output_fname,lfind_ridges)
  
  ELSE
    CALL wrtncdf_unstructured(ntarget,terr_target,sgh_target,sgh30_target,&
         landm_coslat_target,target_center_lon,target_center_lat,target_area,output_fname,lfind_ridges)
  END IF
  !DEALLOCATE(terr_target,landfrac_target,sgh30_target,sgh_target,landm_coslat_target)
  DEALLOCATE(terr_target,sgh30_target,sgh_target,landm_coslat_target)
!---ARH
  DEALLOCATE(weights_all,weights_eul_index_all,terr)
 
end program convterr
!
!
!
!+++ARH
!subroutine wrtncdf_unstructured(n,terr,landfrac,sgh,sgh30,landm_coslat,lon,lat,area,output_fname,lfind_ridges)
subroutine wrtncdf_unstructured(n,terr,sgh,sgh30,landm_coslat,lon,lat,area,output_fname,lfind_ridges)
!---ARH
  use shared_vars, only : rad2deg
  use shr_kind_mod, only: r8 => shr_kind_r8
  use shared_vars, only : terr_uf_target, sgh_uf_target, area_target
  use ridge_ana, only: nsubr, mxdis_target, mxvrx_target, mxvry_target, ang22_target, &
                       anglx_target, aniso_target, anixy_target, hwdth_target, wghts_target, & 
                       clngt_target, cwght_target, count_target,riseq_target,grid_length_scale, &
                       fallq_target



  implicit none
  
#     include         <netcdf.inc>
  
  !
  ! Dummy arguments
  !
  integer, intent(in) :: n
!+++ARH
  !real(r8),dimension(n)  , intent(in) :: terr, landfrac,sgh,sgh30,lon, lat, landm_coslat,area  
  real(r8),dimension(n)  , intent(in) :: terr,sgh,sgh30,lon,lat,landm_coslat,area
!---ARH
  character(len=1024), intent(in) :: output_fname
  logical, intent(in) :: lfind_ridges
  !
  ! Local variables
  !
  character (len=1024) :: fout       ! NetCDF output file
  integer            :: foutid     ! Output file id
  integer            :: lonvid
  integer            :: latvid
  integer            :: terrid, areaid!,nid
!+++ARH
  !integer            :: landfracid,sghid,sgh30id,landm_coslatid
  integer            :: sghid,sgh30id,landm_coslatid
!---ARH
  integer             :: mxdisid, ang22id, anixyid, anisoid, mxvrxid, mxvryid, hwdthid, wghtsid, anglxid, gbxarid
  integer             :: sghufid, terrufid, clngtid, cwghtid, countid,riseqid,fallqid

  integer            :: status    ! return value for error control of netcdf routin
!  integer, dimension(2) :: nc_lat_vid,nc_lon_vid
  character (len=8)  :: datestring
  integer, dimension(2) :: nid
  
  real(r8), parameter :: fillvalue = 1.d36
  integer, dimension(1) :: latdim
  character(len=1024) :: str

  
  fout = trim(output_fname)
  !
  !  Create NetCDF file for output
  !
  print *,"Create NetCDF file for output"
  status = nf_create (fout, NF_64BIT_OFFSET , foutid)
  if (status .ne. NF_NOERR) call handle_err(status)
  !
  ! Create dimensions for output
  !
  status = nf_def_dim (foutid, 'ncol', n, nid(1))
  if (status .ne. NF_NOERR) call handle_err(status)
  !

  if (Lfind_ridges) then 
     status = nf_def_dim (foutid, 'nrdg', nsubr, nid(2))
     if (status .ne. NF_NOERR) call handle_err(status)
  endif

  !
  !
  ! Create variable for output
  !
  print *,"Create variable for output"
  status = nf_def_var (foutid,'PHIS', NF_DOUBLE, 1, nid(1), terrid)
  if (status .ne. NF_NOERR) call handle_err(status)
!+++ARH  
  !status = nf_def_var (foutid,'LANDFRAC', NF_DOUBLE, 1, nid(1), landfracid)
  !if (status .ne. NF_NOERR) call handle_err(status)
!---ARH  
  status = nf_def_var (foutid,'SGH', NF_DOUBLE, 1, nid(1), sghid)
  if (status .ne. NF_NOERR) call handle_err(status)
  
  status = nf_def_var (foutid,'SGH30', NF_DOUBLE, 1, nid(1), sgh30id)
  if (status .ne. NF_NOERR) call handle_err(status)
  
  status = nf_def_var (foutid,'LANDM_COSLAT', NF_DOUBLE, 1, nid, landm_coslatid)
  if (status .ne. NF_NOERR) call handle_err(status)
  !
  status = nf_def_var (foutid,'area', NF_DOUBLE, 1, nid(1), areaid)
  if (status .ne. NF_NOERR) call handle_err(status)

  status = nf_def_var (foutid,'lat', NF_DOUBLE, 1, nid(1), latvid)
  if (status .ne. NF_NOERR) call handle_err(status)
  
  latdim(1) = latvid
  status = nf_def_var (foutid,'lon', NF_DOUBLE, 1, nid(1), lonvid)!xxxx
  !status = nf_def_var (foutid,'lat', NF_DOUBLE, 1, latdim, latvid)!xxx

  if (status .ne. NF_NOERR) call handle_err(status)

  if (Lfind_ridges) then 

     status = nf_def_var (foutid,'SGH_UF', NF_DOUBLE, 1, nid(1), sghufid)
     if (status .ne. NF_NOERR) call handle_err(status)
     status = nf_def_var (foutid,'TERR_UF', NF_DOUBLE, 1, nid(1), terrufid)
     if (status .ne. NF_NOERR) call handle_err(status)
     status = nf_def_var (foutid,'GBXAR', NF_DOUBLE, 1, nid(1), gbxarid)
     if (status .ne. NF_NOERR) call handle_err(status)


     status = nf_def_var (foutid,'MXDIS', NF_DOUBLE, 2, nid , mxdisid)
     if (status .ne. NF_NOERR) call handle_err(status)
     status = nf_def_var (foutid,'RISEQ', NF_DOUBLE, 2, nid , riseqid)
     if (status .ne. NF_NOERR) call handle_err(status)
     status = nf_def_var (foutid,'FALLQ', NF_DOUBLE, 2, nid , fallqid)
     if (status .ne. NF_NOERR) call handle_err(status)



     status = nf_def_var (foutid,'MXVRX', NF_DOUBLE, 2, nid , mxvrxid)
     if (status .ne. NF_NOERR) call handle_err(status)
     status = nf_def_var (foutid,'MXVRY', NF_DOUBLE, 2, nid , mxvryid)
     if (status .ne. NF_NOERR) call handle_err(status)

     status = nf_def_var (foutid,'ANGLL', NF_DOUBLE, 2, nid , ang22id)
     if (status .ne. NF_NOERR) call handle_err(status)
     status = nf_def_var (foutid,'ANGLX', NF_DOUBLE, 2, nid , anglxid)
     if (status .ne. NF_NOERR) call handle_err(status)

     status = nf_def_var (foutid,'ANISO', NF_DOUBLE, 2, nid , anisoid)
     if (status .ne. NF_NOERR) call handle_err(status)
     status = nf_def_var (foutid,'ANIXY', NF_DOUBLE, 2, nid , anixyid)
     if (status .ne. NF_NOERR) call handle_err(status)

     status = nf_def_var (foutid,'HWDTH', NF_DOUBLE, 2, nid , hwdthid)
     if (status .ne. NF_NOERR) call handle_err(status)
     status = nf_def_var (foutid,'WGHTS', NF_DOUBLE, 2, nid , wghtsid)
     if (status .ne. NF_NOERR) call handle_err(status)

     status = nf_def_var (foutid,'CWGHT', NF_DOUBLE, 2, nid , cwghtid)
     if (status .ne. NF_NOERR) call handle_err(status)
     status = nf_def_var (foutid,'CLNGT', NF_DOUBLE, 2, nid , clngtid)
     if (status .ne. NF_NOERR) call handle_err(status)
     status = nf_def_var (foutid,'COUNT', NF_DOUBLE, 2, nid , countid)
     if (status .ne. NF_NOERR) call handle_err(status)

  endif


  
  !
  ! Create attributes for output variables
  !
  status = nf_put_att_text (foutid,terrid,'long_name', 21, 'surface geopotential')
  status = nf_put_att_text (foutid,terrid,'units', 5, 'm2/s2')
  status = nf_put_att_double (foutid, terrid, 'missing_value', nf_double, 1, fillvalue)
  status = nf_put_att_double (foutid, terrid, '_FillValue'   , nf_double, 1, fillvalue)
  !        status = nf_put_att_text (foutid,terrid,'filter', 35, 'area averaged from USGS 30-sec data')
  
  status = nf_put_att_double (foutid, sghid, 'missing_value', nf_double, 1, fillvalue)
  status = nf_put_att_double (foutid, sghid, '_FillValue'   , nf_double, 1, fillvalue)
  status = nf_put_att_text   (foutid, sghid, 'long_name' , 48, &
       'standard deviation of 3km cubed-sphere elevation and target grid elevation')
  status = nf_put_att_text   (foutid, sghid, 'units'     , 1, 'm')
  !        status = nf_put_att_text   (foutid, sghid, 'filter'    , 4, 'none')
  
  status = nf_put_att_double (foutid, sgh30id, 'missing_value', nf_double, 1, fillvalue)
  status = nf_put_att_double (foutid, sgh30id, '_FillValue'   , nf_double, 1, fillvalue)
  status = nf_put_att_text   (foutid, sgh30id, 'long_name' , 49, &
       'standard deviation of 30s elevation from 3km cubed-sphere cell average height')
  status = nf_put_att_text   (foutid, sgh30id, 'units'     , 1, 'm')
  !        status = nf_put_att_text   (foutid, sgh30id, 'filter'    , 4, 'none')
  
  status = nf_put_att_double (foutid, landm_coslatid, 'missing_value', nf_double, 1, fillvalue)
  status = nf_put_att_double (foutid, landm_coslatid, '_FillValue'   , nf_double, 1, fillvalue)
  status = nf_put_att_text   (foutid, landm_coslatid, 'long_name' , 23, 'smoothed land fraction')
  status = nf_put_att_text   (foutid, landm_coslatid, 'filter'    , 4, 'none')
!+++ARH  
  !status = nf_put_att_double (foutid, landfracid, 'missing_value', nf_double, 1, fillvalue)
  !status = nf_put_att_double (foutid, landfracid, '_FillValue'   , nf_double, 1, fillvalue)
  !status = nf_put_att_text   (foutid, landfracid, 'long_name', 21, 'gridbox land fraction')
  !!        status = nf_put_att_text   (foutid, landfracid, 'filter', 40, 'area averaged from 30-sec USGS raw data')
!---ARH  
  
  status = nf_put_att_double (foutid, areaid, 'missing_value', nf_double, 1, fillvalue)
  status = nf_put_att_double (foutid, areaid, '_FillValue'   , nf_double, 1, fillvalue)
  status = nf_put_att_text   (foutid, areaid, 'long_name' , 24, &
       'area of target grid cell')
  status = nf_put_att_text   (foutid, areaid, 'units'     , 1, 'm+2')

  status = nf_put_att_text (foutid,latvid,'long_name', 8, 'latitude')
  if (status .ne. NF_NOERR) call handle_err(status)
  status = nf_put_att_text (foutid,latvid,'units', 13, 'degrees_north')
  if (status .ne. NF_NOERR) call handle_err(status)
  !        status = nf_put_att_text (foutid,latvid,'units', 21, 'cell center locations')
  !        if (status .ne. NF_NOERR) call handle_err(status)
  
  status = nf_put_att_text (foutid,lonvid,'long_name', 9, 'longitude')
  if (status .ne. NF_NOERR) call handle_err(status)
  status = nf_put_att_text (foutid,lonvid,'units', 12, 'degrees_east')
  if (status .ne. NF_NOERR) call handle_err(status)
  !        status = nf_put_att_text (foutid,lonvid,'units' , 21, 'cell center locations')
  !        if (status .ne. NF_NOERR) call handle_err(status)


  call DATE_AND_TIME(DATE=datestring)
  status = nf_put_att_text (foutid,NF_GLOBAL,'history',25, 'Written on date: ' // datestring )
  if (status .ne. NF_NOERR) call handle_err(status)
  !
  ! End define mode for output file
  !
  status = nf_enddef (foutid)
  if (status .ne. NF_NOERR) call handle_err(status)
  !
  ! Write variable for output
  !
  print*,"writing terrain data",MINVAL(terr),MAXVAL(terr)
  status = nf_put_var_double (foutid, terrid, terr*9.80616)
  if (status .ne. NF_NOERR) call handle_err(status)
  print*,"done writing terrain data"
!+++ARH  
  !print*,"writing landfrac data",MINVAL(landfrac),MAXVAL(landfrac)
  !status = nf_put_var_double (foutid, landfracid, landfrac)
  !if (status .ne. NF_NOERR) call handle_err(status)
  !print*,"done writing landfrac data"
!---ARH  
  print*,"writing sgh data",MINVAL(sgh),MAXVAL(sgh)
  status = nf_put_var_double (foutid, sghid, sgh)
  if (status .ne. NF_NOERR) call handle_err(status)
  print*,"done writing sgh data"
  
  print*,"writing sgh30 data",MINVAL(sgh30),MAXVAL(sgh30)
  status = nf_put_var_double (foutid, sgh30id, sgh30)
  if (status .ne. NF_NOERR) call handle_err(status)
  print*,"done writing sgh30 data"
  
  print*,"writing landm_coslat data",MINVAL(landm_coslat),MAXVAL(landm_coslat)
  status = nf_put_var_double (foutid, landm_coslatid, landm_coslat)
  if (status .ne. NF_NOERR) call handle_err(status)
  print*,"done writing sgh30 data"
  !
  print*,"writing area data",MINVAL(area ),MAXVAL(area)
  status = nf_put_var_double (foutid, areaid, area)
  if (status .ne. NF_NOERR) call handle_err(status)
  print*,"done writing area data"
  
  print*,"writing lat data"
  if (maxval(lat)<45.0) then
    status = nf_put_var_double (foutid, latvid, lat*rad2deg)
  else
    status = nf_put_var_double (foutid, latvid, lat)
  endif
  if (status .ne. NF_NOERR) call handle_err(status)
  print*,"done writing lat data"
  
  print*,"writing lon data"
  if (maxval(lon)<100.0) then
    status = nf_put_var_double (foutid, lonvid, lon*rad2deg)    
  else
    status = nf_put_var_double (foutid, lonvid, lon)
  end if

  if (status .ne. NF_NOERR) call handle_err(status)
  print*,"done writing lon data"

  if (Lfind_ridges) then 
     print*,"writing MXDIS data",MINVAL(mxdis_target),MAXVAL(mxdis_target)
     status = nf_put_var_double (foutid, mxdisid, mxdis_target )
     if (status .ne. NF_NOERR) call handle_err(status)
     print*,"done writing MXDIS data"

     print*,"writing RISEQ  data",MINVAL(riseq_target),MAXVAL(riseq_target)
     status = nf_put_var_double (foutid, riseqid, riseq_target)
     if (status .ne. NF_NOERR) call handle_err(status)
     print*,"done writing RISEQ data"

     print*,"writing FALLQ  data",MINVAL(fallq_target),MAXVAL(fallq_target)
     status = nf_put_var_double (foutid, fallqid, fallq_target)
     if (status .ne. NF_NOERR) call handle_err(status)
     print*,"done writing FALLQ data"

     print*,"writing MXVRX  data",MINVAL(mxvrx_target),MAXVAL(mxvrx_target)
     status = nf_put_var_double (foutid, mxvrxid, mxvrx_target)
     if (status .ne. NF_NOERR) call handle_err(status)
     print*,"done writing MXVRX data"

     print*,"writing MXVRY  data",MINVAL(mxvry_target),MAXVAL(mxvry_target)
     status = nf_put_var_double (foutid, mxvryid, mxvry_target)
     if (status .ne. NF_NOERR) call handle_err(status)
     print*,"done writing MXVRY data"

     print*,"writing ANGLL data",MINVAL(ang22_target),MAXVAL(ang22_target)
     status = nf_put_var_double (foutid, ang22id, ang22_target )
     if (status .ne. NF_NOERR) call handle_err(status)
     print*,"done writing ANGLL data"

     print*,"writing ANGLX  data",MINVAL(anglx_target),MAXVAL(anglx_target)
     status = nf_put_var_double (foutid, anglxid, anglx_target)
     if (status .ne. NF_NOERR) call handle_err(status)
     print*,"done writing ANGLX data"

     print*,"writing ANISO  data",MINVAL(aniso_target),MAXVAL(aniso_target)
     status = nf_put_var_double (foutid, anisoid, aniso_target)
     if (status .ne. NF_NOERR) call handle_err(status)
     print*,"done writing ANISO data"

     print*,"writing ANIXY  data",MINVAL(anixy_target),MAXVAL(anixy_target)
     status = nf_put_var_double (foutid, anixyid, anixy_target)
     if (status .ne. NF_NOERR) call handle_err(status)
     print*,"done writing ANIXY data"

     print*,"writing HWDTH  data",MINVAL(hwdth_target),MAXVAL(hwdth_target)
     status = nf_put_var_double (foutid, hwdthid, hwdth_target)
     if (status .ne. NF_NOERR) call handle_err(status)
     print*,"done writing HWDTH data"

     print*,"writing WGHTS  data",MINVAL(wghts_target),MAXVAL(wghts_target)
     status = nf_put_var_double (foutid, wghtsid, wghts_target)
     if (status .ne. NF_NOERR) call handle_err(status)
     print*,"done writing WGHTS data"

     print*,"writing CLNGT  data",MINVAL(clngt_target),MAXVAL(clngt_target)
     status = nf_put_var_double (foutid, clngtid, clngt_target)
     if (status .ne. NF_NOERR) call handle_err(status)
     print*,"done writing CLNGT data"

     print*,"writing CWGHT  data",MINVAL(cwght_target),MAXVAL(cwght_target)
     status = nf_put_var_double (foutid, cwghtid, cwght_target)
     if (status .ne. NF_NOERR) call handle_err(status)
     print*,"done writing CWGHT data"

     print*,"writing COUNT  data",MINVAL(count_target),MAXVAL(count_target)
     status = nf_put_var_double (foutid, countid, count_target)
     if (status .ne. NF_NOERR) call handle_err(status)
     print*,"done writing COUNT data"


     print*,"writing TERR_UF  data",MINVAL(terr_uf_target),MAXVAL(terr_uf_target)
     status = nf_put_var_double (foutid, terrufid, terr_uf_target)
     if (status .ne. NF_NOERR) call handle_err(status)
     print*,"done writing TERR_UF data"

     print*,"writing SGH_UF  data",MINVAL(sgh_uf_target),MAXVAL(sgh_uf_target)
     status = nf_put_var_double (foutid, sghufid, sgh_uf_target)
     if (status .ne. NF_NOERR) call handle_err(status)
     print*,"done writing SGH_UF data"

     print*,"writing GBXAR  data",MINVAL(area_target),MAXVAL(area_target)
     status = nf_put_var_double (foutid, gbxarid, area_target)
     if (status .ne. NF_NOERR) call handle_err(status)
     print*,"done writing GBXAR data"

  endif


  !
  ! Close output file
  !
  print *,"close file"
  status = nf_close (foutid)
  if (status .ne. NF_NOERR) call handle_err(status)
end subroutine wrtncdf_unstructured
!
!**************************************************************     
! 
! if target grid is lat-lon output structured
!
!**************************************************************     
!
!+++ARH
!subroutine wrtncdf_rll(nlon,nlat,lpole,n,terr_in,landfrac_in,sgh_in,sgh30_in,landm_coslat_in,lon,lat,&
!     lprepare_fv_smoothing_routine,output_fname,Lfind_ridges)
subroutine wrtncdf_rll(nlon,nlat,lpole,n,terr_in,sgh_in,sgh30_in,landm_coslat_in,lon,lat,&
     lprepare_fv_smoothing_routine,output_fname,Lfind_ridges)
!---ARH
  use ridge_ana, only: nsubr, mxdis_target, mxvrx_target, mxvry_target, ang22_target, &
                       anglx_target, aniso_target, anixy_target, hwdth_target, wghts_target, & 
                       clngt_target, cwght_target, count_target,riseq_target,grid_length_scale, &
                       fallq_target

  use shared_vars, only : terr_uf_target, sgh_uf_target, area_target
  use shr_kind_mod, only: r8 => shr_kind_r8
  implicit none
  
#     include         <netcdf.inc>
  
  !
  ! Dummy arguments
  !
  integer, intent(in) :: n,nlon,nlat
  character(len=1024), intent(in) :: output_fname
  logical, intent(in) :: Lfind_ridges
  !
  ! lprepare_fv_smoothing_routine is to make a NetCDF file that can be used with the CAM-FV smoothing software
  !
  logical , intent(in) :: lpole,lprepare_fv_smoothing_routine
!+++ARH
  !real(r8),dimension(n)  , intent(in) :: terr_in, landfrac_in,sgh_in,sgh30_in,lon, lat, landm_coslat_in
  real(r8),dimension(n)  , intent(in) :: terr_in,sgh_in,sgh30_in,lon,lat,landm_coslat_in
!---ARH
  !
  ! Local variables
  !
  character (len=1024):: fout       ! NetCDF output file
  integer             :: foutid     ! Output file id
  integer             :: lonid, lonvid
  integer             :: latid, latvid, nrdgid
  integer             :: terrid
!+++ARH
  !integer             :: landfracid,sghid,sgh30id,landm_coslatid
  integer             :: sghid,sgh30id,landm_coslatid
!---ARH
  integer             :: status    ! return value for error control of netcdf routin

  integer             :: mxdisid, ang22id, anixyid, anisoid, mxvrxid, mxvryid, hwdthid, wghtsid, anglxid, gbxarid
  integer             :: sghufid, terrufid, clngtid, cwghtid, countid,riseqid,fallqid

!  integer, dimension(2) :: nc_lat_vid,nc_lon_vid
  character (len=8)   :: datestring
  real(r8), parameter :: fillvalue = 1.d36
  real(r8) :: ave
  
  real(r8),dimension(nlon) :: lonar       ! longitude array
  real(r8),dimension(nlat) :: latar       ! latitude array
  
  integer, dimension(3) :: rdgqdim
!+++ARH
  !integer, dimension(2) :: htopodim,landfdim,sghdim,sgh30dim,landmcoslatdim
  integer, dimension(2) :: htopodim,sghdim,sgh30dim,landmcoslatdim
!---ARH
  integer, dimension(1) :: londim,latdim
!+++ARH
  !real(r8),dimension(n) :: terr, landfrac,sgh,sgh30,landm_coslat
  real(r8),dimension(n) :: terr,sgh,sgh30,landm_coslat
!---ARH
  integer :: i,j
  
  IF (nlon*nlat.NE.n) THEN
    WRITE(*,*) "inconsistent input for wrtncdf_rll"
    write(*,*) "nlon,nlat,n:", nlon,nlat,n
    STOP
  END IF
  !
  ! we assume that the unstructured layout of the lat-lon grid is ordered in latitude rows, that is,
  ! unstructured index n is given by
  !
  !   n = (j-1)*nlon+i
  !
  ! where j is latitude index and i longitude index
  !
  do i = 1,nlon
    lonar(i)=  lon(i)
  enddo
  do j = 1,nlat
    latar(j)= lat((j-1)*nlon+1)
  enddo
  
  terr = terr_in
  sgh=sgh_in
  sgh30 =sgh30_in
!+++ARH
  !landfrac = landfrac_in
!---ARH
  landm_coslat = landm_coslat_in
  
  if (lpole) then
    write(*,*) "average pole control volume"
    !
    ! North pole - terr
    !
    ave = 0.0
    do i=1,nlon
      ave = ave + terr_in(i)
    end do
    terr(1:nlon) = ave/DBLE(nlon)
    !
    ! South pole
    !
    ave = 0.0
    do i=n-(nlon+1),n
      ave = ave + terr_in(i)
    end do
    terr(n-(nlon+1):n) = ave/DBLE(nlon)
    
    !
    ! North pole - sgh
    !
    ave = 0.0
    do i=1,nlon
      ave = ave + sgh_in(i)
    end do
    sgh(1:nlon) = ave/DBLE(nlon)
    !
    ! South pole
    !
    ave = 0.0
    do i=n-(nlon+1),n
      ave = ave + sgh_in(i)
    end do
    sgh(n-(nlon+1):n) = ave/DBLE(nlon)
    
    !
    ! North pole - sgh30
    !
    ave = 0.0
    do i=1,nlon
      ave = ave + sgh30_in(i)
    end do
    sgh30(1:nlon) = ave/DBLE(nlon)
    !
    ! South pole
    !
    ave = 0.0
    do i=n-(nlon+1),n
      ave = ave + sgh30_in(i)
    end do
    sgh30(n-(nlon+1):n) = ave/DBLE(nlon)
!+++ARH    
    !!
    !! North pole - landfrac
    !!
    !ave = 0.0
    !do i=1,nlon
    !  ave = ave + landfrac_in(i)
    !end do
    !landfrac(1:nlon) = ave/DBLE(nlon)
    !!
    !! South pole
    !!
    !ave = 0.0
    !do i=n-(nlon+1),n
    !  ave = ave + landfrac_in(i)
    !end do
    !landfrac(n-(nlon+1):n) = ave/DBLE(nlon)
!---ARH    
    !
    ! North pole - landm_coslat
    !
    ave = 0.0
    do i=1,nlon
      ave = ave + landm_coslat_in(i)
    end do
    landm_coslat(1:nlon) = ave/DBLE(nlon)
    !
    ! South pole
    !
    ave = 0.0
    do i=n-(nlon+1),n
      ave = ave + landm_coslat_in(i)
    end do
    landm_coslat(n-(nlon+1):n) = ave/DBLE(nlon)

  end if
  
  
  fout=TRIM(output_fname)
  !
  !  Create NetCDF file for output
  !
  print *,"Create NetCDF file for output"
  status = nf_create (fout, NF_64BIT_OFFSET , foutid)
  if (status .ne. NF_NOERR) call handle_err(status)
  !
  ! Create dimensions for output
  !
  print *,"Create dimensions for output"
  status = nf_def_dim (foutid, 'lon', nlon, lonid)
  if (status .ne. NF_NOERR) call handle_err(status)
  status = nf_def_dim (foutid, 'lat', nlat, latid)
  if (status .ne. NF_NOERR) call handle_err(status)

  if (Lfind_ridges) then 
     status = nf_def_dim (foutid, 'nrdg', nsubr, nrdgid)
     if (status .ne. NF_NOERR) call handle_err(status)
  endif



  !
  ! Create variable for output
  !
  print *,"Create variable for output"
  
  htopodim(1)=lonid
  htopodim(2)=latid

  if (lprepare_fv_smoothing_routine) then
    status = nf_def_var (foutid,'htopo', NF_DOUBLE, 2, htopodim, terrid)
  else
    status = nf_def_var (foutid,'PHIS', NF_DOUBLE, 2, htopodim, terrid)
  end if
  if (status .ne. NF_NOERR) call handle_err(status)
  
!+++ARH
  !landfdim(1)=lonid
  !landfdim(2)=latid
  !
  !if (lprepare_fv_smoothing_routine) then
  !  status = nf_def_var (foutid,'ftopo', NF_DOUBLE, 2, landfdim, landfracid)
  !else
  !  status = nf_def_var (foutid,'LANDFRAC', NF_DOUBLE, 2, landfdim, landfracid)
  !end if
  !if (status .ne. NF_NOERR) call handle_err(status)
!---ARH  
  sghdim(1)=lonid
  sghdim(2)=latid
  
  status = nf_def_var (foutid,'SGH', NF_DOUBLE, 2, sghdim, sghid)
  if (status .ne. NF_NOERR) call handle_err(status)
  
  sgh30dim(1)=lonid
  sgh30dim(2)=latid
  
  status = nf_def_var (foutid,'SGH30', NF_DOUBLE, 2, sgh30dim, sgh30id)
  if (status .ne. NF_NOERR) call handle_err(status)
  
  landmcoslatdim(1)=lonid
  landmcoslatdim(2)=latid
  
  status = nf_def_var (foutid,'LANDM_COSLAT', NF_DOUBLE, 2, landmcoslatdim, landm_coslatid)
  if (status .ne. NF_NOERR) call handle_err(status)
  
  latdim(1) = latid
  status = nf_def_var (foutid,'lat', NF_DOUBLE, 1, latdim, latvid)!
!  STATUS = NF_DEF_VAR (NCID, 'rh', NF_DOUBLE, 3, RHDIMS, RHID)

  if (status .ne. NF_NOERR) call handle_err(status)
  
  londim(1) = lonid
  status = nf_def_var (foutid,'lon', NF_DOUBLE, 1, londim, lonvid)
  if (status .ne. NF_NOERR) call handle_err(status)

  if (Lfind_ridges) then 


     status = nf_put_att_double (foutid,NF_GLOBAL,'grid_length_scale', NF_DOUBLE, 1, grid_length_scale )
     if (status .ne. NF_NOERR) call handle_err(status)

     rdgqdim(1) = lonid
     rdgqdim(2) = latid
     rdgqdim(3) = nrdgid

     status = nf_def_var (foutid,'TERR_UF', NF_DOUBLE, 2, rdgqdim(1:2) , terrufid)
     if (status .ne. NF_NOERR) call handle_err(status)
     status = nf_def_var (foutid,'SGH_UF', NF_DOUBLE, 2, rdgqdim(1:2) , sghufid)
     if (status .ne. NF_NOERR) call handle_err(status)
     status = nf_def_var (foutid,'GBXAR', NF_DOUBLE, 2, rdgqdim(1:2) , gbxarid)
     if (status .ne. NF_NOERR) call handle_err(status)


     status = nf_def_var (foutid,'MXDIS', NF_DOUBLE, 3, rdgqdim , mxdisid)
     if (status .ne. NF_NOERR) call handle_err(status)

     status = nf_def_var (foutid,'RISEQ', NF_DOUBLE, 3, rdgqdim , riseqid)
     if (status .ne. NF_NOERR) call handle_err(status)
     status = nf_def_var (foutid,'FALLQ', NF_DOUBLE, 3, rdgqdim , fallqid)
     if (status .ne. NF_NOERR) call handle_err(status)


     status = nf_def_var (foutid,'MXVRX', NF_DOUBLE, 3, rdgqdim , mxvrxid)
     if (status .ne. NF_NOERR) call handle_err(status)
     status = nf_def_var (foutid,'MXVRY', NF_DOUBLE, 3, rdgqdim , mxvryid)
     if (status .ne. NF_NOERR) call handle_err(status)

     status = nf_def_var (foutid,'ANGLL', NF_DOUBLE, 3, rdgqdim , ang22id)
     if (status .ne. NF_NOERR) call handle_err(status)
     status = nf_def_var (foutid,'ANGLX', NF_DOUBLE, 3, rdgqdim , anglxid)
     if (status .ne. NF_NOERR) call handle_err(status)

     status = nf_def_var (foutid,'ANISO', NF_DOUBLE, 3, rdgqdim , anisoid)
     if (status .ne. NF_NOERR) call handle_err(status)
     status = nf_def_var (foutid,'ANIXY', NF_DOUBLE, 3, rdgqdim , anixyid)
     if (status .ne. NF_NOERR) call handle_err(status)

     status = nf_def_var (foutid,'HWDTH', NF_DOUBLE, 3, rdgqdim , hwdthid)
     if (status .ne. NF_NOERR) call handle_err(status)
     status = nf_def_var (foutid,'WGHTS', NF_DOUBLE, 3, rdgqdim , wghtsid)
     if (status .ne. NF_NOERR) call handle_err(status)
     status = nf_def_var (foutid,'CLNGT', NF_DOUBLE, 3, rdgqdim , clngtid)
     if (status .ne. NF_NOERR) call handle_err(status)
     status = nf_def_var (foutid,'CWGHT', NF_DOUBLE, 3, rdgqdim , cwghtid)
     if (status .ne. NF_NOERR) call handle_err(status)
     status = nf_def_var (foutid,'COUNT', NF_DOUBLE, 3, rdgqdim , countid)
     if (status .ne. NF_NOERR) call handle_err(status)
  endif






  !
  ! Create attributes for output variables
  !
  status = nf_put_att_text (foutid,terrid,'long_name', 21, 'surface geopotential')
  status = nf_put_att_text (foutid,terrid,'units', 5, 'm2/s2')
  status = nf_put_att_text (foutid,terrid,'filter', 35, 'area averaged from ncube3000 data')
  status = nf_put_att_double (foutid, terrid, 'missing_value', nf_double, 1, fillvalue)
  status = nf_put_att_double (foutid, terrid, '_FillValue'   , nf_double, 1, fillvalue)
  
  
  status = nf_put_att_double (foutid, sghid, 'missing_value', nf_double, 1, fillvalue)
  status = nf_put_att_double (foutid, sghid, '_FillValue'   , nf_double, 1, fillvalue)
  status = nf_put_att_text   (foutid, sghid, 'long_name' , 48, &
       'standard deviation of 3km cubed-sphere elevation and target grid elevation')
  status = nf_put_att_text   (foutid, sghid, 'units'     , 1, 'm')
  status = nf_put_att_text   (foutid, sghid, 'filter'    , 4, 'none')
  
  status = nf_put_att_double (foutid, sgh30id, 'missing_value', nf_double, 1, fillvalue)
  status = nf_put_att_double (foutid, sgh30id, '_FillValue'   , nf_double, 1, fillvalue)
  status = nf_put_att_text   (foutid, sgh30id, 'long_name' , 49, &
       'standard deviation of 30s elevation from 3km cubed-sphere cell average height')
  status = nf_put_att_text   (foutid, sgh30id, 'units'     , 1, 'm')
  status = nf_put_att_text   (foutid, sgh30id, 'filter'    , 4, 'none')
  
  status = nf_put_att_double (foutid, landm_coslatid, 'missing_value', nf_double, 1, fillvalue)
  status = nf_put_att_double (foutid, landm_coslatid, '_FillValue'   , nf_double, 1, fillvalue)
  status = nf_put_att_text   (foutid, landm_coslatid, 'long_name' , 23, 'smoothed land fraction')
  status = nf_put_att_text   (foutid, landm_coslatid, 'filter'    , 4, 'none')
!+++ARH  
  !status = nf_put_att_double (foutid, landfracid, 'missing_value', nf_double, 1, fillvalue)
  !status = nf_put_att_double (foutid, landfracid, '_FillValue'   , nf_double, 1, fillvalue)
  !status = nf_put_att_text   (foutid, landfracid, 'long_name', 21, 'gridbox land fraction')
!  status = nf_put_att_text   (foutid, landfracid, 'filter', 40, 'area averaged from 30-sec USGS raw data')
!---ARH
  
  status = nf_put_att_text (foutid,latvid,'long_name', 8, 'latitude')
  if (status .ne. NF_NOERR) call handle_err(status)
  status = nf_put_att_text (foutid,latvid,'units', 13, 'degrees_north')
  if (status .ne. NF_NOERR) call handle_err(status)
  !        status = nf_put_att_text (foutid,latvid,'units', 21, 'cell center locations')
  !        if (status .ne. NF_NOERR) call handle_err(status)
  
  status = nf_put_att_text (foutid,lonvid,'long_name', 9, 'longitude')
  if (status .ne. NF_NOERR) call handle_err(status)
  status = nf_put_att_text (foutid,lonvid,'units', 12, 'degrees_east')
  if (status .ne. NF_NOERR) call handle_err(status)
  !        status = nf_put_att_text (foutid,lonvid,'units' , 21, 'cell center locations')
  !        if (status .ne. NF_NOERR) call handle_err(status)
  
!  status = nf_put_att_text (foutid,NF_GLOBAL,'source', 27, 'USGS 30-sec dataset GTOPO30')
!  if (status .ne. NF_NOERR) call handle_err(status)
!  status = nf_put_att_text (foutid,NF_GLOBAL,'title',  24, '30-second USGS topo data')
!  if (status .ne. NF_NOERR) call handle_err(status)
  call DATE_AND_TIME(DATE=datestring)
  status = nf_put_att_text (foutid,NF_GLOBAL,'history',25, 'Written on date: ' // datestring )
  if (status .ne. NF_NOERR) call handle_err(status)
  
  !
  ! End define mode for output file
  !
  status = nf_enddef (foutid)
  if (status .ne. NF_NOERR) call handle_err(status)
  !
  ! Write variable for output
  !
  print*,"writing terrain data",MINVAL(terr),MAXVAL(terr)
  if (lprepare_fv_smoothing_routine) then
    status = nf_put_var_double (foutid, terrid, terr)
  else
    status = nf_put_var_double (foutid, terrid, terr*9.80616)
  end if
  if (status .ne. NF_NOERR) call handle_err(status)
  print*,"done writing terrain data"
  
!+++ARH
  !print*,"writing landfrac data",MINVAL(landfrac),MAXVAL(landfrac)
  !status = nf_put_var_double (foutid, landfracid, landfrac)
  !if (status .ne. NF_NOERR) call handle_err(status)
  !print*,"done writing landfrac data"
!---ARH  
  print*,"writing sgh data",MINVAL(sgh),MAXVAL(sgh)
  status = nf_put_var_double (foutid, sghid, sgh)
  if (status .ne. NF_NOERR) call handle_err(status)
  print*,"done writing sgh data"
  
  print*,"writing sgh30 data",MINVAL(sgh30),MAXVAL(sgh30)
  status = nf_put_var_double (foutid, sgh30id, sgh30)
  if (status .ne. NF_NOERR) call handle_err(status)
  print*,"done writing sgh30 data"
  
  print*,"writing landm_coslat data",MINVAL(landm_coslat),MAXVAL(landm_coslat)
  status = nf_put_var_double (foutid, landm_coslatid, landm_coslat)
  if (status .ne. NF_NOERR) call handle_err(status)
  print*,"done writing sgh30 data"
  !
  print*,"writing lat data"
  status = nf_put_var_double (foutid, latvid, latar)
  if (status .ne. NF_NOERR) call handle_err(status)
  print*,"done writing lat data"
  
  print*,"writing lon data"
  status = nf_put_var_double (foutid, lonvid, lonar)
  if (status .ne. NF_NOERR) call handle_err(status)
  print*,"done writing lon data"


  if (Lfind_ridges) then 

     print*,"writing MXDIS  data",MINVAL(mxdis_target),MAXVAL(mxdis_target)
     status = nf_put_var_double (foutid, mxdisid, mxdis_target)
     if (status .ne. NF_NOERR) call handle_err(status)
     print*,"done writing MXDIS data"

     print*,"writing RISEQ  data",MINVAL(riseq_target),MAXVAL(riseq_target)
     status = nf_put_var_double (foutid, riseqid, riseq_target)
     if (status .ne. NF_NOERR) call handle_err(status)
     print*,"done writing RISEQ data"

     print*,"writing FALLQ  data",MINVAL(fallq_target),MAXVAL(fallq_target)
     status = nf_put_var_double (foutid, fallqid, fallq_target)
     if (status .ne. NF_NOERR) call handle_err(status)
     print*,"done writing FALLQ data"

     print*,"writing MXVRX  data",MINVAL(mxvrx_target),MAXVAL(mxvrx_target)
     status = nf_put_var_double (foutid, mxvrxid, mxvrx_target)
     if (status .ne. NF_NOERR) call handle_err(status)
     print*,"done writing MXVRX data"

     print*,"writing MXVRY  data",MINVAL(mxvry_target),MAXVAL(mxvry_target)
     status = nf_put_var_double (foutid, mxvryid, mxvry_target)
     if (status .ne. NF_NOERR) call handle_err(status)
     print*,"done writing MXVRY data"

     print*,"writing ANGLL  data",MINVAL(ang22_target),MAXVAL(ang22_target)
     status = nf_put_var_double (foutid, ang22id, ang22_target)
     if (status .ne. NF_NOERR) call handle_err(status)
     print*,"done writing ANGLL data"

     print*,"writing ANGLX  data",MINVAL(anglx_target),MAXVAL(anglx_target)
     status = nf_put_var_double (foutid, anglxid, anglx_target)
     if (status .ne. NF_NOERR) call handle_err(status)
     print*,"done writing ANGLX data"

     print*,"writing ANISO  data",MINVAL(aniso_target),MAXVAL(aniso_target)
     status = nf_put_var_double (foutid, anisoid, aniso_target)
     if (status .ne. NF_NOERR) call handle_err(status)
     print*,"done writing ANISO data"

     print*,"writing ANIXY  data",MINVAL(anixy_target),MAXVAL(anixy_target)
     status = nf_put_var_double (foutid, anixyid, anixy_target)
     if (status .ne. NF_NOERR) call handle_err(status)
     print*,"done writing ANIXY data"

     print*,"writing WGHTS  data",MINVAL(wghts_target),MAXVAL(wghts_target)
     status = nf_put_var_double (foutid, wghtsid, wghts_target)
     if (status .ne. NF_NOERR) call handle_err(status)
     print*,"done writing WGHTS data"

     print*,"writing HWDTH  data",MINVAL(hwdth_target),MAXVAL(hwdth_target)
     status = nf_put_var_double (foutid, hwdthid, hwdth_target)
     if (status .ne. NF_NOERR) call handle_err(status)
     print*,"done writing HWDTH data"

     print*,"writing CLNGT  data",MINVAL(clngt_target),MAXVAL(clngt_target)
     status = nf_put_var_double (foutid, clngtid, clngt_target)
     if (status .ne. NF_NOERR) call handle_err(status)
     print*,"done writing CLNGT data"

     print*,"writing CWGHT  data",MINVAL(cwght_target),MAXVAL(cwght_target)
     status = nf_put_var_double (foutid, cwghtid, cwght_target)
     if (status .ne. NF_NOERR) call handle_err(status)
     print*,"done writing CWGHT data"

     print*,"writing COUNT  data",MINVAL(count_target),MAXVAL(count_target)
     status = nf_put_var_double (foutid, countid, count_target)
     if (status .ne. NF_NOERR) call handle_err(status)
     print*,"done writing COUNT data"


     print*,"writing TERR_UF  data",MINVAL(terr_uf_target),MAXVAL(terr_uf_target)
     status = nf_put_var_double (foutid, terrufid, terr_uf_target)
     if (status .ne. NF_NOERR) call handle_err(status)
     print*,"done writing TERR_UF data"

     print*,"writing SGH_UF  data",MINVAL(sgh_uf_target),MAXVAL(sgh_uf_target)
     status = nf_put_var_double (foutid, sghufid, sgh_uf_target)
     if (status .ne. NF_NOERR) call handle_err(status)
     print*,"done writing SGH_UF data"

     print*,"writing GBXAR  data",MINVAL(area_target),MAXVAL(area_target)
     status = nf_put_var_double (foutid, gbxarid, area_target)
     if (status .ne. NF_NOERR) call handle_err(status)
     print*,"done writing GBXAR data"

  endif





  !
  ! Close output file
  !
  print *,"close file"
  status = nf_close (foutid)
  if (status .ne. NF_NOERR) call handle_err(status)
end subroutine wrtncdf_rll

!+++ARH
!
! write netCDF file
!
subroutine wrt_cube(ncube,terr_cube,rrfac_cube,output_file)
  use shr_kind_mod, only: r8 => shr_kind_r8
  use shared_vars
  implicit none
#     include         <netcdf.inc>

  !
  ! Dummy arguments
  !
  integer, intent(in) :: ncube
  real (r8), dimension(6*ncube*ncube)          , intent(in) :: terr_cube,rrfac_cube
  character(len=1024) :: output_file, tmp_string
  !
  ! Local variables
  !
  !-----------------------------------------------------------------------
  !
  !     grid coordinates and masks
  !
  !-----------------------------------------------------------------------

  real (r8), dimension(6*ncube*ncube) :: grid_center_lat  ! lat/lon coordinates for
  real (r8), dimension(6*ncube*ncube) :: grid_center_lon  ! each grid center in degrees

  integer  :: ncstat             ! general netCDF status variable
  integer  :: nc_grid_id         ! netCDF grid dataset id
  integer  :: nc_gridsize_id     ! netCDF grid size dim id
  integer  :: nc_gridrank_id     ! netCDF grid rank dim id
  integer  :: nc_griddims_id     ! netCDF grid dimension size id
  integer  :: nc_grdcntrlat_id   ! netCDF grid center lat id
  integer  :: nc_grdcntrlon_id   ! netCDF grid center lon id
  integer  :: nc_terr_id
  integer  :: nc_rrfac_id

  integer :: grid_dims

  character(90), parameter :: grid_name = 'equi-angular gnomonic cubed sphere grid'

  integer            :: status    ! return value for error control of netcdf routin
  integer            :: i,j,k
  character (len=8)  :: datestring

  integer  :: atm_add,n
  real(r8) :: xgno_ce,lon,ygno_ce,lat
  real(r8) :: da

  grid_dims = 6*ncube*ncube

  da = pi / DBLE(2*ncube)
  atm_add = 1
  do k=1,6
    do j=1,ncube
      ygno_ce = -piq + da * (DBLE(j-1)+0.5) !center of cell
      do i=1,ncube
        xgno_ce = -piq + da * (DBLE(i-1)+0.5)
        call CubedSphereRLLFromABP(xgno_ce, ygno_ce, k, lon, lat)
        grid_center_lon(atm_add  ) = lon*rad2deg
        grid_center_lat(atm_add  ) = lat*rad2deg
        atm_add = atm_add+1
      end do
    end do
  end do

  WRITE(*,*) "Create NetCDF file for output: ", TRIM(output_file)
  ncstat = nf_create (TRIM(output_file), NF_64BIT_OFFSET,nc_grid_id)
  call handle_err(ncstat)

  ncstat = nf_put_att_text (nc_grid_id, NF_GLOBAL, 'title',len_trim(grid_name), grid_name)
  call handle_err(ncstat)

  call DATE_AND_TIME(DATE=datestring)
  tmp_string = 'Written on date: ' // datestring
  status = nf_put_att_text (nc_grid_id,NF_GLOBAL,'history',len_trim(tmp_string), TRIM(tmp_string))
  call handle_err(ncstat)

  tmp_string='Peter Hjort Lauritzen (NCAR)'
  ncstat = nf_put_att_text (nc_grid_id, NF_GLOBAL, 'author',len_trim(tmp_string), TRIM(tmp_string))
  call handle_err(ncstat)

  WRITE(*,*) "define grid size dimension"
  ncstat = nf_def_dim (nc_grid_id, 'grid_size', 6*ncube*ncube, nc_gridsize_id)
  call handle_err(ncstat)

  WRITE(*,*) "define grid rank dimension"
  ncstat = nf_def_dim (nc_grid_id, 'grid_rank', 1, nc_gridrank_id)
  call handle_err(ncstat)

  WRITE(*,*) "define grid dimension size array"
  ncstat = nf_def_var (nc_grid_id, 'grid_dims', NF_INT,1, nc_gridrank_id, nc_griddims_id)
  call handle_err(ncstat)

  WRITE(*,*) "define grid center latitude array"
  ncstat = nf_def_var (nc_grid_id, 'lat', NF_DOUBLE,1, nc_gridsize_id, nc_grdcntrlat_id)
  call handle_err(ncstat)
  ncstat = nf_put_att_text (nc_grid_id, nc_grdcntrlat_id, 'units',13, 'degrees_north')
  call handle_err(ncstat)

  WRITE(*,*) "define grid center longitude array"
  ncstat = nf_def_var (nc_grid_id, 'lon', NF_DOUBLE,1, nc_gridsize_id, nc_grdcntrlon_id)
  call handle_err(ncstat)
  ncstat = nf_put_att_text (nc_grid_id, nc_grdcntrlon_id, 'units',12, 'degrees_east')
  call handle_err(ncstat)

  WRITE(*,*) "define terr_cube array"
  ncstat = nf_def_var (nc_grid_id, 'terr', NF_DOUBLE,1, nc_gridsize_id, nc_terr_id)
  call handle_err(ncstat)
  ncstat = nf_put_att_text (nc_grid_id, nc_terr_id, 'units',1, 'm')
  call handle_err(ncstat)

  WRITE(*,*) "define rrfac_cube array"
  ncstat = nf_def_var (nc_grid_id, 'rrfac_cube', NF_DOUBLE,1, nc_gridsize_id, nc_rrfac_id)
  call handle_err(ncstat)
  ncstat = nf_put_att_text (nc_grid_id, nc_rrfac_id, 'units',1, 'neXX/ne30')
  call handle_err(ncstat)

  WRITE(*,*) "end definition stage"
  ncstat = nf_enddef(nc_grid_id)
  call handle_err(ncstat)

  !-----------------------------------------------------------------------
  !
  !     write grid data
  !
  !-----------------------------------------------------------------------


  WRITE(*,*) "write grid data"
  ncstat = nf_put_var_int(nc_grid_id, nc_griddims_id, grid_dims)
  call handle_err(ncstat)

  WRITE(*,*) "write lat data"
  ncstat = nf_put_var_double(nc_grid_id, nc_grdcntrlat_id, grid_center_lat)
  call handle_err(ncstat)

  WRITE(*,*) "write lon data"
  ncstat = nf_put_var_double(nc_grid_id, nc_grdcntrlon_id, grid_center_lon)
  call handle_err(ncstat)
!
!  WRITE(*,*) "write terr data"
!  ncstat = nf_put_var_double(nc_grid_id, nc_terr_id, terr_cube)
!  call handle_err(ncstat)
!
  WRITE(*,*) "write rrfac data"
  ncstat = nf_put_var_double(nc_grid_id, nc_rrfac_id, rrfac_cube)
  call handle_err(ncstat)

  WRITE(*,*) "Close output file"
  ncstat = nf_close(nc_grid_id)
  call handle_err(ncstat)
end subroutine wrt_cube
!---ARH

!************************************************************************
!!handle_err
!************************************************************************
!
!!ROUTINE:      handle_err
!!DESCRIPTION:  error handler
!--------------------------------------------------------------------------

subroutine handle_err(status)
  
  implicit         none
  
#     include          <netcdf.inc>
  
  integer          status
  
  if (status .ne. nf_noerr) then
    print *, nf_strerror(status)
    stop 'Stopped'
  endif
  
end subroutine handle_err


SUBROUTINE coarsen(f,fcoarse,nf,n,dA_coarse)
  use shr_kind_mod, only: r8 => shr_kind_r8
  IMPLICIT NONE
  INTEGER, INTENT(in) :: n,nf
  REAL (R8), DIMENSION(n)       , INTENT(IN)  :: f
  REAL (R8), DIMENSION(n/(nf*nf)), INTENT(OUT) :: fcoarse             
  REAL(R8), DIMENSION(INT(SQRT(DBLE(n/6)))/nf,INT(SQRT(DBLE(n/6)))/nf),INTENT(OUT) :: dA_coarse
  !must be an even number
  !
  ! local workspace
  !
  ! ncube = INT(SQRT(DBLE(n/6)))
  
  REAL(R8), DIMENSION(INT(SQRT(DBLE(n/6))),INT(SQRT(DBLE(n/6)))):: dA        
  REAL (R8)    :: sum, sum_area,tmp
  INTEGER  :: jx,jy,jp,ii,ii_coarse,coarse_ncube,ncube
  INTEGER  :: jx_coarse,jy_coarse,jx_s,jy_s
  
  
  !        REAL(R8), DIMENSION(INT(SQRT(DBLE(n/6)))/nf,INT(SQRT(DBLE(n/6)))/nf) :: dAtmp
  
  ncube = INT(SQRT(DBLE(n/6)))
  coarse_ncube = ncube/nf
  
  IF (ABS(DBLE(ncube)/DBLE(nf)-coarse_ncube)>0.000001) THEN
    WRITE(*,*) "ncube/nf must be an integer"
    WRITE(*,*) "ncube and nf: ",ncube,nf
    STOP
  END IF
  
  da_coarse = 0.0
  
  WRITE(*,*) "compute all areas"
  CALL EquiangularAllAreas(ncube, dA)
  !        CALL EquiangularAllAreas(coarse_ncube, dAtmp)!dbg
  tmp = 0.0
  DO jp=1,6
    DO jy_coarse=1,coarse_ncube
      DO jx_coarse=1,coarse_ncube
        !
        ! inner loop
        !
        sum      = 0.0
        sum_area = 0.0
        DO jy_s=1,nf
          jy = (jy_coarse-1)*nf+jy_s
          DO jx_s=1,nf
            jx = (jx_coarse-1)*nf+jx_s
            ii = (jp-1)*ncube*ncube+(jy-1)*ncube+jx
            sum      = sum     +f(ii)*dA(jx,jy)
            sum_area = sum_area+dA(jx,jy)
            !                  WRITE(*,*) "jx,jy",jx,jy
          END DO
        END DO
        tmp = tmp+sum_area
        da_coarse(jx_coarse,jy_coarse) = sum_area
        !              WRITE(*,*) "jx_coarse,jy_coarse",jx_coarse,jy_coarse,&
        !                   da_coarse(jx_coarse,jy_coarse)-datmp(jx_coarse,jy_coarse)
        ii_coarse = (jp-1)*coarse_ncube*coarse_ncube+(jy_coarse-1)*coarse_ncube+jx_coarse
        fcoarse(ii_coarse) = sum/sum_area 
      END DO
    END DO
  END DO
  WRITE(*,*) "coarsened surface area",tmp-4.0*3.141592654
END SUBROUTINE COARSEN


  !*******************************************************************************
  !  At this point mapping arrays are calculated
  !
  !      weights_lgr_index_all: dimension(JALL). Index of target grid cell that contains
  !                             current exchange grid cell
  ! 
  !      weights_eul_index_all: dimension(JALL,3). 3 indices of cubed-sphere grid cell that 
  !                             contains current exchange grid cell:
  !
  !                                weights_eul_index_all(:,1) = x-index
  !                                weights_eul_index_all(:,2) = y-index
  !                                weights_eul_index_all(:,3) = panel/face number 1-6
  !
  !                             These are then converted to one-dimensional indices 
  !                             for cubed sphere variables terr(n), ... etc. 
  !
  !      weights_all:           dimension(JALL,nreconstrunction). Spherical area of
  !                             exchange grid cell (steradians)
  !
  !********************************************************************************


SUBROUTINE overlap_weights(weights_lgr_index_all,weights_eul_index_all,weights_all,&
     jall,ncube,ngauss,ntarget,ncorner,jmax_segments,target_corner_lon,target_corner_lat,nreconstruction)
  use shr_kind_mod, only: r8 => shr_kind_r8
  use remap
  IMPLICIT NONE
  
  
  INTEGER, INTENT(INOUT) :: jall !anticipated number of weights
  INTEGER, INTENT(IN)    :: ncube, ngauss, ntarget, jmax_segments, ncorner, nreconstruction
!!+++ARH
!  REAL(R8), DIMENSION(ntarget), INTENT(IN) :: target_center_lon,target_center_lat
!!---ARH  
  INTEGER, DIMENSION(jall,3), INTENT(OUT) :: weights_eul_index_all
  REAL(R8), DIMENSION(jall,nreconstruction)  , INTENT(OUT) :: weights_all
  INTEGER, DIMENSION(jall)  , INTENT(OUT) :: weights_lgr_index_all
  
  REAL(R8), DIMENSION(ncorner,ntarget), INTENT(INOUT) :: target_corner_lon, target_corner_lat
  
!xxx  INTEGER,  DIMENSION(ncorner+1) :: ipanel_array, ipanel_tmp
  INTEGER,  DIMENSION(9*(ncorner+1)) :: ipanel_tmp,ipanel_array
  REAL(R8), DIMENSION(ncorner)  :: lat, lon
  REAL(R8), DIMENSION(0:ncube+2):: xgno, ygno
  REAL(R8), DIMENSION(0:ncorner+1) :: xcell, ycell
  
  REAL(R8), DIMENSION(ngauss) :: gauss_weights, abscissae
  
  REAL(R8) :: da, tmp, alpha, beta
  REAL    (r8):: pi,piq,pih
  INTEGER :: i, j,ncorner_this_cell,k,ip,ipanel,ii,jx,jy,jcollect
  integer :: alloc_error, ilon,ilat
  
  REAL    (r8) :: rad2deg
  REAL    (r8) :: deps
  
  real(r8), allocatable, dimension(:,:) :: weights
  integer , allocatable, dimension(:,:) :: weights_eul_index
  
  
  LOGICAL:: ldbg = .FALSE.
  
  INTEGER :: jall_anticipated, count

  pi = 4.D0*DATAN(1.D0)
  piq = pi/4.D0
  pih = pi*0.5D0
  rad2deg = 180.D0/pi

  deps = 10.0D0*pi/180.0_r8
  
  jall_anticipated = jall
  
  ipanel_array = -99
  !
  da = pih/DBLE(ncube)
  xgno(0) = -bignum
  DO i=1,ncube+1
    xgno(i) = TAN(-piq+(i-1)*da)
  END DO
  xgno(ncube+2) = bignum
  ygno = xgno
  
  CALL glwp(ngauss,gauss_weights,abscissae)
  
  
  allocate (weights(jmax_segments,nreconstruction),stat=alloc_error )
  allocate (weights_eul_index(jmax_segments,2),stat=alloc_error )
  
  tmp = 0.0
  jall = 1
  DO i=1,ntarget
    if (MOD(i,10)==0)call progress_bar("# ", i, DBLE(100*i)/DBLE(ntarget))
    !
    !---------------------------------------------------          
    !
    ! determine how many vertices the cell has
    !
    !---------------------------------------------------
    !
    CALL remove_duplicates_latlon(ncorner,target_corner_lon(:,i),target_corner_lat(:,i),&
         ncorner_this_cell,lon,lat,1.0E-10)
  
!+++ARH
!      ldbg = .FALSE.
!      IF (i.eq.17427) ldbg = .TRUE.
!---ARH
 
    IF (ldbg) THEN
      !WRITE(*,*) "itarget",i
      !WRITE(*,*) "cell center ",target_center_lon(i),target_center_lat(i)
      WRITE(*,*) "number of vertices ",ncorner_this_cell
      WRITE(*,*) "vertices locations lon,",lon(1:ncorner_this_cell)*rad2deg
      WRITE(*,*) "vertices locations lat,",lat(1:ncorner_this_cell)*rad2deg
      DO j=1,ncorner_this_cell
        WRITE(*,*) lon(j)*rad2deg, lat(j)*rad2deg
      END DO
      WRITE(*,*) "  "
    END IF
    !
    !---------------------------------------------------
    !
    ! determine how many and which panels the cell spans
    !
    !---------------------------------------------------          
    !
#ifdef old    
    DO j=1,ncorner_this_cell
      CALL CubedSphereABPFromRLL(lon(j), lat(j), alpha, beta, ipanel_tmp(j), .TRUE.)
      IF (ldbg) WRITE(*,*) "ipanel for corner ",j," is ",ipanel_tmp(j)
    END DO
    ipanel_tmp(ncorner_this_cell+1) = ipanel_tmp(1)
    ! make sure to include possible overlap areas not on the face the vertices are located
    IF (MINVAL(lat(1:ncorner_this_cell))<-pi/6.0) THEN
      ! include South-pole panel in search
      ipanel_tmp(ncorner_this_cell+1) = 5
      IF (ldbg) WRITE(*,*)  "add panel 5 to search"
    END IF
    IF (MAXVAL(lat(1:ncorner_this_cell))>pi/6.0) THEN
      ! include North-pole panel in search
      ipanel_tmp(ncorner_this_cell+1) = 6
      IF (ldbg) WRITE(*,*)  "add panel 6 to search"
    END IF
    CALL remove_duplicates_integer(ncorner_this_cell+1,ipanel_tmp(1:ncorner_this_cell+1),&
         k,ipanel_array(1:ncorner_this_cell+1))
#endif
    !
    ! make sure to include possible overlap areas not on the face the vertices are located
    ! For example, a cell could be on panel 3 and 5 but have overlap area on panel 2
    count = 0
    do ilat=-1,1
      do ilon=-1,1
        DO j=1,ncorner_this_cell
          count=count+1
          CALL CubedSphereABPFromRLL(lon(j)+ilon*deps, lat(j)+ilat*deps, alpha, beta, ipanel_tmp(count), .TRUE.)
        END DO
      end do
    end do
    
    !
    ! remove duplicates in ipanel_tmp
    !
    CALL remove_duplicates_integer(count,ipanel_tmp(1:count),&
         k,ipanel_array(1:count))
    !
    !---------------------------------------------------
    !
    ! loop over panels with possible overlap areas
    !
    !---------------------------------------------------          
    !
    DO ip = 1,k
      ipanel = ipanel_array(ip)
      DO j=1,ncorner_this_cell
        ii = ipanel
        CALL CubedSphereABPFromRLL(lon(j), lat(j), alpha, beta, ii,.FALSE.)            
        IF (j==1) THEN
          jx = CEILING((alpha + piq) / da)
          jy = CEILING((beta  + piq) / da)
        END IF
        xcell(ncorner_this_cell+1-j) = TAN(alpha)
        ycell(ncorner_this_cell+1-j) = TAN(beta)
      END DO
      xcell(0) = xcell(ncorner_this_cell)
      ycell(0) = ycell(ncorner_this_cell)
      xcell(ncorner_this_cell+1) = xcell(1)
      ycell(ncorner_this_cell+1) = ycell(1)
      
      jx = MAX(MIN(jx,ncube+1),0)
      jy = MAX(MIN(jy,ncube+1),0)
      
      CALL compute_weights_cell(xcell(0:ncorner_this_cell+1),ycell(0:ncorner_this_cell+1),&
           jx,jy,nreconstruction,xgno,ygno,&
           1, ncube+1, 1,ncube+1, tmp,&
           ngauss,gauss_weights,abscissae,weights,weights_eul_index,jcollect,jmax_segments,&
           ncube,0,ncorner_this_cell,ldbg,i)
      
      weights_all(jall:jall+jcollect-1,1:nreconstruction)  = weights(1:jcollect,1:nreconstruction)

         
      !weights_eul_index_all(jall:jall+jcollect-1,1:2) = weights_eul_index(1:jcollect,:)
      weights_eul_index_all(jall:jall+jcollect-1,  1) = weights_eul_index(1:jcollect,1)
      weights_eul_index_all(jall:jall+jcollect-1,  2) = weights_eul_index(1:jcollect,2)
      weights_eul_index_all(jall:jall+jcollect-1,  3) = ipanel
      weights_lgr_index_all(jall:jall+jcollect-1    ) = i
      
      jall = jall+jcollect
      IF (jall>jall_anticipated) THEN
        WRITE(*,*) "more weights than anticipated"
        WRITE(*,*) "increase jall"
        STOP
      END IF
      IF (ldbg) WRITE(*,*) "jcollect",jcollect
    END DO
  END DO
  jall = jall-1
  WRITE(*,*) "sum of all weights divided by surface area of sphere  =",tmp/(4.0*pi)
  WRITE(*,*) "actual number of weights",jall
  WRITE(*,*) "anticipated number of weights",jall_anticipated
  IF (jall>jall_anticipated) THEN
    WRITE(*,*) "anticipated number of weights < actual number of weights"
    WRITE(*,*) "increase jall!"
    STOP
  END IF
!  WRITE(*,*) MINVAL(weights_all(1:jall,1)),MAXVAL(weights_all(1:jall,1))

  IF (ABS(tmp/(4.0*pi))-1.0>0.001) THEN
    WRITE(*,*) "sum of all weights does not match the surface area of the sphere"
    WRITE(*,*) "sum of all weights is : ",tmp
    WRITE(*,*) "surface area of sphere: ",4.0*pi
    STOP
  END IF




END SUBROUTINE overlap_weights

SUBROUTINE overlap_weights_cube_to_cube(ncube_coarse,weights_lgr_index_all,weights_eul_index_all,weights_all,&
     jall,ncube,ngauss,jmax_segments,nreconstruction)
  use shr_kind_mod, only: r8 => shr_kind_r8
  use remap
  IMPLICIT NONE
  
  INTEGER, PARAMETER :: ncorner = 4
  INTEGER, INTENT(INOUT) :: jall !anticipated number of weights
  INTEGER, INTENT(IN)    :: ncube, ncube_coarse,ngauss, jmax_segments,  nreconstruction
  
  INTEGER, DIMENSION(jall,2), INTENT(OUT) :: weights_eul_index_all
  REAL(R8), DIMENSION(jall,nreconstruction)  , INTENT(OUT) :: weights_all
  INTEGER, DIMENSION(jall,2)  , INTENT(OUT) :: weights_lgr_index_all
  
  
  REAL(R8), DIMENSION(0:ncube+2):: xgno, ygno
  REAL(R8), DIMENSION(ncube_coarse+1):: xgno_coarse, ygno_coarse
  REAL(R8), DIMENSION(0:ncorner+1) :: xcell, ycell
  
  REAL(R8), DIMENSION(ngauss) :: gauss_weights, abscissae
  
  REAL(R8) :: da, tmp, da_coarse
  REAL(R8) :: percentage_complete
  REAL(r8) :: pi,pih,piq
  REAL    (r8)            :: area_sphere_wt
  INTEGER :: i, j,jx,jy,jcollect
  integer :: alloc_error,count
  
!  REAL    (r8), PARAMETER :: rad2deg   = 180.0/pi
  
  real(r8), allocatable, dimension(:,:) :: weights
  integer , allocatable, dimension(:,:) :: weights_eul_index
  
  
  LOGICAL:: ldbg = .FAlSE.
  
  INTEGER :: jall_anticipated

  pi=4.D0*DATAN(1.D0)
  pih=pi/2.D0
  piq=pi/4.D0
  
  jall_anticipated = jall
  
  !
  da = pih/DBLE(ncube)
  xgno(0) = -bignum
  DO i=1,ncube+1
    xgno(i) = TAN(-piq+(i-1)*da)
  END DO
  xgno(ncube+2) = bignum
  ygno = xgno

  da_coarse = pih/DBLE(ncube_coarse)
  DO i=1,ncube_coarse+1
    xgno_coarse(i) = TAN(-piq+(i-1)*da_coarse)
  END DO
  ygno_coarse = xgno_coarse
  
  CALL glwp(ngauss,gauss_weights,abscissae)

  allocate (weights(jmax_segments,nreconstruction),stat=alloc_error )
  allocate (weights_eul_index(jmax_segments,2),stat=alloc_error )
  
  tmp = 0.0
  jall = 1
  count=0
  area_sphere_wt = 0.0D0

  DO j=1,ncube_coarse
    DO i=1,ncube_coarse
      count = count+1
      percentage_complete = DBLE(100*count)/DBLE((ncube_coarse)*(ncube_coarse))
      !        call sleep(1)       ! On most Unix systems
      call progress_bar(" ", count, percentage_complete)
      
      xcell(1) = xgno_coarse(i  );ycell(1) = ygno_coarse(j  );
      xcell(2) = xgno_coarse(i  );ycell(2) = ygno_coarse(j+1);
      xcell(3) = xgno_coarse(i+1);ycell(3) = ygno_coarse(j+1);
      xcell(4) = xgno_coarse(i+1);ycell(4) = ygno_coarse(j  );
      xcell(5) = xcell(1)        ;ycell(5) = ycell(1);
      xcell(0) = xcell(4)        ;ycell(0) = ycell(4);
      
      jx = CEILING((-piq+(i-1)*da_coarse+piq) / da)
      jy = CEILING((-piq+(j-1)*da_coarse+piq) / da)
      
      CALL compute_weights_cell(xcell(0:ncorner+1),ycell(0:ncorner+1),&
           jx,jy,nreconstruction,xgno,ygno,&
           1, ncube+1, 1,ncube+1, tmp,&
           ngauss,gauss_weights,abscissae,weights,weights_eul_index,jcollect,jmax_segments,&
           ncube,0,ncorner,ldbg,i+(j-1)*ncube_coarse)
      
      weights_all(jall:jall+jcollect-1,1:nreconstruction)  = weights(1:jcollect,1:nreconstruction)
      area_sphere_wt = area_sphere_wt + tmp
      
      weights_eul_index_all(jall:jall+jcollect-1,:) = weights_eul_index(1:jcollect,:)

      weights_lgr_index_all(jall:jall+jcollect-1,  1) = i
      weights_lgr_index_all(jall:jall+jcollect-1,  2) = j
      
      jall = jall+jcollect
      IF (jall>jall_anticipated) THEN
          WRITE(*,*) "more weights than anticipated"
          WRITE(*,*) "increase jall"
          STOP
        END IF
        IF (ldbg) WRITE(*,*) "jcollect",jcollect
      END DO
    END DO
  jall = jall-1
  WRITE(*,*) "actual number of weights",jall
  WRITE(*,*) "anticipated number of weights",jall_anticipated
  IF (jall>jall_anticipated) THEN
    WRITE(*,*) "anticipated number of weights < actual number of weights"
    WRITE(*,*) "increase jall!"
    STOP
  END IF
  WRITE(*,*) MINVAL(weights_all(1:jall,1)),MAXVAL(weights_all(1:jall,1))
  IF (ABS((area_sphere_wt-(4.D0*pi/6.D0))/area_sphere_wt)>0.001) THEN
    WRITE(*,*) "sum of all weights does not match the surface area of the sphere"
    WRITE(*,*) "sum of all weights is : ",area_sphere_wt
    WRITE(*,*) "surface area of sphere: ",4.0*pi/6.0
    STOP
  ELSE
    WRITE(*,*) "Absolute error in spanning surface area of panel:",(area_sphere_wt-(4.D0*pi/6.D0))/((4.D0*pi/6.D0))
    WRITE(*,*) "MIN/MAX of area weight                          :",MINVAL(weights_all(1:jall,1)),MAXVAL(weights_all(1:jall,1))
  END IF
END SUBROUTINE overlap_weights_cube_to_cube


!------------------------------------------------------------------------------
! SUBROUTINE CubedSphereABPFromRLL
!
! Description:
!   Determine the (alpha,beta,panel) coordinate of a point on the sphere from
!   a given regular lat lon coordinate.
!
! Parameters:
!   lon - Coordinate longitude
!   lat - Coordinate latitude
!   alpha (OUT) - Alpha coordinate
!   beta (OUT) - Beta coordinate
!   ipanel (OUT) - Face panel
!------------------------------------------------------------------------------
SUBROUTINE CubedSphereABPFromRLL(lon, lat, alpha, beta, ipanel, ldetermine_panel)
  use shr_kind_mod, only: r8 => shr_kind_r8
  use shared_vars, only: rotate_cube, pi, piq
  IMPLICIT NONE
  
  REAL    (R8), INTENT(IN)  :: lon, lat
  REAL    (R8), INTENT(OUT) :: alpha, beta
  INTEGER :: ipanel
  LOGICAL, INTENT(IN) :: ldetermine_panel
  
  ! Local variables
  REAL    (R8) :: xx, yy, zz, pm
  REAL    (R8) :: sx, sy, sz
  INTEGER  :: ix, iy, iz

  ! Translate to (x,y,z) space
  xx = COS(lon-rotate_cube) * COS(lat)
  yy = SIN(lon-rotate_cube) * COS(lat)
  zz = SIN(lat)
  
  pm = MAX(ABS(xx), ABS(yy), ABS(zz))
  
  ! Check maximality of the x coordinate
  IF (pm == ABS(xx)) THEN
    IF (xx > 0) THEN; ix = 1; ELSE; ix = -1; ENDIF
  ELSE
    ix = 0
  ENDIF

  ! Check maximality of the y coordinate
  IF (pm == ABS(yy)) THEN
    IF (yy > 0) THEN; iy = 1; ELSE; iy = -1; ENDIF
  ELSE
    iy = 0
  ENDIF
    
  ! Check maximality of the z coordinate
  IF (pm == ABS(zz)) THEN
    IF (zz > 0) THEN; iz = 1; ELSE; iz = -1; ENDIF
  ELSE
    iz = 0
  ENDIF
  
  ! Panel assignments
  IF (ldetermine_panel) THEN
    IF (iz  ==  1) THEN
      ipanel = 6; sx = yy; sy = -xx; sz = zz
      
    ELSEIF (iz  == -1) THEN
      ipanel = 5; sx = yy; sy = xx; sz = -zz
      
    ELSEIF ((ix == 1) .AND. (iy /= 1)) THEN
      ipanel = 1; sx = yy; sy = zz; sz = xx
      
    ELSEIF ((ix == -1) .AND. (iy /= -1)) THEN
      ipanel = 3; sx = -yy; sy = zz; sz = -xx
      
    ELSEIF ((iy == 1) .AND. (ix /= -1)) THEN
      ipanel = 2; sx = -xx; sy = zz; sz = yy
      
    ELSEIF ((iy == -1) .AND. (ix /=  1)) THEN
      ipanel = 4; sx = xx; sy = zz; sz = -yy
      
    ELSE
      WRITE(*,*) 'Fatal Error: CubedSphereABPFromRLL failed'
      WRITE(*,*) '(xx, yy, zz) = (', xx, ',', yy, ',', zz, ')'
      WRITE(*,*) 'pm =', pm, ' (ix, iy, iz) = (', ix, ',', iy, ',', iz, ')'
      STOP
    ENDIF
  ELSE
    IF (ipanel  ==  6) THEN
      sx = yy; sy = -xx; sz = zz
    ELSEIF (ipanel  == 5) THEN
      sx = yy; sy = xx; sz = -zz
    ELSEIF (ipanel == 1) THEN
      sx = yy; sy = zz; sz = xx        
    ELSEIF (ipanel == 3) THEN
      sx = -yy; sy = zz; sz = -xx
    ELSEIF (ipanel == 2) THEN
      sx = -xx; sy = zz; sz = yy
    ELSEIF (ipanel == 4) THEN
      sx = xx; sy = zz; sz = -yy
    ELSE
      WRITE(*,*) "ipanel out of range",ipanel
      STOP
    END IF
  END IF
  
  ! Use panel information to calculate (alpha, beta) coords
  alpha = ATAN(sx / sz)
  beta = ATAN(sy / sz)
  
END SUBROUTINE CubedSphereABPFromRLL

!------------------------------------------------------------------------------
! SUBROUTINE EquiangularAllAreas
!
! Description:
!   Compute the area of all cubed sphere grid cells, storing the results in
!   a two dimensional array.
!
! Parameters: 
!   icube - Resolution of the cubed sphere
!   dA (OUT) - Output array containing the area of all cubed sphere grid cells
!------------------------------------------------------------------------------
SUBROUTINE EquiangularAllAreas(icube, dA)
  use shr_kind_mod, only: r8 => shr_kind_r8        
  IMPLICIT NONE
  
  INTEGER, INTENT(IN)                           :: icube
  REAL (r8), DIMENSION(icube,icube), INTENT(OUT) :: dA
  
  ! Local variables
  INTEGER                       :: k, k1, k2
  REAL (r8)                          :: a1, a2, a3, a4
  REAL (r8), DIMENSION(icube+1,icube+1)  :: ang
  REAL (r8), DIMENSION(icube+1)      :: gp
  
  REAL    (r8):: pi, piq
  
  !#ifdef DBG 
  REAL (r8)   :: dbg1 !DBG
  !#endif


  pi = 4.D0*DATAN(1.D0)
  piq = pi/4.D0
  ! Recall that we are using equi-angular spherical gridding
  !   Compute the angle between equiangular cubed sphere projection grid lines.
  DO k = 1, icube+1
    gp(k) = -piq + (pi/DBLE(2*(icube))) * DBLE(k-1)
  ENDDO
  
  DO k2=1,icube+1
    DO k1=1,icube+1
      ang(k1,k2) =ACOS(-SIN(gp(k1)) * SIN(gp(k2)))
    ENDDO
  ENDDO
  
  DO k2=1,icube
    DO k1=1,icube
      a1 =      ang(k1  , k2  )
      a2 = pi - ang(k1+1, k2  )
      a3 = pi - ang(k1  , k2+1)
      a4 =      ang(k1+1, k2+1)      
      ! area = r*r*(-2*pi+sum(interior angles))
      DA(k1,k2) = -2.D0*pi+a1+a2+a3+a4
    ENDDO
  ENDDO
  
  !#ifdef DBG 
  ! Only for debugging - test consistency
  dbg1 = 0.0                           !DBG
  DO k2=1,icube
    DO k1=1,icube
      dbg1 = dbg1 + DA(k1,k2)         !DBG
    ENDDO
  ENDDO
  write(*,*) 'DAcube consistency: ',dbg1-4.0*pi/6.0 !DBG
  !#endif
END SUBROUTINE EquiangularAllAreas


!------------------------------------------------------------------------------
! SUBROUTINE CubedSphereRLLFromABP
!
! Description:
!   Determine the lat lon coordinate of a point on a sphere given its
!   (alpha,beta,panel) coordinate.
!
! Parameters:
!   alpha - Alpha coordinate
!   beta - Beta coordinate
!   panel - Cubed sphere panel id
!   lon (OUT) - Calculated longitude
!   lat (OUT) - Calculated latitude
!------------------------------------------------------------------------------
SUBROUTINE CubedSphereRLLFromABP(alpha, beta, ipanel, lon, lat)
  use shr_kind_mod, only: r8 => shr_kind_r8        
  use shared_vars, only: rotate_cube, pi, piq
  IMPLICIT NONE        
  REAL    (r8), INTENT(IN)  :: alpha, beta
  INTEGER     , INTENT(IN)  :: ipanel
  REAL    (r8), INTENT(OUT) :: lon, lat        
  ! Local variables
  REAL    (r8) :: xx, yy, zz

  
  ! Convert to cartesian coordinates
  CALL CubedSphereXYZFromABP(alpha, beta, ipanel, xx, yy, zz)        
  ! Convert back to lat lon
  lat = ASIN(zz)
  if (xx==0.0.and.yy==0.0) THEN
    lon = 0.0
  else
    lon = ATAN2(yy, xx) +rotate_cube 
    IF (lon<0.0) lon=lon+2.D0*pi
    IF (lon>2.D0*pi) lon=lon-2.D0*pi
  end if
END SUBROUTINE CubedSphereRLLFromABP

!------------------------------------------------------------------------------
! SUBROUTINE CubedSphereXYZFromABP
!
! Description:
!   Determine the Cartesian coordinate of a point on a sphere given its
!   (alpha,beta,panel) coordinate.
!
! Parameters:
!   alpha - Alpha coordinate
!   beta - Beta coordinate
!   panel - Cubed sphere panel id
!   xx (OUT) - Calculated x coordinate
!   yy (OUT) - Calculated y coordinate
!   zz (OUT) - Calculated z coordinate
!------------------------------------------------------------------------------
SUBROUTINE CubedSphereXYZFromABP(alpha, beta, ipanel, xx, yy, zz)
  use shr_kind_mod, only: r8 => shr_kind_r8        
  IMPLICIT NONE
  
  REAL    (r8), INTENT(IN)  :: alpha, beta
  INTEGER     , INTENT(IN)  :: ipanel
  REAL    (r8), INTENT(OUT) :: xx, yy, zz        
  ! Local variables
  REAL    (r8) :: a1, b1
  REAL    (r8) :: sx, sy, sz       
  
  ! Convert to Cartesian coordinates
  a1 = TAN(alpha)
  b1 = TAN(beta)
  
  sz = (1.0 + a1 * a1 + b1 * b1)**(-0.5)
  sx = sz * a1
  sy = sz * b1        
  ! Panel assignments
  IF (ipanel == 6) THEN
    yy = sx; xx = -sy; zz = sz          
  ELSEIF (ipanel == 5) THEN
    yy = sx; xx = sy; zz = -sz          
  ELSEIF (ipanel == 1) THEN
    yy = sx; zz = sy; xx = sz          
  ELSEIF (ipanel == 3) THEN
    yy = -sx; zz = sy; xx = -sz          
  ELSEIF (ipanel == 2) THEN
    xx = -sx; zz = sy; yy = sz          
  ELSEIF (ipanel == 4) THEN
    xx = sx; zz = sy; yy = -sz          
  ELSE
    WRITE(*,*) 'Fatal Error: Panel out of range in CubedSphereXYZFromABP'
    WRITE(*,*) '(alpha, beta, panel) = (', alpha, ',', beta, ',', ipanel, ')'
    STOP
  ENDIF
END SUBROUTINE CubedSphereXYZFromABP


SUBROUTINE remove_duplicates_integer(n_in,f_in,n_out,f_out)
  use shr_kind_mod, only: r8 => shr_kind_r8
  integer, intent(in) :: n_in
  integer,dimension(n_in), intent(in) :: f_in
  integer, intent(out) :: n_out
  integer,dimension(n_in), intent(out) :: f_out
  !
  ! local work space
  !
  integer :: k,i,j
  !
  ! remove duplicates in ipanel_tmp
  !
  k = 1
  f_out(1) = f_in(1)
  outer: do i=2,n_in
    do j=1,k
      !            if (f_out(j) == f_in(i)) then
      if (ABS(f_out(j)-f_in(i))<1.0E-10) then
        ! Found a match so start looking again
        cycle outer
      end if
    end do
    ! No match found so add it to the output
    k = k + 1
    f_out(k) = f_in(i)
  end do outer
  n_out = k

  !HACK FOR ANDES FILE
  do j=1,k
    if (f_out(j)>900) then
      n_out=k-1
    end if
  end do

END SUBROUTINE remove_duplicates_integer

SUBROUTINE remove_duplicates_latlon(n_in,lon_in,lat_in,n_out,lon_out,lat_out,tiny)
  use shr_kind_mod, only: r8 => shr_kind_r8
  integer, intent(in) :: n_in
  real(r8),dimension(n_in), intent(inout) :: lon_in,lat_in
  real, intent(in) :: tiny
  integer, intent(out) :: n_out
  real(r8),dimension(n_in), intent(out) :: lon_out,lat_out
  !
  ! local work space
  !
  integer :: k,i,j
  REAL    (r8) :: pi, pih

  pi = 4.D0*DATAN(1.D0)
  pih = pi*0.5D0
  !
  ! for pole points: make sure the longitudes are identical so that algorithm below works properly
  !
  do i=2,n_in
    if (abs(lat_in(i)-pih)<tiny.or.abs(lat_in(i)+pih)<tiny) then 
      lon_in(i) = lon_in(i-1)    
      write(*,*) "pole fix"
    end if
  end do

  lon_out = -9999999.9
  lat_out = -9999999.9
  !
  k = 1
  lon_out(1) = lon_in(1)
  lat_out(1) = lat_in(1)
  outer: do i=2,n_in
    do j=1,k
      if (ABS(lon_out(j)-lon_in(i))<tiny.AND.ABS(lat_out(j)-lat_in(i))<tiny) then
        ! Found a match so start looking again
        cycle outer
      end if
    end do
    ! No match found so add it to the output
    k = k + 1
    lon_out(k) = lon_in(i)
    lat_out(k) = lat_in(i)
  end do outer
  n_out = k
END SUBROUTINE remove_duplicates_latlon

SUBROUTINE internally_smooth(terr,terr_smooth,n,ncube_coarse,norder,nmono,npd,jmax_segments,ngauss)
  use shr_kind_mod, only: r8 => shr_kind_r8
  use reconstruct

  IMPLICIT NONE
  integer               , intent(in) :: ncube_coarse,norder,n,nmono,npd,jmax_segments,ngauss
  real(r8), dimension(n), intent(in) :: terr
  real(r8), dimension(n), intent(out) :: terr_smooth

  real(r8), allocatable, dimension(:,:)   :: weights_all_coarse
  integer , allocatable, dimension(:,:)   :: weights_eul_index_all_coarse
  integer , allocatable, dimension(:,:)   :: weights_lgr_index_all_coarse
  real(r8), allocatable, dimension(:,:,:) :: area_target_coarse
  real(r8), allocatable, dimension(:,:)   :: da_coarse,da
  real(r8), allocatable, dimension(:,:,:)  :: centroids
  real(r8), allocatable, dimension(:,:,:,:) :: recons
  real(r8), allocatable, dimension(:,:,:) :: terr_coarse !for internal smoothing
  
  integer :: ncoarse,nreconstruction,ncube,alloc_error,status,i
  integer :: jall_coarse,ii,jp,jx,jy,count
  integer :: jx_coarse,jy_coarse
  real(r8) :: vol_tmp, wt
  REAL    (r8) :: pi


  pi = 4.D0*DATAN(1.D0)

  nreconstruction = 1
  IF (norder>1) THEN
    IF (norder == 2) THEN
      nreconstruction = 3
    ELSEIF (norder == 3) THEN
      nreconstruction = 6
    END IF
  END IF
  !      WRITE(*,*) "untested software - uncomment this line of you know what you are doing!"
  !      STOP
  !
  !*****************************************************
  !
  ! smoothing topography internally
  !
  !*****************************************************
  !
  WRITE(*,*) "internally smoothing orography"
  !
  ! smooth topography internally
  !            
  ncube = INT(SQRT(DBLE(n/6)))
  ncoarse = ncube_coarse*ncube_coarse*6
  WRITE(*,*) "resolution of coarse grid", 90.0/ncube_coarse
  allocate ( terr_coarse(ncube_coarse,ncube_coarse,6),stat=alloc_error )
  if( alloc_error /= 0 ) then
    print*,'Program could not allocate space for terr'
    stop
  end if
  allocate ( area_target_coarse(ncube_coarse,ncube_coarse,6),stat=alloc_error)
  
  jall_coarse = (ncube*ncube*7) !anticipated number of weights
  WRITE(*,*) "anticipated",jall_coarse
  allocate (weights_all_coarse(jall_coarse,nreconstruction),stat=alloc_error )
  allocate (weights_eul_index_all_coarse(jall_coarse,2),stat=alloc_error )
  allocate (weights_lgr_index_all_coarse(jall_coarse,2),stat=alloc_error )

      !
  !
  !
  write(*,*) "computing weights",ncube_coarse,ncube
  CALL overlap_weights_cube_to_cube(ncube_coarse,weights_lgr_index_all_coarse,&
       weights_eul_index_all_coarse,weights_all_coarse,&
       jall_coarse,ncube,ngauss,jmax_segments,nreconstruction)  
  write(*,*) "done computing weights",jall_coarse

  allocate ( dA_coarse(ncube_coarse,ncube_coarse),stat=alloc_error )
  CALL EquiangularAllAreas(ncube_coarse, dA_coarse)
  !
  ! coarsen
  !
  terr_coarse = 0.D0
  do jp=1,6
    do count=1,jall_coarse
      jx_coarse   = weights_lgr_index_all_coarse(count,1)
      jy_coarse   = weights_lgr_index_all_coarse(count,2)
      !
      jx  = weights_eul_index_all_coarse(count,1)
      jy  = weights_eul_index_all_coarse(count,2)
      !
      ii = (jp-1)*ncube*ncube+(jy-1)*ncube+jx
      wt = weights_all_coarse(count,1)
      
      terr_coarse(jx_coarse,jy_coarse,jp) = terr_coarse(jx_coarse,jy_coarse,jp)+&
           wt*terr(ii)/dA_coarse(jx_coarse,jy_coarse) 
    end do
  end do
  !
  ! Check volume of coarsened topography
  !
  vol_tmp     = 0.D0
  DO jp=1,6
    DO jy=1,ncube_coarse
      DO jx=1,ncube_coarse
        vol_tmp = vol_tmp+terr_coarse(jx,jy,jp)*dA_coarse(jx,jy)
      END DO
    END DO
  END DO
  WRITE(*,*) "Volume of coarsened cubed-sphere terrain           :",vol_tmp


  IF (norder>1) THEN
    ALLOCATE(recons   (nreconstruction-1, ncube_coarse,ncube_coarse,6), STAT=status)
    ALLOCATE(centroids(nreconstruction-1, ncube_coarse,ncube_coarse), STAT=status)
    CALL get_reconstruction(terr_coarse,norder, nmono, recons, npd,da_coarse,&
         ncube_coarse,nreconstruction,centroids)
    SELECT CASE (nmono) 
    CASE (0)
      WRITE(*,*) "coarse grid reconstructions are not filtered with shape-preserving filter"
    CASE (1)
      WRITE(*,*) "coarse grid reconstructions are filtered with shape-preserving filter"
    CASE DEFAULT
      WRITE(*,*) "nmono out of range: ",nmono
      STOP
    END SELECT
    SELECT CASE (0)
    CASE (0)
      WRITE(*,*) "coarse grid reconstructions are not filtered with positive definite filter"
    CASE (1)
      WRITE(*,*) "coarse grid reconstructions filtered with positive definite filter"
    CASE DEFAULT
      WRITE(*,*) "npd out of range: ",npd
      STOP
    END SELECT
  END IF
  DEALLOCATE(dA_coarse)
  
  
  ! 
  ! do mapping
  !
  allocate ( dA(ncube,ncube),stat=alloc_error )
  CALL EquiangularAllAreas(ncube, dA)
  terr_smooth = 0.D0
!  recons = 0.0
  do jp=1,6
    do count=1,jall_coarse
      jx_coarse   = weights_lgr_index_all_coarse(count,1)
      jy_coarse   = weights_lgr_index_all_coarse(count,2)
      !
      jx  = weights_eul_index_all_coarse(count,1)
      jy  = weights_eul_index_all_coarse(count,2)
      !
      i  = (jp-1)*ncube_coarse*ncube_coarse+(jy_coarse-1)*ncube_coarse+jx_coarse
      ii = (jp-1)*ncube*ncube+(jy-1)*ncube+jx
      !

      
      if (norder==1) then
        wt = weights_all_coarse(count,1)
        terr_smooth(ii) = terr_smooth(ii) + wt*terr_coarse(jx_coarse,jy_coarse,jp)/dA(jx,jy)
      end if
      if (norder==2) then
        terr_smooth(ii) = terr_smooth(ii) + (weights_all_coarse(count,1)*(&
             !
             ! all constant terms 
             !
             terr_coarse(jx_coarse,jy_coarse,jp) &
             - recons(1,jx_coarse,jy_coarse,jp)*centroids(1,jx_coarse,jy_coarse) &
             - recons(2,jx_coarse,jy_coarse,jp)*centroids(2,jx_coarse,jy_coarse) &
             )+&
             !
             ! linear terms
             !
             weights_all_coarse(count,2)*recons(1,jx_coarse,jy_coarse,jp)+&
             weights_all_coarse(count,3)*recons(2,jx_coarse,jy_coarse,jp)&
             )/dA(jx,jy)
      end if
      if (norder==3) then
        terr_smooth(ii) = terr_smooth(ii) + (weights_all_coarse(count,1)*(&
             !
             ! all constant terms 
             !
             terr_coarse(jx_coarse,jy_coarse,jp) &
             - recons(1,jx_coarse,jy_coarse,jp)*centroids(1,jx_coarse,jy_coarse) &
             - recons(2,jx_coarse,jy_coarse,jp)*centroids(2,jx_coarse,jy_coarse) &
             !
             + recons(3,jx_coarse,jy_coarse,jp)*(2.D0*centroids(1,jx_coarse,jy_coarse)**2-centroids(3,jx_coarse,jy_coarse))&
             + recons(4,jx_coarse,jy_coarse,jp)*(2.D0*centroids(2,jx_coarse,jy_coarse)**2-centroids(4,jx_coarse,jy_coarse))&
             !
             + recons(5,jx_coarse,jy_coarse,jp)*(2.D0*centroids(1,jx_coarse,jy_coarse)*centroids(2,jx_coarse,jy_coarse)&
             -centroids(5,jx_coarse,jy_coarse))&
             )+&
             !
             ! linear terms
             !
             weights_all_coarse(count,2)*(&
             
             recons(1,jx_coarse,jy_coarse,jp)&
             
            - recons(3,jx_coarse,jy_coarse,jp)*2.D0*centroids(1,jx_coarse,jy_coarse)&
             - recons(5,jx_coarse,jy_coarse,jp)*    centroids(2,jx_coarse,jy_coarse)&
             )+&
             !
             weights_all_coarse(count,3)*(&
             recons(2,jx_coarse,jy_coarse,jp)&
             !
             - recons(4,jx_coarse,jy_coarse,jp)*2.D0*centroids(2,jx_coarse,jy_coarse)&
             - recons(5,jx_coarse,jy_coarse,jp)*    centroids(1,jx_coarse,jy_coarse)&
             )+&
             !
               ! quadratic terms
             !
             weights_all_coarse(count,4)*recons(3,jx_coarse,jy_coarse,jp)+&
             weights_all_coarse(count,5)*recons(4,jx_coarse,jy_coarse,jp)+&
             weights_all_coarse(count,6)*recons(5,jx_coarse,jy_coarse,jp))/dA(jx,jy)
      end if

    end do
  end do
  if (norder>1) then
    DEALLOCATE(centroids)
    DEALLOCATE(recons)
  end if
  DEALLOCATE(weights_all_coarse)
  !
  ! Check volume of smoothed topography
  !
  write(*,*) "start data"
  vol_tmp     = 0.D0
  DO jp=1,6
    DO jy=1,ncube
      DO jx=1,ncube
        ii = (jp-1)*ncube*ncube+(jy-1)*ncube+jx
        vol_tmp = vol_tmp+terr_smooth(ii)*dA(jx,jy)
      END DO
    END DO
  END DO
  WRITE(*,*) "Volume of smoothed cubed-sphere terrain           :",vol_tmp
  write(*,*) "end data"
  deallocate(da)

    END SUBROUTINE internally_smooth



   subroutine progress_bar(txt, n, x)
     use shr_kind_mod, only: r8 => shr_kind_r8
     implicit none
     character(*) :: txt
     integer :: n
     real(r8) :: x
     integer, parameter :: s = 5
     character :: c(0:s-1) = (/ achar(127), "/", "|", "/","-" /)
     character, parameter :: CR = achar(13)
     write( *, "((a1,a, t4,i10, f10.2,' percent  done ', a1, '  '))", advance = "NO") CR, txt, n, x, c(mod(n, s))
   end subroutine progress_bar
