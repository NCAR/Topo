!#define idealized_test
!
!  DESCRIPTION:  Remap topo data from cubed-sphere grid to target grid using rigorous remapping
!                (Lauritzen, Nair and Ullrich, 2010, J. Comput. Phys.)
!
!  Author: Peter Hjort Lauritzen (pel@ucar.edu), AMP/CGD/NCAR 
!          Julio Bacmeister, AMP/CGD/NCAR 
!          Adam Herrington, AMP/CGD/NCAR
!
! ex: ./cube_to_target --help to get list of long and short option names.
!
program convterr
  use shr_kind_mod, only: r8 => shr_kind_r8
  use smooth_topo_cube_sph
  use ridge_ana
  use shared_vars
  use reconstruct
  use f90getopt
  
  implicit none
#     include         <netcdf.inc>

  integer :: ncube                   !dimension of intermediate cubed-sphere grid
  
  integer :: alloc_error 
  !
  ! turn extra debugging on/off
  ! 
  logical :: ldbg=.false.
  real(r8):: wt
  integer :: ii,ip,jx,jy,jp,np,counti !counters,dimensions
  integer :: jmax_segments,jall,jall_anticipated !overlap segments
  integer, parameter :: ngauss = 3               !quadrature for line integrals
  
  integer                              :: ntarget, ncorner, nrank, nlon, nlat               !target grid dimensions
  logical                              :: ltarget_latlon,lpole                              !if target grid lat-lon
  real(r8), allocatable, dimension(:)  :: rrfac_target,target_rrfac
  real(r8), allocatable, dimension(:,:):: target_corner_lon, target_corner_lat              !target grid coordinates
  real(r8), allocatable, dimension(:)  :: target_center_lon, target_center_lat, target_area, area_target !target grid coordinates
  
  real(r8), allocatable, dimension(:,:) :: weights_all                        !overlap weights
  integer , allocatable, dimension(:)   :: weights_lgr_index_all              !overlap index
  integer , allocatable, dimension(:,:) :: weights_eul_index_all              !overlap index
  integer :: ix,iy  , i
  !
  ! volume of topography
  !
  real(r8) :: vol_target, vol_target_un, area_target_total,vol_source,area_source,mea_source
  
  logical :: lphis_gll=.false.
  logical :: llandfrac=.false. !if landfrac is on the intermediate cubed-sphere file it will be mapped to target grid
  !
  ! for internal filtering
  !
  real(r8), allocatable, dimension(:,:) :: da, terr_2(:,:,:),rrfac(:,:,:),rrfac_tmp(:,:,:)
  integer  :: nreconstruction
  real(r8) :: da_min_ncube, da_min_target ! used to compute jmax_segments
  real(r8) :: volterr, volterr_sm  
  !
  ! namelist variables
  !
  logical :: ldevelopment_diags    = .FALSE.
  logical :: lzero_negative_peaks  = .TRUE.
  logical :: lread_smooth_topofile = .FALSE.
  logical :: luse_prefilter        = .FALSE.
  logical :: lstop_after_smoothing = .FALSE.
  !
  ! Cubed sphere terr is band-pass filtered using circular kernels
  !                             *Radii* of smoothing circles
  integer :: ncube_sph_smooth_coarse = -1
  integer :: ncube_sph_smooth_fine   =  0
  !
  ! namelist variables for detection of sub-grid scale orientation
  ! i.e., "ridge finding"
  !
  logical :: lfind_ridges = .FALSE.
  !                             Ridge analysis takes place on
  !                             squares of 2*NW+1
  integer :: nwindow_halfwidth =  0
  !                             
  !                             for backwards compat with CESM2.0
  !                             Not used, 0 here for naming
  integer :: nridge_subsample = 0 !
  !
  logical :: lridgetiles = .FALSE.
  
  logical :: lregional_refinement = .FALSE. !set in read_target_grid if rrfac is on file
  integer :: rrfac_max = 1
  logical :: lread_pre_smoothtopo = .FALSE.      !use pre-smoothed (on intermediate cubed-sphere grid) topo file
  logical :: lwrite_rrfac_to_topo_file = .FALSE. !for debugging write rrfac on target grid to topo file
  logical :: linterp_phis = .FALSE.              !interpolate PHIS to grid center (instead of area average remapping; used for GLL grids)
  logical :: lsmoothing_over_ocean = .FALSE.      !default is that no smoothing is applied where landfrac=0; turn off
  logical :: ldistance_weighted_smoother         !use distance weighted smoother instead of Laplacian smoother

  real (r8):: nu_dt = -1
  integer  :: smooth_phis_numcycle=-1

  !
  INTEGER :: UNIT, ioptarg
  
  INTEGER :: NSCL_f, NSCL_c, nhalo,nsw
  !
  ! namelist filenames
  !
  character(len=1024) :: grid_descriptor_fname,intermediate_cubed_sphere_fname,output_fname=''
  character(len=1024) :: grid_descriptor_fname_gll
  character(len=1024) :: output_grid='', ofile,smooth_topo_fname = '',str_dir=''
  character(len=1024) :: rrfactor_fname, command_line_arguments, str, str_creator, str_source=''

  character(len=8)  :: date
  character(len=10) :: time


  type(option_s):: opts(23)
  !               
  !                     long name                   has     | short | specified    | required
  !                                                 argument| name  | command line | argument
  ! 
  opts(1 ) = option_s( "coarse_radius"             ,.true.    , 'c'   ,.false.       ,.false.)
  opts(2 ) = option_s( "fine_radius"               ,.true.    , 'f'   ,.false.       ,.false.)
  opts(3 ) = option_s( "grid_descriptor_file"      ,.true.    , 'g'   ,.false.       ,.true.)
  opts(4 ) = option_s( "help"                      ,.false.   , 'h'   ,.false.       ,.false.)
  opts(5 ) = option_s( "intermediate_cs_name"      ,.true.    , 'i'   ,.false.       ,.true.)
  opts(6 ) = option_s( "output_grid"               ,.true.    , 'o'   ,.false.       ,.true.)
  opts(7 ) = option_s( "use_prefilter"             ,.false.   , 'p'   ,.false.       ,.false.)
  opts(8 ) = option_s( "find_ridges"               ,.false.   , 'r'   ,.false.       ,.false.)
  opts(9)  = option_s( "stop_after_smooth"         ,.false.   , 'x'   ,.false.       ,.false.)
  opts(10) = option_s( "rrfac_max"                 ,.true.    , 'y'   ,.false.       ,.false.)
  opts(11) = option_s( "development_diags"         ,.false.   , 'z'   ,.false.       ,.false.)
  opts(12) = option_s( "zero_negative_peaks"       ,.false.   , '0'   ,.false.       ,.false.)
  opts(13) = option_s( "ridge2tiles"               ,.false.   , '1'   ,.false.       ,.false.)
  opts(14) = option_s( "smooth_topo_file"          ,.true.    , 't'   ,.false.       ,.false.)
  opts(15) = option_s( "write_rrfac_to_topo_file"  ,.true.    , 'd'   ,.false.       ,.false.)
  opts(16) = option_s( "name_email_of_creator"     ,.true.    , 'u'   ,.false.       ,.true.)
  opts(17) = option_s( "source_data_identifier"    ,.true.    , 'n'   ,.false.       ,.false.)
  opts(18) = option_s( "output_data_directory"     ,.true.    , 'q'   ,.false.       ,.false.)
  opts(19) = option_s( "grid_descriptor_file_gll"  ,.true.    , 'a'   ,.false.       ,.false.)
  opts(20) = option_s( "interpolate_phis"          ,.false.   , 's'   ,.false.       ,.false.)
  opts(21) = option_s( "laplace_nu_dt"             ,.true.    , 'b'   ,.false.       ,.false.)
  opts(22) = option_s( "smooth_phis_numcycle"      ,.true.    , 'l'   ,.false.       ,.false.)
  opts(23) = option_s( "smoothing_over_ocean"      ,.false.   , 'm'   ,.false.       ,.false.)
  
  ! END longopts
  ! If no options were committed
  if (command_argument_count() .eq. 0 ) call print_help
  !
  ! collect command line arguments in this string for netCDF meta data
  !
  command_line_arguments    = './cube_to_target'
  grid_descriptor_fname     = ''
  grid_descriptor_fname_gll = ''
  
  ! Process options one by one
  do
    select case( getopt( "c:f:g:hi:o:prxy:z01:t:du:n:q:a:sjb:l:m", opts ) ) ! opts is optional (for longopts only)
    case( char(0) )
      exit
    case( 'c' )
      read (optarg, '(i3)') ioptarg
      ncube_sph_smooth_coarse = ioptarg
      write(str,*) ioptarg
      command_line_arguments = TRIM(command_line_arguments)//' -c '//TRIM(ADJUSTL(str))
      opts(1)%specified = .true.
    case( 'f' )
      read (optarg, '(i3)') ioptarg
      ncube_sph_smooth_fine = ioptarg
      write(str,*) ioptarg
      command_line_arguments = TRIM(command_line_arguments)//' -f '//TRIM(ADJUSTL(str))
      opts(2)%specified = .true.
    case( 'g' )
      grid_descriptor_fname = optarg
      write(str,*) TRIM(optarg)
      command_line_arguments = TRIM(command_line_arguments)//' -g '//TRIM(ADJUSTL(str))
      opts(3)%specified = .true.
    case( 'h' )
      call print_help
      opts(4)%specified = .true.
    case( 'i' )
      intermediate_cubed_sphere_fname = optarg
      write(str,*) TRIM(optarg)
      command_line_arguments = TRIM(command_line_arguments)//' -i '//TRIM(ADJUSTL(str))
      opts(5)%specified = .true.
    case( 'o' )
      output_grid = optarg
      write(str,*) TRIM(optarg)
      command_line_arguments = TRIM(command_line_arguments)//' -o '//TRIM(ADJUSTL(str))
      opts(6)%specified = .true.
    case( 'p' )
      luse_prefilter=.TRUE.
      command_line_arguments = TRIM(command_line_arguments)//' -p '
      opts(7)%specified = .true.
    case( 'r' )
      lfind_ridges = .TRUE.
      command_line_arguments = TRIM(command_line_arguments)//' -r '
      opts(8)%specified = .true.
    case( 'x' )
      lstop_after_smoothing = .TRUE.
      command_line_arguments = TRIM(command_line_arguments)//' -x '//TRIM(ADJUSTL(str))
      opts(9)%specified = .true.
    case( 'y' )
      read (optarg, '(i3)') ioptarg
      rrfac_max = ioptarg
      lregional_refinement =.true.
      write(str,*) ioptarg
      command_line_arguments = TRIM(command_line_arguments)//' -y '//TRIM(ADJUSTL(str))
      opts(10)%specified = .true.
    case( 'z' )
      ldevelopment_diags = .TRUE.
      command_line_arguments = TRIM(command_line_arguments)//' -z '
      opts(11)%specified = .true.
    case( '0' )
      lzero_negative_peaks = .TRUE.
      command_line_arguments = TRIM(command_line_arguments)//' -0 '
      write(*,*) "check support"
      opts(12)%specified = .true.
      stop
    case( '1' )
      lridgetiles = .TRUE.
      command_line_arguments = TRIM(command_line_arguments)//' -1 '
      opts(13)%specified = .true.
    case( 't' )
      smooth_topo_fname = optarg
      write(str,*) TRIM(optarg)
      write(*,*) str
      command_line_arguments = TRIM(command_line_arguments)//' -t '//TRIM(ADJUSTL(str))
      opts(14)%specified = .true.
    case( 'd' )
      lwrite_rrfac_to_topo_file = .TRUE.
      command_line_arguments = TRIM(command_line_arguments)//' -d '
      opts(15)%specified = .true.
    case( 'u' )
      str_creator = optarg
      write(str,*) TRIM(optarg)
      command_line_arguments = TRIM(command_line_arguments)//' -u '//TRIM(ADJUSTL(str))
      opts(16)%specified = .true.
    case( 'n' )
      str_source = optarg
      write(str,*) TRIM(optarg)
      command_line_arguments = TRIM(command_line_arguments)//' -n '//TRIM(ADJUSTL(str))
      opts(17)%specified = .true.
    case( 'q' )
      str_dir = optarg
      write(str,*) TRIM(optarg)
      command_line_arguments = TRIM(command_line_arguments)//' -q '//TRIM(ADJUSTL(str))
      opts(18)%specified = .true.
    case( 'a' )
      lphis_gll=.TRUE.
      grid_descriptor_fname_gll = optarg
      write(str,*) TRIM(optarg)
      command_line_arguments = TRIM(command_line_arguments)//' -a '//TRIM(ADJUSTL(str))
      opts(19)%specified = .true.
    case( 's' )
      linterp_phis = .TRUE.
      command_line_arguments = TRIM(command_line_arguments)//' -s '
      opts(20)%specified = .true.
    case( 'b' )
      read (optarg, *) nu_dt
      write(str,*) nu_dt
      write(*,*) str
      command_line_arguments = TRIM(command_line_arguments)//' -u '//TRIM(ADJUSTL(str))
      opts(21)%specified = .true.
    case( 'l' )
      read (optarg, '(i5)') smooth_phis_numcycle
      write(str,*) smooth_phis_numcycle
      write(*,*) str
      command_line_arguments = TRIM(command_line_arguments)//' -l '//TRIM(ADJUSTL(str))
      opts(22)%specified = .true.
    case( 'm' )
      lsmoothing_over_ocean = .TRUE.
      command_line_arguments = TRIM(command_line_arguments)//' -m '
      opts(23)%specified = .true.
    case default
      write(*,*) "Option unknown: ",char(0)        
      stop
    end select
  end do
  
  if (TRIM(smooth_topo_fname).ne.'') then
    lread_smooth_topofile = .TRUE.
    write(*,*) " Use pre-computed smooth topo " 
    write(*,*) " File = ", trim(smooth_topo_fname)
  else 
    write(*,*) " No smooth topo"
  end if
  !
  ! check that all required arguments are specified/initialized
  ! (stopping after smoothing is only for developers so it
  ! is not checked if required arguments are present)
  !
  if (.not.lstop_after_smoothing) then
    do i=1,SIZE(opts)
      write(*,*) i
      if (.not.opts(i)%specified.and.opts(i)%required) then
        write(*,*) "Required argument not specified: ",opts(i)%name
        stop
      end if
    end do
  end if

  if (nu_dt<0.and.smooth_phis_numcycle<0) then
    ldistance_weighted_smoother = .TRUE.
    write(*,*) "Using distance weighted smoother"
    if (ncube_sph_smooth_coarse<0) then
      write(*,*) "ncube_sph_smooth_coarse must be specified; ncube_sph_smooth_coarse=",ncube_sph_smooth_coarse
      stop
    end if
  else
    if (nu_dt==0) then
      write(*,*) "Automatically setting nu_dt"
      nu_dt = 20e7
    end if
    write(*,*) " "
    write(*,*) "Recommended setting is nu_dt = 28e7 * (90.0/ncube)**2 where"
    write(*,*) "ncube = 90 for 1 degree, ncube = 45 for 0.5 degree, etc."
    write(*,*)
    
    write(*,*) "Laplacian smoother: setting nu_dt=",nu_dt

    ldistance_weighted_smoother = .FALSE.
    write(*,*) "Using Laplacian smoother"
  end if

  !
  ! calculate some defaults
  !
  if (lfind_ridges) then
    if (ldistance_weighted_smoother) then
      if (nwindow_halfwidth<=0) then
        nwindow_halfwidth = floor(real(ncube_sph_smooth_coarse)/sqrt(2.))
        !
        ! nwindow_halfwidth does NOT actually have to be even (JTB Mar 2022)
        !
        if (nwindow_halfwidth<5) then
          write(*,*) "nwindow_halfwidth can not be < 4"
          write(*,*) "setting nwindow_halfwidth=4"
          nwindow_halfwidth = 4
        end if
      end if
      if (ncube_sph_smooth_coarse<5) then
        write(*,*) "can not find ridges when ncube_sph_smooth_coarse<5"
        STOP
      end if
    else
      write(*,*) "find_ridges not support with Laplacian smoother - yet - help Bacmeister :-)"
    end if
  end if
  
  if (LEN(TRIM(str_source))==0) then
    !
    ! default setting for source topography
    !
    str_source = 'gmted2010_bedmachine'
  end if
  if (LEN(TRIM(str_dir))==0) then
    !
    ! default output directory
    !
    str_dir = 'output'
  end if
  
  if (ncube_sph_smooth_fine > 0) then 
    luse_prefilter=.TRUE.
  else
    luse_prefilter=.FALSE.
  end if
  
  write(*,*) " "
  write(*,*) "Namelist settings"
  write(*,*) "================="
  write(*,*)
  write(*,*) "ncube_sph_smooth_coarse         = ",ncube_sph_smooth_coarse
  write(*,*) "nwindow_halfwidth               = ",nwindow_halfwidth
  write(*,*) "ncube_sph_smooth_fine           = ",ncube_sph_smooth_fine
  write(*,*) "grid_descriptor_fname           = ",trim(grid_descriptor_fname)
  write(*,*) "intermediate_cubed_sphere_fname = ",trim(intermediate_cubed_sphere_fname)
  write(*,*) "output_grid                     = ",trim(output_grid)
  write(*,*) "luse_prefilter                  = ",luse_prefilter
  write(*,*) "lfind_ridges                    = ",lfind_ridges
  write(*,*) "rrfac_max                       = ",rrfac_max
  write(*,*) "ldevelopment_diags              = ",ldevelopment_diags
  write(*,*) "lzero_negative_peaks            = ",lzero_negative_peaks
  write(*,*) "lridgetiles                     = ",lridgetiles
  write(*,*) "smooth_topo_fname               = ",trim(smooth_topo_fname)
  write(*,*) "lwrite_rrfac_to_topo_file       = ",lwrite_rrfac_to_topo_file
  write(*,*) "str_source                      = ",trim(str_source)
  write(*,*) "interpolate_phis                = ",linterp_phis
  write(*,*) "nu_dt                           = ",nu_dt
  write(*,*) "smooth_phis_numcycle            = ",smooth_phis_numcycle
  write(*,*) "smoothing_over_ocean            = ",lsmoothing_over_ocean

  !*********************************************************
  
  call  set_constants
  
  ! Read in target grid
  !------------------------------------------------------------------------------------------------
  if (.not.lstop_after_smoothing) then
    call read_target_grid(grid_descriptor_fname,lregional_refinement,ltarget_latlon,lpole,nlat,nlon,ntarget,ncorner,nrank,&
         target_corner_lon, target_corner_lat, target_center_lon, target_center_lat, target_area, target_rrfac)
    allocate (area_target(ntarget),stat=alloc_error )
    area_target = 0.0
  end if
  
  ! Read in topo data on cubed sphere grid
  !------------------------------------------------------------------------------------------------

  call read_intermediate_cubed_sphere_grid(intermediate_cubed_sphere_fname,ncube,llandfrac)
  !
  ! sanity check
  !
  if (.not.lsmoothing_over_ocean.and..not.llandfrac) then
    write(*,*) "landfrac is needed for not smoothing over ocean"
    write(*,*) "LANDFRAC not found in file: ",intermediate_cubed_sphere_fname
    write(*,*) "ABORT"
    stop
  end if

#ifdef idealized_test
  call idealized(terr,ncube)
#endif

  allocate ( dA(ncube,ncube),stat=alloc_error )
  CALL EquiangularAllAreas(ncube, dA)
  
  !*********************************************************
  !
  ! set standard output file name
  !
  !*********************************************************
  if (ldistance_weighted_smoother) then
    if( ncube_sph_smooth_fine==0) then
      if(lfind_ridges) then
        nsw = nwindow_halfwidth
        call DATE_AND_TIME( DATE=date,TIME=time)
        write( ofile , "('_nc',i0.4, '_Co',i0.3)" ) ncube, ncube_sph_smooth_coarse
        
      else
        call DATE_AND_TIME( DATE=date,TIME=time)
        write( ofile , &
             "('_nc',i0.4,'_NoAniso_Co',i0.3)" ) ncube, ncube_sph_smooth_coarse
        
      endif
    else
      if(lfind_ridges) then
        nsw = nwindow_halfwidth
        call DATE_AND_TIME( DATE=date,TIME=time)
        write( ofile ,"('_nc',i0.4,'_Co',i0.3,'_Fi',i0.3 )" ) &
             ncube, ncube_sph_smooth_coarse,ncube_sph_smooth_fine
        
        
      else
        call DATE_AND_TIME( DATE=date,TIME=time)
        write( ofile , &
             "('_nc',i0.4,'_NoAniso_Co',i0.3,'_Fi',i0.3)" ) & 
             ncube, ncube_sph_smooth_coarse,ncube_sph_smooth_fine
      endif
    end if
  else
    !
    ! Laplacian smoother standard file name
    !
    if(lfind_ridges) then
      nsw = nwindow_halfwidth
      call DATE_AND_TIME( DATE=date,TIME=time)
      write( ofile ,"('_nc',i0.4,'_Laplace',i0.4)" ) &
           ncube, smooth_phis_numcycle
    else
      call DATE_AND_TIME( DATE=date,TIME=time)
      write( ofile , &
           "('_nc',i0.4,'_NoAniso_Laplace',i0.4)" ) & 
           ncube, smooth_phis_numcycle
    endif
  end if
  
  output_fname = TRIM(str_dir)//'/'//trim(output_grid)//'_'//trim(str_source)//trim(ofile)//'_'//date//'.nc'
  write(*,*) "Writing topo file to ",output_fname
  !*********************************************************
  !
  ! script for plotting
  !
  !*********************************************************
  if (.not.lstop_after_smoothing) then
    OPEN (unit = 711, file= 'plot.sh' ,STATUS='REPLACE',form="FORMATTED")
    write(711,*) 'ncl plot.ncl ''topoFile="',TRIM(output_fname),'"''',&
         ' ''scripFile="',TRIM(grid_descriptor_fname),'"'''
    CLOSE(711)
  end if  
  
  !+++ARH
  ! Compute overlap weights
  !------------------------------------------------------------------------------------------------
  
  ! On entry to overlap_weights 'jall' is a generous guess at the number of cells in
  ! in the 'exchange grid'
  allocate( rrfac(ncube,ncube,6)  )
  rrfac = 0.0
  
  if (.not.lstop_after_smoothing) then    
    if (nrank == 1) then
      da_min_ncube  = 4.0*pi/(6.0*DBLE(ncube*ncube))
      da_min_target = MAXVAL(target_area)
      if (da_min_target==0) then !bug with MPAS files
        jmax_segments = 100000
      else
        write(*,*) "using dynamic estimate for jmax_segments " 
        !++ jtb : Increased by 4x. Needed for c1440 FV3
        !jmax_segments = 10 * ncorner*NINT(da_min_target/da_min_ncube)!phl - FAILS for MPAS ~3km
        jmax_segments = 4 * ncorner*NINT(da_min_target/da_min_ncube)
      end if
      write(*,*) "ncorner, da_min_target, da_min_ncube =", ncorner, da_min_target, da_min_ncube
      write(*,*) "jmax_segments",jmax_segments,da_min_target,da_min_ncube
    else
      jmax_segments = 100000   !can be tweaked
    end if
    if (real(ntarget)*real(jmax_segments)>huge(real(jall_anticipated))) then
      jall_anticipated = 1080000000 !huge(jmax_segments) !anticipated number of weights (can be tweaked)
      write(*,*) "truncating jall_anticipated to ",jall_anticipated
    else
      jall_anticipated = ntarget*jmax_segments !anticipated number of weights (can be tweaked)
    end if
    if (jall_anticipated<0) then
      write(*,*) "anticipated number of overlaps likely not representable: jall_anticipated=", jall_anticipated
      stop
    else
      write(*,*) "anticipated number of overlaps jall_anticipated=", jall_anticipated
    end if
    
    jmax_segments = MIN( jmax_segments, 5000 )
    
    nreconstruction = 1
    allocate (weights_all(jall_anticipated,nreconstruction),stat=alloc_error )
    allocate (weights_eul_index_all(jall_anticipated,3),stat=alloc_error )
    allocate (weights_lgr_index_all(jall_anticipated),stat=alloc_error )
    jall=jall_anticipated
    
    if (.not.lstop_after_smoothing) then
      write(*,*) "Compute overlap weights: "
      CALL overlap_weights(weights_lgr_index_all,weights_eul_index_all,weights_all,&
           jall,ncube,ngauss,ntarget,ncorner,jmax_segments,target_corner_lon,target_corner_lat,nreconstruction,ldbg)
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
  endif !if (.not.lstop_after_smoothing)
  !!rrfac( 400:2400,2000:3000,4) = 4.
  
  !++jtb
  NSCL_c = 2*ncube_sph_smooth_coarse
  NSCL_f = 2*ncube_sph_smooth_fine
  nhalo  = NSCL_c  !*ncube_sph_smooth_iter ! 120      
  
  allocate( terr_sm(ncube,ncube,6)  )
  allocate( terr_dev(ncube,ncube,6) )
  allocate( terr_2(ncube,ncube,6)  )
  terr_2 = reshape( terr,    (/ncube,ncube,6/) )
  
  write(*,*) " SMOOTHING on CUBED SPHERE 10/7/15 "
  
  if (NSCL_c > 0 .or. .not.ldistance_weighted_smoother) then
    !+++ARH
    !!NSCL_c = 4*2*ncube_sph_smooth_coarse
    nhalo  = NSCL_c
    
    write(*,*) "rrfac_max",rrfac_max
    
    allocate( rrfac_tmp(ncube,ncube,6)  )
    rrfac_tmp(:,:,:) = rrfac(:,:,:)
    !---rrfac limiting
    rrfac = REAL(NINT(rrfac))
    where (rrfac.gt.rrfac_max) rrfac = rrfac_max
    where (rrfac.lt.1.0) rrfac = 1.0
    
    write(*,*) "RRFAC Massaged .... "
    write(*,*) "MINMAX RRFAC FINAL",minval(rrfac),maxval(rrfac)
    
    write(*,*) "Entering smooth_intermediate_topo_wrap ..."

    call  smooth_intermediate_topo_wrap (terr, rrfac, da,  & 
         ncube,nhalo, NSCL_f,NSCL_c, &
         terr_sm, terr_dev ,         &
         smooth_topo_fname,          &
         lread_smooth_topofile,      &
         ldistance_weighted_smoother,&
         luse_prefilter, &
         lstop_after_smoothing, &
         lregional_refinement, rrfac_max, &
         ldevelopment_diags, &
         command_line_arguments,str_dir,str_source,&
         output_grid,&
         nu_dt, smooth_phis_numcycle,landfrac,&
         lsmoothing_over_ocean,&
         smooth_topo_fname=smooth_topo_fname&
         )
    
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
  
  if (ldistance_weighted_smoother) then
    terr_sm = (volterr/volterr_sm)*terr_sm! should we do this?
  end if
  volterr_sm=0.
  do np=1,6 
    volterr_sm =  volterr_sm + sum( terr_sm(:,:,np) * da )
    end do
    
    write(*,*) " Topo volume  AFTER smoother AND fixer = ",volterr_sm/(6*sum(da))
    
    
    if(lfind_ridges) then
      nsw = nwindow_halfwidth
      nhalo=2*nsw
      
      call find_local_maxes ( terr_dev, ncube, nhalo, nsw ) !, npeaks, peaks )


      call find_ridges ( terr_dev, terr, ncube, nhalo, nsw,&
           ncube_sph_smooth_coarse   , ncube_sph_smooth_fine,   &
           ldevelopment_diags, lregional_refinement=lregional_refinement,&
           rr_factor = rrfac  )

    endif
    
    !*********************************************************
    !
    ! Begin actual remapping calculations
    !
    !*********************************************************
    
    call allocate_target_vars(ntarget)
    if (lwrite_rrfac_to_topo_file) allocate (rrfac_target(ntarget))
    
    !*********************************************************************
    !      In the following loops "counti" is the index of a piece of 
    !      the "exchange grid"
    !********************************************************************
    
    !
    ! Sum exchange grid cells within each target
    ! grid cell
    !
    do counti=1,jall
      i    = weights_lgr_index_all(counti)
      wt = weights_all(counti,1)
      area_target        (i) = area_target(i) + wt
    end do
    write(*,*) "MIN/MAX area_target",MINVAL(area_target),MAXVAl(area_target)
    write(*,*) "MIN/MAX target_area",MINVAL(target_area),MAXVAl(target_area)
    
    !+++ARH
    if (llandfrac) then
      write(*,*) "Remapping landfrac"
      write(*,*) "MIN/MAX before remap:", MINVAL(landfrac), MAXVAL(landfrac)
      landfrac_target = remap_field(landfrac,area_target,weights_eul_index_all(1:jall,:),weights_lgr_index_all(1:jall),&       
           weights_all(1:jall,:),ncube,jall,nreconstruction,ntarget)
      write(*,*) "MIN/MAX after remap:", MINVAL(landfrac_target), MAXVAL(landfrac_target)
    end if
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
    deallocate(landfrac)
    !---ARH
    !
    ! compute variance with respect to cubed-sphere data
    !
    WRITE(*,*) "compute variance with respect to 3km cubed-sphere data: SGH"
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
      
      sgh_target  (i) = sgh_target  (i) + wt*(terr_dev(ix,iy,ip))**2/area_target(i)
      terr_target (i) = terr_target (i) + wt*(terr_sm(ix,iy,ip))/area_target(i) 
      sgh_uf_target(i) = sgh_uf_target(i)+wt*((terr_uf_target(i)-terr(ii))**2)/area_target(i)
      
    end do

    if (linterp_phis) then
      write(*,*) "bilinear interpolation of PHIS from intermediate cubed-sphere grid to target grid"
      CALL bilinear_interp(ncube,ntarget,target_center_lon,target_center_lat,terr_sm(1:ncube,1:ncube,:),terr_target)
    end if


    if(lfind_ridges) then
      call remapridge2target(area_target,target_center_lon,target_center_lat, & 
           weights_eul_index_all(1:jall,:), & 
           weights_lgr_index_all(1:jall),weights_all(1:jall,:),ncube,jall,&
           nreconstruction,ntarget,nhalo,nsw, &
           ncube_sph_smooth_coarse,ncube_sph_smooth_fine,lzero_negative_peaks, &
           output_grid, ldevelopment_diags,&
           lregional_refinement=lregional_refinement,           &
           rr_factor = rrfac  )
    
      if (lridgetiles) then 
         call remapridge2tiles(area_target,target_center_lon,target_center_lat, & 
              weights_eul_index_all(1:jall,:), & 
              weights_lgr_index_all(1:jall),weights_all(1:jall,:),ncube,jall,&
              nreconstruction,ntarget,nhalo,ldevelopment_diags)
      endif
    endif

    if (lwrite_rrfac_to_topo_file) then
      rrfac_target = 0.0_r8
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
        
        rrfac_target  (i) = rrfac_target  (i) + wt*rrfac(ix,iy,ip)/area_target(i)
      end do
    end if
    DEALLOCATE(weights_all,weights_eul_index_all)
    
    write(*,*) " !!!!!!!!  ******* maxval terr_target " , maxval(terr_target)
    
    !!    if(lfind_ridges)  call paintridgeoncube ( ncube,nhalo,nsb,nsw , terr_dev )
    
    !
    ! zero out small values
    !
    if (llandfrac) then
      do i=1,ntarget
        !+++ARH
        IF (landfrac_target(i)<.001_r8)  landfrac_target(i) = 0.0D0
        !---ARH
      end do
      WRITE(*,*) "min/max of landfac_target                : ",MINVAL(landfrac_target    ),MAXVAL(landfrac_target    )
    end if

    DO i=1,ntarget
      IF (sgh_target(i)     <    0.5)  sgh_target(i)       = 0.0D0
      IF (sgh30_target(i)<       0.5D0) sgh30_target(i)    = 0.0D0
    END DO
    sgh30_target = SQRT(sgh30_target)
    sgh_target = SQRT(sgh_target)
    
    WRITE(*,*) "min/max of terr source                   : ",MINVAL(terr),MAXVAL(terr)
    WRITE(*,*) "min/max of terr_target                   : ",MINVAL(terr_target    ),MAXVAL(terr_target    )
    if (lwrite_rrfac_to_topo_file) then
      WRITE(*,*) "min/max of rrfac                       : ",MINVAL(rrfac_target),MAXVAL(rrfac_target)
    end if
    WRITE(*,*) "min/max of landm_coslat_target           : ",&
         MINVAL(landm_coslat_target),MAXVAL(landm_coslat_target)
    WRITE(*,*) "min/max of var30_target                  : ",MINVAL(sgh30_target   ),MAXVAL(sgh30_target   )
    WRITE(*,*) "min/max of var_target                    : ",MINVAL(sgh_target   ),MAXVAL(sgh_target   )
    
    write(*,*) " Model topo output file ",trim(output_fname)

    IF (ltarget_latlon) THEN
      if (lphis_gll) then
        write(*,*) "separate grid for PHIS not supported for lat-lon grids"
        stop
      end if
      CALL wrtncdf_rll(nlon,nlat,lpole,ntarget,terr_target,landfrac_target,sgh_target,sgh30_target,&
           landm_coslat_target,target_center_lon,target_center_lat,output_fname,&
           lfind_ridges,str_creator, command_line_arguments,area_target,llandfrac)
      
    ELSE
      CALL wrtncdf_unstructured(ntarget,terr_target,landfrac_target,sgh_target,sgh30_target,&
           landm_coslat_target,target_center_lon,target_center_lat,target_area,&
           output_fname,lfind_ridges, command_line_arguments,&
           lwrite_rrfac_to_topo_file,rrfac_target,str_creator,area_target,llandfrac)
    END IF
    DEALLOCATE(terr_target,landfrac_target,sgh30_target,sgh_target,landm_coslat_target)
    !---ARH

    DEALLOCATE(target_center_lon, target_center_lat, target_area,area_target)
    if (lwrite_rrfac_to_topo_file) deallocate (rrfac_target,target_rrfac)
    !**********************************************************************************************************************************
    !
    ! Dual grid physics grid configuration
    !
    !**********************************************************************************************************************************
    if (lphis_gll) then
      call read_target_grid(grid_descriptor_fname_gll,lregional_refinement,ltarget_latlon,lpole,nlat,nlon,ntarget,ncorner,nrank,&
           target_corner_lon, target_corner_lat, target_center_lon, target_center_lat, target_area, target_rrfac)
      allocate (terr_target(ntarget))
      if (linterp_phis) then
        CALL bilinear_interp(ncube,ntarget,target_center_lon,target_center_lat,terr_sm(1:ncube,1:ncube,:),terr_target)
      else
        allocate (weights_all(jall_anticipated,nreconstruction),stat=alloc_error )
        allocate (weights_eul_index_all(jall_anticipated,3),stat=alloc_error )
        allocate (weights_lgr_index_all(jall_anticipated),stat=alloc_error )
        weights_all = 0.0_r8
        weights_eul_index_all = 0
        weights_lgr_index_all = 0
        jall=jall_anticipated
        
        write(*,*) "Compute overlap weights for GLL grid: "
        CALL overlap_weights(weights_lgr_index_all,weights_eul_index_all,weights_all,&
             jall,ncube,ngauss,ntarget,ncorner,jmax_segments,target_corner_lon,target_corner_lat,nreconstruction,ldbg)
        
        allocate (area_target(ntarget))
        
        area_target = 0.0
        do counti=1,jall
          i    = weights_lgr_index_all(counti)
          wt = weights_all(counti,1)
          area_target        (i) = area_target(i) + wt
        end do
        
        write(*,*) "Remapping terrain"
        
        terr_target=0.0
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
          
          terr_target (i) = terr_target (i) + wt*(terr_sm(ix,iy,ip))/area_target(i) 
        end do
        DEALLOCATE(weights_all,weights_eul_index_all)
      end if
      CALL wrtncdf_unstructured_append_phis(ntarget,terr_target, &
           target_center_lon,target_center_lat,output_fname)
    end if
    end program convterr
    
  subroutine print_help
    write (6,*) "Usage: cube_to_target [options] ..."
    write (6,*) "Options:"
    write (6,*) "-c, --coarse_radius=<int>                      "
    write (6,*) "-f, --fine_radius=<int>                        "
    write (6,*) "-g, --grid_descriptor_file=<string>            "
    write (6,*) "-i, --intermediate_cs_name=<string>            "
    write (6,*) "-o, --output_grid=<string>                     "
    write (6,*) "-p, --use_prefilter            Enable [default:disabled]"
    write (6,*) "-r, --find_ridges              Enable [default:disabled]"
    write (6,*) "-x, --stop_after_smooth        Enable [default:disabled]"
    write (6,*) "-y, --rrfac_max=<int>                          "
    write (6,*) "-z, --zero_out_ocean_point_phis  Enable [default:disabled]"
    write (6,*) "-0, --zero_negative_peaks        Enable [default:disabled]"
    write (6,*) "-1, --ridge2tiles                Enable [default:disabled]"
    write (6,*) "-d, --write_rrfac_to_topo_file"
    write (6,*) "-u, --name_email_of_creator"
    write (6,*) "-n, --source_data_identifier"
    write (6,*) "-a, --grid_descriptor_file_gll"
    write (6,*) "-s, --interpolate_phis"
    write (6,*) "-j, --distance_weighted_smoother"
    stop
  end subroutine print_help
  !
  !
  !
  !+++ARH
  !subroutine wrtncdf_unstructured(n,terr,landfrac,sgh,sgh30,landm_coslat,lon,lat,area,output_fname,lfind_ridges)
  subroutine wrtncdf_unstructured(n,terr,landfrac,sgh,sgh30,landm_coslat,lon,lat,area,&
       output_fname,lfind_ridges,command_line_arguments,&
       lwrite_rrfac_to_topo_file,rrfac_target,str_creator,area_target,llandfrac)
    !---ARH
    use shared_vars, only : rad2deg
    use shr_kind_mod, only: r8 => shr_kind_r8
    use shared_vars, only : terr_uf_target, sgh_uf_target
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
    real(r8),dimension(n), intent(in)   :: terr,landfrac,sgh,sgh30,lon,lat,landm_coslat,area,area_target !xxx can we remove area_target?
    !---ARH
    character(len=1024),   intent(in) :: output_fname
    logical,               intent(in) :: lfind_ridges
    character(len=1024),   intent(in) :: command_line_arguments
    logical,               intent(in) :: lwrite_rrfac_to_topo_file
    real(r8),dimension(n), intent(in) :: rrfac_target
    character(len=1024),   intent(in) :: str_creator
    logical,               intent(in) :: llandfrac
    !
    ! Local variables
    !
    integer            :: foutid     ! Output file id
    integer            :: lonvid
    integer            :: latvid
    integer            :: terrid, areaid!,nid
    !+++ARH
    !integer            :: landfracid,sghid,sgh30id,landm_coslatid
    integer            :: landfracid,sghid,sgh30id,landm_coslatid
    !---ARH
    integer             :: mxdisid, ang22id, anixyid, anisoid, mxvrxid, mxvryid, hwdthid, wghtsid, anglxid, gbxarid
    integer             :: sghufid, terrufid, clngtid, cwghtid, countid,riseqid,fallqid,rrfacid
    
    integer            :: status    ! return value for error control of netcdf routin
    !  integer, dimension(2) :: nc_lat_vid,nc_lon_vid
    character (len=8)  :: datestring
    integer, dimension(2) :: nid
    
    real(r8), parameter :: fillvalue = 1.d36
    character(len=1024) :: str
    
    !
    !  Create NetCDF file for output
    !
    print *,"Create NetCDF file for output"
    status = nf_create (trim(output_fname), NF_64BIT_OFFSET , foutid)
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
    if (status .ne. NF_NOERR) then
      call handle_err(status)
      write(*,*) "PHIS error"
    end if

    if (lwrite_rrfac_to_topo_file) then
      status = nf_def_var (foutid,'RRFAC', NF_DOUBLE, 1, nid(1), rrfacid)
      if (status .ne. NF_NOERR) then
        call handle_err(status)
        write(*,*) "RRFAC error"
      end if
    end if

    if (llandfrac) then
      !+++ARH  
      status = nf_def_var (foutid,'LANDFRAC', NF_DOUBLE, 1, nid(1), landfracid)
      if (status .ne. NF_NOERR) call handle_err(status)
      !---ARH  
    end if
    status = nf_def_var (foutid,'SGH', NF_DOUBLE, 1, nid(1), sghid)
    if (status .ne. NF_NOERR) then
      call handle_err(status)
      write(*,*) "SGH error"
    end if
    
    status = nf_def_var (foutid,'SGH30', NF_DOUBLE, 1, nid(1), sgh30id)
    if (status .ne. NF_NOERR) then
      call handle_err(status)
      write(*,*) "SGH30 error"
    end if
    
    status = nf_def_var (foutid,'LANDM_COSLAT', NF_DOUBLE, 1, nid, landm_coslatid)
    if (status .ne. NF_NOERR) then
      call handle_err(status)
      write(*,*) "LANDM_COSLAT error"
    end if
    !
    status = nf_def_var (foutid,'area', NF_DOUBLE, 1, nid(1), areaid)
    if (status .ne. NF_NOERR) then
      call handle_err(status)
      write(*,*) "area error"
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
    
    if (Lfind_ridges) then 
      
      status = nf_def_var (foutid,'SGH_UF', NF_DOUBLE, 1, nid(1), sghufid)
      if (status .ne. NF_NOERR) then
        call handle_err(status)
        write(*,*) "SGH_UF error"
      end if
      status = nf_def_var (foutid,'TERR_UF', NF_DOUBLE, 1, nid(1), terrufid)
      if (status .ne. NF_NOERR) then
        call handle_err(status)
        write(*,*) "TERR_UF error"
      end if
      status = nf_def_var (foutid,'GBXAR', NF_DOUBLE, 1, nid(1), gbxarid)
      if (status .ne. NF_NOERR) then
        call handle_err(status)
        write(*,*) "GBXAR error"
      end if
      
      
      status = nf_def_var (foutid,'MXDIS', NF_DOUBLE, 2, nid , mxdisid)
      if (status .ne. NF_NOERR) then
        call handle_err(status)
        write(*,*) "MXDIS error"
      end if
      status = nf_def_var (foutid,'RISEQ', NF_DOUBLE, 2, nid , riseqid)
      if (status .ne. NF_NOERR) then
        call handle_err(status)
        write(*,*) "RISEQ error"
      end if
      status = nf_def_var (foutid,'FALLQ', NF_DOUBLE, 2, nid , fallqid)
      if (status .ne. NF_NOERR) then
        call handle_err(status)
        write(*,*) "FALLQ error"
      endif
      
      status = nf_def_var (foutid,'MXVRX', NF_DOUBLE, 2, nid , mxvrxid)
      if (status .ne. NF_NOERR) then
        call handle_err(status)
        write(*,*) "MXVRX error"
      end if
      
      status = nf_def_var (foutid,'MXVRY', NF_DOUBLE, 2, nid , mxvryid)
      if (status .ne. NF_NOERR) then
        call handle_err(status)
        write(*,*) "MXVRY"
      end if
      
      status = nf_def_var (foutid,'ANGLL', NF_DOUBLE, 2, nid , ang22id)
      if (status .ne. NF_NOERR) then
        call handle_err(status)
        write(*,*) "ANGLL error"
      end if
      status = nf_def_var (foutid,'ANGLX', NF_DOUBLE, 2, nid , anglxid)
      if (status .ne. NF_NOERR) then
        call handle_err(status)
        write(*,*) "ANGLX error"
      endif
      
      status = nf_def_var (foutid,'ANISO', NF_DOUBLE, 2, nid , anisoid)
      if (status .ne. NF_NOERR) then
        call handle_err(status)
        write(*,*) "ANISO error"
      end if
      status = nf_def_var (foutid,'ANIXY', NF_DOUBLE, 2, nid , anixyid)
      if (status .ne. NF_NOERR) then
        call handle_err(status)
        write(*,*) "ANIXY error"
      end if
      
      status = nf_def_var (foutid,'HWDTH', NF_DOUBLE, 2, nid , hwdthid)
      if (status .ne. NF_NOERR) then
        call handle_err(status)
        write(*,*) "HWDTH error"
      end if
      status = nf_def_var (foutid,'WGHTS', NF_DOUBLE, 2, nid , wghtsid)
      if (status .ne. NF_NOERR) then
        call handle_err(status)
        write(*,*) "WGHTS error"
      end if
      status = nf_def_var (foutid,'CWGHT', NF_DOUBLE, 2, nid , cwghtid)
      if (status .ne. NF_NOERR) then
        call handle_err(status)
        write(*,*) "CWGHT error"
      end if
      status = nf_def_var (foutid,'CLNGT', NF_DOUBLE, 2, nid , clngtid)
      if (status .ne. NF_NOERR) then
        call handle_err(status)
        write(*,*) "CLNGT error"
      end if
      status = nf_def_var (foutid,'COUNT', NF_DOUBLE, 2, nid , countid)
      if (status .ne. NF_NOERR) then
        call handle_err(status)
        write(*,*) "COUNT error"
      end if
      
    endif
    
    
    
    !
    ! Create attributes for output variables
    !
    status = nf_put_att_text (foutid,terrid,'long_name', 21, 'surface geopotential')
    status = nf_put_att_text (foutid,terrid,'units', 5, 'm2/s2')
    status = nf_put_att_double (foutid, terrid, 'missing_value', nf_double, 1, fillvalue)
    status = nf_put_att_double (foutid, terrid, '_FillValue'   , nf_double, 1, fillvalue)
    !        status = nf_put_att_text (foutid,terrid,'filter', 35, 'area averaged from USGS 30-sec data')
    if (lwrite_rrfac_to_topo_file) then
      status = nf_put_att_text (foutid,rrfacid,'long_name', 17, 'refinement factor')
      status = nf_put_att_text (foutid,rrfacid,'units', 0, '')
      status = nf_put_att_double (foutid, rrfacid, 'missing_value', nf_double, 1, fillvalue)
      status = nf_put_att_double (foutid, rrfacid, '_FillValue'   , nf_double, 1, fillvalue)
    end if
    
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
    if (llandfrac) then
      !+++ARH  
      status = nf_put_att_double (foutid, landfracid, 'missing_value', nf_double, 1, fillvalue)
      status = nf_put_att_double (foutid, landfracid, '_FillValue'   , nf_double, 1, fillvalue)
      status = nf_put_att_text   (foutid, landfracid, 'long_name', 21, 'gridbox land fraction')
      !!        status = nf_put_att_text   (foutid, landfracid, 'filter', 40, 'area averaged from 30-sec USGS raw data')
      !---ARH  
    end if
    
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
    call wrt_cesm_meta_data(foutid,command_line_arguments,str_creator)
    !
    ! End define mode for output file
    !
    status = nf_enddef (foutid)
    if (status .ne. NF_NOERR) call handle_err(status)
    !
    ! Write variable for output
    !
    print*,"writing terrain data",MINVAL(terr),MAXVAL(terr)
#ifdef idealized_test
    status = nf_put_var_double (foutid, terrid, terr)
#else
    status = nf_put_var_double (foutid, terrid, terr*9.80616)
#endif
    if (status .ne. NF_NOERR) call handle_err(status)
    print*,"done writing terrain data"

    if (lwrite_rrfac_to_topo_file) then
      status = nf_put_var_double (foutid, rrfacid, rrfac_target)
      if (status .ne. NF_NOERR) call handle_err(status)
      print*,"done writing rrfac data"
    end if
    if (llandfrac) then
      !+++ARH  
      print*,"writing landfrac data",MINVAL(landfrac),MAXVAL(landfrac)
      status = nf_put_var_double (foutid, landfracid, landfrac)
      if (status .ne. NF_NOERR) call handle_err(status)
      print*,"done writing landfrac data"
      !---ARH  
    end if
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

  subroutine wrtncdf_unstructured_append_phis(n,terr,lon,lat,output_fname)
    !---ARH
    use shared_vars, only : rad2deg
    use shr_kind_mod, only: r8 => shr_kind_r8
    implicit none
    
#     include         <netcdf.inc>
    
    !
    ! Dummy arguments
    !
    integer, intent(in) :: n
    real(r8),dimension(n), intent(in) :: terr,lon,lat
    !---ARH
    character(len=1024),   intent(in) :: output_fname
    !
    ! Local variables
    !
    integer            :: foutid     ! Output file id
    integer            :: lonvid
    integer            :: latvid
    integer            :: terrid
    integer            :: status    ! return value for error control of netcdf routin
    integer, dimension(2) :: nid
    
    real(r8), parameter :: fillvalue = 1.d36
    character(len=1024) :: str
    
    
    !
    !  Create NetCDF file for output
    !
    print *,"Opening ",trim(output_fname)
    status = nf_open (trim(output_fname), nf_write, foutid)
    if (status .ne. NF_NOERR) call handle_err(status)

    status = nf_redef(foutid)
    if (status .ne. NF_NOERR) call handle_err(status)
    !
    ! Create dimensions for output
    !
    status = nf_def_dim (foutid, 'ncol_gll', n, nid(1))
    if (status .ne. NF_NOERR) call handle_err(status)
    !
    print *,"Create variable for output"
    status = nf_def_var (foutid,'PHIS_gll', NF_DOUBLE, 1, nid(1), terrid)
    if (status .ne. NF_NOERR) then
      call handle_err(status)
      write(*,*) "PHIS_gll error"
    end if
    
    status = nf_def_var (foutid,'lat_gll', NF_DOUBLE, 1, nid(1), latvid)
    if (status .ne. NF_NOERR) then
      call handle_err(status)
      write(*,*) "lat error"
    end if
    
    status = nf_def_var (foutid,'lon_gll', NF_DOUBLE, 1, nid(1), lonvid)
    if (status .ne. NF_NOERR) then
      call handle_err(status)
      write(*,*) "lon error"
    end if
    
    

    !
    ! Create attributes for output variables
    !
    status = nf_put_att_text (foutid,terrid,'long_name', 33, 'surface geopotential on GLL grid')
    status = nf_put_att_text (foutid,terrid,'units', 5, 'm2/s2')
    status = nf_put_att_double (foutid, terrid, 'missing_value', nf_double, 1, fillvalue)
    status = nf_put_att_double (foutid, terrid, '_FillValue'   , nf_double, 1, fillvalue)
    !        status = nf_put_att_text (foutid,terrid,'filter', 35, 'area averaged from USGS 30-sec data')
    
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
    
    !
    ! Close output file
    !
    print *,"close file"
    status = nf_close (foutid)
    if (status .ne. NF_NOERR) call handle_err(status)
  end subroutine 
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
  subroutine wrtncdf_rll(nlon,nlat,lpole,n,terr_in,landfrac_in,sgh_in,sgh30_in,landm_coslat_in,lon,lat,&
       output_fname,Lfind_ridges,str_creator,command_line_arguments,area_target,llandfrac)
    !---ARH
    use ridge_ana, only: nsubr, mxdis_target, mxvrx_target, mxvry_target, ang22_target, &
         anglx_target, aniso_target, anixy_target, hwdth_target, wghts_target, & 
         clngt_target, cwght_target, count_target,riseq_target,grid_length_scale, &
         fallq_target
    
    use shared_vars, only : terr_uf_target, sgh_uf_target
    use shr_kind_mod, only: r8 => shr_kind_r8
    implicit none
    
#     include         <netcdf.inc>
    
    !
    ! Dummy arguments
    !
    integer, intent(in) :: n,nlon,nlat
    character(len=1024), intent(in) :: output_fname
    logical, intent(in) :: Lfind_ridges
    logical , intent(in) :: lpole
    !+++ARH
    !real(r8),dimension(n)  , intent(in) :: terr_in, landfrac_in,sgh_in,sgh30_in,lon, lat, landm_coslat_in
    real(r8),dimension(n)  , intent(in) :: terr_in, landfrac_in,sgh_in,sgh30_in,lon,lat,landm_coslat_in,area_target
    character(len=1024), intent(in) :: str_creator, command_line_arguments
    logical, intent(in) :: llandfrac
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
    integer             :: landfracid,sghid,sgh30id,landm_coslatid
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
    integer, dimension(2) :: htopodim,landfdim,sghdim,sgh30dim,landmcoslatdim
    !integer, dimension(2) :: htopodim,sghdim,sgh30dim,landmcoslatdim
    !---ARH
    integer, dimension(1) :: londim,latdim
    !+++ARH
    !real(r8),dimension(n) :: terr, landfrac,sgh,sgh30,landm_coslat
    real(r8),dimension(n) :: terr, landfrac,sgh,sgh30,landm_coslat
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
    if (llandfrac) then
      !+++ARH
      landfrac = landfrac_in
      !---ARH
    end if
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
      if (llandfrac) then
        !+++ARH    
        !!
        !! North pole - landfrac
        !!
        ave = 0.0
        do i=1,nlon
          ave = ave + landfrac_in(i)
        end do
        landfrac(1:nlon) = ave/DBLE(nlon)
        !
        ! South pole
        !
        ave = 0.0
        do i=n-(nlon+1),n
          ave = ave + landfrac_in(i)
        end do
        landfrac(n-(nlon+1):n) = ave/DBLE(nlon)
        !---ARH    
      end if
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
    
    status = nf_def_var (foutid,'PHIS', NF_DOUBLE, 2, htopodim, terrid)

    if (status .ne. NF_NOERR) call handle_err(status)
    
    !+++ARH
    landfdim(1)=lonid
    landfdim(2)=latid
    !
    if (llandfrac) then
      status = nf_def_var (foutid,'LANDFRAC', NF_DOUBLE, 2, landfdim, landfracid)
    end if

    if (status .ne. NF_NOERR) call handle_err(status)
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
    if (llandfrac) then
      !+++ARH  
      status = nf_put_att_double (foutid, landfracid, 'missing_value', nf_double, 1, fillvalue)
      status = nf_put_att_double (foutid, landfracid, '_FillValue'   , nf_double, 1, fillvalue)
      status = nf_put_att_text   (foutid, landfracid, 'long_name', 21, 'gridbox land fraction')
      status = nf_put_att_text   (foutid, landfracid, 'filter', 40, 'area averaged from 30-sec USGS raw data')
      !---ARH
    end if
    
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

    call wrt_cesm_meta_data(foutid,command_line_arguments,str_creator)   
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
    
    if (llandfrac) then
      !+++ARH
      print*,"writing landfrac data",MINVAL(landfrac),MAXVAL(landfrac)
      status = nf_put_var_double (foutid, landfracid, landfrac)
      if (status .ne. NF_NOERR) call handle_err(status)
      print*,"done writing landfrac data"
      !---ARH  
    end if

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

  subroutine wrt_cesm_meta_data(foutid,command_line_arguments,str_creator)
    implicit none    
#     include         <netcdf.inc>
    integer,               intent(in) :: foutid                              ! Output file id
    character(len=1024),   intent(in) :: command_line_arguments, str_creator ! Meta data strings

    character(len=1024) :: str
    integer             :: status    ! return value for error control of netcdf routin
    character (len=8)   :: datestring
    !
    ! Meta data for CESM compliance
    !
    !-data_summary		|	Short paragraph about the data.
    !-data_creator 		| 	Name and email of the person who created the dataset
    !-cesm_contact    	        |     	The liaison of the relevant WG
    !-creation_date    	        |     	Full date of dataset creation
    !-update_date    	        |     	Full date of most recent modification
    !-history 		        |     	Updates to changes made to the data.
    !-data_script    	        |     	script to generate data (will be available in the SVN repository ?)
    !-data_description_url 	|     	A web-page with a description if available  (this could be the climatedataguide webpage.)
    !-data_source_url  	        |     	The web page where the raw data can be downloaded
    !-data_reference   	        |     	Full reference for the dataset if available
    !-data_doi    		|     	If doi of data exists
    !-climo_years    	        |     	Year 1-year N of the climatological averaging period.
    !-data_mods    		|     	Any special substantive (non resolution) modifications that were made to the input data set purely for the purpose of using it in CESM. 
    !
    str = 'Topo file for NCAR CAM'
    status = nf_put_att_text (foutid,NF_GLOBAL,'data_summary',LEN(TRIM(str)), TRIM(str))
    if (status .ne. NF_NOERR) call handle_err(status)

    str = str_creator
    status = nf_put_att_text (foutid,NF_GLOBAL,'data_creator',LEN(TRIM(str)), TRIM(str))
    if (status .ne. NF_NOERR) call handle_err(status)
    
    call DATE_AND_TIME(DATE=datestring)
    status = nf_put_att_text (foutid,NF_GLOBAL,'creation_date',8, TRIM(datestring) )
    if (status .ne. NF_NOERR) call handle_err(status)
    
    str = 'Cecille Hannay'
    status = nf_put_att_text (foutid,NF_GLOBAL,'cesm_contact',LEN(TRIM(str)), TRIM(str))
    if (status .ne. NF_NOERR) call handle_err(status)
    
    str = 'https://github.com/NCAR/Topo.git'
    status = nf_put_att_text (foutid,NF_GLOBAL,'data_source',LEN(TRIM(str)), TRIM(str))
    if (status .ne. NF_NOERR) call handle_err(status)
    
    status = nf_put_att_text (foutid,NF_GLOBAL,'data_script',LEN(TRIM(command_line_arguments)),&
         TRIM(command_line_arguments))
    if (status .ne. NF_NOERR) call handle_err(status)
    
    str = TRIM('Lauritzen, P. H. et al.: NCAR global model topography generation software for unstructured grids, '// &
         'Geosci. Model Dev., 8, 1-12, doi:10.5194/gmd-8-1-2015, 2015.')
    status = nf_put_att_text (foutid,NF_GLOBAL,'data_reference',LEN(TRIM(str)), TRIM(str))
    if (status .ne. NF_NOERR) call handle_err(status)
  end subroutine wrt_cesm_meta_data

  
  
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
       jall,ncube,ngauss,ntarget,ncorner,jmax_segments,target_corner_lon,target_corner_lat,nreconstruction,ldbg)
    use shr_kind_mod, only: r8 => shr_kind_r8
    use remap
    IMPLICIT NONE
    
    
    INTEGER,                                     INTENT(INOUT):: jall !anticipated number of weights
    INTEGER,                                     INTENT(IN)   :: ncube, ngauss, ntarget, jmax_segments, ncorner, nreconstruction
    
    INTEGER, DIMENSION(jall,3),                  INTENT(OUT)  :: weights_eul_index_all
    REAL(R8), DIMENSION(jall,nreconstruction)  , INTENT(OUT)  :: weights_all
    INTEGER, DIMENSION(jall)  ,                  INTENT(OUT)  :: weights_lgr_index_all
    
    REAL(R8), DIMENSION(ncorner,ntarget),        INTENT(INOUT):: target_corner_lon, target_corner_lat
    LOGICAL,                                     INTENT(IN)   :: ldbg
    
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
      
      IF (ldbg) THEN
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
  
  SUBROUTINE bilinear_interp(ncube,ntarget,target_center_lon,target_center_lat,terr_cube,terr_target)
    use shr_kind_mod, only: r8 => shr_kind_r8
!    use reconstruct
    IMPLICIT NONE
    
    
    INTEGER,                            INTENT(IN) :: ncube, ntarget
    REAL(R8), DIMENSION(ntarget),       INTENT(IN) :: target_center_lon, target_center_lat
    REAL(R8), DIMENSION(ncube,ncube,6), INTENT(IN) :: terr_cube
    REAL(R8), DIMENSION(ntarget),       INTENT(OUT):: terr_target
    
    REAL(R8)                           :: lat, lon
    REAL(R8), DIMENSION(1:ncube+1)     :: xgno, ygno
    
    REAL(R8) :: da, alpha, beta, piq
    INTEGER  :: i,ip,jx,jy,nhalo

!    REAL(R8), DIMENSION(0:ncube+1,0:ncube+1,6) :: terr_cube_halo
    real(r8) :: x,y,x1,x2,y1,y2,w11,w12,w21,w22 !variables for bi-linear interpolation
    
    piq = DATAN(1.D0)
    da = 2.0_r8*piq/DBLE(ncube)

    DO i=1,ncube+1
      xgno(i) = TAN(-piq+(i-1)*da)
    END DO
    ygno = xgno
    DO i=1,ntarget
      if (MOD(i,10)==0)call progress_bar("# ", i, DBLE(100*i)/DBLE(ntarget))
      CALL CubedSphereABPFromRLL(target_center_lon(i), target_center_lat(i), alpha, beta, ip, .TRUE.)
      jx = CEILING((alpha + piq) / da)
      jy = CEILING((beta  + piq) / da)
      jx = MIN(MAX(1,jx),ncube-1); jy = MIN(MAX(1,jy),ncube-1)

      x = tan(alpha);y = tan(beta)

      x1 = xgno(jx); x2 = xgno(jx+1); y1 = ygno(jy); y2 = ygno(jy+1)

      w11 = (x2-x )*(y2-y )/((x2-x1)*(y2-y1))
      w12 = (x2-x )*(y -y1)/((x2-x1)*(y2-y1))
      w21 = (x -x1)*(y2-y )/((x2-x1)*(y2-y1))
      w22 = (x -x1)*(y -y1)/((x2-x1)*(y2-y1))
      terr_target(i) = w11*terr_cube(jx  ,jy,ip)+w12*terr_cube(jx  ,jy+1,ip)+&
                       w21*terr_cube(jx+1,jy,ip)+w22*terr_cube(jx+1,jy+1,ip)
    END DO
  END SUBROUTINE bilinear_interp
  
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


  subroutine idealized(psi,ncube)
    use shr_kind_mod, only: r8 => shr_kind_r8
    real(r8), intent(out) :: psi(ncube,ncube,6)
    integer, intent(in)   :: ncube

    real(r8) :: piq,da,alpha,beta,lon,lat
    integer  :: i,j,ip
    piq = DATAN(1.D0)
    da = 2.0_r8*piq/DBLE(ncube)
    do ip=1,6
      do j=1,ncube
        do i=1,ncube
          alpha = -piq+(i-0.5)*da; beta = -piq+(j-0.5)*da
          call CubedSphereRLLFromABP(alpha, beta, ip, lon, lat)
          psi(i,j,ip) = (cos(lat)*cos(lat)*cos(2.0*(lon)))!Y22
!          psi(i,j,ip) = (2.0+(sin(2.0*lat)**16)*cos(16.0*lon)) !Y16_32
        end do
      end do
    end do
  end subroutine idealized
