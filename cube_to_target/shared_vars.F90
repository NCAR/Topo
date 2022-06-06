MODULE shared_vars
  use shr_kind_mod, only: r8 => shr_kind_r8

!---ARH
!+++ARH
  real(r8), allocatable, dimension(:) :: landm_coslat, landfrac, terr, var30, refine_l
  !real(r8), allocatable, dimension(:) :: landm_coslat, terr, var30, refine_l
!---ARH
  integer,  allocatable, dimension(:) :: refine_li

!+++ARH
  real(r8), allocatable, dimension(:) :: landfrac_target, terr_target, sgh30_target, sgh_target
  !real(r8), allocatable, dimension(:) :: terr_target, sgh30_target, sgh_target
!---ARH
  real(r8), allocatable, dimension(:) :: landm_coslat_target
!+++ARH
  real(r8), allocatable, dimension(:) :: sumwgts_target
!---ARH
  real(r8), allocatable, dimension(:) :: terr_uf_target, sgh_uf_target

  real(r8) , allocatable, dimension(:,:,:) :: terr_sm, terr_dev

  REAL    (r8):: pi, piq, pih, deg2rad, rad2deg, rotate_cube

  contains 
    subroutine set_constants
      implicit none
      pi          = 4.D0*DATAN(1.D0)
      piq         = 0.25*pi
      pih         = 0.50*pi
      deg2rad     = pi/180.0
      rad2deg     = 180.0/pi
      rotate_cube = 0.0D0 !default is 0
    end subroutine set_constants

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
  
    
    subroutine smooth_terrain(lexternal_smooth_terr,ltarget_latlon,terr_target,ntarget,externally_smoothed_topo_file,&
         nlon,nlat)
      implicit none
#     include         <netcdf.inc>
      logical, intent(in) :: lexternal_smooth_terr, ltarget_latlon
      integer, intent(in) :: ntarget,nlon,nlat
      character(len=1024), intent(in) :: externally_smoothed_topo_file
      real(r8), intent(out):: terr_target(ntarget)


      integer :: ncid,status, alloc_error, ntarget_id, ntarget_smooth, phisid
      integer :: i,j,ii
      integer :: nlon_smooth,nlat_smooth
      real(r8), allocatable :: terr_smooth(:,:)

      WRITE(*,*) "smoothing PHIS"
      IF (lexternal_smooth_terr) THEN
        WRITE(*,*) "Using externally generated smoothed topography"
        
        status = nf_open(externally_smoothed_topo_file, 0, ncid)
        IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR(STATUS)           
        !
        IF (.NOT.ltarget_latlon) THEN
          !
          !*********************************************************
          !
          ! read in smoothed topography
          !
          !*********************************************************
          !
          status = NF_INQ_DIMID (ncid, 'ncol', ntarget_id    )
          status = NF_INQ_DIMLEN(ncid, ntarget_id , ntarget_smooth)
          IF (ntarget.NE.ntarget_smooth) THEN
            WRITE(*,*) "mismatch in smoothed data-set and target grid specification"
            WRITE(*,*) ntarget, ntarget_smooth
            STOP
          END IF
          status = NF_INQ_VARID(ncid, 'PHIS', phisid)
          !
          ! overwrite terr_target with smoothed version
          !
          status = NF_GET_VAR_DOUBLE(ncid, phisid,terr_target)
          terr_target = terr_target/9.80616
        ELSE
          !
          ! read in smoothed lat-lon topography
          !
          status = NF_INQ_DIMID(ncid, 'lon', ntarget_id)
          status = NF_INQ_DIMLEN(ncid, ntarget_id, nlon_smooth)
          status = NF_INQ_DIMID(ncid, 'lat', ntarget_id)
          status = NF_INQ_DIMLEN(ncid, ntarget_id, nlat_smooth)
          IF (nlon.NE.nlon_smooth.OR.nlat.NE.nlat_smooth) THEN
            WRITE(*,*) "smoothed topography dimensions do not match target grid dimensions"
            WRITE(*,*) "target grid  : nlon       ,nlat        =",nlon,nlat
            WRITE(*,*) "smoothed topo: nlon_smooth,nlat_smooth =",nlon_smooth,nlat_smooth
            STOP
          END IF
          ALLOCATE(terr_smooth(nlon_smooth,nlat_smooth),stat=alloc_error)
          status = NF_INQ_VARID(ncid, 'PHIS', phisid)
          status = NF_GET_VAR_DOUBLE(ncid, phisid,terr_smooth)
          !
          ! overwrite terr_target with smoothed version
          !
          ii=1
          DO j=1,nlat
            DO i=1,nlon
              terr_target(ii) = terr_smooth(i,j)/9.80616                  
              ii=ii+1
            END DO
          END DO
          DEALLOCATE(terr_smooth)
        END IF
      END IF
    
end subroutine smooth_terrain

subroutine allocate_target_vars(ntarget)
  implicit none
#     include         <netcdf.inc>
  integer, intent(in) :: ntarget

  integer :: alloc_error
  allocate (terr_target(ntarget),stat=alloc_error )
  if( alloc_error /= 0 ) then
    print*,'Program could not allocate space for terr_target'
    stop
  end if
!+++ARH
  allocate (landfrac_target(ntarget),stat=alloc_error )
  if( alloc_error /= 0 ) then
    print*,'Program could not allocate space for landfrac_target'
    stop
  end if
!---ARH
  allocate (landm_coslat_target(ntarget),stat=alloc_error )
  if( alloc_error /= 0 ) then
    print*,'Program could not allocate space for landfrac_target'
    stop
  end if
  allocate (sgh30_target(ntarget),stat=alloc_error )
  if( alloc_error /= 0 ) then
    print*,'Program could not allocate space for sgh30_target'
    stop
  end if
  allocate (sgh_target(ntarget),stat=alloc_error )
  if( alloc_error /= 0 ) then
    print*,'Program could not allocate space for sgh_target'
    stop
  end if

  allocate (terr_uf_target(ntarget),stat=alloc_error )
  if( alloc_error /= 0 ) then
    print*,'Program could not allocate space for terr_uf_target'
    stop
  end if
  allocate (sgh_uf_target(ntarget),stat=alloc_error )
  if( alloc_error /= 0 ) then
    print*,'Program could not allocate space for sgh_uf_target'
    stop
  end if
!+++ARH
  allocate (sumwgts_target(ntarget),stat=alloc_error )
  if( alloc_error /= 0 ) then
    print*,'Program could not allocate space for sgh_target'
    stop
  end if
!---ARH
end subroutine allocate_target_vars


subroutine read_intermediate_cubed_sphere_grid(intermediate_cubed_sphere_fname,ncube,llandfrac)
  implicit none
#     include         <netcdf.inc>
  character(len=1024), intent(in) :: intermediate_cubed_sphere_fname
  integer, intent(out) :: ncube
  logical, intent(out) :: llandfrac
  integer :: ncid,status, dimid, alloc_error, landid,n
  !
  !****************************************************
  !
  ! get dimension of cubed-sphere grid
  !
  !****************************************************
  !
  write(*,*) "Opening intermediate cubed-sphere file : ",TRIM(intermediate_cubed_sphere_fname)
  status = nf_open(TRIM(intermediate_cubed_sphere_fname), 0, ncid)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR(STATUS)
  
  status = NF_INQ_DIMID(ncid, 'grid_size', dimid)
  IF (status .NE. NF_NOERR) CALL HANDLE_ERR(status)
  status = NF_INQ_DIMLEN(ncid, dimid, n)
  IF (status .NE. NF_NOERR) CALL HANDLE_ERR(status)
  
  ncube = INT(SQRT(DBLE(n/6)))
  WRITE(*,*) "cubed-sphere dimension: ncube = ",ncube
  WRITE(*,*) "average grid-spacing at the Equator (degrees):" ,90.0/ncube
  
  if (status .ne. NF_NOERR) call handle_err(status)          
    !
  !****************************************************
  !
  ! read cubed-sphere 3km data
  !
  !****************************************************
  !
  status = NF_INQ_DIMID(ncid, 'grid_size', dimid)
  IF (status .NE. NF_NOERR) CALL HANDLE_ERR(status)
  status = NF_INQ_DIMLEN(ncid, dimid, n)
  IF (status .NE. NF_NOERR) CALL HANDLE_ERR(status)

  !
  !  Calculate no. of cells per side of cubed-sphere faces
  !   - 6 faces of ncube x ncube cells
  !  
  ncube = INT(SQRT(DBLE(n/6)))
  WRITE(*,*) "cubed-sphere dimension, ncube: ",ncube
  

  !********************************************
  !
  !  Begin reading variables from file
  !   - All are dimension(n) where n is the
  !     number of cells in the cubed sphere 
  !
  !********************************************
  !
  !  read LANDM_COSLAT. A smoothed land fraction variable.
  !  (left in for backwards compatibility with CAM4)
  !
  allocate ( landm_coslat(n),stat=alloc_error )
  if( alloc_error /= 0 ) then
    print*,'Program could not allocate space for landm_coslat'
    stop
  end if
  
  status = NF_INQ_VARID(ncid, 'LANDM_COSLAT', landid)
  IF (status .NE. NF_NOERR) CALL HANDLE_ERR(status)

  status = NF_GET_VAR_DOUBLE(ncid, landid,landm_coslat)
  IF (status .NE. NF_NOERR) CALL HANDLE_ERR(status)
  WRITE(*,*) "min/max of landm_coslat",MINVAL(landm_coslat),MAXVAL(landm_coslat)
!+++ARH
  !!
  !! read LANDFRAC
  !!
  allocate ( landfrac(n),stat=alloc_error )
  if( alloc_error /= 0 ) then
    print*,'Program could not allocate space for landfrac'
    stop
  end if
  !
  status = NF_INQ_VARID(ncid, 'LANDFRAC', landid)
  IF (status .NE. NF_NOERR) then
    write(*,*) "LANDFRAC not on file"
    llandfrac = .false.
    landfrac  = 1.0
  else  
    status = NF_GET_VAR_DOUBLE(ncid, landid,landfrac)
    IF (status .NE. NF_NOERR) CALL HANDLE_ERR(status)
    WRITE(*,*) "min/max of landfrac",MINVAL(landfrac),MAXVAL(landfrac)
    llandfrac = .true.
  end if
!---ARH
  !
  ! read terr - this is the elevation data (meters) on cubed sphere grid
  !
  allocate ( terr(n),stat=alloc_error )
  if( alloc_error /= 0 ) then
    print*,'Program could not allocate space for terr'
    stop
  end if
  
  status = NF_INQ_VARID(ncid, 'terr', landid)
  IF (status .NE. NF_NOERR) CALL HANDLE_ERR(status)
  
  status = NF_GET_VAR_DOUBLE(ncid, landid,terr)
  IF (status .NE. NF_NOERR) CALL HANDLE_ERR(status)
  WRITE(*,*) "min/max of terr",MINVAL(terr),MAXVAL(terr)

  !
  ! read var30 (variance) of 1km topography in each
  ! cubed sphere cell (in meters) 
  !
  allocate ( var30(n),stat=alloc_error )
  if( alloc_error /= 0 ) then
    print*,'Program could not allocate space for var30'
    stop
  end if
  
  status = NF_INQ_VARID(ncid, 'var30', landid)
  IF (status .NE. NF_NOERR) CALL HANDLE_ERR(status)
  
  status = NF_GET_VAR_DOUBLE(ncid, landid,var30)
  IF (status .NE. NF_NOERR) CALL HANDLE_ERR(status)
  WRITE(*,*) "min/max of sgh30",MINVAL(SQRT(var30)),MAXVAL(SQRT(var30))
  print *,"close file"
  status = nf_close (ncid)
  if (status .ne. NF_NOERR) call handle_err(status)
  
  WRITE(*,*) 'done reading in data from netCDF file'

end subroutine read_intermediate_cubed_sphere_grid


subroutine read_target_grid(grid_descriptor_fname,lregional_refinement,ltarget_latlon,lpole,nlat,nlon,ntarget,ncorner,nrank,&
     target_corner_lon, target_corner_lat, target_center_lon, target_center_lat, target_area, target_rrfac)
  implicit none
#     include         <netcdf.inc>
  character(len=1024), intent(in) :: grid_descriptor_fname
  logical, intent(out) :: lregional_refinement
  logical, intent(out) :: lpole, ltarget_latlon
  integer, intent(out) :: nlat,nlon,ntarget,ncorner,nrank
  real(r8), allocatable, dimension(:,:), intent(out) :: target_corner_lon, target_corner_lat
  real(r8), allocatable, dimension(:)  , intent(out) :: target_center_lon, target_center_lat, target_area
  real(r8), allocatable, dimension(:)  , intent(out) :: target_rrfac


  integer :: ncid,status
  integer :: ntarget_id, ncorner_id, nrank_id, nodeCount_id,nodeCoords_id,elementConn_id,numElementConn_id,centerCoords_id
  integer :: alloc_error
  integer :: lonid, latid,nodeCount

  real(r8), allocatable, dimension(:,:):: centerCoords,nodeCoords
  integer,  allocatable, dimension(:,:):: elementConn
  integer,  allocatable, dimension(:)  :: numElementConn
!+++ARH
  integer :: rrfacid
!---ARH
  character(len=23)  :: str_size(2), str_corners(2), str_rank(2), str_corner_lon(2), str_corner_lat(2), str_area(2),str_rrfac(2)
  character(len=23)  :: str_center_lon(2), str_center_lat(2)
  integer            :: esmf_file = 1 ! =1 SCRIP naming convention; =2 ESMF naming convention
  integer            :: icorner,icell,num
  integer            :: grid_dims(2)

  ltarget_latlon = .FALSE.
  !
  !*********************************************************
  !
  ! read in target grid
  !
  !*********************************************************
  !
  !
  ! SCRIP and ESMF naming convention
  ! 
  str_size         = (/"grid_size             ","elementCount          "/)
  str_corners      = (/"grid_corners          ","maxNodePElement       "/)
  str_rank         = (/"grid_rank             ","1                     "/)
  str_corner_lon   = (/"grid_corner_lon       ","nodeCoords            "/) !(nodeCount, coordDim)
  str_corner_lat   = (/"grid_corner_lat       ","nodeCoords            "/) !(nodeCount, coordDim)
  str_center_lon   = (/"grid_center_lon       ","centerCoordsnodeCoords"/) !(elementCount, coordDim)
  str_center_lat   = (/"grid_center_lat       ","centerCoordsnodeCoords"/) !(elementCount, coordDim)
  str_area         = (/"grid_area             ","elementArea           "/)
  str_rrfac        = (/"rrfac                 ","elementRefinementRatio"/)

  write(*,*) "Opening grid descriptor file :  ",TRIM(grid_descriptor_fname)
  status = nf_open(TRIM(grid_descriptor_fname), 0, ncid)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR(STATUS)

  status = NF_INQ_DIMID(ncid, TRIM(str_size(1)), ntarget_id)
  IF (STATUS .NE. NF_NOERR) then
    write(*,*) "File appears to be using ESMF grid descriptor convention"
    esmf_file = 2
    status = NF_INQ_DIMID(ncid, TRIM(str_size(esmf_file)), ntarget_id)
  else
    write(*,*) "File appears to be using SCRIP grid desciptor convention"
  end IF

  status = NF_INQ_DIMLEN(ncid, ntarget_id, ntarget)
  WRITE(*,*) "dimension of target grid: ntarget=",ntarget

  status = NF_INQ_DIMID(ncid, TRIM(str_corners(esmf_file)), ncorner_id)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR(STATUS)
  
  status = NF_INQ_DIMLEN(ncid, ncorner_id, ncorner)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR(STATUS)
  WRITE(*,*) "maximum number of corners: ncorner=",ncorner
  
  status = NF_INQ_DIMID(ncid, TRIM(str_rank(esmf_file)), nrank_id)
  status = NF_INQ_DIMLEN(ncid, nrank_id, nrank)

  allocate ( target_center_lon(ntarget),stat=alloc_error)
  allocate ( target_center_lat(ntarget),stat=alloc_error)
  allocate ( target_area      (ntarget),stat=alloc_error)

  allocate ( target_corner_lon(ncorner,ntarget),stat=alloc_error)
  allocate ( target_corner_lat(ncorner,ntarget),stat=alloc_error)

  if (esmf_file==2) then
    !
    ! ESMF format uses 2D arrays for vertices, etc. -> special code is needed
    !
    !
    ! how many nodes (unique corners)
    !
    status = NF_INQ_DIMID(ncid, 'nodeCount', nodeCount_id)
    status = NF_INQ_DIMLEN(ncid, nodeCount_id, nodeCount)
    !
    ! note that array dimensions are swapped compared to netCDF file
    !
    allocate (centerCoords(2,ntarget),stat=alloc_error)
    allocate (nodeCoords(2,nodeCount),stat=alloc_error)
    allocate (elementConn(ncorner,ntarget),stat=alloc_error)
    allocate (numElementConn(ntarget),stat=alloc_error)

    status = NF_INQ_VARID(ncid, 'centerCoords', centerCoords_id)
    IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR(STATUS)

    status = NF_GET_VAR_DOUBLE(ncid, centerCoords_id,centerCoords)
    IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR(STATUS)

    IF (maxval(centerCoords(:,1))>10.0) then
      target_center_lon = centerCoords(1,:)*deg2rad
      target_center_lat = centerCoords(2,:)*deg2rad
    else
      target_center_lon = centerCoords(1,:)
      target_center_lat = centerCoords(2,:)
    endif

    status = NF_INQ_VARID(ncid, 'nodeCoords', nodeCoords_id)
    IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR(STATUS)

    status = NF_GET_VAR_DOUBLE(ncid, nodeCoords_id,nodeCoords)
    IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR(STATUS)

    IF (maxval(nodeCoords(:,1))>10.0) then
      nodeCoords = nodeCoords*deg2rad
    ENDIF

    status = NF_INQ_VARID(ncid, 'elementConn', elementConn_id)
    IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR(STATUS)

    status = NF_GET_VAR_INT(ncid, elementConn_id,elementConn)
    IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR(STATUS)

    status = NF_INQ_VARID(ncid, 'numElementConn', numElementConn_id)
    IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR(STATUS)

    status = NF_GET_VAR_INT(ncid, numElementConn_id,numElementConn)
    IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR(STATUS)

    do icell=1,ntarget
      num = numElementConn(icell)
      do icorner=1,num
        target_corner_lon(icorner,icell) = nodeCoords(1,elementConn(icorner,icell))
        target_corner_lat(icorner,icell) = nodeCoords(2,elementConn(icorner,icell))
      end do
      do icorner=num+1,ncorner
        target_corner_lon(icorner,icell) = nodeCoords(1,elementConn(numElementConn(icell),icell))
        target_corner_lat(icorner,icell) = nodeCoords(2,elementConn(numElementConn(icell),icell))
      end do
    end do
    deallocate(centerCoords,nodeCoords,elementConn)
  else
    !
    ! file uses SCRIP format
    !
    WRITE(*,*) "grid rank: nrank=",nrank
    IF (nrank==2) THEN
      WRITE(*,*) "target grid is a lat-lon grid"
      ltarget_latlon = .TRUE.
      status = NF_INQ_VARID(ncid,'grid_dims', ntarget_id)
      IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR(STATUS)
      status = NF_GET_VAR_INT(ncid, ntarget_id,grid_dims)
      nlon = grid_dims(1)
      nlat = grid_dims(2)

      WRITE(*,*) "nlon=",nlon,"nlat=",nlat
    ELSE IF (nrank==1) THEN
      ltarget_latlon = .FALSE.
    ELSE
      WRITE(*,*) "nrank out of range",nrank
      STOP
    ENDIF
        
    status = NF_INQ_VARID(ncid, TRIM(str_corner_lon(esmf_file)), lonid)
    IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR(STATUS)
    status = NF_GET_VAR_DOUBLE(ncid, lonid,target_corner_lon)
    IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR(STATUS)
    IF (maxval(target_corner_lon)>10.0) target_corner_lon = deg2rad*target_corner_lon
    
    status = NF_INQ_VARID(ncid, TRIM(str_corner_lat(esmf_file)), latid)
    IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR(STATUS)
    status = NF_GET_VAR_DOUBLE(ncid, latid,target_corner_lat)

    IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR(STATUS)
    IF (maxval(target_corner_lat)>10.0) target_corner_lat = deg2rad*target_corner_lat    
    !
    ! for writing remapped data on file at the end of the program
    !    
    status = NF_INQ_VARID(ncid, 'grid_center_lon', lonid)
    status = NF_GET_VAR_DOUBLE(ncid, lonid,target_center_lon)
    IF (maxval(target_center_lon)>10.0) target_center_lon = deg2rad*target_center_lon    
    
    status = NF_INQ_VARID(ncid, 'grid_center_lat', latid)
    status = NF_GET_VAR_DOUBLE(ncid, latid,target_center_lat)
    IF (maxval(target_center_lat)>10.0) target_center_lat = deg2rad*target_center_lat    
    if (ltarget_latlon) then
      if (maxval(target_center_lat)>pih-1E-5) then
        lpole=.true.
      end if
    end if
    IF (lpole) THEN      
      WRITE(*,*) "center of most Northern grid cell is lat=90; similarly for South pole"
    ELSE
      WRITE(*,*) "center of most Northern grid cell is NOT lat=90; similarly for South pole"
    END IF
  end if

  status = NF_INQ_VARID(ncid, TRIM(str_rrfac(esmf_file)), rrfacid)
  if (STATUS .NE. NF_NOERR) then
    lregional_refinement = .false.
    write(*,*) "rrfac not on file; setting lregional_refinement = .false."
    write(*,*) "  ... allocating target_rrfac ANYWAY"
    ! allocate target_rrfac anyway since it may be invoked
    ! if rrfac write is requested
    allocate ( target_rrfac(ntarget),stat=alloc_error)
  else
    lregional_refinement = .true.
    allocate ( target_rrfac(ntarget),stat=alloc_error)
    status = NF_GET_VAR_DOUBLE(ncid, rrfacid,target_rrfac)
    IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR(STATUS)
    write(*,*) "rrfac on file; setting lregional_refinement = .true."
  end if

  status = NF_INQ_VARID(ncid, TRIM(str_area(esmf_file)), latid)
  status = NF_GET_VAR_DOUBLE(ncid, latid,target_area)
  
  status = nf_close (ncid)
  if (status .ne. NF_NOERR) call handle_err(status)          
  end subroutine read_target_grid
  

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine read_refinement_factor(rrfactor_fname,ncube_rr)
  implicit none
#     include         <netcdf.inc>
  character(len=1024), intent(in) :: rrfactor_fname
  integer, intent(out) :: ncube_rr
  integer :: ncid,status, dimid, alloc_error, landid,n

  write(*,*) "Opening file w/ refinement factors : ",TRIM(rrfactor_fname)
  status = nf_open(TRIM(rrfactor_fname) , 0, ncid)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR(STATUS)
  
  status = NF_INQ_DIMID(ncid, 'grid_size', dimid)
  IF (status .NE. NF_NOERR) CALL HANDLE_ERR(status)
  status = NF_INQ_DIMLEN(ncid, dimid, n)
  IF (status .NE. NF_NOERR) CALL HANDLE_ERR(status)
  
  ncube_rr = INT(SQRT(DBLE(n/6)))
  WRITE(*,*) "RR_factor dimension: ncube = ",ncube_rr

  !
  ! read  floating point refinement level
  !
  allocate ( refine_l(n),stat=alloc_error )
  if( alloc_error /= 0 ) then
    print*,'Program could not allocate space for refine_l'
    stop
  end if
  
  status = NF_INQ_VARID(ncid, 'refine_level', landid)
  IF (status .NE. NF_NOERR) CALL HANDLE_ERR(status)
  
  status = NF_GET_VAR_DOUBLE(ncid, landid,refine_l)
  IF (status .NE. NF_NOERR) CALL HANDLE_ERR(status)
  WRITE(*,*) "min/max of FLOAT Refine level",MINVAL(refine_l),MAXVAL(refine_l)

  !
  ! read  INTEGER refinement level
  !
  allocate ( refine_li(n),stat=alloc_error )
  if( alloc_error /= 0 ) then
    print*,'Program could not allocate space for refine_l'
    stop
  end if
  
  status = NF_INQ_VARID(ncid, 'refine_level', landid)
  IF (status .NE. NF_NOERR) CALL HANDLE_ERR(status)
  
  status = NF_GET_VAR_INT(ncid, landid,refine_li)
  IF (status .NE. NF_NOERR) CALL HANDLE_ERR(status)
  WRITE(*,*) "min/max of INT Refine level",MINVAL(refine_li),MAXVAL(refine_li)


  print *,"close file"
  status = nf_close (ncid)
  if (status .ne. NF_NOERR) call handle_err(status)
  
  WRITE(*,*) 'done reading in data from netCDF file'

end subroutine read_refinement_factor


END MODULE shared_vars
