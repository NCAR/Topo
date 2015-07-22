MODULE shared_vars
  use shr_kind_mod, only: r8 => shr_kind_r8

  real(r8), allocatable, dimension(:,:):: target_corner_lon, target_corner_lat
  real(r8), allocatable, dimension(:)  :: target_center_lon, target_center_lat, target_area
  real(r8),  allocatable, dimension(:) :: landm_coslat, landfrac, terr, sgh30

  real(r8), allocatable, dimension(:) :: landfrac_target, terr_target, sgh30_target, sgh_target
  real(r8), allocatable, dimension(:) :: landm_coslat_target, area_target


  REAL    (r8):: pi, piq, pih, deg2rad

  contains 
    subroutine set_constants
      implicit none
      pi        = 4.D0*DATAN(1.D0)
      piq       = 0.25*pi
      pih       = 0.50*pi
      deg2rad   = pi/180.0
    end subroutine set_constants

  
    
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
  allocate (landfrac_target(ntarget),stat=alloc_error )
  if( alloc_error /= 0 ) then
    print*,'Program could not allocate space for landfrac_target'
    stop
  end if
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

  allocate (area_target(ntarget),stat=alloc_error )

  area_target = 0.0
end subroutine allocate_target_vars


subroutine read_intermediate_cubed_sphere_grid(intermediate_cubed_sphere_fname,ncube)
  implicit none
#     include         <netcdf.inc>
  character(len=1024), intent(in) :: intermediate_cubed_sphere_fname
  integer, intent(out) :: ncube
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

  !
  ! read LANDFRAC
  !
  allocate ( landfrac(n),stat=alloc_error )
  if( alloc_error /= 0 ) then
    print*,'Program could not allocate space for landfrac'
    stop
  end if
  
  status = NF_INQ_VARID(ncid, 'LANDFRAC', landid)
  IF (status .NE. NF_NOERR) CALL HANDLE_ERR(status)
  
  status = NF_GET_VAR_DOUBLE(ncid, landid,landfrac)
  IF (status .NE. NF_NOERR) CALL HANDLE_ERR(status)
  WRITE(*,*) "min/max of landfrac",MINVAL(landfrac),MAXVAL(landfrac)

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
  ! read SGH30 - SQRT(variance) of 1km topography in each
  ! cubed sphere cell (in meters) 
  !
  allocate ( sgh30(n),stat=alloc_error )
  if( alloc_error /= 0 ) then
    print*,'Program could not allocate space for sgh30'
    stop
  end if
  
  status = NF_INQ_VARID(ncid, 'SGH30', landid)
  IF (status .NE. NF_NOERR) CALL HANDLE_ERR(status)
  
  status = NF_GET_VAR_DOUBLE(ncid, landid,sgh30)
  IF (status .NE. NF_NOERR) CALL HANDLE_ERR(status)
  WRITE(*,*) "min/max of sgh30",MINVAL(sgh30),MAXVAL(sgh30)
  print *,"close file"
  status = nf_close (ncid)
  if (status .ne. NF_NOERR) call handle_err(status)
  
  WRITE(*,*) 'done reading in data from netCDF file'

end subroutine read_intermediate_cubed_sphere_grid


subroutine read_target_grid(grid_descriptor_fname,ltarget_latlon,lpole,nlat,nlon,ntarget,ncorner,nrank)
  implicit none
#     include         <netcdf.inc>
  character(len=1024), intent(in) :: grid_descriptor_fname
  logical, intent(out) :: lpole, ltarget_latlon
  integer, intent(out) :: nlat,nlon,ntarget,ncorner,nrank
  integer :: ncid,status
  integer :: ntarget_id, ncorner_id, nrank_id
  integer :: alloc_error
  integer :: lonid, latid
  !
  !*********************************************************
  !
  ! read in target grid
  !
  !*********************************************************
  !
  write(*,*) "Opening grid descriptor file :  ",TRIM(grid_descriptor_fname)
  status = nf_open(TRIM(grid_descriptor_fname), 0, ncid)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR(STATUS)
  
  status = NF_INQ_DIMID(ncid, 'grid_size', ntarget_id)
  status = NF_INQ_DIMLEN(ncid, ntarget_id, ntarget)
  WRITE(*,*) "dimension of target grid: ntarget=",ntarget
  
  status = NF_INQ_DIMID(ncid, 'grid_corners', ncorner_id)
  status = NF_INQ_DIMLEN(ncid, ncorner_id, ncorner)
  WRITE(*,*) "maximum number of corners: ncorner=",ncorner
  
  status = NF_INQ_DIMID(ncid, 'grid_rank', nrank_id);status = NF_INQ_DIMLEN(ncid, nrank_id, nrank)
  WRITE(*,*) "grid rank: nrank=",nrank
  IF (nrank==2) THEN
    WRITE(*,*) "target grid is a lat-lon grid"
    ltarget_latlon = .TRUE.
    status = NF_INQ_DIMID(ncid, 'nlon', ntarget_id)
    status = NF_INQ_DIMLEN(ncid, ntarget_id, nlon)
    status = NF_INQ_DIMID(ncid, 'nlat', ntarget_id)
    status = NF_INQ_DIMLEN(ncid, ntarget_id, nlat)
    status = NF_INQ_DIMID(ncid, 'lpole', ntarget_id)
    status = NF_INQ_DIMLEN(ncid, ntarget_id, ntarget_id)
!    status = NF_INQ_DIMLEN(ncid, ntarget_id, lpole)
    WRITE(*,*) "nlon=",nlon,"nlat=",nlat
    IF (lpole) THEN
      WRITE(*,*) "center of most Northern grid cell is lat=90; similarly for South pole"
    ELSE
      WRITE(*,*) "center of most Northern grid cell is NOT lat=90; similarly for South pole"
    END IF
  ELSE IF (nrank==1) THEN
    ltarget_latlon = .FALSE.
  ELSE
    WRITE(*,*) "nrank out of range",nrank
    STOP
  ENDIF
  
  allocate ( target_corner_lon(ncorner,ntarget),stat=alloc_error)
  allocate ( target_corner_lat(ncorner,ntarget),stat=alloc_error)
  
  status = NF_INQ_VARID(ncid, 'grid_corner_lon', lonid)
  status = NF_GET_VAR_DOUBLE(ncid, lonid,target_corner_lon)
  IF (maxval(target_corner_lon)>10.0) target_corner_lon = deg2rad*target_corner_lon
  
  status = NF_INQ_VARID(ncid, 'grid_corner_lat', latid)
  status = NF_GET_VAR_DOUBLE(ncid, latid,target_corner_lat)
  IF (maxval(target_corner_lat)>10.0) target_corner_lat = deg2rad*target_corner_lat
  !
  ! for writing remapped data on file at the end of the program
  !
  allocate ( target_center_lon(ntarget),stat=alloc_error)
  allocate ( target_center_lat(ntarget),stat=alloc_error)
  allocate ( target_area      (ntarget),stat=alloc_error)
  
  status = NF_INQ_VARID(ncid, 'grid_center_lon', lonid)
  status = NF_GET_VAR_DOUBLE(ncid, lonid,target_center_lon)
  
  status = NF_INQ_VARID(ncid, 'grid_center_lat', latid)
  status = NF_GET_VAR_DOUBLE(ncid, latid,target_center_lat)
  
  status = NF_INQ_VARID(ncid, 'grid_area', latid)
  status = NF_GET_VAR_DOUBLE(ncid, latid,target_area)

  status = nf_close (ncid)
  if (status .ne. NF_NOERR) call handle_err(status)          
end subroutine read_target_grid

END MODULE shared_vars
