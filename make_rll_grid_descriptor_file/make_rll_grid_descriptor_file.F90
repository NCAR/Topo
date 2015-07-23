!
!  DATE CODED:   January, 2012
!  DESCRIPTION:  This program creates a grid descriptior file for ESMF/SCRIP software in NetCDF file format.
!
!  Author: Peter Hjort Lauritzen (pel@ucar.edu)
      program make_rll_grid_descriptor_file
        use shr_kind_mod, only: r8 => shr_kind_r8
        implicit none

        !
        ! dimensions for regular lat-lon grid
        !

!        integer, parameter :: im = 2160       !10 min
!        integer, parameter :: jm = 1080       !10 min
!        logical, parameter :: lpole=.FALSE.   !10 min


!        integer, parameter :: im = 288       !0.9x1.25 CAM-FV
!        integer, parameter :: jm = 192       !0.9x1.25 CAM-FV 
!        logical, parameter :: lpole=.TRUE.   !CAM-FV setting is lpole=.TRUE.

!        integer, parameter :: im = 360       !1x1 CAM-FV
!        integer, parameter :: jm = 181       !1x1 CAM-FV 
!        logical, parameter :: lpole=.TRUE.   !CAM-FV setting is lpole=.TRUE.

!        integer, parameter :: im = 128       
!        integer, parameter :: jm = 64       
!        logical, parameter :: lpole=.TRUE.   

        integer, parameter :: im = 144       !CAM-FV 2 degree
        integer, parameter :: jm = 96        !CAM-FV 2 degree
        logical, parameter :: lpole=.TRUE.   !CAM-FV 2 degree


!        integer, parameter :: im = 1152       !0.23x0.31 CAM-FV
!        integer, parameter :: jm =768         !0.23x0.31 CAM-FV 
!        logical, parameter :: lpole=.TRUE.    !CAM-FV setting is

!        integer, parameter :: im = 1440       !0.23x0.31 CAM-FV
!        integer, parameter :: jm =720         !0.23x0.31 CAM-FV 
!        logical, parameter :: lpole=.FALSE.    !CAM-FV setting is


!        integer, parameter :: im = 2160       !USGS 10 min data setting
!        integer, parameter :: jm = 1080       !USGS 10 min data setting
!        logical, parameter :: lpole=.TRUE.    !CAM-FV setting is


!        integer, parameter :: im = 288       !NASA
!        integer, parameter :: jm = 181       !NASA
!        logical, parameter :: lpole=.TRUE.   !CAM-FV setting is lpole=.TRUE.

!        integer, parameter :: im = 43200     !USGS 30sec data
!        integer, parameter :: jm = 21600     !USGS 30sec data
!        logical, parameter :: lpole=.FALSE.   !USGS 30sec data

        character(35), parameter :: grid_name = 'regular lat-lon'
        !  
        call wrt_esmf_rll(im,jm,grid_name,lpole)
      end program make_rll_grid_descriptor_file



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


      !
      ! write netCDF grid descriptor file for ESMF remapping
      ! 
      subroutine wrt_esmf_rll(im,jm,grid_name,lpole)
        use shr_kind_mod, only: r8 => shr_kind_r8
        implicit none

#     include         <netcdf.inc>

!
! Dummy arguments
!
        integer      , intent(in) :: im,jm 
        character(35), intent(in) :: grid_name
        logical      , intent(in) :: lpole
!
! Local variables
!
        real(r8) :: dx, dy
        real(r8),dimension(im) :: lonar       ! longitude array
        real(r8),dimension(im) :: latar       ! latitude array

!-----------------------------------------------------------------------
!
!     grid coordinates and masks
!
!-----------------------------------------------------------------------

        integer, dimension(im*jm)   :: grid_imask
        
        real (r8), dimension(im*jm) :: grid_center_lat  ! lat/lon coordinates for
        real (r8), dimension(im*jm) :: grid_center_lon  ! each grid center in degrees
        
        real (r8), dimension(4,im*jm) :: grid_corner_lat  ! lat/lon coordinates for
        real (r8), dimension(4,im*jm) :: grid_corner_lon   ! each grid corner in degrees


        integer  :: ncstat             ! general netCDF status variable
        integer  :: nc_grid_id         ! netCDF grid dataset id
        integer  :: nc_lon_id          ! netCDF grid dataset id
        integer  :: nc_lat_id          ! netCDF grid dataset id
        integer  :: nc_pole_id          ! netCDF grid dataset id
        integer  :: nc_gridsize_id     ! netCDF grid size dim id
        integer  :: nc_gridcorn_id     ! netCDF grid corner dim id
        integer  :: nc_gridrank_id     ! netCDF grid rank dim id
        integer  :: nc_griddims_id     ! netCDF grid dimension size id
        integer  :: nc_grdcntrlat_id   ! netCDF grid center lat id
        integer  :: nc_grdcntrlon_id   ! netCDF grid center lon id
        integer  :: nc_grdcrnrlat_id   ! netCDF grid corner lat id
        integer  :: nc_grdcrnrlon_id   ! netCDF grid corner lon id
        integer  :: nc_grdimask_id     ! netCDF grid mask id

        integer, dimension(2) :: nc_dims2_id ! netCDF dim id array for 2-d arrays
        integer, dimension(2) :: grid_dims
        
!        integer , dimension(2) ::
!       &        nc_dims2_id        ! netCDF dim id array for 2-d arrays
        
      character(20), parameter :: grid_file_out = 'rll.nc'
!     &             grid_name = 'Lat/lon 1 degree Grid',             

        character (len=32) :: fout       ! NetCDF output file
        integer            :: foutid     ! Output file id
        integer            :: lonid, lonvid
        integer            :: latid, latvid
        integer            :: status    ! return value for error control of netcdf routin
        integer            :: i,j
        character (len=8)  :: datestring


        integer :: atm_add
        real(r8) :: centerlon,centerlat,minlat,minlon,maxlat,maxlon

        dx = 360.0D0/real(im)
        if (lpole) then
          dy = 180.0D0/real(jm-1)
        else
          dy = 180.0D0/real(jm)
        end if
        grid_dims(1) = im
        grid_dims(2) = jm
        grid_imask = 1

        !
        ! Fill lat and lon arrays
        !  
        do i = 1,im
!          lonar(i)=  dx * (i-0.5)  !this line must be uncommented to
!          match 10 min data
          lonar(i)=  dx * DBLE((i-1)) !CAM-FV grid
        enddo
        if (lpole) THEN
          do j = 1,jm
            latar(j)= -90.0D0 + dy * DBLE(j-1)
          enddo
        else
          do j = 1,jm
            latar(j)= -90.0D0 + dy * (DBLE(j)-0.5D0)
          enddo          
        end if
        
        
        do j=1,jm
          centerlat = latar(j)
          IF (lpole.AND.j==1) THEN
            minlat = centerlat
          ELSE
            minlat = centerlat - 0.5D0*dy
          END IF
          IF (lpole.AND.j==jm) THEN
            maxlat = centerlat 
          ELSE
            maxlat = centerlat + 0.5D0*dy
          END IF
          
          do i=1,im
            centerlon = lonar(i)
            minlon = centerlon - 0.5D0*dx
            maxlon = centerlon + 0.5D0*dx
            
            atm_add = (j-1)*im + i
            
            grid_center_lat(atm_add  ) = centerlat
            grid_corner_lat(1,atm_add) = minlat
            grid_corner_lat(2,atm_add) = minlat
            grid_corner_lat(3,atm_add) = maxlat
            grid_corner_lat(4,atm_add) = maxlat
            
            grid_center_lon(atm_add  ) = centerlon
            grid_corner_lon(1,atm_add) = minlon
            grid_corner_lon(2,atm_add) = maxlon
            grid_corner_lon(3,atm_add) = maxlon
            grid_corner_lon(4,atm_add) = minlon
          end do
        end do

        fout='rll.nc'
        !
        !  Create NetCDF file for output
        !
        status = nf_create (fout, NF_WRITE, foutid)
        if (status .ne. NF_NOERR) call handle_err(status)

        
        !-----------------------------------------------------------------------
        !
        !     set up attributes for netCDF file
        !
        !-----------------------------------------------------------------------
        
        !***
        !*** create netCDF dataset for this grid
        !***
        
        ncstat = nf_create (grid_file_out, NF_CLOBBER,nc_grid_id)
        call handle_err(ncstat)
        
        ncstat = nf_put_att_text (nc_grid_id, NF_GLOBAL, 'title',len_trim(grid_name), grid_name)
        call handle_err(ncstat)

        !***
        !*** define grid size dimension
        !***
        ncstat = nf_def_dim (nc_grid_id, 'nlon', im, nc_lon_id)
        call handle_err(ncstat)        

        ncstat = nf_def_dim (nc_grid_id, 'nlat', jm, nc_lat_id)
        call handle_err(ncstat)        

        ncstat = nf_def_dim (nc_grid_id, 'lpole', lpole, nc_lat_id)
        call handle_err(ncstat)        



        ncstat = nf_def_dim (nc_grid_id, 'grid_size', im*jm, nc_gridsize_id)
        call handle_err(ncstat)

        !***
        !*** define grid corner dimension
        !***
        
        ncstat = nf_def_dim (nc_grid_id, 'grid_corners', 4, nc_gridcorn_id)
        call handle_err(ncstat)
        
        !***
        !*** define grid rank dimension
        !***
        
        ncstat = nf_def_dim (nc_grid_id, 'grid_rank', 2, nc_gridrank_id)
        call handle_err(ncstat)
        
        !***
        !*** define grid dimension size array
        !***
        
        ncstat = nf_def_var (nc_grid_id, 'grid_dims', NF_INT,1, nc_gridrank_id, nc_griddims_id)
        call handle_err(ncstat)
        
        !***
        !*** define grid center latitude array
        !***
        
        ncstat = nf_def_var (nc_grid_id, 'grid_center_lat', NF_DOUBLE,1, nc_gridsize_id, nc_grdcntrlat_id)
        call handle_err(ncstat)
        
        ncstat = nf_put_att_text (nc_grid_id, nc_grdcntrlat_id, 'units',7, 'degrees')
        call handle_err(ncstat)
        
        !***
        !*** define grid center longitude array
        !***
        
        ncstat = nf_def_var (nc_grid_id, 'grid_center_lon', NF_DOUBLE,1, nc_gridsize_id, nc_grdcntrlon_id)
        call handle_err(ncstat)
        
        ncstat = nf_put_att_text (nc_grid_id, nc_grdcntrlon_id, 'units',7, 'degrees')
        call handle_err(ncstat)
        

        !***
        !*** define grid mask
        !***
        
        ncstat = nf_def_var (nc_grid_id, 'grid_imask', NF_INT,1, nc_gridsize_id, nc_grdimask_id)
        call handle_err(ncstat)

        ncstat = nf_put_att_text (nc_grid_id, nc_grdimask_id, 'units',8, 'unitless')
        call handle_err(ncstat)
        
        !***
        !*** define grid corner latitude array
        !***
        
        nc_dims2_id(1) = nc_gridcorn_id
        nc_dims2_id(2) = nc_gridsize_id
        
        ncstat = nf_def_var (nc_grid_id, 'grid_corner_lat', NF_DOUBLE,2, nc_dims2_id, nc_grdcrnrlat_id)
        call handle_err(ncstat)
        
        ncstat = nf_put_att_text (nc_grid_id, nc_grdcrnrlat_id, 'units',7, 'degrees')
        call handle_err(ncstat)
        
        !***
        !*** define grid corner longitude array
        !***
        
        ncstat = nf_def_var (nc_grid_id, 'grid_corner_lon', NF_DOUBLE,2, nc_dims2_id, nc_grdcrnrlon_id)
        call handle_err(ncstat)
        
        ncstat = nf_put_att_text (nc_grid_id, nc_grdcrnrlon_id, 'units',7, 'degrees')
        call handle_err(ncstat)
        
        !***
        !*** end definition stage
        !***
        
        ncstat = nf_enddef(nc_grid_id)
        call handle_err(ncstat)
        
        !-----------------------------------------------------------------------
        !
        !     write grid data
        !
        !-----------------------------------------------------------------------
        
        ncstat = nf_put_var_int(nc_grid_id, nc_griddims_id, grid_dims)
        call handle_err(ncstat)
        
        ncstat = nf_put_var_int(nc_grid_id, nc_grdimask_id, grid_imask)
        call handle_err(ncstat)
        
        ncstat = nf_put_var_double(nc_grid_id, nc_grdcntrlat_id, grid_center_lat)
        call handle_err(ncstat)
        
        ncstat = nf_put_var_double(nc_grid_id, nc_grdcntrlon_id, grid_center_lon)
        call handle_err(ncstat)
        
        ncstat = nf_put_var_double(nc_grid_id, nc_grdcrnrlat_id, grid_corner_lat)
        call handle_err(ncstat)
        
        ncstat = nf_put_var_double(nc_grid_id, nc_grdcrnrlon_id, grid_corner_lon)
        call handle_err(ncstat)

        !
        ! Close output file
        !        
        ncstat = nf_close(nc_grid_id)
        call handle_err(ncstat)
      end subroutine wrt_esmf_rll
