!
!  Author: Peter Hjort Lauritzen (pel@ucar.edu) 
!

program convterr
  use shr_kind_mod, only: r8 => shr_kind_r8
  implicit none
#     include         <netcdf.inc>
  !
  integer :: im, jm
  
  integer*2,  allocatable, dimension(:,:) :: terr               ! global 30-sec terrain data
  integer*1,  allocatable, dimension(:,:) :: landfrac ! global 30-sec land fraction
  
  integer :: alloc_error,dealloc_error  
  integer :: i,j,ii                             ! index
  integer ncid,status, dimlatid,dimlonid, landid, topoid  ! for netCDF USGS data file
  integer :: srcid,dstid                                  ! for netCDF weight file
  
  real(r8), allocatable, dimension(:)   :: lon  , lat
  integer :: lonid, latid
  
  
  real(r8) :: elev
!
  ! variable for regridding
  !
  integer ::  src_grid_dim  ! for netCDF weight file

  INTEGER :: UNIT

  character(len=1024) :: raw_latlon_data_file,output_file
  logical :: lcontinue
  
  namelist /params/ &
       raw_latlon_data_file,output_file
  
  UNIT=221
  OPEN( UNIT=UNIT, FILE="fix_inland_water_elevation.nl" ) !, NML =  cntrls )
  READ( UNIT=UNIT, NML=params)
  CLOSE(UNIT=UNIT)

  !
  ! read in data from netCDF file
  !
  status = nf_open(raw_latlon_data_file, 0, ncid)
  write(*,*) "Opening: ",TRIM(raw_latlon_data_file)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR(STATUS)
  
  status = NF_INQ_DIMID(ncid, 'lat', dimlatid)
  IF (status .NE. NF_NOERR) CALL HANDLE_ERR(status)
  status = NF_INQ_DIMLEN(ncid, dimlatid, jm)
  IF (status .NE. NF_NOERR) CALL HANDLE_ERR(status)
  
  status = NF_INQ_DIMID(ncid, 'lon', dimlonid)
  IF (status .NE. NF_NOERR) CALL HANDLE_ERR(status)
  status = NF_INQ_DIMLEN(ncid, dimlonid, im)
  IF (status .NE. NF_NOERR) CALL HANDLE_ERR(status)
  
  WRITE(*,*) "lon-lat dimensions: ",im,jm
  
  allocate ( landfrac(im,jm),stat=alloc_error )
  if( alloc_error /= 0 ) then
    print*,'Program could not allocate space for landfrac'
    stop
  end if
  
  allocate ( terr(im,jm),stat=alloc_error )
  if( alloc_error /= 0 ) then
    print*,'Program could not allocate space for terr'
    stop
  end if
  
  allocate ( lon(im),stat=alloc_error )
  if( alloc_error /= 0 ) then
    print*,'Program could not allocate space for landfrac'
    stop
  end if
  
  allocate ( lat(jm),stat=alloc_error )
  if( alloc_error /= 0 ) then
    print*,'Program could not allocate space for landfrac'
    stop
  end if
  
  terr = -9999
  landfrac = -99.0
  
  status = NF_INQ_VARID(ncid, 'landfract', landid)
  IF (status .NE. NF_NOERR) CALL HANDLE_ERR(status)
  
  status = NF_GET_VAR_INT1(ncid, landid,landfrac)
  IF (status .NE. NF_NOERR) CALL HANDLE_ERR(status)
  WRITE(*,*) "min/max of 30sec land fraction",MINVAL(landfrac),MAXVAL(landfrac)
  
  
  status = NF_INQ_VARID(ncid, 'htopo', topoid)
  IF (status .NE. NF_NOERR) CALL HANDLE_ERR(status)
  
  WRITE(*,*) "read terrain data"
  status = NF_GET_VAR_INT2(ncid, topoid,terr)
  IF (status .NE. NF_NOERR) CALL HANDLE_ERR(status)
  
  status = NF_INQ_VARID(ncid, 'lon', lonid)
  IF (status .NE. NF_NOERR) CALL HANDLE_ERR(status)
  
  WRITE(*,*) "read lon"
  status = NF_GET_VAR_DOUBLE(ncid, lonid,lon)
  IF (status .NE. NF_NOERR) CALL HANDLE_ERR(status)
  
  status = NF_INQ_VARID(ncid, 'lat', latid)
  IF (status .NE. NF_NOERR) CALL HANDLE_ERR(status)
  
  WRITE(*,*) "read lat"
  status = NF_GET_VAR_DOUBLE(ncid, latid,lat)
  IF (status .NE. NF_NOERR) CALL HANDLE_ERR(status)
  
  print *,"close file"
  status = nf_close (ncid)
  if (status .ne. NF_NOERR) call handle_err(status)
  
  WRITE(*,*) 'done reading data from netCDF file'
  
  
  elev=-99999
  DO j=1,jm
     write(*,*) j
     i=1
     do while (i<im+1)
        !
        ! detect start of inland water along latitude
        !     
        if (SUM(landfrac(i:i+9,j))==30) then
           elev = terr(i+9,j) 
           landfrac(i,j)=0
           ii=i+1
           lcontinue=.TRUE.
           do while (lcontinue)
              if (landfrac(ii,j)==3) then
                 terr(ii,j)    =elev
                 landfrac(ii,j)=0
                 ii=ii+1
              else
                 i=ii-1
                 lcontinue=.FALSE.
              end if
           end do
        else if (landfrac(i,j)==3) then
           landfrac(i,j)=0
        end if
        i=i+1
     END DO
  END DO
  
  !
  ! write data to NetCDF file
  !
!  CALL wrt_cube(ncube,terr_cube,landfrac_cube,landm_coslat_cube,var30_cube,raw_latlon_data_file,output_file)
!
!  Write 30-sec terrain dataset, and land_fraction to NetCDF file
!
  call wrtncdf(im,jm,terr,landfrac,lon,lat,output_file)

  DEALLOCATE(terr,landfrac,lat,lon)
  WRITE(*,*) "done"
end program convterr


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
  
      subroutine wrtncdf(im,jm,terr,land_fraction,lonar,latar,output_file)
        use shr_kind_mod, only: r8 => shr_kind_r8
!
! This subroutine save 30-sec terrain data, land fraction to NetCDF file
!
        implicit none

#     include         <netcdf.inc>

!
! Dummy arguments
!
        integer, intent(in) :: im,jm    ! the dimensions of the 30-sec global dataset
        integer*2,dimension(im,jm), intent(in) :: terr       ! global 30-sec terrain data
        integer*1,dimension(im,jm), intent(in) :: land_fraction !global 30-sec land fraction
!
! Local variables
!
        real(r8),dimension(im), intent(in) :: lonar       ! longitude array
        real(r8),dimension(jm), intent(in) :: latar       ! latitude array
        character(len=1024)   , intent(in) :: output_file
        integer            :: foutid     ! Output file id
        integer            :: lonid, lonvid
        integer            :: latid, latvid
        integer            :: htopoid
        integer            :: landfid
        integer, dimension(2) :: htopodim,landfdim
        integer            :: status    ! return value for error control of netcdf routin
        integer            :: i,j
        character (len=8)  :: datestring

        integer*2,dimension(im,jm) :: h       ! global 30-sec terrain data
        integer*1,dimension(im,jm) :: lnd


        do j=1,jm
          do i=1,im
            h(i,j) = terr(i,j)
            lnd(i,j) = land_fraction(i,j)
          end do
        end do

!
!  Create NetCDF file for output
!
        print *,"Create NetCDF file for output"
        status = nf_create (TRIM(output_file), NF_64BIT_OFFSET , foutid)
        if (status .ne. NF_NOERR) call handle_err(status)
!
! Create dimensions for output
!
        print *,"Create dimensions for output"
        status = nf_def_dim (foutid, 'lon', im, lonid)
        if (status .ne. NF_NOERR) call handle_err(status)
        status = nf_def_dim (foutid, 'lat', jm, latid)
        if (status .ne. NF_NOERR) call handle_err(status)
!
! Create variable for output
!
        print *,"Create variable for output"
        htopodim(1)=lonid
        htopodim(2)=latid
!        status = nf_def_var (foutid,'htopo', NF_INT, 2, htopodim, htopoid)
        status = nf_def_var (foutid,'htopo', NF_SHORT, 2, htopodim, htopoid)
        if (status .ne. NF_NOERR) call handle_err(status)
!
        landfdim(1)=lonid
        landfdim(2)=latid
        status = nf_def_var (foutid,'landfract', NF_BYTE, 2, landfdim, landfid)
!        status = nf_def_var (foutid,'landfract', NF_SHORT, 2, landfdim, landfid)
        if (status .ne. NF_NOERR) call handle_err(status)

        status = nf_def_var (foutid,'lat', NF_DOUBLE, 1, latid, latvid)
        if (status .ne. NF_NOERR) call handle_err(status)

        status = nf_def_var (foutid,'lon', NF_DOUBLE, 1, lonid, lonvid)
        if (status .ne. NF_NOERR) call handle_err(status)

!
! Create attributes for output variables
!
        status = nf_put_att_text (foutid,htopoid,'long_name', 41, '30-sec elevation from USGS 30-sec dataset')
        if (status .ne. NF_NOERR) call handle_err(status)
        status = nf_put_att_text (foutid,htopoid,'units', 5, 'meter')
        if (status .ne. NF_NOERR) call handle_err(status)
!
        status = nf_put_att_text (foutid,landfid,'long_name', 23, '30-second land fraction')
        if (status .ne. NF_NOERR) call handle_err(status)
        status = nf_put_att_text (foutid,landfid,'units', 14, 'fraction (0-1)')
        if (status .ne. NF_NOERR) call handle_err(status)
!
        status = nf_put_att_text (foutid,latvid,'long_name', 8, 'latitude')
        if (status .ne. NF_NOERR) call handle_err(status)
        status = nf_put_att_text (foutid,latvid,'units', 13, 'degrees_north')
        if (status .ne. NF_NOERR) call handle_err(status)
        status = nf_put_att_text (foutid,latvid,'units', 21, 'cell center locations')
        if (status .ne. NF_NOERR) call handle_err(status)

        status = nf_put_att_text (foutid,lonvid,'long_name', 9, 'longitude')
        if (status .ne. NF_NOERR) call handle_err(status)
        status = nf_put_att_text (foutid,lonvid,'units', 12, 'degrees_east')
        if (status .ne. NF_NOERR) call handle_err(status)
        status = nf_put_att_text (foutid,lonvid,'units' , 21, 'cell center locations')
        if (status .ne. NF_NOERR) call handle_err(status)

        status = nf_put_att_text (foutid,NF_GLOBAL,'source', 27, 'USGS 30-sec dataset GTOPO30')
        if (status .ne. NF_NOERR) call handle_err(status)
        status = nf_put_att_text (foutid,NF_GLOBAL,'title',  24, '30-second USGS topo data')
        if (status .ne. NF_NOERR) call handle_err(status)
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
        print*,"writing terrain data"
        status = nf_put_var_int2 (foutid, htopoid, h)
        if (status .ne. NF_NOERR) call handle_err(status)
        print*,"done writing terrain data"
!
        status = nf_put_var_int1 (foutid, landfid, lnd)
        if (status .ne. NF_NOERR) call handle_err(status)
!
        print*,"writing lat data"
        status = nf_put_var_double (foutid, latvid, latar)
        if (status .ne. NF_NOERR) call handle_err(status)
        print*,"done writing lat data"

        print*,"writing lon data"
        status = nf_put_var_double (foutid, lonvid, lonar)
        if (status .ne. NF_NOERR) call handle_err(status)
        print*,"done writing lon data"
!
! Close output file
!
        print *,"close file"
        status = nf_close (foutid)
        if (status .ne. NF_NOERR) call handle_err(status)

      end subroutine wrtncdf






