module write_ncoutput_mod

use shr_kind_mod, only: r8 => shr_kind_r8

IMPLICIT NONE
private

public wrtnc2_unstructured

contains

!=======================================================================================
subroutine wrtnc2_unstructured(n,terr,sgh,sgh30,landm_coslat,lon,lat,area,output_fname,lfind_ridges,command_line_arguments, &
           lwrite_rrfac_to_topo_file,rrfac_target,str_creator)
    !---ARH
    use shared_vars, only : rad2deg, gravity
    use shr_kind_mod, only: r8 => shr_kind_r8
    use shared_vars, only : terr_uf_target, sgh_uf_target, area_target
    use ridge_ana, only: nsubr, mxdis_target, mxvrx_target, mxvry_target, ang22_target, &
         anglx_target, aniso_target, anixy_target, hwdth_target, wghts_target, & 
         clngt_target, cwght_target, count_target,riseq_target,grid_length_scale, &
         fallq_target, isovar_target
    
    
    
    implicit none
    
#     include         <netcdf.inc>
    
    !
    ! Dummy arguments
    !
    integer, intent(in) :: n
    !+++ARH
    !real(r8),dimension(n)  , intent(in) :: terr, landfrac,sgh,sgh30,lon, lat, landm_coslat,area  
    real(r8),dimension(n), intent(in) :: terr,sgh,sgh30,lon,lat,landm_coslat,area
    !---ARH
    character(len=1024),   intent(in) :: output_fname
    logical,               intent(in) :: lfind_ridges
    character(len=1024),   intent(in) :: command_line_arguments
    logical,               intent(in) :: lwrite_rrfac_to_topo_file
    real(r8),dimension(n), intent(in) :: rrfac_target
    character(len=1024),   intent(in) :: str_creator
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
    integer             :: sghufid, terrufid, clngtid, cwghtid, countid,riseqid,fallqid,rrfacid,isovarid
    integer             :: ThisId
    
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
    status = nf_create (fout, NF_64BIT_DATA, foutid)
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

    !+++ARH  
    !status = nf_def_var (foutid,'LANDFRAC', NF_DOUBLE, 1, nid(1), landfracid)
    !if (status .ne. NF_NOERR) call handle_err(status)
    !---ARH  
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
    
    latdim(1) = latvid
    status = nf_def_var (foutid,'lon', NF_DOUBLE, 1, nid(1), lonvid)
    if (status .ne. NF_NOERR) then
      call handle_err(status)
      write(*,*) "lon error"
    end if

   
    if (Lfind_ridges) then 
      status = nf_def_var (foutid,'ISOVAR', NF_DOUBLE, 1, nid(1), isovarid)
      if (status .ne. NF_NOERR) then
        call handle_err(status)
        write(*,*) "ISOVAR error"
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
      status = nf_def_var (foutid,'CLNGT', NF_DOUBLE, 2, nid , clngtid)
      if (status .ne. NF_NOERR) then
        call handle_err(status)
        write(*,*) "CLNGT error"
      end if
#if 0 
      status = nf_def_var (foutid,'COUNT', NF_DOUBLE, 2, nid , countid)
      if (status .ne. NF_NOERR) then
        call handle_err(status)
        write(*,*) "COUNT error"
      end if
      status = nf_def_var (foutid,'CWGHT', NF_DOUBLE, 2, nid , cwghtid)
      if (status .ne. NF_NOERR) then
        call handle_err(status)
        write(*,*) "CWGHT error"
      end if
      status = nf_def_var (foutid,'WGHTS', NF_DOUBLE, 2, nid , wghtsid)
      if (status .ne. NF_NOERR) then
        call handle_err(status)
        write(*,*) "WGHTS error"
      end if
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
#endif
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
    
    status = nf_put_att_double (foutid, areaid, 'missing_value', nf_double, 1, fillvalue)
    status = nf_put_att_double (foutid, areaid, '_FillValue'   , nf_double, 1, fillvalue)
    status = nf_put_att_text   (foutid, areaid, 'long_name' , 24, &
         'area of target grid cell')
    status = nf_put_att_text   (foutid, areaid, 'units'     , 1, 'm+2')

    status = nf_put_att_double (foutid, isovarid, 'missing_value', nf_double, 1, fillvalue)
    status = nf_put_att_double (foutid, isovarid, '_FillValue'   , nf_double, 1, fillvalue)
    status = nf_put_att_text   (foutid, isovarid, 'long_name' , 30, &
         'residual variance after ridges')
    status = nf_put_att_text   (foutid, isovarid, 'units'     , 1, 'm+2')
    
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

#if 1
    if (Lfind_ridges) then 
       ThisId = mxdisid
       status = nf_put_att_double (foutid, ThisId, 'missing_value', nf_double, 1, fillvalue)
       status = nf_put_att_double (foutid, ThisId, '_FillValue'   , nf_double, 1, fillvalue)
       status = nf_put_att_text   (foutid, ThisId, 'long_name' , 48, &
       'Obtsacle height diagnosed by ridge-finding alg. ')
       status = nf_put_att_text   (foutid, ThisId, 'units'     , 1, 'm')
       status = nf_put_att_text   (foutid, ThisId, 'filter'    , 4, 'none')

       ThisId = riseqid
       status = nf_put_att_double (foutid, ThisId, 'missing_value', nf_double, 1, fillvalue)
       status = nf_put_att_double (foutid, ThisId, '_FillValue'   , nf_double, 1, fillvalue)
       status = nf_put_att_text   (foutid, ThisId, 'long_name' , 38, &
       'Rise to peak from left (ridge_finding)')
       !12345678901234567890123456789012345678901234567890
       !         10        20        30        40        
       status = nf_put_att_text   (foutid, ThisId, 'units'     , 1, 'm')
       status = nf_put_att_text   (foutid, ThisId, 'filter'    , 4, 'none')

       ThisId = fallqid
       status = nf_put_att_double (foutid, ThisId, 'missing_value', nf_double, 1, fillvalue)
       status = nf_put_att_double (foutid, ThisId, '_FillValue'   , nf_double, 1, fillvalue)
       status = nf_put_att_text   (foutid, ThisId, 'long_name' , 43, &
       'Fall from peak toward right (ridge_finding)')
       !12345678901234567890123456789012345678901234567890
       !         10        20        30        40        
       status = nf_put_att_text   (foutid, ThisId, 'units'     , 1, 'm')
       status = nf_put_att_text   (foutid, ThisId, 'filter'    , 4, 'none')

       ThisId = ang22id
       status = nf_put_att_double (foutid, ThisId, 'missing_value', nf_double, 1, fillvalue)
       status = nf_put_att_double (foutid, ThisId, '_FillValue'   , nf_double, 1, fillvalue)
       status = nf_put_att_text   (foutid, ThisId, 'long_name' , 48, &
       'Ridge orientation clockwise from true north     ')
       !12345678901234567890123456789012345678901234567890
       !         10        20        30        40        
       status = nf_put_att_text   (foutid, ThisId, 'units'     , 7, 'degrees')
       status = nf_put_att_text   (foutid, ThisId, 'filter'    , 4, 'none')

       ThisId = anglxid
       status = nf_put_att_double (foutid, ThisId, 'missing_value', nf_double, 1, fillvalue)
       status = nf_put_att_double (foutid, ThisId, '_FillValue'   , nf_double, 1, fillvalue)
       status = nf_put_att_text   (foutid, ThisId, 'long_name' , 61, &
       'Ridge orientation clockwise from b-axis in cubed sphere panel')
       !1234567890123456789012345678901234567890123456789012345678901
       !         10        20        30        40        50        60
       status = nf_put_att_text   (foutid, ThisId, 'units'     , 7, 'degrees')
       status = nf_put_att_text   (foutid, ThisId, 'filter'    , 4, 'none')

       ThisId = hwdthid
       status = nf_put_att_double (foutid, ThisId, 'missing_value', nf_double, 1, fillvalue)
       status = nf_put_att_double (foutid, ThisId, '_FillValue'   , nf_double, 1, fillvalue)
       status = nf_put_att_text   (foutid, ThisId, 'long_name' , 21, &
       'Estimated Ridge width')
       !12345678901234567890123456789012345678901234567890
       !         10        20        30        40        
       status = nf_put_att_text   (foutid, ThisId, 'units'     , 2, 'km')
       status = nf_put_att_text   (foutid, ThisId, 'filter'    , 4, 'none')

       ThisId = clngtid
       status = nf_put_att_double (foutid, ThisId, 'missing_value', nf_double, 1, fillvalue)
       status = nf_put_att_double (foutid, ThisId, '_FillValue'   , nf_double, 1, fillvalue)
       status = nf_put_att_text   (foutid, ThisId, 'long_name' , 34, &
       'Estimated Ridge length along crest')
       !12345678901234567890123456789012345678901234567890
       !         10        20        30        40        
       status = nf_put_att_text   (foutid, ThisId, 'units'     , 2, 'km')
       status = nf_put_att_text   (foutid, ThisId, 'filter'    , 4, 'none')

       ThisId = anixyid
       status = nf_put_att_double (foutid, ThisId, 'missing_value', nf_double, 1, fillvalue)
       status = nf_put_att_double (foutid, ThisId, '_FillValue'   , nf_double, 1, fillvalue)
       status = nf_put_att_text   (foutid, ThisId, 'long_name' , 42, &
       'Variance ratio: cross/(cross+length) -wise')
       !12345678901234567890123456789012345678901234567890
       !         10        20        30        40        
       status = nf_put_att_text   (foutid, ThisId, 'units'     , 1, '1')
       status = nf_put_att_text   (foutid, ThisId, 'filter'    , 4, 'none')

       ThisId = anisoid
       status = nf_put_att_double (foutid, ThisId, 'missing_value', nf_double, 1, fillvalue)
       status = nf_put_att_double (foutid, ThisId, '_FillValue'   , nf_double, 1, fillvalue)
       status = nf_put_att_text   (foutid, ThisId, 'long_name' , 36, &
       'Variance fraction explained by ridge')
       !12345678901234567890123456789012345678901234567890
       !         10        20        30        40        
       status = nf_put_att_text   (foutid, ThisId, 'units'     , 1, '1')
       status = nf_put_att_text   (foutid, ThisId, 'filter'    , 4, 'none')

       ThisId = isovarid
       status = nf_put_att_double (foutid, ThisId, 'missing_value', nf_double, 1, fillvalue)
       status = nf_put_att_double (foutid, ThisId, '_FillValue'   , nf_double, 1, fillvalue)
       status = nf_put_att_text   (foutid, ThisId, 'long_name' , 50, &
       'SQRT(Variance) from topo NOT represented by ridges')
       !12345678901234567890123456789012345678901234567890
       !         10        20        30        40        
       status = nf_put_att_text   (foutid, ThisId, 'units'     , 1, '1')
       status = nf_put_att_text   (foutid, ThisId, 'filter'    , 4, 'none')

       ThisId=gbxarid
       status = nf_put_att_double (foutid, ThisId, 'missing_value', nf_double, 1, fillvalue)
       status = nf_put_att_double (foutid, ThisId, '_FillValue'   , nf_double, 1, fillvalue)
       status = nf_put_att_text   (foutid, ThisId, 'long_name' , 46, &
       'angular area of target grid cell from scheme')
       !12345678901234567890123456789012345678901234
       !              10        20        30        40
       status = nf_put_att_text   (foutid, ThisId, 'units'     , 7, 'm+2 m-2')
       status = nf_put_att_text   (foutid, ThisId, 'filter'    , 4, 'none')
    end if

#endif
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
    status = nf_put_var_double (foutid, terrid, terr*9.80616)!xxxgravity)
    if (status .ne. NF_NOERR) call handle_err(status)
    print*,"done writing terrain data"

    if (lwrite_rrfac_to_topo_file) then
      status = nf_put_var_double (foutid, rrfacid, rrfac_target)
      if (status .ne. NF_NOERR) call handle_err(status)
      print*,"done writing rrfac data"
    end if
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
      
      print*,"writing ISOVAR data",MINVAL(isovar_target),MAXVAL(isovar_target)
      status = nf_put_var_double (foutid, isovarid, isovar_target )
      if (status .ne. NF_NOERR) call handle_err(status)
      print*,"done writing ISOVAR data"
      
      print*,"writing RISEQ  data",MINVAL(riseq_target),MAXVAL(riseq_target)
      status = nf_put_var_double (foutid, riseqid, riseq_target)
      if (status .ne. NF_NOERR) call handle_err(status)
      print*,"done writing RISEQ data"
      
      print*,"writing FALLQ  data",MINVAL(fallq_target),MAXVAL(fallq_target)
      status = nf_put_var_double (foutid, fallqid, fallq_target)
      if (status .ne. NF_NOERR) call handle_err(status)
      print*,"done writing FALLQ data"
      
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
      
      
      print*,"writing CLNGT  data",MINVAL(clngt_target),MAXVAL(clngt_target)
      status = nf_put_var_double (foutid, clngtid, clngt_target)
      if (status .ne. NF_NOERR) call handle_err(status)
      print*,"done writing CLNGT data"
      

      print*,"writing GBXAR  data",MINVAL(area_target),MAXVAL(area_target)
      status = nf_put_var_double (foutid, gbxarid, area_target)
      if (status .ne. NF_NOERR) call handle_err(status)
      print*,"done writing GBXAR data"

#if 0      
      print*,"writing TERR_UF  data",MINVAL(terr_uf_target),MAXVAL(terr_uf_target)
      status = nf_put_var_double (foutid, terrufid, terr_uf_target)
      if (status .ne. NF_NOERR) call handle_err(status)
      print*,"done writing TERR_UF data"
      
      print*,"writing SGH_UF  data",MINVAL(sgh_uf_target),MAXVAL(sgh_uf_target)
      status = nf_put_var_double (foutid, sghufid, sgh_uf_target)
      if (status .ne. NF_NOERR) call handle_err(status)
      print*,"done writing SGH_UF data"

      print*,"writing MXVRX  data",MINVAL(mxvrx_target),MAXVAL(mxvrx_target)
      status = nf_put_var_double (foutid, mxvrxid, mxvrx_target)
      if (status .ne. NF_NOERR) call handle_err(status)
      print*,"done writing MXVRX data"
      
      print*,"writing MXVRY  data",MINVAL(mxvry_target),MAXVAL(mxvry_target)
      status = nf_put_var_double (foutid, mxvryid, mxvry_target)
      if (status .ne. NF_NOERR) call handle_err(status)
      print*,"done writing MXVRY data"
      
      print*,"writing CWGHT  data",MINVAL(cwght_target),MAXVAL(cwght_target)
      status = nf_put_var_double (foutid, cwghtid, cwght_target)
      if (status .ne. NF_NOERR) call handle_err(status)
      print*,"done writing CWGHT data"
      
      print*,"writing COUNT  data",MINVAL(count_target),MAXVAL(count_target)
      status = nf_put_var_double (foutid, countid, count_target)
      if (status .ne. NF_NOERR) call handle_err(status)
      print*,"done writing COUNT data"

      print*,"writing WGHTS  data",MINVAL(wghts_target),MAXVAL(wghts_target)
      status = nf_put_var_double (foutid, wghtsid, wghts_target)
      if (status .ne. NF_NOERR) call handle_err(status)
      print*,"done writing WGHTS data"
#endif 
      
    endif
    
    
    !
    ! Close output file
    !
    print *,"close file"
    status = nf_close (foutid)
    if (status .ne. NF_NOERR) call handle_err(status)
 end subroutine wrtnc2_unstructured

end module write_ncoutput_mod
