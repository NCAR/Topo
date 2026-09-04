!*******************************************************************************
! wrtncdf_ridge_tiles
!
! Writes a complete, CAM-readable topo file in which the ridge variables come
! from the *tile* arrays produced by remapridge2tiles, rather than from the
! angle-binned *_target arrays written by wrtncdf_unstructured.
!
! The only structural difference from wrtncdf_unstructured is that the 'nrdg'
! dimension is maxtiles (objects per target cell) instead of nsubr (angle bins).
! Variable names, types and attributes are unchanged, so CAM's ridge scheme
! reads this file exactly as it reads the existing one.
!
! Two corrections are applied here rather than in ridge_ana, so that
! Ridge_tile_map.dat keeps its native units:
!
!   1. HWDTH and CLNGT are scaled by grid_length_scale to convert from
!      cube-cell counts to km, matching what remapridge2target does for
!      hwdth_target/clngt_target.
!   2. Unused tile slots hold -9999 and are zeroed, so that CAM never sees a
!      fill value as an obstacle height.
!
! Paste into cube_to_target.F90 after wrtncdf_unstructured.
!
! Requires in ridge_ana: mxdis_tiles, aniso_tiles, anglx_tiles, angll_tiles,
! hwdth_tiles, clngt_tiles, anixy_tiles, wghts_tiles, riseq_tiles, fallq_tiles
! and ntiles_out to be PUBLIC. See the companion patch.
!*******************************************************************************
subroutine wrtncdf_ridge_tiles(n,nrdg,terr,landfrac,sgh,sgh30,landm_coslat,lon,lat,area,&
     output_fname,command_line_arguments,str_creator,area_target,llandfrac)

  use shr_kind_mod, only: r8 => shr_kind_r8
  use shared_vars,  only: rad2deg
  use ridge_ana,    only: mxdis_tiles, aniso_tiles, anglx_tiles, angll_tiles, &
                          hwdth_tiles, clngt_tiles, anixy_tiles, wghts_tiles, &
                          riseq_tiles, fallq_tiles,                           &
                          isovar_target, isowgt_target, grid_length_scale

  implicit none

#     include         <netcdf.inc>

  !
  ! Dummy arguments
  !
  integer,               intent(in) :: n          ! ncol  (= ntarget)
  integer,               intent(in) :: nrdg       ! ridges per column (= maxtiles)
  real(r8),dimension(n), intent(in) :: terr,landfrac,sgh,sgh30,lon,lat
  real(r8),dimension(n), intent(in) :: landm_coslat,area,area_target
  character(len=1024),   intent(in) :: output_fname
  character(len=1024),   intent(in) :: command_line_arguments
  character(len=1024),   intent(in) :: str_creator
  logical,               intent(in) :: llandfrac
  !
  ! Local variables
  !
  integer :: foutid
  integer :: nid(2)
  integer :: terrid, landfracid, sghid, sgh30id, landm_coslatid, areaid
  integer :: latvid, lonvid, isovarid, isowgtid, gbxarid
  integer :: mxdisid, ang22id, anglxid, anisoid, anixyid
  integer :: hwdthid, clngtid, wghtsid, riseqid, fallqid
  integer :: status

  real(r8), allocatable :: tmp(:,:)

  real(r8), parameter :: fillvalue = 1.d36
  real(r8), parameter :: EMPTY_TILE = -900.0_r8   ! catches -9000 and -9999

!-------------------------------------------------------------------------------
  write(*,*) " "
  write(*,*) "wrtncdf_ridge_tiles: writing tile-based ridge topo file"
  write(*,*) "  ncol = ",n,"   nrdg (maxtiles) = ",nrdg
  write(*,*) "  file = ",TRIM(output_fname)

  if (nrdg < 1) then
     write(*,*) "wrtncdf_ridge_tiles: nrdg < 1 - remapridge2tiles was not run?"
     stop
  end if

  allocate( tmp(n,nrdg) )
  !
  !  Create NetCDF file for output
  !
  status = nf_create (TRIM(output_fname), NF_64BIT_DATA, foutid)
  if (status .ne. NF_NOERR) call handle_err(status)
  !
  ! Dimensions
  !
  status = nf_def_dim (foutid, 'ncol', n, nid(1))
  if (status .ne. NF_NOERR) call handle_err(status)
  status = nf_def_dim (foutid, 'nrdg', nrdg, nid(2))
  if (status .ne. NF_NOERR) call handle_err(status)
  !
  ! Column variables
  !
  call defvar('PHIS'        ,1,terrid)
  if (llandfrac) call defvar('LANDFRAC',1,landfracid)
  call defvar('SGH'         ,1,sghid)
  call defvar('SGH30'       ,1,sgh30id)
  call defvar('LANDM_COSLAT',1,landm_coslatid)
  call defvar('area'        ,1,areaid)
  call defvar('lat'         ,1,latvid)
  call defvar('lon'         ,1,lonvid)
  call defvar('ISOVAR'      ,1,isovarid)
  call defvar('ISOWGT'      ,1,isowgtid)
  call defvar('GBXAR'       ,1,gbxarid)
  !
  ! Ridge variables, dimensioned (ncol,nrdg)
  !
  call defvar('MXDIS',2,mxdisid)
  call defvar('ANGLL',2,ang22id)
  call defvar('ANGLX',2,anglxid)
  call defvar('ANISO',2,anisoid)
  call defvar('ANIXY',2,anixyid)
  call defvar('HWDTH',2,hwdthid)
  call defvar('CLNGT',2,clngtid)
  call defvar('WGHTS',2,wghtsid)
  call defvar('RISEQ',2,riseqid)
  call defvar('FALLQ',2,fallqid)
  !
  ! Attributes
  !
  call put_atts(terrid        ,'surface geopotential','m2/s2')
  if (llandfrac) call put_atts(landfracid,'gridbox land fraction','1')
  call put_atts(sghid         ,'standard deviation of 3km cubed-sphere elevation and target grid elevation','m')
  call put_atts(sgh30id       ,'standard deviation of 30s elevation from 3km cubed-sphere cell average height','m')
  call put_atts(landm_coslatid,'smoothed land fraction','1')
  call put_atts(areaid        ,'area of target grid cell','m+2')
  call put_atts(latvid        ,'latitude','degrees_north')
  call put_atts(lonvid        ,'longitude','degrees_east')
  call put_atts(isovarid      ,'Residual variance from topo NOT rep by ridges','m')
  call put_atts(isowgtid      ,'area weight of residual variance','1')
  call put_atts(gbxarid       ,'angular area of target grid cell from scheme','m+2 m-2')

  call put_atts(mxdisid,'Obstacle height diagnosed by ridge-finding alg.','m')
  call put_atts(ang22id,'Ridge orientation clockwise from true north','degrees')
  call put_atts(anglxid,'Ridge orientation clockwise from b-axis in cubed sphere panel','degrees')
  call put_atts(anisoid,'Variance fraction explained by ridge','1')
  call put_atts(anixyid,'Variance ratio: cross/(cross+length) -wise','1')
  call put_atts(hwdthid,'Estimated Ridge width','km')
  call put_atts(clngtid,'Estimated Ridge length along crest','km')
  call put_atts(wghtsid,'Area of target cell covered by ridge wedge','m+2')
  call put_atts(riseqid,'Rise to peak from left (ridge_finding)','m')
  call put_atts(fallqid,'Fall from peak toward right (ridge_finding)','m')
  !
  ! Record that this file uses tile-based ridges, so it can be told apart
  ! from a file written by wrtncdf_unstructured.
  !
  status = nf_put_att_text (foutid,NF_GLOBAL,'ridge_representation',27, &
       'tiles (remapridge2tiles)   ')
  if (status .ne. NF_NOERR) call handle_err(status)

  call wrt_cesm_meta_data(foutid,command_line_arguments,str_creator)

  status = nf_enddef (foutid)
  if (status .ne. NF_NOERR) call handle_err(status)
  !
  ! Column data
  !
  call put1d(terrid,'PHIS',terr*9.80616_r8)
  if (llandfrac) call put1d(landfracid,'LANDFRAC',landfrac)
  call put1d(sghid         ,'SGH'         ,sgh)
  call put1d(sgh30id       ,'SGH30'       ,sgh30)
  call put1d(landm_coslatid,'LANDM_COSLAT',landm_coslat)
  call put1d(areaid        ,'area'        ,area)
  call put1d(isovarid      ,'ISOVAR'      ,isovar_target)
  call put1d(isowgtid      ,'ISOWGT'      ,isowgt_target)
  call put1d(gbxarid       ,'GBXAR'       ,area_target)

  if (maxval(lat)<45.0_r8) then
     call put1d(latvid,'lat',lat*rad2deg)
  else
     call put1d(latvid,'lat',lat)
  end if
  if (maxval(lon)<100.0_r8) then
     call put1d(lonvid,'lon',lon*rad2deg)
  else
     call put1d(lonvid,'lon',lon)
  end if
  !
  ! Ridge data. clean() zeroes unused tile slots; HWDTH and CLNGT are
  ! additionally converted from cube-cell counts to km.
  !
  call put2d(mxdisid,'MXDIS',clean(mxdis_tiles))
  call put2d(ang22id,'ANGLL',clean(angll_tiles))
  call put2d(anglxid,'ANGLX',clean(anglx_tiles))
  call put2d(anisoid,'ANISO',clean(aniso_tiles))
  call put2d(anixyid,'ANIXY',clean(anixy_tiles))
  call put2d(hwdthid,'HWDTH',clean(hwdth_tiles)*grid_length_scale)
  call put2d(clngtid,'CLNGT',clean(clngt_tiles)*grid_length_scale)
  call put2d(wghtsid,'WGHTS',clean(wghts_tiles))
  call put2d(riseqid,'RISEQ',clean(riseq_tiles))
  call put2d(fallqid,'FALLQ',clean(fallq_tiles))

  deallocate( tmp )

  status = nf_close (foutid)
  if (status .ne. NF_NOERR) call handle_err(status)
  write(*,*) "wrtncdf_ridge_tiles: done"

contains

  !-----------------------------------------------------------------------
  subroutine defvar(name,ndims,vid)
    character(len=*), intent(in)  :: name
    integer,          intent(in)  :: ndims
    integer,          intent(out) :: vid
    integer :: st
    st = nf_def_var (foutid, name, NF_DOUBLE, ndims, nid(1:ndims), vid)
    if (st .ne. NF_NOERR) then
       write(*,*) "wrtncdf_ridge_tiles: nf_def_var failed for ",TRIM(name)
       call handle_err(st)
    end if
  end subroutine defvar

  !-----------------------------------------------------------------------
  subroutine put_atts(vid,long_name,units)
    integer,          intent(in) :: vid
    character(len=*), intent(in) :: long_name, units
    integer :: st
    st = nf_put_att_double (foutid, vid, 'missing_value', nf_double, 1, fillvalue)
    st = nf_put_att_double (foutid, vid, '_FillValue'   , nf_double, 1, fillvalue)
    st = nf_put_att_text   (foutid, vid, 'long_name', LEN_TRIM(long_name), TRIM(long_name))
    st = nf_put_att_text   (foutid, vid, 'units'    , LEN_TRIM(units)    , TRIM(units))
  end subroutine put_atts

  !-----------------------------------------------------------------------
  subroutine put1d(vid,name,vals)
    integer,          intent(in) :: vid
    character(len=*), intent(in) :: name
    real(r8),         intent(in) :: vals(n)
    integer :: st
    write(*,*) "  writing ",TRIM(name),MINVAL(vals),MAXVAL(vals)
    st = nf_put_var_double (foutid, vid, vals)
    if (st .ne. NF_NOERR) then
       write(*,*) "wrtncdf_ridge_tiles: write failed for ",TRIM(name)
       call handle_err(st)
    end if
  end subroutine put1d

  !-----------------------------------------------------------------------
  subroutine put2d(vid,name,vals)
    integer,          intent(in) :: vid
    character(len=*), intent(in) :: name
    real(r8),         intent(in) :: vals(n,nrdg)
    integer :: st
    write(*,*) "  writing ",TRIM(name),MINVAL(vals),MAXVAL(vals)
    st = nf_put_var_double (foutid, vid, vals)
    if (st .ne. NF_NOERR) then
       write(*,*) "wrtncdf_ridge_tiles: write failed for ",TRIM(name)
       call handle_err(st)
    end if
  end subroutine put2d

  !-----------------------------------------------------------------------
  ! Replace fill values in unused tile slots with zero, so that CAM sees an
  ! absent ridge rather than a -9999 obstacle.
  !-----------------------------------------------------------------------
  function clean(a) result(b)
    real(r8), intent(in) :: a(n,nrdg)
    real(r8)             :: b(n,nrdg)
    b = a
    where (b < EMPTY_TILE) b = 0.0_r8
  end function clean

end subroutine wrtncdf_ridge_tiles
