module ridge_angles_mod
!===============================================================================
! Convert sub-grid ridge crest orientations from cubed-sphere (alpha,beta)
! coordinates to local geographic (east/north) coordinates.
!
! Replacement for the legacy subroutine `latlonangles`.  Everything the routine
! reads or writes is an explicit dummy argument; nothing arrives through a
! module common block.
!
! Array shape is (ntarget,maxtiles), matching the tile arrays written by
! `remapridge2tiles` and stored in Ridge_tile_map.dat.  Since Fortran is
! column-major, the target index is the fast one, so the target loop is the
! INNER loop below.
!
! Angle convention (unchanged from the legacy code, so results are comparable):
!
!   ANGLX is measured in DEGREES from the +beta axis toward the +alpha axis,
!   i.e. the crest direction in the panel chart is
!        ( d_alpha, d_beta ) = ( sin(ANGLX), cos(ANGLX) ).
!
!   ANG22 is measured in DEGREES from local north toward local east, folded
!   into [0,180).  A ridge is an undirected line, so NE-SW and SW-NE are the
!   same object and map to the same value.
!
! Method:
!   At each ridge crest location the map from (alpha,beta) to local (east,north)
!   is linearised by central differencing the gnomonic map, giving a 2x2
!   Jacobian.  The crest direction vector is pushed through that Jacobian and
!   the result converted to an azimuth.  A Jacobian is required rather than a
!   single rotation angle because the equiangular gnomonic projection is not
!   conformal: it shears, so the transformed angle depends on the input angle
!   and not on position alone.
!
! Author: modernisation of legacy CAM/CESM ridge-scheme topography code.
!===============================================================================

  use shr_kind_mod, only: r8 => shr_kind_r8

  ! Adjust this `use` to wherever the cubed-sphere transforms actually live.
  ! use remap, only: CubedSphereABPFromRLL, CubedSphereRLLFromABP

  implicit none
  private

  public :: ridge_anglx_to_latlon

  real(r8), parameter :: PI_R8   = 3.14159265358979323846_r8
  real(r8), parameter :: TWOPI   = 2.0_r8 * PI_R8
  real(r8), parameter :: HALFPI  = 0.5_r8 * PI_R8
  real(r8), parameter :: RAD2DEG = 180.0_r8 / PI_R8
  real(r8), parameter :: DEG2RAD = PI_R8 / 180.0_r8

  ! Default fill value written into ANG22 for absent ridges.
  real(r8), parameter :: FILL_DEFAULT = -9999.0_r8

  ! Anything at or below this is treated as a fill marker on input.  Chosen to
  ! catch BOTH sentinel conventions in the legacy code (-9000 and -9999) while
  ! staying clear of every physically meaningful angle, longitude or latitude.
  real(r8), parameter :: SENTINEL_TEST = -900.0_r8

  ! Central-difference step in equiangular (alpha,beta) radians.  Truncation
  ! error is O(h**2 * curvature) ~ 1e-10 and roundoff ~ eps/h ~ 1e-11, so this
  ! sits near the accuracy optimum for r8.
  real(r8), parameter :: HSTEP_DEFAULT = 1.0e-5_r8

  ! Ridges closer to a pole than this (radians) get the fill value: the local
  ! east direction is undefined at the pole, so an azimuth is meaningless there.
  real(r8), parameter :: POLE_GUARD = 1.0e-7_r8

contains

!-------------------------------------------------------------------------------
subroutine ridge_anglx_to_latlon( ntarget, maxtiles, lonc, latc, anglx, ang22, &
                                  latlon_in_degrees, missing, hstep, ierr )
!-------------------------------------------------------------------------------
! Arguments
!-------------------------------------------------------------------------------
  integer,  intent(in)  :: ntarget                   ! target grid cells
  integer,  intent(in)  :: maxtiles                  ! ridges (tiles) per target cell

  real(r8), intent(in)  :: lonc (ntarget,maxtiles)   ! crest longitude
  real(r8), intent(in)  :: latc (ntarget,maxtiles)   ! crest latitude
  real(r8), intent(in)  :: anglx(ntarget,maxtiles)   ! crest angle in (alpha,beta), DEGREES

  real(r8), intent(out) :: ang22(ntarget,maxtiles)   ! crest azimuth from north, DEGREES, [0,180)

  ! Units of LONC/LATC.  There is no reliable way to infer this from the data,
  ! so the caller must say.  Default .false. = radians, which is what
  ! remapridge2tiles writes.
  logical,  intent(in),  optional :: latlon_in_degrees

  real(r8), intent(in),  optional :: missing         ! fill for ANG22 (default -9999)
  real(r8), intent(in),  optional :: hstep           ! FD step in (alpha,beta) radians

  ! 0 = success.  1 = latitudes inconsistent with the declared units.
  ! If absent, an inconsistency prints a message and stops.
  integer,  intent(out), optional :: ierr

!-------------------------------------------------------------------------------
! Locals
!-------------------------------------------------------------------------------
  real(r8) :: scale, fill, h, coslat0
  real(r8) :: lon0, lat0, a0, b0
  real(r8) :: lonp, latp, lonm, latm
  real(r8) :: dEast_da, dNorth_da, dEast_db, dNorth_db
  real(r8) :: ua, ub, dx, dy, dmag, latmax
  integer  :: i, ir, ipanel, istat

  
  real(r8) :: lon22, lat22, a22, b22
  real(r8) :: lon22s, lat22s, a22s, b22s
  real(r8) :: dx2, dy2, cosLL, To_Radians


  
  logical  :: in_degrees
  
!-------------------------------------------------------------------------------
! Option handling
!-------------------------------------------------------------------------------

  fill = FILL_DEFAULT
  if ( present(missing) ) fill = missing

  h = HSTEP_DEFAULT
  if ( present(hstep) ) h = hstep

  istat = 0
  ang22(:,:) = fill

!-------------------------------------------------------------------------------
! lat-lon Option handling
!-------------------------------------------------------------------------------
#if 0
  in_degrees = .false.
  if ( present(latlon_in_degrees) ) in_degrees = latlon_in_degrees
  scale = 1.0_r8
  if ( in_degrees ) scale = DEG2RAD

  if ( maxval( abs( latc )) > 2.0 ) then
     ! Center coords are in DEGREES. Rescale.
     To_Radians = DEG2RAD
  else
     ! Center coords are in RADIANS. Leave alone.
     To_Radians=  1.
  endif

!-------------------------------------------------------------------------------
! Validate the declared units before doing any work.  Rather than guessing from
! the magnitude of the data, check that the data are consistent with what the
! caller declared, and fail loudly if they are not.
!-------------------------------------------------------------------------------
  latmax = 0.0_r8
  do ir = 1, maxtiles
     do i = 1, ntarget
        if ( latc(i,ir) > SENTINEL_TEST ) latmax = max( latmax, abs(latc(i,ir)) )
     end do
  end do

  if ( in_degrees ) then
     if ( latmax > 90.0_r8 + 1.0e-6_r8 ) istat = 1
  else
     if ( latmax > HALFPI + 1.0e-6_r8 ) istat = 1
  end if

  if ( istat /= 0 ) then
     if ( present(ierr) ) then
        ierr = istat
        return
     else
        write(*,*) 'ridge_anglx_to_latlon: latitudes inconsistent with declared units.'
        write(*,*) '  latlon_in_degrees = ', in_degrees, '   max|lat| = ', latmax
        stop 1
     end if
  end if
  if ( present(ierr) ) ierr = 0
#else
  ! tile lat-lons are in radians
  scale = 1.0_r8
  To_Radians=  1.0_r8

#endif
!-------------------------------------------------------------------------------
! Main loop.  Tile index outer, target index inner: with (ntarget,maxtiles) the
! target index is the contiguous one.
!-------------------------------------------------------------------------------
  do ir = 1, maxtiles
     do i = 1, ntarget

        ! Absent ridge, or missing position: leave the fill value in place.
        if ( anglx(i,ir) <= SENTINEL_TEST ) cycle
        if ( latc (i,ir) <= SENTINEL_TEST ) cycle
        if ( lonc (i,ir) <= SENTINEL_TEST ) cycle

        lon0 = lonc(i,ir) * scale
        lat0 = latc(i,ir) * scale

        ! East is undefined at the poles; a crest azimuth there is meaningless.
        if ( abs(lat0) > HALFPI - POLE_GUARD ) cycle

        ! Locate this crest in the panel chart.  The panel returned here is
        ! reused for the displaced points below: staying on one chart avoids a
        ! discontinuity for ridges sitting on a panel edge, and the gnomonic
        ! formula extrapolates smoothly a little past |alpha| = pi/4.
        call CubedSphereABPFromRLL( lon0, lat0, a0, b0, ipanel, .true. )

        coslat0 = cos(lat0)

#if 0
        ! ---- Jacobian of (east,north) with respect to (alpha,beta) ----------
        ! Central differences: second-order accurate, and symmetric about the
        ! crest so there is no bias in the direction of the step.
        call CubedSphereRLLFromABP( a0 + h, b0, ipanel, lonp, latp )
        call CubedSphereRLLFromABP( a0 - h, b0, ipanel, lonm, latm )
        dEast_da  = coslat0 * wrap_lon( lonp - lonm ) / ( 2.0_r8 * h )
        dNorth_da =         ( latp - latm )           / ( 2.0_r8 * h )

        call CubedSphereRLLFromABP( a0, b0 + h, ipanel, lonp, latp )
        call CubedSphereRLLFromABP( a0, b0 - h, ipanel, lonm, latm )
        dEast_db  = coslat0 * wrap_lon( lonp - lonm ) / ( 2.0_r8 * h )
        dNorth_db =         ( latp - latm )           / ( 2.0_r8 * h )

        ! ---- Push the crest direction through the Jacobian ------------------
        ua = sin( anglx(i,ir) * DEG2RAD )
        ub = cos( anglx(i,ir) * DEG2RAD )

        dx = dEast_da  * ua + dEast_db  * ub      ! eastward component
        dy = dNorth_da * ua + dNorth_db * ub      ! northward component

        dmag = sqrt( dx*dx + dy*dy )
        if ( dmag <= tiny(1.0_r8) ) cycle         ! degenerate Jacobian

        ! ---- Azimuth --------------------------------------------------------
        ! ATAN2 is accurate over the whole circle and needs no sign kluge.
        ! MODULO folds the result into [0,180) because a ridge is undirected.
        ang22(i,ir) = modulo( atan2( dx, dy ) * RAD2DEG, 180.0_r8 )
#else
        lon22 = lonc(i,ir) * To_Radians
        lat22 = latc(i,ir) * To_Radians
        call CubedSphereABPFromRLL(lon22, lat22, a22, b22, ipanel , .true. )
        a22s = a22 + 0.01*SIN( ANGLX(i,ir)*PI_R8/180. )   
        b22s = b22 + 0.01*COS( ANGLX(i,ir)*PI_R8/180. )
        call CubedSphereRLLFromABP(a22s, b22s , ipanel, lon22s, lat22s )
        dx2 = COS( lat22 )*(lon22s-lon22 )
        dy2 = ( lat22s-lat22 )
        if ( dx2 < 0.0 ) dy2  = -1.*dy2  ! 
        COSLL  = dy2 /sqrt( dx2**2 + dy2**2 )
        ang22(i,ir) = ACOS( COSLL )*180./PI_R8
#endif


     end do
  end do

end subroutine ridge_anglx_to_latlon

!-------------------------------------------------------------------------------
pure function wrap_lon( dlon ) result( d )
!-------------------------------------------------------------------------------
! Wrap a longitude difference into (-pi,pi].  Without this, any crest whose
! displaced points straddle the branch cut of the longitude convention produces
! a difference near +/-2*pi instead of near zero.
!-------------------------------------------------------------------------------
  real(r8), intent(in) :: dlon
  real(r8)             :: d

  d = modulo( dlon + PI_R8, TWOPI ) - PI_R8

end function wrap_lon

end module ridge_angles_mod
