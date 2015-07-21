!-----------------------------------------------------------------------------
! MODULE CubedSphereTrans
!
! Purpose:
!   Provides functions for transforming coordinates between the lat lon
!   grid and cubed sphere coordinate system.
!
! Record of revisions (code developed from R.Nair DG code):
!
!      Date       Programmer       Affiliation          Description of change
!      ====       ==========       ===========          =====================
!    06/13/06    P.H.Lauritzen     CMS,NCAR             Original code
!    05/14/08    P.A.Ullrich       CGD,NCAR             Version 2
!    10/28/08    P.H.Lauritzen     AMP,NCAR             Port to advection code
!
!-----------------------------------------------------------------------------

MODULE cube_trfs

  IMPLICIT NONE

CONTAINS

!------------------------------------------------------------------------------
! SUBROUTINE GnomonicToRad
!
! Description:
!   Gnomonic transformation from cube face np to sphere, that is,
!   transformation from cubed sphere grid to global sph. coordinates
!
!   Transformations are in, e.g., 
!
!   Ramachandran D. Nair, Stephen J. Thomas and Richard D. Loft. 2005: 
!   A Discontinuous Galerkin Transport Scheme on the Cubed Sphere. 
!   Monthly Weather Review: Vol. 133, No. 4, pp. 814-828.
!
! Record of revisions (code developed from R.Nair DG code):
!
!      Date       Programmer       Affiliation          Description of change
!      ====       ==========       ===========          =====================
!    05/05/03     R.Nair           SCD,NCAR             Original code
!    06/06/06     P.H.Lauritzen    CMS,NCAR             Minor adaptations to remapping code
!    05/14/08     P.A.Ullrich      CGD,NCAR             Code cleanup
!
! Parameters:
!   klon - Number of longitudes in lat-lon mesh
!   klat - Number of latitudes in lat-lon mesh
!   lon_rad (OUT) - Array of longitudes calculated by function
!   lat_rad (OUT) - Array of latitudes calculated by function
!   x_gno - Input array of gnomonic x-coordinates (paired with y_gno)
!   y_gno - Input array of gnomonic y-coordinates (paired with x_gno)
!   nface - Index of this face
!   zrot - Rotation angle of lat lon grid with respect to cubed sphere edge
!------------------------------------------------------------------------------
 
  SUBROUTINE GnomonicToRad(klon, klat, lon_rad, lat_rad, &
                           x_gno, y_gno, nface)
    IMPLICIT NONE

    ! Input variables
    INTEGER (KIND=int_kind) :: klon, klat, nface

    REAL (KIND=dbl_kind), &
         DIMENSION(klon,klat) :: lon_rad, lat_rad, x_gno, y_gno

    REAL (KIND=dbl_kind) :: zrot

!    INTENT(IN)  :: klon, klat, nface, x_gno, y_gno, zrot
    INTENT(IN)  :: klon, klat, nface, x_gno, y_gno!, zrot
    INTENT(OUT) :: lon_rad, lat_rad

    
 
    ! Local variables
    REAL (KIND=dbl_kind), DIMENSION(KLON,KLAT) :: lam1, the1
    REAL(KIND=dbl_kind) :: clam
    REAL(KIND=dbl_kind) :: x, y, t1, t2, t3, lm, th, tmp
    
    INTEGER (KIND=int_kind) :: i,j,n1,n2,l,k

    
    IF (nface .GE. 1 .AND. nface .LE. 4) THEN
       !
       ! Standarad gnomonic transformation on Face-1     
       !   x = a * tan(lam1)            
       !   y = a * tan(the1) * sec(lam1)
       ! (lam1,the1) are the Sph coordinates corresponding to the local (x,y) on face-1
       !
       ! Note: lam1,the1 are in [-pi/4, +pi/4]
       !       gx(:,:,:,:) = aa*tan(glx(:,:,:,:))
       !       gy(:,:,:,:) = aa*tan(gly(:,:,:,:))
       
       DO n2 = 1, klat
          DO n1 = 1, klon
             t1   = ATAN2(x_gno(n1,n2),aa)
             lam1(n1,n2)  = t1
             clam = cos(t1)
             t2  = atan(clam * y_gno(n1,n2) / aa)
             the1(n1,n2) = t2
          ENDDO
       ENDDO
       
       ! Global (lambda, theta) grid coordinates
       
       ! Local transformation for Cube faces 1-2-3-4 (by rotation)
       !  x = a * tan(lm + k)            
       !  y = a * tan(th) * sec(lm + k),   k= 0, pi/2, pi, 3pi/2
       ! where,   lm => [0, pi2), th => [-pi/4, pi/4]
       DO  n2 = 1, klat
          DO  n1 = 1, klon  
             t1 = the1(n1,n2)
             !lambda=0 is at left edge of panel 1
             t2 = lam1(n1,n2)  + dble(nface-1)*pih + piq + rotate_cube
             IF (t2 < zero ) t2 = t2 + pi2 
             IF (t2 > pi2 ) t2 = t2 - pi2 
             
             lat_rad(n1,n2) = t1
             lon_rad(n1,n2) = t2      !Global storage           
          ENDDO
       ENDDO

    ELSE IF (nface == 6) THEN   
       !For Face-6 (top pancube):   Local tranformation
       !
       !           x =  a* sin(lm)* cot(th)
       !           y = -a* cos(lm)* cot(th)
       !           lm => [0, pi2),  th => [pi/4, pi/2]     
       DO  n2 = 1, klat
          DO  n1 = 1, klon  
             x = x_gno(n1,n2)
             y = y_gno(n1,n2)
             t1 = sqrt(x**2 + y**2)       !theta +ve
             IF (t1 == zero) THEN
                th = ATAN2(aa,t1)
             ELSE
                th = atan(aa/t1)
             ENDIF
             
             
             IF (x == zero .and. y == zero) THEN !add phl
                t2 = zero                        !add phl
             ELSE                                !add phl
                t2 = ATAN2(x,-y) 
             ENDIF                               !add phl
             t2 = t2+piq+rotate_cube !add phl
             IF (t2 < zero  ) t2 = t2 + pi2
             IF (t2 > pi2 ) t2 = t2 - pi2
             lm = t2  
             
             lon_rad(n1,n2) = lm
             lat_rad(n1,n2) = th
          ENDDO
       ENDDO
    ELSE IF (nface == 5) THEN
       !For face-5 (Bottom pancube):   Local tranformation:- 
       !       x = -a* sin(lm)* cot(th)
       !       y = -a* cos(lm)* cot(th)
       !      lm => [0, pi2),  th => [-pi/4, -pi/2]
       
       DO n2 = 1, klat
          DO n1 = 1, klon 
             x = x_gno(n1,n2)
             y = y_gno(n1,n2)
             t1 =-sqrt(x**2 + y**2)        !theta < zero
             th = atan(aa/t1)
             IF (t1 == zero) th = ATAN2(aa,t1)
             IF (th.ge.zero) THEN 
                th = th-pi
             ENDIF
             
             IF (x == zero .and. y == zero) THEN !add phl
                t2 = zero                        !add phl
             ELSE                                !add phl
                t2 = ATAN2(x,y)  
             ENDIF                               !add phl              
             t2 = t2+piq+rotate_cube !add phl
             IF (t2 < zero  ) t2 = t2 + pi2
             IF (t2 > pi2-tiny ) t2 = t2 - pi2
             lm = t2        
             lon_rad(n1,n2) = lm
             lat_rad(n1,n2) = th        
          ENDDO
       ENDDO
    ELSE 
       WRITE(*,*) 'nface out of range in gnomonic transformation'
       STOP
    ENDIF
  END SUBROUTINE GnomonicToRad
  


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

    IMPLICIT NONE

    REAL    (KIND=dbl_kind), INTENT(IN)  :: alpha, beta
    INTEGER (KIND=int_kind), INTENT(IN)  :: ipanel
    REAL    (KIND=dbl_kind), INTENT(OUT) :: xx, yy, zz

    ! Local variables
    REAL    (KIND=dbl_kind) :: a1, b1, pm
    REAL    (KIND=dbl_kind) :: sx, sy, sz

    ! Convert to Cartesian coordinates
    a1 = TAN(alpha)
    b1 = TAN(beta)

    sz = (one + a1 * a1 + b1 * b1)**(-half)
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

  END SUBROUTINE

!------------------------------------------------------------------------------
! SUBROUTINE CubedSphereXYZFromRLL
!
! Description:
!   Determine the Cartesian coordinate of a point on the sphere from
!   a given regular lat lon coordinate.
!
! Parameters:
!   lon - Coordinate longitude
!   lat - Coordinate latitude
!   xx (OUT) - X coordinate
!   yy (OUT) - Y coordinate
!   zz (OUT) - Z coordinate
!------------------------------------------------------------------------------
  SUBROUTINE CubedSphereXYZFromRLL(lon, lat, xx, yy, zz)

    IMPLICIT NONE

    REAL    (KIND=dbl_kind), INTENT(IN)  :: lon, lat
    REAL    (KIND=dbl_kind), INTENT(OUT) :: xx, yy, zz

    ! Translate to (x,y,z) space
    xx = COS(lon - piq-rotate_cube) * COS(lat)
    yy = SIN(lon - piq-rotate_cube) * COS(lat)
    zz = SIN(lat)

  END SUBROUTINE CubedSphereXYZFromRLL

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

    IMPLICIT NONE

    REAL    (KIND=dbl_kind), INTENT(IN)  :: alpha, beta
    INTEGER (KIND=int_kind), INTENT(IN)  :: ipanel
    REAL    (KIND=dbl_kind), INTENT(OUT) :: lon, lat

    ! Local variables
    REAL    (KIND=dbl_kind) :: xx, yy, zz

    ! Convert to cartesian coordinates
    CALL CubedSphereXYZFromABP(alpha, beta, ipanel, xx, yy, zz)

    ! Convert back to lat lon
    lat = ASIN(zz)
    lon = ATAN2(yy, xx) + piq+rotate_cube

  END SUBROUTINE

! Eigil: Obs
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
  SUBROUTINE CubedSphereABPFromRLL(lon, lat, alpha, beta, ipanel)

    IMPLICIT NONE

    REAL    (KIND=dbl_kind), INTENT(IN)  :: lon, lat
    REAL    (KIND=dbl_kind), INTENT(OUT) :: alpha, beta
    INTEGER (KIND=int_kind), INTENT(OUT) :: ipanel

    ! Local variables
    REAL    (KIND=dbl_kind) :: xx, yy, zz, pm
    REAL    (KIND=dbl_kind) :: sx, sy, sz
    INTEGER (KIND=int_kind) :: ix, iy, iz

    ! Translate to (x,y,z) space
    xx = COS(lon - piq-rotate_cube) * COS(lat)
    yy = SIN(lon - piq-rotate_cube) * COS(lat)
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

    ! Use panel information to calculate (alpha, beta) coords
    alpha = ATAN(sx / sz)
    beta = ATAN(sy / sz)

  END SUBROUTINE CubedSphereABPFromRLL

!------------------------------------------------------------------------------
! SUBROUTINE CubedSphereABPFromABP
!
! Description:
!   Determine the (alpha,beta,idest) coordinate of a source point on
!   panel isource.
!
! Parameters:
!   alpha_in - Alpha coordinate in
!   beta_in - Beta coordinate in
!   isource - Source panel
!   idest - Destination panel
!   alpha_out (OUT) - Alpha coordinate out
!   beta_out (OUT) - Beta coordiante out
!------------------------------------------------------------------------------
  SUBROUTINE CubedSphereABPFromABP(alpha_in,  beta_in, isource, idest, &
                                   alpha_out, beta_out)

    IMPLICIT NONE

    REAL    (KIND=dbl_kind), INTENT(IN)  :: alpha_in, beta_in
    INTEGER (KIND=int_kind), INTENT(IN)  :: isource, idest
    REAL    (KIND=dbl_kind), INTENT(OUT) :: alpha_out, beta_out

    ! Local variables
    REAL    (KIND=dbl_kind) :: a1, b1
    REAL    (KIND=dbl_kind) :: xx, yy, zz
    REAL    (KIND=dbl_kind) :: sx, sy, sz

    ! Convert to relative Cartesian coordinates
    a1 = TAN(alpha_in)
    b1 = TAN(beta_in)

    sz = (one + a1 * a1 + b1 * b1)**(-half)
    sx = sz * a1
    sy = sz * b1

    ! Convert to full Cartesian coordinates
    IF (isource == 6) THEN
      yy = sx; xx = -sy; zz = sz

    ELSEIF (isource == 5) THEN
      yy = sx; xx = sy; zz = -sz

    ELSEIF (isource == 1) THEN
      yy = sx; zz = sy; xx = sz

    ELSEIF (isource == 3) THEN
      yy = -sx; zz = sy; xx = -sz

    ELSEIF (isource == 2) THEN
      xx = -sx; zz = sy; yy = sz

    ELSEIF (isource == 4) THEN
      xx = sx; zz = sy; yy = -sz

    ELSE
      WRITE(*,*) 'Fatal Error: Source panel invalid in CubedSphereABPFromABP'
      WRITE(*,*) 'panel = ', isource
      STOP
    ENDIF

    ! Convert to relative Cartesian coordinates on destination panel
    IF (idest == 6) THEN
      sx = yy; sy = -xx; sz = zz

    ELSEIF (idest == 5) THEN
      sx = yy; sy = xx; sz = -zz

    ELSEIF (idest == 1) THEN
      sx = yy; sy = zz; sz = xx

    ELSEIF (idest == 3) THEN
      sx = -yy; sy = zz; sz = -xx

    ELSEIF (idest == 2) THEN
      sx = -xx; sy = zz; sz = yy

    ELSEIF (idest == 4) THEN
      sx = xx; sy = zz; sz = -yy

    ELSE
      WRITE(*,*) 'Fatal Error: Dest panel invalid in CubedSphereABPFromABP'
      WRITE(*,*) 'panel = ', idest
      STOP
    ENDIF

    IF (sz < 0) THEN
      WRITE(*,*) 'Fatal Error: In CubedSphereABPFromABP'
      WRITE(*,*) 'Invalid relative Z coordinate'
      STOP
    ENDIF

    ! Use panel information to calculate (alpha, beta) coords
    alpha_out = ATAN(sx / sz)
    beta_out = ATAN(sy / sz)

  END SUBROUTINE

!------------------------------------------------------------------------------
! FUNCTION LatLonElementFromCoord
!
! Description:
!   Determine the (i,j) coordinates of the lat lon grid element that contains
!   the specified coordinate, given in (lon, lat) coordinates
!
! Parameters:
!   lon - Longitude of specified point
!   lat - Latitude of specified point
!   nlon - Number of longitudes in grid
!   nlat - Number of latitudes in grid
!   ii (OUT) - Longitude index of element
!   jj (OUT) - Latitude index of element
!   dlon (OUT) - Longitude excess of specified point within element
!   dlat (OUT) - Latitude excess of specified point within element
!------------------------------------------------------------------------------
  SUBROUTINE LatLonElementFromCoord(lon, lat, nlon, nlat, ii, jj, dlon, dlat)

    USE MathUtils

    IMPLICIT NONE

    REAL    (KIND=dbl_kind), INTENT(IN)  :: lon, lat
    INTEGER (KIND=int_kind), INTENT(IN)  :: nlon, nlat
    INTEGER (KIND=int_kind), INTENT(OUT) :: ii, jj
    REAL    (KIND=dbl_kind), INTENT(OUT) :: dlon, dlat

    ! Local variables
    REAL (KIND=dbl_kind) :: a1, b1, xlon, oneovernlone, oneovernlate

    ! Optimization
    IF (lon < zero) THEN
      xlon = lon + pi2 * DBLE(INT(-lon / pi2 + 1))
    ELSE
      xlon = MOD(lon, pi2)
    ENDIF

    oneovernlone = one / DBLE(nlon)
    oneovernlate = one / DBLE(nlat-1)

    ! Bounds checking
    IF ((lat < -pih) .OR. (lat > pih)) THEN
      WRITE(*,*) 'Fatal error: latitude out of bounds, lat =', lat
      STOP
    ENDIF

    a1 = one + (xlon / pi2 * DBLE(nlon))
    b1 = one + ((lat + pih) / pi * DBLE(nlat-1))

    ii = CLAMP_INT(INT(a1), 1, nlon)
    jj = CLAMP_INT(INT(b1), 1, nlat-1)

    dlon = xlon - pi2 * DBLE(ii-1) * oneovernlone
    dlat = lat + pih - pi * DBLE(jj-1) * oneovernlate

    dlon = CLAMP(dlon, zero, pi2 * oneovernlone)
    dlat = CLAMP(dlat, zero, pi  * oneovernlate)

  END SUBROUTINE

!------------------------------------------------------------------------------
! SUBROUTINE EquiangularElementFromCoord
!
! Description:
!   Determine the (i,j) coordinates of the equiangular cubed-sphere element 
!   that contains the specified coordinate, given in (alpha, beta) coordinates
!
! Parameters:
!   alpha - Alpha coordinate in the grid
!   beta - Beta coordinate in the grid
!   ncube - Resolution of the cubed sphere (in # of grid lines)
!   ii (OUT) - Element i coordinate (1 <= ii <= ncube)
!   jj (OUT) - Element j coordinate (1 <= jj <= ncube)
!   da (OUT) - Subgrid alpha coordinate excess
!   db (OUT) - Subgrid beta coordinate excess
!------------------------------------------------------------------------------
  SUBROUTINE EquiangularElementFromCoord(alpha, beta, ncube, ii, jj, da, db)
    USE MathUtils

    IMPLICIT NONE

    REAL    (KIND=dbl_kind), INTENT(IN)  :: alpha, beta
    INTEGER (KIND=int_kind), INTENT(IN)  :: ncube
    INTEGER (KIND=int_kind), INTENT(OUT) :: ii, jj
    REAL    (KIND=dbl_kind), INTENT(OUT) :: da, db

    REAL (KIND=dbl_kind) :: a1, b1, oneovernelem

    ! Bounds checking
    IF ((alpha < -piq-tiny) .OR. (alpha > piq+tiny)) THEN
      WRITE(*,*) 'Fatal error: alpha out of bounds, alpha =', alpha
      STOP
    ELSEIF ((beta < -piq-tiny) .OR. (beta > piq+tiny)) THEN
      WRITE(*,*) 'Fatal error: beta out of bounds, beta =', beta
      STOP
    ENDIF

    ! Optimization
    oneovernelem = one / DBLE(ncube-1)

    ! See eq. (17) in Lauritzen and Nair (2007)
    a1 = one + ((alpha + piq) / pih * DBLE(ncube-1))
    b1 = one + ((beta  + piq) / pih * DBLE(ncube-1))

    ii = CLAMP_INT(INT(a1), 1, ncube-1)
    jj = CLAMP_INT(INT(b1), 1, ncube-1)

    da = alpha + piq - pih * DBLE(ii-1) * oneovernelem
    db = beta  + piq - pih * DBLE(jj-1) * oneovernelem

    da = CLAMP(da, zero, pih * oneovernelem)
    db = CLAMP(db, zero, pih * oneovernelem)

  END SUBROUTINE

!------------------------------------------------------------------------------
! SUBROUTINE CubedSphereRelCoord
!
! Description:
!   Determine the (i,j,p) coordinates of an element outside the boundaries of
!   the designated panel given (i,j,p) coordinates within the panel.
!
! Parameters:
!   ii - Horizontal element coordinate
!   jj - Vertical element coordinate
!   ipanel - Panel for relative calculation
!   ncube - Resolution of the cubed sphere (in # of grid lines)
!   iout (OUT) - Horizontal element coordinate of resulting element
!   jout (OUT) - Vertical element coordinate of resulting element
!   pout (OUT) - Panel of resulting element
!------------------------------------------------------------------------------
  SUBROUTINE CubedSphereRelCoord(ii, jj, ipanel, ncube, iout, jout, pout)

    IMPLICIT NONE

    INTEGER (KIND=int_kind), INTENT(IN ) :: ii, jj, ipanel, ncube
    INTEGER (KIND=int_kind), INTENT(OUT) :: iout, jout, pout

    IF ((ii > 0) .AND. (jj > 0) .AND. (ii < ncube) .AND. (jj < ncube)) THEN
      iout = ii
      jout = jj
      pout = ipanel
      RETURN
 
    ELSEIF (ipanel == 1) THEN
      IF (ii < 1) THEN
        pout = 4
        iout = ii + (ncube - 1)
        jout = jj

      ELSEIF (ii > ncube-1) THEN
        pout = 2
        iout = ii - (ncube - 1)
        jout = jj

      ELSEIF (jj < 1) THEN
        pout = 5
        iout = ii
        jout = jj + (ncube - 1)

      ELSEIF (jj > ncube-1) THEN
        pout = 6
        iout = ii
        jout = jj - (ncube - 1)
      ENDIF

    ELSEIF (ipanel == 2) THEN
      IF (ii < 1) THEN
        pout = 1
        iout = ii + (ncube - 1)
        jout = jj

      ELSEIF (ii > ncube-1) THEN
        pout = 3
        iout = ii - (ncube - 1)
        jout = jj

      ELSEIF (jj < 1) THEN
        pout = 5
        iout = jj + (ncube - 1)
        jout = ncube - ii

      ELSEIF (jj > ncube-1) THEN
        pout = 6
        iout = 2 * ncube - 1 - jj
        jout = ii
      ENDIF

    ELSEIF (ipanel == 3) THEN
      IF (ii < 1) THEN
        pout = 2
        iout = ii + (ncube - 1)
        jout = jj

      ELSEIF (ii > ncube-1) THEN
        pout = 4
        iout = ii - (ncube - 1)
        jout = jj

      ELSEIF (jj < 1) THEN
        pout = 5
        iout = ncube - ii
        jout = 1 - jj

      ELSEIF (jj > ncube-1) THEN
        pout = 6
        iout = ncube - ii
        jout = 2 * ncube - 1 - jj
      ENDIF

    ELSEIF (ipanel == 4) THEN
      IF (ii < 1) THEN
        pout = 3
        iout = ii + (ncube - 1)
        jout = jj

      ELSEIF (ii > ncube-1) THEN
        pout = 1
        iout = ii - (ncube - 1)
        jout = jj

      ELSEIF (jj < 1) THEN
        pout = 5
        iout = 1 - jj
        jout = ii

      ELSEIF (jj > ncube-1) THEN
        pout = 6
        iout = jj - (ncube - 1)
        jout = ncube - ii
      ENDIF

    ELSEIF (ipanel == 5) THEN
      IF (ii < 1) THEN
        pout = 4
        iout = jj
        jout = 1 - ii

      ELSEIF (ii > ncube-1) THEN
        pout = 2
        iout = ncube - jj
        jout = ii - (ncube - 1)

      ELSEIF (jj < 1) THEN
        pout = 3
        iout = ncube - ii
        jout = 1 - jj

      ELSEIF (jj > ncube-1) THEN
        pout = 1
        iout = ii
        jout = jj - (ncube - 1)
      ENDIF

    ELSEIF (ipanel == 6) THEN
      IF (ii < 1) THEN
        pout = 4
        iout = ncube - jj
        jout = ii + (ncube - 1)

      ELSEIF (ii > ncube-1) THEN
        pout = 2
        iout = jj
        jout = 2 * ncube - 1 - ii

      ELSEIF (jj < 1) THEN
        pout = 1
        iout = ii
        jout = jj + (ncube - 1)

      ELSEIF (jj > ncube-1) THEN
        pout = 3
        iout = ncube - ii
        jout = 2 * ncube - 1 - jj
      ENDIF

    ELSE
      WRITE (*,*) 'Fatal error: In CubedSphereRelCoord'
      WRITE (*,*) '  Invalid panel ',ipanel
      STOP
    ENDIF

    ! (DBG) Test output
    IF ((iout<1) .OR. (iout>ncube-1) .OR. (jout<1) .OR. (jout>ncube-1)) THEN
      WRITE (*,*) 'Fatal error: In CubedSphereRelCoord'
      WRITE (*,*) '  Invalid output coord (', iout, ',', jout, ')'
      STOP
    ENDIF

  END SUBROUTINE

!------------------------------------------------------------------------------
! SUBROUTINE CoordBGridToAGrid
!
! Description:
!   Convert B-grid relative coordinates (i,j,di,dj) to A-grid relative
!   coordinates (i,j,di,dj).
!
! Parameters:
!   ii (INOUT) - B-grid relative i coordinate (IN)
!                A-grid relative i coordinate (OUT)
!   jj (INOUT) - B-grid relative j coordinate (IN)
!                A-grid relative j coordinate (OUT)
!   di (INOUT) - B-grid i coordinate excess (IN)
!                A-grid i coordinate excess (OUT)
!   dj (INOUT) - B-grid j coordinate excess (IN)
!                A-grid j coordinate excess (OUT)
!   i_halfwidth - Halfwidth of the i-coordinate direction
!   j_halfwidth - Halfwidth of the j-coordinate direction
!------------------------------------------------------------------------------
  SUBROUTINE CoordBGridToAGrid(ii, jj, di, dj, i_halfwidth, j_halfwidth)

    IMPLICIT NONE

    INTEGER (KIND=int_kind), INTENT(INOUT) :: ii, jj
    REAL    (KIND=dbl_kind), INTENT(INOUT) :: di, dj
    REAL    (KIND=dbl_kind), INTENT(IN)    :: i_halfwidth, j_halfwidth

    ! Debugging check
    IF ((di < zero) .OR. (di > two * i_halfwidth)) THEN
      WRITE (*,*) 'Fatal error: In CoordBGridToAGrid'
      WRITE (*,*) 'di (',di,') out of range [',zero,',',i_halfwidth,']'
      STOP

    ELSEIF ((dj < zero) .OR. (dj > two * j_halfwidth)) THEN
      WRITE (*,*) 'Fatal error: In CoordBGridToAGrid'
      WRITE (*,*) 'dj (',dj,') out of range [',zero,',',j_halfwidth,']'
      STOP
    ENDIF

    ! Remap coordinates
    IF (di < i_halfwidth) THEN
      di = di + i_halfwidth
      ii = ii - 1
  
      IF (dj < j_halfwidth) THEN
        dj = dj + j_halfwidth
        jj = jj - 1
      ELSE
        dj = dj - j_halfwidth
      ENDIF
  
    ELSE
      di = di - i_halfwidth
  
      IF (dj < j_halfwidth) THEN
        dj = dj + j_halfwidth
        jj = jj - 1
      ELSE
        dj = dj - j_halfwidth
      ENDIF
    ENDIF

  END SUBROUTINE CoordBGridToAGrid

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

    IMPLICIT NONE

    INTEGER (KIND=int_kind), INTENT(IN)                           :: icube
    REAL (kind=dbl_kind), DIMENSION(icube-1,icube-1), INTENT(OUT) :: dA

    ! Local variables
    INTEGER (KIND=int_kind)                       :: k, k1, k2
    REAL (kind=dbl_kind)                          :: a1, a2, a3, a4
    REAL (kind=dbl_kind), DIMENSION(icube,icube)  :: ang
    REAL (kind=dbl_kind), DIMENSION(icube)        :: gp

!#ifdef DBG 
    REAL (KIND=dbl_kind)   :: dbg1 !DBG
!#endif
    
    ! Recall that we are using equi-angular spherical gridding
    DO k = 1, icube
       gp(k) = -piq + (pi/DBLE(2*(icube-1))) * DBLE(k-1)
    ENDDO

    DO k2=1,icube
       DO k1=1,icube
          ang(k1,k2) = EquiangularGridAngle(gp(k1), gp(k2))
       ENDDO
    ENDDO

    DO k2=1,icube-1
       DO k1=1,icube-1
          a1 =      ang(k1  , k2  )
          a2 = pi - ang(k1+1, k2  )
          a3 = pi - ang(k1  , k2+1)
          a4 =      ang(k1+1, k2+1)

          ! area = r*r*(-2*pi+sum(interior angles))
          DA(k1,k2) = -pi2+a1+a2+a3+a4
       ENDDO
    ENDDO

!#ifdef DBG 
    ! Only for debugging - test consistency
    dbg1 = zero                           !DBG
    DO k2=1,icube-1
       DO k1=1,icube-1
          dbg1 = dbg1 + DA(k1,k2)         !DBG
       ENDDO
    ENDDO
    write(*,*) 'DAcube consistency: ',dbg1-four*pi/six !DBG
!#endif
  END SUBROUTINE EquiangularAllAreas

!------------------------------------------------------------------------------
! SUBROUTINE EquiangularVecTransABtoEN
!
! Description:
!   Transform the given vector in (alpha, beta) form on given panel with origin
!   at (alpha, beta, ipanel) into a vector in (eastward, northward) form.
!
! Parameters:
!   alpha - Alpha coordinate of vector origin
!   beta - Beta coordinate of vector origin
!   ipanel - Panel coordinate of vector origin
!   Uin - Alpha velocity in
!   Vin - Beta velociy in
!   Uout (OUT) - Eastward velocity out
!   Vout (OUT) - Northward velocity out
!------------------------------------------------------------------------------
  SUBROUTINE EquiangularVecTransABtoEN( &
               alpha, beta, ipanel, Uin, Vin, Uout, Vout)

    IMPLICIT NONE

    REAL    (KIND=dbl_kind), INTENT(IN) :: alpha, beta
    INTEGER (KIND=int_kind), INTENT(IN) :: ipanel

    REAL (KIND=dbl_kind), INTENT(IN)  :: Uin, Vin
    REAL (KIND=dbl_kind), INTENT(OUT) :: Uout, Vout

    REAL (KIND=dbl_kind) :: f, invA, invB, lon, lat

    CALL CubedSphereRLLFromABP(alpha, beta, ipanel, lon, lat)

    invA = one / (COS(alpha)**2)
    invB = one / (COS(beta)**2)

    ! lon = 0 should be centered in middle of panel 4 for these transforms
    ! (CubedSphereRLLFromABP gives lon = 0 along cube edge)
    lon = lon - piq

    IF (ipanel < 5) THEN
      IF (ipanel == 2) THEN
        lon = lon - pih
      ELSEIF (ipanel == 3) THEN
        lon = lon - two * pih
      ELSEIF (ipanel == 4) THEN
        lon = lon - three * pih
      ENDIF

      f = COS(lat) * COS(lon)

      Uout = f * COS(lon) * invA * Uin
      Vout = - f * SIN(lon) * SIN(lat) * invA * Uin &
             + f * COS(lat) * invB * Vin

    ELSEIF (ipanel == 5) THEN
      f = SIN(lat)

      Uout = - f * COS(lon) * invA * Uin &
             + f * SIN(lon) * invB * Vin
      Vout = f * SIN(lat) * SIN(lon) * invA * Uin + &
             f * SIN(lat) * COS(lon) * invB * Vin

    ELSEIF (ipanel == 6) THEN
      f = SIN(lat)

      Uout = f * COS(lon) * invA * Uin + &
             f * SIN(lon) * invB * Vin
      Vout = - f * SIN(lon) * SIN(lat) * invA * Uin &
             + f * SIN(lat) * COS(lon) * invB * Vin

    ENDIF

  END SUBROUTINE EquiangularVecTransABtoEN

!------------------------------------------------------------------------------
! SUBROUTINE EquiangularVecTransENtoAB
!
! Description:
!   Transform the given vector in (eastward, northward) form with origin at
!   (alpha, beta, ipanel) into a vector in (alpha, beta) form on given panel.
!
! Parameters:
!   alpha - Alpha coordinate of vector origin
!   beta - Beta coordinate of vector origin
!   ipanel - Panel coordinate of vector origin
!   Uin - Eastward velocity in
!   Vin - Northward velocity in
!   Uout (OUT) - Alpha velocity out
!   Vout (OUT) - Beta velocity out
!------------------------------------------------------------------------------
  SUBROUTINE EquiangularVecTransENtoAB( &
               alpha, beta, ipanel, Uin, Vin, Uout, Vout)

    IMPLICIT NONE

    REAL    (KIND=dbl_kind), INTENT(IN) :: alpha, beta
    INTEGER (KIND=int_kind), INTENT(IN) :: ipanel

    REAL (KIND=dbl_kind), INTENT(IN)  :: Uin, Vin
    REAL (KIND=dbl_kind), INTENT(OUT) :: Uout, Vout

    REAL (KIND=dbl_kind) :: f, sqA, sqB, lon, lat

    CALL CubedSphereRLLFromABP(alpha, beta, ipanel, lon, lat)

    sqA = COS(alpha)**2
    sqB = COS(beta)**2

    ! lon = 0 should be centered in middle of panel 1 for these transforms
    ! (CubedSphereRLLFromABP gives lon = 0 along cube edge)
    lon = lon - piq

    IF (ipanel < 5) THEN
      IF (ipanel == 2) THEN
        lon = lon - pih
      ELSEIF (ipanel == 3) THEN
        lon = lon - two * pih
      ELSEIF (ipanel == 4) THEN
        lon = lon - three * pih
      ENDIF

      f = one / (COS(lon) * COS(lat))

      Uout = f / COS(lon) * sqA * Uin
      Vout = f * TAN(lat) * TAN(lon) * sqB * Uin + &
             f / COS(lat) * sqB * Vin

    ELSEIF (ipanel == 5) THEN

      f = one / (SIN(lat)**2)

      Uout = - f * SIN(lat) * COS(lon) * sqA * Uin &
             + f * SIN(lon) * sqA * Vin
      Vout = + f * SIN(lat) * SIN(lon) * sqB * Uin &
             + f * COS(lon) * sqB * Vin

    ELSEIF (ipanel == 6) THEN

      f = one / (SIN(lat)**2)

      Uout = + f * SIN(lat) * COS(lon) * sqA * Uin &
             - f * SIN(lon) * sqA * Vin
      Vout = + f * SIN(lat) * SIN(lon) * sqB * Uin &
             + f * COS(lon) * sqB * Vin

    ELSE
      WRITE (*,*) 'Fatal Error: In EquiangularVecTransENtoAB'
      WRITE (*,*) 'Specified panel out of range, given: ', ipanel
      STOP
    ENDIF

  END SUBROUTINE EquiangularVecTransENtoAB



!------------------------------------------------------------------------------
! FUNCTION IntegralSqrtG
!
! Description:
!   Compute and return the integrated square root of the metric tensor for an 
!   equiangular cubed sphere grid over the specified grid region.
!
! Parameters: 
!   alpha1 - 
!   alpha2 -
!   beta1 -
!   beta2 -
!   nod -
!   gl -
!   gw -
!------------------------------------------------------------------------------
  REAL(KIND=dbl_kind) FUNCTION IntegralSqrtG(alpha1, alpha2, beta1, beta2, &
                                             nod, gl, gw)
    IMPLICIT NONE

    REAL (KIND=dbl_kind), INTENT(IN)    :: alpha1, alpha2, beta1, beta2
    INTEGER (KIND=int_kind), INTENT(IN) :: nod

    REAL (KIND=dbl_kind), DIMENSION(nod+1), INTENT(IN) :: gl, gw 

    ! Local variables
    REAL (KIND=dbl_kind), DIMENSION(nod+1) :: a, b
    REAL (KIND=dbl_kind)                   :: s1, s2, da, db
    INTEGER (KIND=int_kind)                :: i, j

    da = alpha2 - alpha1
    db = beta2 - beta1

    DO i = 1, nod+1
       a(i) = (alpha1 + alpha2 + da*gl(i)) / two
    ENDDO

    DO j = 1, nod+1
       b(j) = (beta1 + beta2 + db*gl(j)) / two
    ENDDO

    IntegralSqrtG = zero
    DO j=1,nod+1
       DO i=1,nod+1
          s1 = s1 + EquiangularSqrtG(a(i),b(j)) * gw(i)
       ENDDO
       s2 = s2 + s1 * gw(j)
    ENDDO
    IntegralSqrtG = s2 * quart * da * db
  END FUNCTION IntegralSqrtG

!------------------------------------------------------------------------------
! FUNCTION EquiangularSqrtG
!
! Description:
!   Evaluate the square root of the metric tensor for an equiangular
!   projection at a single point.
!
! Parameters: 
!   alpha - Alpha coordinate of evaluation point
!   beta - Beta coordinate of evaluation point
!------------------------------------------------------------------------------
  REAL(KIND=dbl_kind) FUNCTION EquiangularSqrtG(alpha, beta)

    IMPLICIT NONE

    REAL (KIND=dbl_kind), INTENT(IN) :: alpha, beta
    REAL (KIND=dbl_kind)             :: r2, ta, tb, cab, rr

    rr  = one ! Radius of sphere
    cab = COS(alpha) * COS(alpha) * COS(beta) * COS(beta)
    ta  = TAN(alpha) * TAN(alpha)
    tb  = TAN(beta ) * TAN(beta )
    r2  = one + ta + tb

    ! Square root of G-determinent  [sqrt(|g_ij|) = g = R^2/(rho^3*cab)]
    EquiangularSqrtG =  rr * rr / (cab * (r2**onehalf))

  END FUNCTION EquiangularSqrtG

!------------------------------------------------------------------------------
! FUNCTION EquiangularGridAngle
!
! Description:
!   Compute the angle between equiangular cubed sphere projection grid lines.
!
! Parameters: 
!   alpha - Alpha coordinate of evaluation point
!   beta - Beta coordinate of evaluation point
!------------------------------------------------------------------------------
  REAL(KIND=dbl_kind) FUNCTION EquiangularGridAngle(alpha, beta)
    IMPLICIT NONE
    REAL (kind=dbl_kind) :: alpha, beta
    EquiangularGridAngle = ACOS(-SIN(alpha) * SIN(beta))
  END FUNCTION EquiangularGridAngle

!------------------------------------------------------------------------------
! SUBROUTINE EquiangularElementArea
!
! Description:
!   Compute the area of a single equiangular cubed sphere grid cell.
!
! Parameters: 
!   alpha - Alpha coordinate of lower-left corner of grid cell
!   da - Delta alpha
!   beta - Beta coordinate of lower-left corner of grid cell
!   db - Delta beta
!------------------------------------------------------------------------------
  REAL(KIND=dbl_kind) FUNCTION EquiangularElementArea(alpha, da, beta, db)

    IMPLICIT NONE

!    REAL (kind=dbl_kind) :: EquiangularElementArea
    REAL (kind=dbl_kind) :: alpha, da, beta, db
    REAL (kind=dbl_kind) :: a1, a2, a3, a4

    ! Calculate interior grid angles
    a1 =      EquiangularGridAngle(alpha   , beta   )
    a2 = pi - EquiangularGridAngle(alpha+da, beta   )
    a3 = pi - EquiangularGridAngle(alpha   , beta+db)
    a4 =      EquiangularGridAngle(alpha+da, beta+db)

    ! Area = r*r*(-2*pi+sum(interior angles))
    EquiangularElementArea = -pi2 + a1 + a2 + a3 + a4

  END FUNCTION EquiangularElementArea

!------------------------------------------------------------------------------
! SUBROUTINE ZBRENT
!
! Description:
!   Apply Brent's 1D root finding method for solving F(X) = Y for X.
!
! Parameters: 
!   FUNC - Input function F(X)
!   X1 - Lower bound on location of root
!   X2 - Upper bound on location of root
!   TOL - Tolerance bound on root
!   Y - See description
!------------------------------------------------------------------------------
  REAL (KIND=dbl_kind) function ZBRENT(FUNC, X1, X2, TOL,X0,Y0)
    
    INTEGER (KIND=int_kind), PARAMETER :: ITMAX=100
    REAL (KIND=dbl_kind)  , PARAMETER  :: EPS=tiny
    REAL (KIND=dbl_kind)  , EXTERNAL   :: FUNC
!    REAL (KIND=dbl_kind)               :: Y
    REAL (KIND=dbl_kind), INTENT(IN) :: x1, x2, tol,X0,Y0
    
    REAL (KIND=dbl_kind) :: a, b, fa, fb
    integer :: iter
    REAL (KIND=dbl_kind) :: c, fc,d, e
    REAL (KIND=dbl_kind) :: s, p, q, r
    REAL (KIND=dbl_kind) :: tol1, xm
    
    A=X1
    B=X2
    FA=FUNC(A,X0,Y0)
    FB=FUNC(B,X0,Y0)
    IF(FB*FA.GT.zero) then
!       print*, 'Root must be bracketed for ZBRENT.'
!       stop
      IF (X0<half*(X2-X1)) THEN
        ZBRENT = X1
      ELSE
        ZBRENT = X2
      END IF
    endif
    FC=FB
    DO  ITER=1,ITMAX
       IF(FB*FC.GT.zero) THEN
          C=A
          FC=FA
          D=B-A
          E=D
       ENDIF
       IF(ABS(FC).LT.ABS(FB)) THEN
          A=B
          B=C
          C=A
          FA=FB
          FB=FC
          FC=FA
       ENDIF
       TOL1=two*EPS*ABS(B)+half*TOL
       XM=half*(C-B)
       IF(ABS(XM).LE.TOL1 .OR. FB.EQ.zero)THEN
          ZBRENT=B
          RETURN
       ENDIF
       IF(ABS(E).GE.TOL1 .AND. ABS(FA).GT.ABS(FB)) THEN
          S=FB/FA
          IF(A.EQ.C) THEN
             P=two*XM*S
             Q=one-S
          ELSE
             Q=FA/FC
             R=FB/FC
             P=S*(two*XM*Q*(Q-R)-(B-A)*(R-one))
             Q=(Q-one)*(R-one)*(S-one)
          ENDIF
          IF(P.GT.zero) Q=-Q
          P=ABS(P)
          IF(two*P .LT. MIN(three*XM*Q-ABS(TOL1*Q),ABS(E*Q))) THEN
             E=D
             D=P/Q
          ELSE
             D=XM
             E=D
          ENDIF
       ELSE
          D=XM
          E=D
       ENDIF
       A=B
       FA=FB
       IF(ABS(D) .GT. TOL1) THEN
          B=B+D
       ELSE
          B=B+SIGN(TOL1,XM)
       ENDIF
       FB=FUNC(B,X0,Y0)
    enddo
    print*, 'ZBRENT exceeding maximum iterations.'
    stop
    RETURN
  END FUNCTION ZBRENT

!------------------------------------------------------------------------------
  !
  !******************************
  !
  ! fill halo zone for panel np
  !
  !******************************
  !
  SUBROUTINE fill_halo(parg,zarg,np,ncube,nhalo)
    IMPLICIT NONE
    REAL (KIND=dbl_kind), DIMENSION(ncube-1,ncube-1,6)                            , INTENT(IN)  :: parg
    REAL (KIND=dbl_kind), DIMENSION(1-nhalo:ncube-1+nhalo,1-nhalo:ncube-1+nhalo,6), INTENT(INOUT) :: zarg
    INTEGER (KIND=int_kind)                                                   , INTENT(IN)  :: np, ncube,nhalo
    !
    ! local workspace
    !
    INTEGER (KIND=int_kind)               :: jh,jhy    
    !
    zarg(:,:,np) = zero !DBG
    zarg(1:ncube-1,1:ncube-1,np) = parg(1:ncube-1,1:ncube-1,np)

    IF (np==1) THEN
       DO jh=1,nhalo
          zarg(ncube-1+jh,1:ncube-1 ,1) = parg(jh        ,1:ncube-1   ,2)  !exchange right
          zarg(1-jh    ,1:ncube-1 ,1) = parg(ncube-jh,1:ncube-1   ,4)  !exchange left
          zarg(1:ncube-1 ,1-jh    ,1) = parg(1:ncube-1   ,ncube-jh,5)  !exchange below
          zarg(1:ncube-1 ,ncube-1+jh,1) = parg(1:ncube-1   ,jh        ,6)  !exchange over
       ENDDO
    ELSE IF (np==2) THEN
       DO jh=1,nhalo
          zarg(1-jh    ,1:ncube-1 ,2) = parg(ncube-jh,1:ncube-1   ,1)  !exchange left  
          zarg(ncube-1+jh,1:ncube-1 ,2) = parg(jh        ,1:ncube-1   ,3)  !exchange right
          zarg(1:ncube-1 ,1-jh    ,2) = parg(ncube-jh,ncube-1:1:-1,5)  !exchange below
          zarg(1:ncube-1 ,ncube-1+jh,2) = parg(ncube-jh,1:ncube-1   ,6)  !exchange over
       ENDDO
    ELSE IF (np==3) THEN
       DO jh=1,nhalo
          zarg(ncube-1+jh,1:ncube-1 ,3) = parg(jh        ,1:ncube-1   ,4)  !exchange right
          zarg(1-jh    ,1:ncube-1 ,3) = parg(ncube-jh,1:ncube-1   ,2)  !exchange left
          zarg(1:ncube-1 ,1-jh    ,3) = parg(ncube-1:1:-1,jh        ,5)  !exchange below
          zarg(1:ncube-1 ,ncube-1+jh,3) = parg(ncube-1:1:-1,ncube-jh,6)  !exchange over
       ENDDO
    ELSE IF (np==4) THEN
       DO jh=1,nhalo
          zarg(1-jh    ,1:ncube-1 ,4) = parg(ncube-jh,1:ncube-1   ,3) !exchange left  
          zarg(ncube-1+jh,1:ncube-1 ,4) = parg(jh        ,1:ncube-1   ,1) !exchange right
          zarg(1:ncube-1 ,1-jh    ,4) = parg(jh        ,1:ncube-1   ,5) !exchange below
          zarg(1:ncube-1 ,ncube-1+jh,4) = parg(jh        ,ncube-1:1:-1,6) !exchange over
       ENDDO
       !
       ! bottom panel
       !
    ELSE IF (np==5) THEN
       DO jh=1,nhalo
          zarg(1-jh    ,1:ncube-1 ,5) = parg(1:ncube-1   ,jh        ,4) !exchange left  
          zarg(ncube-1+jh,1:ncube-1 ,5) = parg(ncube-1:1:-1,jh        ,2) !exchange right
          zarg(1:ncube-1 ,1-jh    ,5) = parg(ncube-1:1:-1,jh        ,3) !exchange below
          zarg(1:ncube-1 ,ncube-1+jh,5) = parg(1:ncube-1   ,jh        ,1) !exchange over
       ENDDO
       !
       ! top panel
       !
    ELSE IF (np==6) THEN
       DO jh=1,nhalo
          zarg(1-jh    ,1:ncube-1 ,6) = parg(ncube-1:1:-1,ncube-jh,4) !exchange left  
          zarg(ncube-1+jh,1:ncube-1 ,6) = parg(1:ncube-1   ,ncube-jh,2) !exchange right
          zarg(1:ncube-1 ,1-jh    ,6) = parg(1:ncube-1   ,ncube-jh,1) !exchange below
          zarg(1:ncube-1 ,ncube-1+jh,6) = parg(ncube-1:1:-1,ncube-jh,3) !exchange over
       ENDDO
    ELSE
       STOP
    ENDIF
  END SUBROUTINE fill_halo

  !Note that we need not be concerned with the shared edge fluxes; the only 
  !ones that need to be copied over are in the halos proper. Also, be sure 
  !to call this AFTER calling collect_boundary_fluxform.
  SUBROUTINE fill_halo_fluxes(xflux, yflux, np, ncube, nhalo, ncfl)

    IMPLICIT NONE
    REAL (KIND=dbl_kind), INTENT(INOUT), &
         DIMENSION(1-nhalo:ncube+nhalo,1-nhalo:ncube+nhalo,6) :: xflux, yflux
    INTEGER (KIND=int_kind), INTENT(IN)  :: np, ncube, nhalo, ncfl

    INTEGER (KIND=int_kind) :: jh

    SELECT CASE (np)
    CASE (1)
       DO jh=2,nhalo+1
          xflux(ncube-1+jh,1:ncube-1 ,1) = xflux(jh        ,1:ncube-1 ,2)
          xflux(2-jh      ,1:ncube-1 ,1) = xflux(ncube-jh+1,1:ncube-1 ,4)
          xflux(1:ncube   ,2-jh      ,1) = xflux(1:ncube   ,ncube-jh+1,5)
          xflux(1:ncube   ,ncube-2+jh,1) = xflux(1:ncube   ,jh-1      ,6)

          yflux(ncube-2+jh,1:ncube   ,1) = yflux(jh-1      ,1:ncube   ,2)
          yflux(2-jh      ,1:ncube   ,1) = yflux(ncube-jh+1,1:ncube   ,4)
          yflux(1:ncube-1 ,2-jh      ,1) = yflux(1:ncube-1 ,ncube-jh+1,5)
          yflux(1:ncube-1 ,ncube-1+jh,1) = yflux(1:ncube-1 ,jh        ,6)
       ENDDO
    CASE (2)
       DO jh=2,nhalo+1
          xflux(2-jh        ,1:ncube-1 ,2) =  xflux(ncube-jh+1,1:ncube-1,1)
          xflux(ncube-1+jh  ,1:ncube-1 ,2) =  xflux(jh        ,1:ncube-1,3)
          yflux(ncube-1:1:-1,2-jh      ,2) =  xflux(ncube-jh  ,1:ncube-1,5)
          yflux(1:ncube-1   ,ncube-1+jh,2) = -xflux(ncube-jh  ,1:ncube-1,6)

          yflux(2-jh        ,1:ncube   ,2) =  yflux(ncube-jh+1,1:ncube,1)
          yflux(ncube-2+jh  ,1:ncube   ,2) =  yflux(jh-1      ,1:ncube,3)
          xflux(ncube:1:-1  ,2-jh      ,2) = -yflux(ncube-jh+1,1:ncube,5)
          xflux(1:ncube     ,ncube-2+jh,2) =  yflux(ncube-jh+1,1:ncube,6)
       ENDDO
    CASE (3)
       DO jh=2,nhalo+1
          xflux(2-jh      ,1:ncube-1 ,3) =  xflux(ncube-jh+1,1:ncube-1 ,2)
          xflux(ncube-1+jh,1:ncube-1 ,3) =  xflux(jh        ,1:ncube-1 ,4)
          xflux(ncube:1:-1,2-jh      ,3) = -xflux(1:ncube   ,jh-1      ,5)
          xflux(ncube:1:-1,ncube-2+jh,3) = -xflux(1:ncube   ,ncube-jh+1,6)

          yflux(2-jh        ,1:ncube   ,3) =  yflux(ncube-jh+1,1:ncube   ,2)
          yflux(ncube-2+jh  ,1:ncube   ,3) =  yflux(jh-1      ,1:ncube   ,4)
          yflux(ncube-1:1:-1,2-jh      ,3) = -yflux(1:ncube-1 ,jh        ,5)
          yflux(ncube-1:1:-1,ncube-1+jh,3) = -yflux(1:ncube-1 ,ncube-jh+1,6)
       ENDDO
    CASE (4)
       DO jh=2,nhalo+1
          xflux(ncube-1+jh  ,1:ncube-1 ,4) =  xflux(jh        ,1:ncube-1 ,1)
          xflux(2-jh        ,1:ncube-1 ,4) =  xflux(ncube-jh+1,1:ncube-1 ,3)
          yflux(1:ncube-1   ,2-jh      ,4) = -xflux(jh        ,1:ncube-1 ,5)
          yflux(ncube-1:1:-1,ncube-1+jh,4) =  xflux(jh        ,1:ncube-1 ,6)

          yflux(ncube-2+jh,1:ncube   ,4) =  yflux(jh-1      ,1:ncube   ,1)
          yflux(2-jh      ,1:ncube   ,4) =  yflux(ncube-jh+1,1:ncube   ,3)
          xflux(1:ncube   ,2-jh      ,4) =  yflux(jh-1      ,1:ncube   ,5)
          xflux(ncube:1:-1,ncube-2+jh,4) = -yflux(jh-1      ,1:ncube   ,6)
       ENDDO
    CASE (5)
       DO jh=2,nhalo+1
          xflux(1:ncube   ,ncube-2+jh,5) =  xflux(1:ncube  ,jh-1,1)
          yflux(ncube-2+jh,ncube:1:-1,5) = -xflux(1:ncube  ,jh-1,2)
          xflux(ncube:1:-1,2-jh      ,5) = -xflux(1:ncube  ,jh-1,3)
          yflux(2-jh      ,1:ncube   ,5) =  xflux(1:ncube  ,jh-1,4)

          yflux(1:ncube-1   ,ncube-1+jh,5) = yflux(1:ncube-1   ,jh       ,1)
          xflux(ncube-1+jh  ,ncube-1:1:-1,5) = yflux(1:ncube-1   ,jh        ,2)
          yflux(ncube-1:1:-1,2-jh    ,5) = -yflux(1:ncube-1   ,jh        ,3)
          xflux(2-jh        ,1:ncube-1   ,5) = -yflux(1:ncube-1   ,jh        ,4)
       ENDDO
    CASE (6)
       DO jh=2,nhalo+1
          xflux(1:ncube ,2-jh    ,6) = xflux(1:ncube   ,ncube-jh+1,1) 
          yflux(ncube-2+jh,1:ncube,6) = xflux(1:ncube   ,ncube-jh+1,2)
          xflux(ncube:1:-1,ncube-2+jh ,6) = -xflux(1:ncube  ,ncube-jh+1   ,3)
          yflux(2-jh    ,ncube:1:-1,6) = -xflux(1:ncube   ,ncube-jh+1,4)

          yflux(1:ncube-1 ,2-jh    ,6) = yflux(1:ncube-1   ,ncube-jh+1,1)
          xflux(ncube-1+jh,1:ncube-1   ,6)= -yflux(1:ncube-1   ,ncube-jh+1,2)
          yflux(ncube-1:1:-1,ncube-1+jh,6) = -yflux(1:ncube-1   ,ncube-jh+1,3)
          xflux(2-jh    ,ncube-1:1:-1,6) = yflux(1:ncube-1   ,ncube-jh+1,4)
       ENDDO
    CASE DEFAULT
       WRITE(*,*) " IN fill_halo_fluxes: np out of the range 1:6. Stop."
       STOP 201
    END SELECT

  END SUBROUTINE fill_halo_fluxes

  !
  ! Convert from radians to central angle coordinates (alpha,beta)
  !
  SUBROUTINE rad_to_ab(klon,klat,lon_rad,lat_rad,alpha,beta,nface)
    IMPLICIT NONE

!-----------------------------------------------------------------------
!
!     input variables
!
!-----------------------------------------------------------------------

    INTEGER (KIND=int_kind)                    :: klon,klat,nface
    REAL (KIND=dbl_kind), DIMENSION(KLON,KLAT) :: lon_rad,lat_rad,alpha,beta
    
    INTENT(IN)   :: klon,klat,nface,lon_rad,lat_rad
    INTENT (OUT) :: alpha,beta
    
!-----------------------------------------------------------------------
!
!     local variables
!
!-----------------------------------------------------------------------
  
    INTEGER (KIND=int_kind) :: i,j

    WRITE(*,*) "this subroutine has problems with near pole points"
    WRITE(*,*) "use CubedSphereABPFromRLL_version2 instead!"
    STOP


    SELECT CASE (nface)
    CASE (1)      
      alpha = lon_rad-piq-rotate_cube
      DO j=1,klat
        DO i=1,klon
          IF (alpha(i,j)>pi) alpha(i,j)=alpha(i,j)-pi2
        ENDDO
      ENDDO
      beta  = ATAN(TAN(lat_rad)/COS(lon_rad-piq-rotate_cube))
    CASE(2)
      alpha = lon_rad-pih-piq-rotate_cube
      beta  = ATAN(TAN(lat_rad)/COS(lon_rad-pih-piq-rotate_cube))
    CASE(3)
      alpha = lon_rad-pi-piq-rotate_cube
      beta  = ATAN(TAN(lat_rad)/COS(lon_rad-pi-piq-rotate_cube))
    CASE(4)
      alpha = lon_rad-three*pih-piq-rotate_cube
      DO j=1,klat
        DO i=1,klon
          IF (alpha(i,j)<-pi) alpha(i,j)=alpha(i,j)+pi2
        ENDDO
      ENDDO
      beta  = ATAN(TAN(lat_rad)/COS(lon_rad-half*three*pi-piq-rotate_cube))
    CASE(5)
      DO j=1,klat
        DO i=1,klon
          IF (ABS(lat_rad(i,j)+pih).LE.tiny) THEN
            alpha(i,j) = zero
            beta(i,j)  = zero
          ELSE
            alpha(i,j) = ATAN(-SIN(lon_rad(i,j)-piq-rotate_cube)/TAN(lat_rad(i,j)))
            beta(i,j)  = ATAN(-COS(lon_rad(i,j)-piq-rotate_cube)/TAN(lat_rad(i,j)))
          ENDIF
        END DO
      END DO
    CASE(6)
      DO j=1,klat
        DO i=1,klon
          IF (ABS(lat_rad(i,j)-pih).LE.tiny) THEN
            alpha(i,j) = zero
            beta(i,j)  = zero
          ELSE
!            alpha(i,j) = ATAN( SIN(lon_rad(i,j)-piq-rotate_cube)/TAN(lat_rad(i,j)))
!            beta(i,j)  = ATAN(-COS(lon_rad(i,j)-piq-rotate_cube)/TAN(lat_rad(i,j)))
            alpha(i,j) = ATAN2(TAN(lat_rad(i,j)),SIN(lon_rad(i,j)-piq-rotate_cube))
            beta(i,j)  = ATAN2(TAN(lat_rad(i,j)),-COS(lon_rad(i,j)-piq-rotate_cube))
          END IF
        END DO
      END DO
    CASE DEFAULT
      WRITE(*,*) "nface out of rangfe on rad_to_ab"
      STOP
    END SELECT
  END SUBROUTINE rad_to_ab

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
  SUBROUTINE CubedSphereABPFromRLL_version2(lon, lat, alpha, beta, ipanel)

    IMPLICIT NONE

    REAL    (KIND=dbl_kind), INTENT(IN)  :: lon, lat
    REAL    (KIND=dbl_kind), INTENT(OUT) :: alpha, beta
    INTEGER (KIND=int_kind), INTENT(IN)  :: ipanel

    ! Local variables
    REAL    (KIND=dbl_kind) :: xx, yy, zz, pm
    REAL    (KIND=dbl_kind) :: sx, sy, sz
    INTEGER (KIND=int_kind) :: ix, iy, iz

    ! Translate to (x,y,z) space
    xx = COS(lon - piq-rotate_cube) * COS(lat)
    yy = SIN(lon - piq-rotate_cube) * COS(lat)
    zz = SIN(lat)


    SELECT CASE (ipanel)
    CASE (1)      
      sx = yy; sy = zz; sz = xx
    CASE(2)
      sx = -xx; sy = zz; sz = yy        
    CASE(3)
      sx = -yy; sy = zz; sz = -xx
    CASE(4)
      sx = xx; sy = zz; sz = -yy      
    CASE(5)
      sx = yy; sy = xx; sz = -zz
    CASE(6)
      sx = yy; sy = -xx; sz = zz
    CASE DEFAULT
      WRITE(*,*) "ipanel out of range"
      STOP
    END SELECT
    ! Use panel information to calculate (alpha, beta) coords
    alpha = ATAN(sx / sz)
    beta = ATAN(sy / sz)

  END SUBROUTINE CubedSphereABPFromRLL_version2
END MODULE cube_trfs

