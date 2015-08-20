!!#define REMOVEMEX
!-----------------------------------------------------------------------------
! MODULE subgrid_orientation
!
! Purpose:
!
!      Date       Programmer       Affiliation          Description of change
!      ====       ==========       ===========          =====================
!
!-----------------------------------------------------------------------------
#define WRITERESULTS
MODULE subgrid_topo_ana
  USE reconstruct
  !USE ridge_ana

IMPLICIT NONE
PRIVATE

PUBLIC smooth_intermediate_topo
CONTAINS
  SUBROUTINE smooth_intermediate_topo(terr, ncube,nhalo, NSCL_f,NSCL_c )

    REAL (KIND=dbl_kind), PARAMETER :: pi        = 3.14159265358979323846264338327

    INTEGER (KIND=int_kind), INTENT(IN) :: ncube, nhalo,NSCL_f,NSCL_c

    REAL (KIND=dbl_kind), &
            DIMENSION(ncube,ncube,6), INTENT(INOUT) :: terr
#if 0
    REAL (KIND=dbl_kind), &
            DIMENSION(ncube,ncube,6), INTENT(INOUT) :: terr_bp
    REAL (KIND=dbl_kind), &
            DIMENSION(ncube,ncube,6), INTENT(INOUT) :: terr_sm
#endif

    !-----------------------------------------------------------------
    !PRIMARY Outputs
    !-----------------------------------------------------------------
    !------------------------------

    REAL (KIND=dbl_kind),                                            &
         DIMENSION(1-nhalo:ncube+nhalo, 1-nhalo:ncube+nhalo, 6) :: terr_halo
    REAL  ,                                                          &
         DIMENSION(1-nhalo:ncube+nhalo, 1-nhalo:ncube+nhalo, 6) :: terr_halo_r4
    REAL  ,                                                          &
         DIMENSION(1-nhalo:ncube+nhalo, 1-nhalo:ncube+nhalo, 6) :: terr_halo_r4_sm
    REAL  ,                                                          &
         DIMENSION(1-nhalo:ncube+nhalo, 1-nhalo:ncube+nhalo, 6) :: terr_halo_r4_rw
    REAL  ,                                                          &
         DIMENSION(1-nhalo:ncube+nhalo, 1-nhalo:ncube+nhalo, 6) :: terr_halo_r4_fx
    REAL  ,                                                          &
         DIMENSION(1-nhalo:ncube+nhalo, 1-nhalo:ncube+nhalo)    :: smwt,ggaa,ggbb,ggab
    REAL  ,                                                          &
         DIMENSION(1-nhalo:ncube+nhalo )                          :: xv,yv,alph,beta

    INTEGER (KIND=int_kind):: np,i,j, ncube_halo,norx,nory,ipanel,x0,&
         x1,y0,y1,initd,ii0,ii1,jj0,jj1,nctest,NSM,NS2,ismi,NSB


    REAL, allocatable ::  wt1p(:,:)
    REAL(KIND=dbl_kind)  :: cosll, dx, dy ,dbet,dalp,diss,diss00

    INTEGER :: NOCTV , isx0, isx1, jsy0, jsy1,i2,j2,iix,jjx,i00

    REAL :: RSM_scl, smoo,irho


write(*,*) " NCUBE !!! " , ncube


                                       
    DO np = 1, 6
     CALL CubedSphereFillHalo_Linear_extended(terr, terr_halo(:,:,np), np, ncube+1,nhalo)  
    END DO

    ncube_halo = size( terr_halo(:,:,1), 1 )



    DO i=1-nhalo,ncube+nhalo
       xv(i)=1.*i
       yv(i)=1.*i
    END DO
    DO i=1-nhalo,ncube+nhalo
       alph(i)=(pi/4.)*(1.*i - 0.5 + nhalo - (ncube+2.*nhalo)/2.) / ((ncube+2.*nhalo)/2.)
       beta(i)=(pi/4.)*(1.*i - 0.5 + nhalo - (ncube+2.*nhalo)/2.) / ((ncube+2.*nhalo)/2.)
    END DO
    DO j=1-nhalo,ncube+nhalo
    DO i=1-nhalo,ncube+nhalo
       irho = ( 1. + (tan(alph(i))**2) + (tan(beta(j))**2 ) )**2   
       irho = 1. / ( ( cos(alph(i))**2 ) * (cos(beta(j))**2) * irho )  
       !irho = 1./ ( ( cos(alph(i))**2)*(cos(beta(j))**2)* ( ( 1. + (tan(alph(i))**2) + (tan(beta(j))**2 ) )**2  ))   
       ggaa(i,j) = irho * ( 1. + ( tan( alph(i) ) )**2 )
       ggbb(i,j) = irho * ( 1. + ( tan( beta(j) ) )**2 )
       ggab(i,j) = -irho *( tan( beta(j) ) ) * ( tan( alph(i) ) )
    END DO
    END DO


    terr_halo_r4 = terr_halo
    terr_halo_r4_sm = terr_halo_r4
    terr_halo_r4_fx = terr_halo_r4
    terr_halo_r4_rw = terr_halo_r4


 ! SINGLE PASS SMOOTHING


      if (NSCL_f > 1 ) then
      NSM=NSCL_f
      NS2=NSM/2
      allocate( wt1p(-ns2:ns2, -ns2:ns2 ) )
      DO j=-ns2,ns2
      DO i=-ns2,ns2
         wt1p(i,j)=1.0 - sqrt( 1.*i**2 + 1.*j**2 )/(1.*ns2)
      END DO
      END DO
      where (wt1p < 0.)
         wt1p = 0.
      end where

      i00 = ncube/2
      dalp   = alph(i00+ns2)-alph(i00)
      diss00 = 1./sqrt(  ggaa(i00,i00)*dalp*dalp )

      DO np=1,6
        DO j=1-nhalo+ns2,ncube+nhalo-ns2
        DO i=1-nhalo+ns2,ncube+nhalo-ns2

           do j2=-ns2,ns2
           do i2=-ns2,ns2
              jjx = j+j2
              iix = i+i2
              dalp = alph(iix)-alph(i)
              dbet = beta(jjx)-beta(j)
              diss = ggaa(i,j)*dalp*dalp + ggbb(i,j)*dbet*dbet + 2.*ggab(i,j)*dalp*dbet
              wt1p(i2,j2) = 1. - diss00 * sqrt( diss )
           end do
           end do
           where (wt1p < 0.)
             wt1p = 0.
           end where


           terr_halo_r4_sm( i, j, np ) = sum( wt1p*terr_halo_r4(i-ns2:i+ns2,j-ns2:j+ns2, np ) )/(sum(wt1p)) 

        END DO
        END DO
      END DO

             deallocate( wt1p )


#ifdef NOZEROCN
      where( terr_halo_r4 <= 0. )
           terr_halo_r4_sm = terr_halo_r4
      endwhere
#endif
      else
       write(*,*)" No fine scale smoother "
       terr_halo_r4_sm  = terr_halo_r4
      endif


      ! store off partially-smoothed data at inner scale
      terr_halo_r4_fx = terr_halo_r4_sm
      ! continue smoothing to outer scale


      NSM=NSCL_c
      NS2=NSM/2
      allocate( wt1p(-ns2:ns2, -ns2:ns2 ) )
      DO j=-ns2,ns2
      DO i=-ns2,ns2
         wt1p(i,j)=1.0 - sqrt( 1.*i**2 + 1.*j**2 )/(1.*ns2)
      END DO
      END DO
      where (wt1p < 0.)
         wt1p = 0.
      end where

      i00 = ncube/2
      dalp   = alph(i00+ns2)-alph(i00)
      diss00 = 1./sqrt(  ggaa(i00,i00)*dalp*dalp )

      DO np=1,6
        DO j=1-nhalo+ns2,ncube+nhalo-ns2
        DO i=1-nhalo+ns2,ncube+nhalo-ns2


           do j2=-ns2,ns2
           do i2=-ns2,ns2
              jjx = j+j2
              iix = i+i2
              dalp = alph(iix)-alph(i)
              dbet = beta(jjx)-beta(j)
              diss = ggaa(i,j)*dalp*dalp + ggbb(i,j)*dbet*dbet + 2.*ggab(i,j)*dalp*dbet
              wt1p(i2,j2) = 1. - diss00 * sqrt( diss )
           end do
           end do
           where (wt1p < 0.)
             wt1p = 0.
           end where

                if ((i==0).and.(j==0).and.(np==6)) then

                     write(711) size(wt1p,1),size(wt1p,2)
                     write(711) alph(i-ns2:i+ns2),beta(j-ns2:j+ns2),wt1p
                            write(*,*) " wrote wt1p "   

                endif
                if ((i==i00).and.(j==i00).and.(np==6)) then

                     write(711) size(wt1p,1),size(wt1p,2)
                     write(711) alph(i-ns2:i+ns2),beta(j-ns2:j+ns2),wt1p
                            write(*,*) " wrote wt1p "   

                endif

           terr_halo_r4_sm( i, j, np ) = sum( wt1p * terr_halo_r4(i-ns2:i+ns2,j-ns2:j+ns2, np ) )/(sum(wt1p) ) 

        END DO
                if (mod(j,100) ==0 ) write(*,*) "One pass Crs Sm J = ",J, " Panel=",np
        END DO
      END DO


write(711) size(terr_halo_r4,1), size(terr_halo_r4,2) ,size(terr_halo_r4,3)
write(711) ggaa,ggbb,ggab,alph,beta
WRITE(711) terr_halo_r4
WRITE(711) terr_halo_r4_fx
WRITE(711) terr_halo_r4_sm

WRITE(711) terr_halo
write(711) size(terr,1), size(terr,2) ,size(terr,3)
write(711) terr

close(711)

#ifdef NOZEROCN
      where( terr_halo_r4 <= 0. )
           terr_halo_r4_sm=terr_halo_r4
      endwhere
#endif

    terr_halo_r4 =terr_halo_r4_sm

    terr_halo_r4 = terr_halo_r4_fx - terr_halo_r4_sm

#if 0
  do np=1,6
    terr_bp(1:ncube,1:ncube,np) = DBLE ( terr_halo_r4(1:ncube,1:ncube,np ) )
    terr_sm(1:ncube,1:ncube,np) = DBLE ( terr_halo_r4_sm(1:ncube,1:ncube,np ) )
  end do
#endif 


END SUBROUTINE smooth_intermediate_topo


END MODULE subgrid_topo_ana
