!
! qqqq: ARH+JTB: merge notes
!
! SUBROUTINE prolong2 does not exist in Julio's repo
! SUBROUTINE smooth_intermediate_topo_wrap does not exist in Julio's repo
! SUBROUTINE restrict2 does not exist in Julio's repo
! SUBROUTINE restrict2_consrv does not exist in Julio's repo
!
! SUBROUTINE smooth_intermediate_topo is significantly different in Julio's repo
!


!-----------------------------------------------------------------------------
! MODULE subgrid_topo_ana
!
! Purpose:
!
!      Date       Programmer       Affiliation          Description of change
!      ====       ==========       ===========          =====================
!
!-----------------------------------------------------------------------------
#define WRITERESULTS
MODULE smooth_topo_cube_sph
  USE reconstruct
  !USE ridge_ana

IMPLICIT NONE
PRIVATE

PUBLIC smooth_intermediate_topo


CONTAINS

!=============================================================================
  SUBROUTINE smooth_intermediate_topo(terr, da, ncube,nhalo, NSCL_f,NSCL_c, SMITER &
                                    , terr_sm,terr_dev, lread_smooth_topofile, rr_factor & 
                                    , smoothtopofile )

    REAL (KIND=dbl_kind), PARAMETER :: pi        = 3.14159265358979323846264338327

    INTEGER (KIND=int_kind), INTENT(IN) :: ncube, nhalo,NSCL_f,NSCL_c,SMITER

    REAL (KIND=dbl_kind), &
            DIMENSION(ncube,ncube,6), INTENT(INOUT) :: terr
    REAL (KIND=dbl_kind), &
            DIMENSION(ncube,ncube),   INTENT(INOUT) :: da
#if 1
    REAL (KIND=dbl_kind), &
            DIMENSION(ncube,ncube,6), INTENT(INOUT) :: terr_dev
    REAL (KIND=dbl_kind), &
            DIMENSION(ncube,ncube,6), INTENT(INOUT) :: terr_sm
#endif

    LOGICAL, INTENT(IN)  :: lread_smooth_topofile 

    !-----------------------------------------------------------------
    !PRIMARY Outputs
    !-----------------------------------------------------------------
    !------------------------------

    REAL (KIND=dbl_kind),                                            &
         DIMENSION(1-nhalo:ncube+nhalo, 1-nhalo:ncube+nhalo, 6) :: terr_halo
    REAL (KIND=dbl_kind),                                            &
         DIMENSION(1-nhalo:ncube+nhalo, 1-nhalo:ncube+nhalo, 6) :: terr_halo_sm
    REAL (KIND=dbl_kind),                                            &
         DIMENSION(1-nhalo:ncube+nhalo, 1-nhalo:ncube+nhalo, 6)    :: da_halo
    REAL  (KIND=dbl_kind) ,                                                          &
         DIMENSION(1-nhalo:ncube+nhalo, 1-nhalo:ncube+nhalo, 6) :: terr_halo_r4
    REAL  (KIND=dbl_kind) ,                                                          &
         DIMENSION(1-nhalo:ncube+nhalo, 1-nhalo:ncube+nhalo, 6) :: terr_halo_rw
    REAL  (KIND=dbl_kind) ,                                                          &
         DIMENSION(1-nhalo:ncube+nhalo, 1-nhalo:ncube+nhalo, 6) :: terr_halo_fx,terr_halo_fx_sv
    REAL  (KIND=dbl_kind) ,                                                          &
         DIMENSION(1-nhalo:ncube+nhalo, 1-nhalo:ncube+nhalo)    :: smwt,ggaa,ggbb,ggab
    REAL  (KIND=dbl_kind) ,                                          &
         DIMENSION(1-nhalo:ncube+nhalo )                          :: xv,yv,alph,beta
    REAL (KIND=dbl_kind), &
         DIMENSION(ncube,ncube,6), OPTIONAL, INTENT(IN)    :: rr_factor
    CHARACTER(len=1024),  & 
                         OPTIONAL, INTENT(IN)              :: smoothtopofile

    INTEGER (KIND=int_kind):: np,i,j, ncube_halo,norx,nory,ipanel
    INTEGER (KIND=int_kind):: x0,x1,y0,y1,initd,ii0,ii1,jj0,jj1
    INTEGER (KIND=int_kind):: nctest,NSM,NS2,ismi,NSB,ns2x


    REAL (KIND=dbl_kind), allocatable ::  daxx(:,:,:)
    REAL (KIND=dbl_kind), allocatable ::  wt1p(:,:),terr_patch(:,:)
    REAL(KIND=dbl_kind)  :: cosll, dx, dy ,dbet,dalp,diss,diss00,lon_ij,lat_ij,diss0r,wt1ps

    INTEGER :: NOCTV , isx0, isx1, jsy0, jsy1,i2,j2,iix,jjx,i00,ncube_in_file

    REAL(KIND=dbl_kind) :: RSM_scl, smoo,irho,volt0,volt1,volume_after,volume_before


    CHARACTER(len=1024) :: ofile

    logical ::     read_in_precomputed

    if (present(rr_factor)) then
      write(*,*) "not merget"
      stop
    end if

    !read_in_precomputed = .FALSE.
    read_in_precomputed = lread_smooth_topofile  !.TRUE.

    IF (read_in_precomputed) then

       IF (.NOT.( PRESENT(smoothtopofile) ) ) THEN
          write( ofile , &
          "('./output/topo_smooth_nc',i0.4,'_Co',i0.3,'_Fi',i0.3)" ) & 
          ncube, NSCL_c/2, NSCL_f/2
          ofile= trim(ofile)//'.dat'
       ELSE
          ofile= trim(smoothtopofile)
       END IF


       OPEN (unit = 711, file= trim(ofile) ,form="UNFORMATTED" )
       READ(711) ncube_in_file
       READ(711) terr
       READ(711) terr_sm
       READ(711) terr_dev

       close(711)

       write(*,*) " Read precomputed filtered topography from "
       write(*,*) ofile

       RETURN

     ENDIF

    write(*,*) " NCUBE !!! " , ncube

       write( ofile , &
       "('./output/topo_smooth_nc',i0.4,'_Co',i0.3,'_Fi',i0.3)" ) & 
        ncube, NSCL_c/2, NSCL_f/2

       ofile= trim(ofile)//'.dat'

       write(*,*) " Will do smoothing of topo on cubed sphere "
       write(*,*) " Output will go to:"
       write(*,*) ofile


    allocate( daxx(ncube,ncube,6) )
    DO np = 1, 6
       daxx(:,:,np) = da
    end do                                
    DO np = 1, 6
     CALL CubedSphereFillHalo_Linear_extended(terr, terr_halo(:,:,np), np, ncube+1,nhalo)  
     CALL CubedSphereFillHalo_Linear_extended(daxx, da_halo(:,:,np), np, ncube+1,nhalo)  
     !CALL CubedSphereFillHalo(terr, terr_halo, np, ncube+1,nhalo)  
     !CALL CubedSphereFillHalo(daxx, da_halo, np, ncube+1,nhalo)  
    END DO
    deallocate( daxx )

    ncube_halo = size( terr_halo(:,:,1), 1 )


    !terr_halo(1-nhalo:1,1-nhalo:1,:)=0.
    !terr_halo(ncube:ncube+nhalo,ncube:ncube+nhalo , :)=0.


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

    terr_halo_sm = terr_halo
    terr_halo_fx = terr_halo
    terr_halo_rw = terr_halo


    if (NSCL_f > 1 ) then
      NSM=NSCL_f
      NS2=NSM/2
      
      write(*,*)" smoothing fine scle w/" ,NSCL_f
      
      allocate( wt1p(-ns2:ns2, -ns2:ns2 ) )
      allocate( terr_patch(-ns2:ns2, -ns2:ns2 ) )
      
      i00 = ncube/2
      dalp   = alph(i00+ns2 )-alph(i00)
      diss00 = 1./sqrt(  ggaa(i00,i00)*dalp*dalp )
      
      terr_halo_fx = 0.0
      
#if 1
      write(*,*) "Convoluted smoothing option "
      DO np=1,6
        DO j=1-nhalo+ns2,ncube+nhalo-ns2
          DO i=1-nhalo+ns2,ncube+nhalo-ns2
            volt0  = terr_halo(i,j,np)*da_halo(i,j,np)
            volt1 = 0.
            do j2=-ns2,ns2
            do i2=-ns2,ns2
                jjx = j+j2
                iix = i+i2
                dalp = alph(iix)-alph(i)
                dbet = beta(jjx)-beta(j)
                diss = ggaa(i,j)*dalp*dalp + ggbb(i,j)*dbet*dbet + 2.*ggab(i,j)*dalp*dbet
                wt1p(i2,j2) = da_halo(iix,jjx,np)
                terr_patch(i2,j2) = terr_halo(i,j,np)*( 1. - diss00 * sqrt( diss ) ) !*da_halo(iix,jjx,np)
                if ((volt0*terr_patch(i2,j2)<=0.).or.(wt1p(i2,j2)<=0.) ) then 
                  terr_patch(i2,j2)=0.
                  wt1p(i2,j2)      =0.
                end if
                volt1 = volt1 + terr_patch(i2,j2)*wt1p(i2,j2)
            end do
            end do
            
            if ( abs(volt1) > 0.) terr_patch = (volt0 / volt1) * terr_patch 
            
            do j2=-ns2,ns2
              do i2=-ns2,ns2
                jjx = j+j2
                iix = i+i2
                terr_halo_fx(iix,jjx,np) = terr_halo_fx(iix,jjx,np) + terr_patch(i2,j2)
              end do
            end do
          END DO
        END DO
      END DO
#else
      write(*,*) "Direct smoothting option "
      DO np=1,6
        DO j=1-nhalo+ns2,ncube+nhalo-ns2
        DO i=1-nhalo+ns2,ncube+nhalo-ns2
           ! Smooth topography with Conical kernel
           !----------------------------------------------
           wt1p(:,:)=0.
           wt1ps=0.
           ns2x = ns2  !/rr_fact_halo( i,j,np )
           do j2=-ns2x,ns2x
           do i2=-ns2x,ns2x
              jjx = j+j2
              iix = i+i2
              dalp = alph(iix)-alph(i)
              dbet = beta(jjx)-beta(j)
              diss = ggaa(i,j)*dalp*dalp + ggbb(i,j)*dbet*dbet + 2.*ggab(i,j)*dalp*dbet

                 diss0r = diss00 ! * rr_fact_halo( i,j,np )
                 wt1p(i2,j2) = ( 1. - diss0r * sqrt( diss ) )*da_halo(iix,jjx,np)

              if (wt1p(i2,j2)<0.) wt1p(i2,j2)=0.
              wt1ps = wt1ps + wt1p(i2,j2)
           end do
           end do


           do j2=-ns2x,ns2x
           do i2=-ns2x,ns2x
              jjx = j+j2
              iix = i+i2
              terr_halo_fx(i,j,np) = terr_halo(i,j,np) + terr_halo(iix,jjx,np)*wt1p(i2,j2)/wt1ps
           end do
           end do
           ! end of smoothing with conical kernel
           !---------------------------------------
           ! Block above is functionally like the one-line smoothing command here:
           !  terr_halo_sm(i,j,np) = SUM( terr_halo_fx( i-ns2:i+ns2  ,  j-ns2:j+ns2   ,np ) )/ ((2.*ns2+1)**2)        

        END DO  !  i-loop   
         !!if (mod(j,1) ==0 ) write(*,*) "Crs Sm J = ",J, " Panel=",np
         !!if (mod(j,1) ==0 ) write(*,900,advance='no') achar(13), J, Ncube+2*Nhalo, Np
        END DO  ! j-loop
      END DO  ! panel loop
#endif
      
      deallocate( wt1p )
      deallocate( terr_patch )
      
    else
      write(*,*)" No fine scale smoother "
      terr_halo_fx  = terr_halo
    endif
    
    
    
    NSM=NSCL_c
    NS2=NSM/2
    
    allocate( wt1p(-ns2:ns2, -ns2:ns2 ) )
    allocate( terr_patch(-ns2:ns2, -ns2:ns2 ) )
    
    i00 = ncube/2
    dalp   = alph(i00+ns2 )-alph(i00)
    diss00 = 1./sqrt(  ggaa(i00,i00)*dalp*dalp )
    
    !terr_halo_sm    = 0.0
    terr_halo_fx_sv = terr_halo_fx

    write(*,*) "LIMITS in smoother "
    write(*,*) 1-nhalo+ns2,ncube+nhalo-ns2
    
    
      terr_halo_sm =  0.0
#if 1
      write(*,*) "Convoluted smoothing option "
      DO np=1,6
        DO j=1-nhalo+ns2,ncube+nhalo-ns2
          DO i=1-nhalo+ns2,ncube+nhalo-ns2                  
            volt0  = terr_halo_fx(i,j,np)*da_halo(i,j,np)
            volt1 = 0.                              
            do j2=-ns2,ns2
              do i2=-ns2,ns2
                jjx = j+j2
                iix = i+i2
                dalp = alph(iix)-alph(i)
                dbet = beta(jjx)-beta(j)
                diss = ggaa(i,j)*dalp*dalp + ggbb(i,j)*dbet*dbet + 2.*ggab(i,j)*dalp*dbet
                wt1p(i2,j2) = da_halo(iix,jjx,np)
                terr_patch(i2,j2) = terr_halo_fx(i,j,np)*( 1. - diss00 * sqrt( diss ) ) !*da_halo(iix,jjx,np)
                if ((volt0*terr_patch(i2,j2)<=0.).or.(wt1p(i2,j2)<=0.) ) then 
                  terr_patch(i2,j2)=0.
                  wt1p(i2,j2)      =0.
                end if
                volt1 = volt1 + terr_patch(i2,j2)*wt1p(i2,j2)
              end do
            end do
            
            if ( abs(volt1) > 0.) terr_patch = (volt0 / volt1) * terr_patch 
            
            do j2=-ns2,ns2
              do i2=-ns2,ns2
                jjx = j+j2
                iix = i+i2
                terr_halo_sm(iix,jjx,np) = terr_halo_sm(iix,jjx,np) + terr_patch(i2,j2)
              end do
            end do
          END DO
          if (mod(j,1) ==0 ) write(*,*) "Crs Sm J = ",J, " Panel=",np," iter=",ismi
        END DO
     END DO ! Panel loop
#else
      write(*,*) "Direct smoothting option "
      DO np=1,6
        DO j=1-nhalo+ns2,ncube+nhalo-ns2
        DO i=1-nhalo+ns2,ncube+nhalo-ns2
           ! Smooth topography with Conical kernel
           !----------------------------------------------
           wt1p(:,:)=0.
           wt1ps=0.
           ns2x = ns2   ! /rr_fact_halo( i,j,np )
           do j2=-ns2x,ns2x
           do i2=-ns2x,ns2x
              jjx = j+j2
              iix = i+i2
              dalp = alph(iix)-alph(i)
              dbet = beta(jjx)-beta(j)
              diss = ggaa(i,j)*dalp*dalp + ggbb(i,j)*dbet*dbet + 2.*ggab(i,j)*dalp*dbet

                 diss0r = diss00 ! * rr_fact_halo( i,j,np )
                 wt1p(i2,j2) = ( 1. - diss0r * sqrt( diss ) )*da_halo(iix,jjx,np)

              if (wt1p(i2,j2)<0.) wt1p(i2,j2)=0.
              wt1ps = wt1ps + wt1p(i2,j2)
           end do
           end do


           do j2=-ns2x,ns2x
           do i2=-ns2x,ns2x
              jjx = j+j2
              iix = i+i2
              terr_halo_sm(i,j,np) = terr_halo_sm(i,j,np) + terr_halo_fx(iix,jjx,np)*wt1p(i2,j2)/wt1ps
           end do
           end do
           ! end of smoothing with conical kernel
           !---------------------------------------
           ! Block above is functionally like the one-line smoothing command here:
           !  terr_halo_sm(i,j,np) = SUM( terr_halo_fx( i-ns2:i+ns2  ,  j-ns2:j+ns2   ,np ) )/ ((2.*ns2+1)**2)        

        END DO  !  i-loop   
         if (mod(j,1) ==0 ) write(*,*) "Crs Sm J = ",J, " Panel=",np
         !if (mod(j,1) ==0 ) write(*,900,advance='no') achar(13), J, Ncube+2*Nhalo, Np
        END DO  ! j-loop
     END DO  ! Panel loop
#endif
      !terr_halo_fx =  terr_halo_sm
      do np=1,6 
        terr_sm (1:ncube,1:ncube,np) = terr_halo_sm(1:ncube,1:ncube,np )
        CALL CubedSphereFillHalo_Linear_extended(terr_sm, terr_halo_fx(:,:,np), np, ncube+1,nhalo)  
      end do
    
    deallocate( wt1p )
    deallocate( terr_patch )
    
    
    do np=1,6
      terr_dev(1:ncube,1:ncube,np) = terr_halo_fx_sv(1:ncube,1:ncube,np ) - terr_halo_sm(1:ncube,1:ncube,np )
      terr_sm (1:ncube,1:ncube,np) = terr_halo_sm(1:ncube,1:ncube,np )
    end do
    
    
    
    OPEN (unit = 711, file= trim(ofile) ,form="UNFORMATTED" )
    write(711) ncube
    WRITE(711) terr
    WRITE(711) terr_sm
    WRITE(711) terr_dev
    
    close(711)
    
           !STOP
          
    
  END SUBROUTINE smooth_intermediate_topo
  

END MODULE smooth_topo_cube_sph
