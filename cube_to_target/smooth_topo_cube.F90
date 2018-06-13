!-----------------------------------------------------------------------------
! MODULE subgrid_topo_ana
!
! Purpose:
!
!      Date       Programmer       Affiliation          Description of change
!      ====       ==========       ===========          =====================
!
!-----------------------------------------------------------------------------
MODULE smooth_topo_cube_sph
  USE reconstruct

IMPLICIT NONE
PRIVATE

PUBLIC smooth_intermediate_topo_wrap


CONTAINS

!=============================================================================
!=============================================================================
  SUBROUTINE smooth_intermediate_topo_wrap(terr, da, ncube,nhalo, NSCL_f,NSCL_c &
                                    , terr_sm,terr_dev,ofname  & 
                                    , lread_smooth_topofile  &
                                    , lsmooth_topo_cubesph &
                                    , luse_multigrid &
                                    , luse_prefilter &
                                    , lstop_after_smoothing & 
                                    , lb4b_with_cesm2 &
                                    , rr_factor  )

    REAL (KIND=dbl_kind), PARAMETER :: pi        = 3.14159265358979323846264338327

    INTEGER (KIND=int_kind), INTENT(IN) :: ncube, nhalo,NSCL_f,NSCL_c

    REAL (KIND=dbl_kind), &
            DIMENSION(ncube,ncube,6), INTENT(INOUT) :: terr
    REAL (KIND=dbl_kind), &
            DIMENSION(ncube,ncube),   INTENT(INOUT) :: da
    REAL (KIND=dbl_kind), &
            DIMENSION(ncube,ncube,6), INTENT(INOUT) :: terr_dev
    REAL (KIND=dbl_kind), &
            DIMENSION(ncube,ncube,6), INTENT(INOUT) :: terr_sm
    REAL (KIND=dbl_kind), &
            DIMENSION(ncube,ncube,6), INTENT(IN)    :: rr_factor
    REAL (KIND=dbl_kind), &
            DIMENSION(ncube,ncube) :: terr_face,da_face,terr_face2
    REAL (KIND=dbl_kind), &
            DIMENSION(1-nhalo:ncube+nhalo,1-nhalo:ncube+nhalo) :: t1_halo,t2_halo

    LOGICAL, INTENT(IN)  :: lread_smooth_topofile , lsmooth_topo_cubesph
    LOGICAL, INTENT(IN)  :: luse_multigrid, luse_prefilter, lstop_after_smoothing, lb4b_with_cesm2
    !!!LOGICAL, INTENT(IN), OPTIONAL  :: lregional_refinement
    CHARACTER(len=1024), INTENT(IN) :: ofname


    REAL (KIND=dbl_kind), &
            DIMENSION(ncube,ncube,6)  :: terr_in,rrfac1

    !REAL (KIND=dbl_kind), &
    !        DIMENSION(ncube,ncube,6) :: rrfac
    REAL (KIND=dbl_kind), &
            DIMENSION(ncube/4,ncube/4,6) :: terrx, terr_devx, terr_smx, rrfacx
    REAL (KIND=dbl_kind), &
            DIMENSION(1-nhalo/4 : ncube/4+nhalo/4, 1-nhalo/4: ncube/4+nhalo/4 ,6) :: terr_smx_halo
    REAL (KIND=dbl_kind), &
            DIMENSION(ncube/4,ncube/4)  :: dax
    INTEGER (KIND=int_kind)  :: ncubex, nhalox,NSCL_fx,NSCL_cx,ip
    REAL (KIND=dbl_kind)     :: volterr_in,volterr_sm
     

    INTEGER (KIND=int_kind)   :: ncube_in_file

    logical ::     read_in_precomputed, use_multigrid, use_prefilter, stop_after_smoothing
    logical ::     b4b_with_cesm2, smooth_topo_cubesph, do_refine


    !read_in_precomputed = .FALSE.
    read_in_precomputed = lread_smooth_topofile  !.TRUE.
    use_multigrid = luse_multigrid 
    use_prefilter = luse_prefilter 
    stop_after_smoothing = lstop_after_smoothing 
    b4b_with_cesm2 = lb4b_with_cesm2
    smooth_topo_cubesph = lsmooth_topo_cubesph


    terr_in = terr

    IF (read_in_precomputed) then

       OPEN (unit = 711, file= trim(ofname) ,form="UNFORMATTED" )
       READ(711) ncube_in_file
       READ(711) terr
       READ(711) terr_sm
       READ(711) terr_dev

       close(711)

       write(*,*) " Read precomputed filtered topography from "
       write(*,*) ofname

       ! return to main program after
       ! reading topography variables
       RETURN

     ENDIF

    ! Smooth cubed sphere topography
    !--------------------------------------
    IF (smooth_topo_cubesph) then

      terr_sm = 0.
      terr_dev = 0.


      if (use_multigrid) then

          write(*,*) "writing smooth topo to ",ofname

         ncubex = ncube/4
         nhalox = nhalo/4
         NSCL_fx = NSCL_f/4
         NSCL_cx = NSCL_c/4

         if (use_prefilter) then
         ! Need for consistency with CESM2 version. Also
         ! reduces number of searchable peaks which is good. 
            rrfac1(:,:,:) = 1. ! disable refinement for pre-filter
            call smooth_intermediate_topo(terr, da, ncube,nhalo, 1, 2  &
                                        , terr_sm,terr_dev,rrfac1  )
            terr = terr_sm
            terr_dev = 0.
         endif 

         do ip = 1,6 
          terr_face = rr_factor(:,:,ip)
          da_face   = da
          call restrict2_consrv( terr_face , terr_face, da_face, da_face, ncube )
          call restrict2_consrv( terr_face , terr_face,  da_face, da_face, ncube )
          rrfacx(:,:,ip) = terr_face(1:ncubex,1:ncubex)
         end do

         do ip = 1,6 
          terr_face = terr(:,:,ip)
          da_face   = da
          call restrict2_consrv( terr_face , terr_face, da_face, da_face, ncube )
          call restrict2_consrv( terr_face , terr_face,  da_face, da_face, ncube )
          terrx(:,:,ip) = terr_face(1:ncubex,1:ncubex)
          dax(:,:)      = da_face(1:ncubex,1:ncubex)
         end do

           call smooth_intermediate_topo(terrx, dax, ncubex,nhalox, NSCL_fx,NSCL_cx &
                                    , terr_smx,terr_devx , rrfacx )

                !terr_smx = terrx
         DO ip = 1, 6
            CALL CubedSphereFillHalo(terr_smx, terr_smx_halo, ip, ncubex+1,nhalox)  
         END DO

                write(*,*) " now prolong "

         do ip = 1,6 
          t1_halo(1-nhalo: , 1-nhalo: ) = terr_smx_halo(: ,: , ip)
          t2_halo(:,:)=0.
          call prolong2( t1_halo(1-nhalo: , 1-nhalo:)  , t2_halo(1-nhalo: , 1-nhalo:), ncube+2*nhalo )
          t1_halo = t2_halo
          t2_halo=0.
          call prolong2( t1_halo(1-nhalo: , 1-nhalo:) , t2_halo(1-nhalo: , 1-nhalo:), ncube+2*nhalo )
          terr_sm(:,:,ip) = t2_halo(1:ncube,1:ncube)
         end do

         terr_dev = terr - terr_sm

       else ! No Multigrid
         if (.not.(b4b_with_cesm2)) then
            write(*,*) "writing smooth topo to ",ofname
            if (use_prefilter) then
            ! Need for consistency with CESM2 version. Also
            ! reduces number of searchable peaks which is good. 
               call smooth_intermediate_topo(terr, da, ncube,nhalo, 1,NSCL_f &
                                        , terr_sm,terr_dev,rr_factor  )
               terr = terr_sm
               terr_dev = 0.
            end if 
            call smooth_intermediate_topo(terr, da, ncube,nhalo, 1, NSCL_c &
                                    , terr_sm,terr_dev,rr_factor  )
         else ! b4b w/ CESM2
            call smooth_intermediate_topo(terr, da, ncube,nhalo, 1, NSCL_c &
                                    , terr_sm,terr_dev,rr_factor  )
         end if   
      endif


      volterr_in=0.
      volterr_sm=0.
      do ip=1,6 
         volterr_in =  volterr_in + sum( terr_in(:,:,ip) * da )
         volterr_sm =  volterr_sm + sum( terr_sm(:,:,ip) * da )
      end do
      write(*,*) " Topo volume BEFORE smoother = ",volterr_in/(6*sum(da))
      write(*,*) " Topo volume  AFTER smoother = ",volterr_sm/(6*sum(da))
      write(*,*) "            Difference       = ",(volterr_in - volterr_sm)/(6*sum(da))

  

      
      OPEN (unit = 711, file= trim(ofname) ,form="UNFORMATTED" )
      write(711) ncube
      WRITE(711) terr
      WRITE(711) terr_sm
      WRITE(711) terr_dev
      close(711)

      if (stop_after_smoothing) STOP

    endif ! (smooth_topo_cubesph)


  end SUBROUTINE smooth_intermediate_topo_wrap
   


!=============================================================================
  SUBROUTINE smooth_intermediate_topo(terr, da, ncube,nhalo, NSCL_f,NSCL_c &
                                    , terr_sm,terr_dev,rr_factor )

    REAL (KIND=dbl_kind), PARAMETER :: pi        = 3.14159265358979323846264338327

    INTEGER (KIND=int_kind), INTENT(IN) :: ncube, nhalo,NSCL_f,NSCL_c

    REAL (KIND=dbl_kind), &
            DIMENSION(ncube,ncube,6), INTENT(INOUT) :: terr
    REAL (KIND=dbl_kind), &
            DIMENSION(ncube,ncube),   INTENT(INOUT) :: da
    REAL (KIND=dbl_kind), &
            DIMENSION(ncube,ncube,6), INTENT(INOUT) :: terr_dev
    REAL (KIND=dbl_kind), &
            DIMENSION(ncube,ncube,6), INTENT(INOUT) :: terr_sm
    REAL (KIND=dbl_kind), &
            DIMENSION(ncube,ncube,6), INTENT(IN)    :: rr_factor

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
         DIMENSION(1-nhalo:ncube+nhalo, 1-nhalo:ncube+nhalo, 6) :: rr_fact_halo
    REAL  (KIND=dbl_kind) ,                                                          &
         DIMENSION(1-nhalo:ncube+nhalo, 1-nhalo:ncube+nhalo)    :: smwt,ggaa,ggbb,ggab
    REAL  (KIND=dbl_kind) ,                                          &
         DIMENSION(1-nhalo:ncube+nhalo )                          :: xv,yv,alph,beta

    REAL (KIND=dbl_kind),                                            &
         DIMENSION(1-nhalo:ncube+nhalo, 1-nhalo:ncube+nhalo ) :: terr_face,terr_face2,da_face


    INTEGER (KIND=int_kind):: np,i,j, ncube_halo,norx,nory,ipanel,x0,&
         x1,y0,y1,initd,ii0,ii1,jj0,jj1,nctest,NSM,NS2,ismi,NSB,ns2x


    REAL (KIND=dbl_kind), allocatable ::  daxx(:,:,:)
    REAL (KIND=dbl_kind), allocatable ::  wt1p(:,:),terr_patch(:,:)
    REAL(KIND=dbl_kind)  :: cosll, dx, dy ,dbet,dalp,diss,diss00,lon_ij,lat_ij,latfactor,diss0r

    INTEGER :: NOCTV , isx0, isx1, jsy0, jsy1,i2,j2,iix,jjx,i00,ncube_in_file

    REAL(KIND=dbl_kind) :: RSM_scl, smoo,irho,volt0,volt1,volume_after,volume_before,wt1ps

    CHARACTER(len=1024) :: ofile$

    write(*,*) " NCUBE !!! " , ncube


    allocate( daxx(ncube,ncube,6) )
    DO np = 1, 6
       daxx(:,:,np) = da
    end do                                
    DO np = 1, 6
     CALL CubedSphereFillHalo_Linear_extended(terr, terr_halo(:,:,np), np, ncube+1,nhalo)  
     CALL CubedSphereFillHalo_Linear_extended(daxx, da_halo(:,:,np), np, ncube+1,nhalo)  
     !!CALL CubedSphereFillHalo(terr, terr_halo, np, ncube+1,nhalo)  
     !!CALL CubedSphereFillHalo(daxx, da_halo, np, ncube+1,nhalo)  
    END DO
    deallocate( daxx )

    DO np = 1, 6
     !!CALL CubedSphereFillHalo(rr_factor, rr_fact_halo, np, ncube+1,nhalo)  
     CALL CubedSphereFillHalo_Linear_extended(rr_factor, rr_fact_halo(:,:,np), np, ncube+1,nhalo)  
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
       !irho = 1./ ( ( cos(alph(i))**2)*(cos(beta(j))**2)* ( ( 1. + (tan(alph(i))**2) + (tan(beta(j))**2 ) )**2  ))   ???
       ggaa(i,j) = irho * ( 1. + ( tan( alph(i) ) )**2 )
       ggbb(i,j) = irho * ( 1. + ( tan( beta(j) ) )**2 )
       ggab(i,j) = -irho *( tan( beta(j) ) ) * ( tan( alph(i) ) )
    END DO
    END DO

    terr_halo_sm = terr_halo
    terr_halo_fx = terr_halo
    terr_halo_rw = terr_halo

#if 0
write(915) ncube,nhalo
write(915) terr_halo
write(915) da_halo
write(915) rr_fact_halo
write(915) ggaa,ggbb,ggab
STOP
#endif

      if (NSCL_f > 1 ) then
       write(*,*)" Fine scale pre-smoother no longer used "
       write(*,*) "Use prefilter instead"
       STOP       
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


      terr_halo_sm(:,:,:) =  0.0
#if 0
      write(*,*) "Convoluted smoothing option "
      DO np=1,6
        DO j=1-nhalo+ns2,ncube+nhalo-ns2
        DO i=1-nhalo+ns2,ncube+nhalo-ns2

           volt0  = terr_halo_fx(i,j,np)*da_halo(i,j,np)
           volt1 = 0.

           ! Smooth topography with Conical kernel
           !----------------------------------------------
           do j2=-ns2,ns2
           do i2=-ns2,ns2
              jjx = j+j2
              iix = i+i2
              dalp = alph(iix)-alph(i)
              dbet = beta(jjx)-beta(j)
              diss = ggaa(i,j)*dalp*dalp + ggbb(i,j)*dbet*dbet + 2.*ggab(i,j)*dalp*dbet
              wt1p(i2,j2) = da_halo(iix,jjx,np)
              
              !!!if(do_refine) then
              !!diss0r = diss00 * rr_fact_halo( i,j,np )
                 diss0r = diss00 * MAX( rr_fact_halo( iix,jjx,np ), rr_fact_halo(i,j,np) )
                 terr_patch(i2,j2) = terr_halo_fx(i,j,np)*( 1. - diss0r * sqrt( diss ) ) !*da_halo(iix,jjx,np)

              if(terr_patch(i2,j2)<0.) terr_patch(i2,j2)=0.             

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
           ! end of smoothing with conical kernel
           !---------------------------------------
           ! Block above is functionally like the one-line smoothing command here:
           !  terr_halo_sm(i,j,np) = SUM( terr_halo_fx( i-ns2:i+ns2  ,  j-ns2:j+ns2   ,np ) )/ ((2.*ns2+1)**2)        

        END DO  !  i-loop   
        if (mod(j,1) ==0 ) write(*,900,advance='no') achar(13), J, Ncube+2*Nhalo, Np
        END DO  ! j-loop
      END DO  ! panel loop
#else
      write(*,*) "Direct smoothting option "
      DO np=1,6
        DO j=1-nhalo+ns2,ncube+nhalo-ns2
        DO i=1-nhalo+ns2,ncube+nhalo-ns2
           ! Smooth topography with Conical kernel
           !----------------------------------------------
           wt1p(:,:)=0.
           wt1ps=0.
           ns2x = ns2/rr_fact_halo( i,j,np )
           do j2=-ns2x,ns2x
           do i2=-ns2x,ns2x
              jjx = j+j2
              iix = i+i2
              dalp = alph(iix)-alph(i)
              dbet = beta(jjx)-beta(j)
              diss = ggaa(i,j)*dalp*dalp + ggbb(i,j)*dbet*dbet + 2.*ggab(i,j)*dalp*dbet

                 diss0r = diss00 * rr_fact_halo( i,j,np )
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
        if (mod(j,1) ==0 ) write(*,900,advance='no') achar(13), J, Ncube+2*Nhalo, Np
        END DO  ! j-loop
      END DO  ! panel loop
#endif

900   format( a1, " Smoothed Row ",i4," out of ",i4," Panel=",i2 )

          do np=1,6 
             terr_sm (1:ncube,1:ncube,np) = terr_halo_sm(1:ncube,1:ncube,np )
          end do
          do np=1,6 
             CALL CubedSphereFillHalo(terr_sm, terr_halo_fx, np, ncube+1,nhalo)  
          end do

      deallocate( wt1p )
      deallocate( terr_patch )


  do np=1,6
    terr_dev(1:ncube,1:ncube,np) = terr_halo_fx_sv(1:ncube,1:ncube,np ) - terr_halo_sm(1:ncube,1:ncube,np )
    terr_sm (1:ncube,1:ncube,np) = terr_halo_sm(1:ncube,1:ncube,np )
  end do


END SUBROUTINE smooth_intermediate_topo


!==========================================================
  SUBROUTINE restrict2( Ain , Aout, N )
    integer , intent(IN) :: N
    REAL (KIND=dbl_kind), &
            DIMENSION(N,N), INTENT(IN) :: Ain
    REAL (KIND=dbl_kind), &
            DIMENSION(N,N), INTENT(OUT) :: Aout

    integer :: i,j

    do j=1,N/2
    do i=1,N/2
       aout(i,j) = ( ain(2*i,  2*j  ) + &
                     ain(2*i+1,2*j  ) + & 
                     ain(2*i,  2*j+1) + & 
                     ain(2*i+1,2*j+1) ) /4   
    end do
    end do


  end SUBROUTINE restrict2
!==========================================================
  SUBROUTINE restrict2_consrv( Ain , Aout, DAin, DAout, N )
    integer , intent(IN) :: N
    REAL (KIND=dbl_kind), &
            DIMENSION(N,N), INTENT(IN) :: Ain,DAin
    REAL (KIND=dbl_kind), &
            DIMENSION(N,N), INTENT(OUT) :: Aout,DAout

    integer :: i,j,a,b

    do j=1,N/2
    do i=1,N/2
       
        a=2*i-1
        b=2*j-1
       DAout(i,j) = (Dain(a,  b  ) + &
                     Dain(a+1,b  ) + & 
                     Dain(a,  b+1) + & 
                     Dain(a+1,b+1) )  
       if ( DAout(i,j) > 0.) then
       aout(i,j) = ( ain(a,  b  ) * Dain(a,  b  ) + &
                     ain(a+1,b  ) * Dain(a+1,b  ) + & 
                     ain(a,  b+1) * Dain(a,  b+1) + & 
                     ain(a+1,b+1) * Dain(a+1,b+1) ) /  DAout(i,j) 
       else
       aout(i,j)=0.
       endif
    end do
    end do


 end SUBROUTINE restrict2_consrv
!==========================================================
  SUBROUTINE prolong2( Ain , Aout, N )
    integer , intent(IN) :: N
    REAL (KIND=dbl_kind), &
            DIMENSION(1:N,1:N), INTENT(INOUT) :: Ain
    REAL (KIND=dbl_kind), &
            DIMENSION(1:N,1:N), INTENT(INOUT) :: Aout
    REAL (KIND=dbl_kind), &
            DIMENSION(N,N)   :: Atmp

    integer :: i,j,a,b
    real (KIND=dbl_kind)   :: w1,w23,w4,wt,x0,x1,y0,y1,a0,a1,b0,b1,fq00,fq10,fq01,fq11
  
    wt  =  2./SQRT(2.) + 4./SQRT(10.) + 2./SQRT(18.) 
    w1  =  2./SQRT(2.) / wt
    w23 =  2./SQRT(10.) / wt
    w4  =  2./SQRT(18.)  / wt

    do j=1,N/2-1
    do i=1,N/2-1
       ! Indices for prolonged array
       a=2*i
       b=2*j
       ! External corner coords
       x0=1.*i 
       x1=1.*i+1
       y0=1.*j
       y1=1.*j+1
       ! Interior point coordinates
       a0=1.*i+0.25
       a1=1.*i+0.75
       b0=1.*j+0.25
       b1=1.*j+0.75
       ! Function values at external corner points
       fq00 = ain(i,j)
       fq01 = ain(i,j+1)
       fq10 = ain(i+1,j)
       fq11 = ain(i+1,j+1)   
       ! Function values at four interior points defined by bi-linear interpolation
       aout(a  ,b  ) = ( fq00*(x1-a0)*(y1-b0) +fq10*(a0-x0)*(y1-b0) + fq01*(x1-a0)*(b0-y0) + fq11*(a0-x0)*(b0-y0) ) 
       aout(a+1,b  ) = ( fq00*(x1-a1)*(y1-b0) +fq10*(a1-x0)*(y1-b0) + fq01*(x1-a1)*(b0-y0) + fq11*(a1-x0)*(b0-y0) ) 
       aout(a  ,b+1) = ( fq00*(x1-a0)*(y1-b1) +fq10*(a0-x0)*(y1-b1) + fq01*(x1-a0)*(b1-y0) + fq11*(a0-x0)*(b1-y0) ) 
       aout(a+1,b+1) = ( fq00*(x1-a1)*(y1-b1) +fq10*(a1-x0)*(y1-b1) + fq01*(x1-a1)*(b1-y0) + fq11*(a1-x0)*(b1-y0) ) 
    end do
    end do

    j=1 !,N/2
    do i=1,N/2-1
       a=2*i
       b=1
       aout(a  ,b  ) = (w1* ain(i,j) + w23* ain(i+1,j) ) / (w1+w23)
       aout(a+1,b  ) = (w1* ain(i+1,j) + w23* ain(i,j) ) / (w1+w23)
    end do
    i=1
    do j=1,N/2-1
       a=1
       b=2*j
       aout(a  ,b  ) = (w1* ain(i,j) + w23 * ain(i,j+1) ) / (w1+w23)
       aout(a  ,b+1) = (w1* ain(i,j+1) + w23* ain(i,j) ) / (w1+w23)
    end do
    ! N BC
    j=N/2
    do i=1,N/2-1
       a=2*i
       b=N
       aout(a  ,b  ) = (w1* ain(i,j) + w23* ain(i+1,j) ) / (w1+w23)
       aout(a+1,b  ) = (w1* ain(i+1,j) + w23* ain(i,j) ) / (w1+w23)
    end do
    i=N/2
    do j=1,N/2-1
       a=N
       b=2*j
       aout(a  ,b  ) = (w1* ain(i,j) + w23 * ain(i,j+1) ) / (w1+w23)
       aout(a  ,b+1) = (w1* ain(i,j+1) + w23* ain(i,j) ) / (w1+w23)
    end do


    aout(1  ,1  ) = ain(1,1)
    aout(1  ,N  ) = ain(1,N/2)
    aout(N  ,1  ) = ain(N/2,1)
    aout(N  ,N  ) = ain(N/2,N/2)

#if 0
    do a=1,2
    atmp=aout
    do j=2,N-1
    do i=2,N-1
       atmp(i,j) = (aout(i-1,j)+aout(i+1,j)+aout(i,j-1)+aout(i,j+1) + &
                    2*aout(i,j) )/6.
    end do
    end do
    aout = atmp
    end do
#endif

 end SUBROUTINE prolong2
!==========================================================

END MODULE smooth_topo_cube_sph
