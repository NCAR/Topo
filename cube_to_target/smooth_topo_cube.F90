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
  SUBROUTINE smooth_intermediate_topo_wrap(terr, rrfac, da &
                                    , ncube,nhalo, NSCL_f,NSCL_c &
                                    , terr_sm,terr_dev,ofname  & 
                                    , lread_smooth_topofile  &
                                    , luse_prefilter &
                                    , lstop_after_smoothing & 
                                    , lregional_refinement &
                                    , smooth_topo_fname )

    REAL (KIND=dbl_kind), PARAMETER :: pi        = 3.14159265358979323846264338327

    INTEGER (KIND=int_kind), INTENT(IN) :: ncube, nhalo,NSCL_f,NSCL_c

    REAL (KIND=dbl_kind), &
            DIMENSION(ncube,ncube,6), INTENT(INOUT) :: terr
    REAL (KIND=dbl_kind), &
            DIMENSION(ncube,ncube),   INTENT(IN)    :: da
    REAL (KIND=dbl_kind), &
            DIMENSION(ncube,ncube,6), INTENT(OUT)   :: terr_dev
    REAL (KIND=dbl_kind), &
            DIMENSION(ncube,ncube,6), INTENT(OUT)   :: terr_sm
    REAL (KIND=dbl_kind), &
            DIMENSION(ncube,ncube,6), INTENT(IN)    :: rrfac

    REAL (KIND=dbl_kind),                                            &
         DIMENSION(1-nhalo:ncube+nhalo, 1-nhalo:ncube+nhalo, 6) :: terr_halo
    REAL (KIND=dbl_kind),                                            &
         DIMENSION(1-nhalo:ncube+nhalo, 1-nhalo:ncube+nhalo, 6) :: terr_halo_sm, terr_halo_dev
    REAL (KIND=dbl_kind),                                            &
         DIMENSION(1-nhalo:ncube+nhalo, 1-nhalo:ncube+nhalo, 6) :: da_halo, rr_halo
    REAL (KIND=dbl_kind), &
            DIMENSION(ncube,ncube,6) :: daxx




    LOGICAL, INTENT(IN)  :: lread_smooth_topofile    ! , lsmooth_topo_cubesph
    LOGICAL, INTENT(IN)  :: luse_prefilter, lstop_after_smoothing
    LOGICAL, INTENT(IN)  :: lregional_refinement
    CHARACTER(len=1024), INTENT(  OUT) :: ofname
    CHARACTER(len=1024), INTENT(IN   ), optional :: smooth_topo_fname

    INTEGER (KIND=int_kind)  :: ncubex, nhalox,NSCL_fx,NSCL_cx,ip
    REAL (KIND=dbl_kind)     :: volterr_in,volterr_sm
     

    INTEGER (KIND=int_kind)   :: ncube_in_file

    logical ::     read_in_precomputed, use_prefilter, stop_after_smoothing
    logical ::     smooth_topo_cubesph, do_refine
    logical ::     read_in_and_refine, new_smooth_topo


    !read_in_precomputed = .FALSE.
    read_in_precomputed = lread_smooth_topofile  !.TRUE.
    use_prefilter = luse_prefilter 
    stop_after_smoothing = lstop_after_smoothing 
    smooth_topo_cubesph = .TRUE.  
    read_in_and_refine=.FALSE.

    IF (read_in_precomputed) then

       OPEN (unit = 711, file= trim(smooth_topo_fname) ,form="UNFORMATTED" )
       READ(711) ncube_in_file
       READ(711) terr
       READ(711) terr_sm
       READ(711) terr_dev
       !!READ(711) rr_factor
       close(711)

       write(*,*) " Read precomputed filtered topography from "
       write(*,*) trim(smooth_topo_fname)

       ! return to main program after
       ! reading topography variables
       if (.NOT. lregional_refinement) then 
          RETURN
       else
          read_in_and_refine=.TRUE.
       end if

     ENDIF

     ! If your are here and read_in_and_refine=.FALSE. then
     ! then you must want to generate a new smooth topo.  So 
     ! assign logical new_smooth_topo here. This could be 
     ! done in subr. smooth_intermediate_topo_halo, but code
     ! is more readable if done here and passed in.
     !---------------------------------------
     new_smooth_topo = .NOT.(read_in_and_refine)


     write( ofname , &
       "('./output/topo_smooth_nc',i0.4,'_Co',i0.3,'_Fi',i0.3)" ) & 
        ncube, NSCL_c/2, NSCL_f/2

       if (lregional_refinement) then
          ofname= trim(ofname)//'_VRtest.dat'
       else
          ofname= trim(ofname)//'.dat'
       end if

       write(*,*) " Will do smoothing of topo on cubed sphere "
       write(*,*) " Output will go to:"
       write(*,*) ofname

     !terr_in = terr
     DO ip = 1, 6
       daxx(:,:,ip) = da
     end do                                

    ! Smooth cubed sphere topography
    !--------------------------------------

      write(*,*) " Smoothing parameters : ",NSCL_f,NSCL_c
      terr_sm = 0.
      terr_dev = 0.

         DO ip = 1, 6
            CALL CubedSphereFillHalo_Linear_extended(terr,  terr_halo(:,:,ip), ip, ncube+1,nhalo)  
            CALL CubedSphereFillHalo_Linear_extended(daxx,  da_halo(:,:,ip),   ip, ncube+1,nhalo)  
            CALL CubedSphereFillHalo_Linear_extended(rrfac, rr_halo(:,:,ip),   ip, ncube+1,nhalo)  
         end DO
                    write(*,*) " MINVAL(abs(rr_halo) ) , MAXVAL(abs(rr_halo) ) "
                    write(*,*) MINVAL(abs(rr_halo) ) , MAXVAL(abs(rr_halo) )
                    write(*,*) " MINVAL(abs(rrfac) ) , MAXVAL(abs(rrfac) ) "
                    write(*,*) MINVAL(abs(rrfac) ) , MAXVAL(abs(rrfac) )
         if (use_prefilter) then
            call smooth_intermediate_topo_halo(terr_halo, da_halo,rr_halo, ncube,nhalo, 1,NSCL_f &
                                         , terr_halo_sm,  read_in_and_refine, new_smooth_topo  ) 
            terr_halo = terr_halo_sm
         else
            write(*,*) " No prefilter in smooth_topo !!!!!! "
         end if
         call smooth_intermediate_topo_halo(terr_halo, da_halo, rr_halo, ncube,nhalo, 1,NSCL_c &
                                     , terr_halo_sm,  read_in_and_refine, new_smooth_topo )

         terr_sm( 1:ncube , 1:ncube, :)  = terr_halo_sm( 1:ncube , 1:ncube, :) 
         terr_dev( 1:ncube , 1:ncube, :) = terr_halo( 1:ncube , 1:ncube, :) -  terr_halo_sm( 1:ncube , 1:ncube, :) 
    

      volterr_in=0.
      volterr_sm=0.
      do ip=1,6 
         volterr_in =  volterr_in + sum( terr   (:,:,ip) * da )
         volterr_sm =  volterr_sm + sum( terr_sm(:,:,ip) * da )
      end do
      write(*,*) " Topo volume BEFORE smoother = ",volterr_in/(6*sum(da))
      write(*,*) " Topo volume  AFTER smoother = ",volterr_sm/(6*sum(da))
      write(*,*) "            Difference       = ",(volterr_in - volterr_sm)/(6*sum(da))

  
      !terr = terr_in
 
      
      OPEN (unit = 711, file= trim(ofname) ,form="UNFORMATTED" )
      write(711) ncube
      WRITE(711) terr
      WRITE(711) terr_sm
      WRITE(711) terr_dev
      if (lregional_refinement) then
        WRITE(711) rrfac
      end if
      close(711)

      if (stop_after_smoothing) STOP

  end SUBROUTINE smooth_intermediate_topo_wrap
   

!=============================================================================
SUBROUTINE smooth_intermediate_topo_halo(terr_halo, da_halo, rr_halo &
                                       , ncube,nhalo, NSCL_f,NSCL_c &
                                       , terr_halo_sm & 
                                       , read_in_and_refine, new_smooth_topo  ) 

    REAL (KIND=dbl_kind), PARAMETER :: pi        = 3.14159265358979323846264338327

    INTEGER (KIND=int_kind), INTENT(IN) :: ncube, nhalo,NSCL_f,NSCL_c

    REAL (KIND=dbl_kind), &
            DIMENSION(1-nhalo:ncube+nhalo, 1-nhalo:ncube+nhalo, 6), INTENT(IN)  :: terr_halo
    REAL (KIND=dbl_kind), &
            DIMENSION(1-nhalo:ncube+nhalo, 1-nhalo:ncube+nhalo, 6), INTENT(IN)  :: da_halo
    REAL (KIND=dbl_kind), &
            DIMENSION(1-nhalo:ncube+nhalo, 1-nhalo:ncube+nhalo, 6), INTENT(IN)  :: rr_halo
    REAL (KIND=dbl_kind), &
            DIMENSION(1-nhalo:ncube+nhalo, 1-nhalo:ncube+nhalo, 6), INTENT(OUT) :: terr_halo_sm

    LOGICAL, INTENT(IN)  :: read_in_and_refine , new_smooth_topo

    !-----------------------------------------------------------------
    !Internal work arrays
    !-----------------------------------------------------------------
    !------------------------------
    REAL  (KIND=dbl_kind) ,                                                          &
         DIMENSION(1-nhalo:ncube+nhalo, 1-nhalo:ncube+nhalo)    :: smwt,ggaa,ggbb,ggab
    REAL  (KIND=dbl_kind) ,                                          &
         DIMENSION(1-nhalo:ncube+nhalo )                          :: xv,yv,alph,beta

    INTEGER (KIND=int_kind):: np,i,j, ncube_halo,norx,nory,ipanel,x0,&
         x1,y0,y1,initd,ii0,ii1,jj0,jj1,nctest,NSM,NS2,ismi,NSB,ns2x


    !!REAL (KIND=dbl_kind), allocatable ::  daxx(:,:,:)
    REAL (KIND=dbl_kind), allocatable ::  wt1p(:,:),terr_patch(:,:)
    REAL(KIND=dbl_kind)  :: cosll, dx, dy ,dbet,dalp,diss,diss00,lon_ij,lat_ij,latfactor,diss0r

    INTEGER :: NOCTV , isx0, isx1, jsy0, jsy1,i2,j2,iix,jjx,i00,ncube_in_file

    REAL(KIND=dbl_kind) :: RSM_scl, smoo,irho,volt0,volt1,volume_after,volume_before,wt1ps,diss0e

    CHARACTER(len=1024) :: ofile

    LOGICAL :: do_smooth_ij

    write(*,*) " NCUBE !!! " , ncube

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

      if (NSCL_f  > 1 ) then
       write(*,*)" Fine scale pre-smoother no longer used "
       write(*,*) "Use prefilter instead"
       STOP       
     end if


      NSM=NSCL_c
      NS2=NSM/2
      i00 = ncube/2
      ! Set smoothing radius based on namelist param
      dalp   = alph(i00+ns2 )-alph(i00)
      diss00 = 1./sqrt(  ggaa(i00,i00)*dalp*dalp )
      diss0e = 1./sqrt(  ggaa(i00,  1)*dalp*dalp )

write(*,*) " diss0e , diss00 diss0e/diss00 ",diss0e , diss00, diss0e/diss00 

      NS2=INT( 1.3 * NSM/2  ) ! Add 30% padding to acommodate compression
                              ! of equi-angular grid at panel edges 
      allocate( wt1p(-ns2:ns2, -ns2:ns2 ) )
      allocate( terr_patch(-ns2:ns2, -ns2:ns2 ) )


write(*,*) " ncube, nhalo, ns2 ",ncube, nhalo, ns2

write(*,*) "LIMITS in smoother "
write(*,*) 1-nhalo+ns2,ncube+nhalo-ns2

  if (NSCL_c > 1 ) then
#if 0
      terr_halo_sm(:,:,:) =  0.0
      write(*,*) "Convoluted smoothing option "
      DO np=1,6
        DO j=1-nhalo+ns2,ncube+nhalo-ns2
        DO i=1-nhalo+ns2,ncube+nhalo-ns2

           volt0  = terr_halo(i,j,np)*da_halo(i,j,np)
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
              diss0r = diss00 !* MAX( rr_fact_halo( iix,jjx,np ), rr_fact_halo(i,j,np) )
              terr_patch(i2,j2) = terr_halo(i,j,np)*( 1. - diss00 * sqrt( diss ) ) !*da_halo(iix,jjx,np)

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
      write(*,*) "Direct smoothing option "
      write(*,*) " new_smooth_topo , read_in_and_refine ", new_smooth_topo , read_in_and_refine
       write(*,*) " max RR in panels "
       write(*,*) maxval(rr_halo(:,:,1) ),maxval(rr_halo(:,:,2) ),maxval(rr_halo(:,:,3) ),  & 
                  maxval(rr_halo(:,:,4) ),maxval(rr_halo(:,:,5) ),maxval(rr_halo(:,:,6) )
      !-----------------------------------------------------
      ! Smooth topography with Conical kernel.
      !   i,j    :: Central point in smoothing window
      !   i2,j2  :: Distal point in smoothing window
      !   wt1p   :: Is a weighting array dim(-ns2:ns2,-ns2:ns2).
      !   wt1ps  :: Is sum of weights wt1p for normalization
      !   diss00 :: Is distance SQRT(g_aa*da*da) spanned by ns2
      !             cells at center of panel. Sh/Could modify 
      !             to be at i,j?
      !   diss   :: Is distance from (i2,j2) to (i,j)
      !-----------------------------------------------------
      DO np=1,6
        DO j=1-nhalo+ns2,ncube+nhalo-ns2
        DO i=1-nhalo+ns2,ncube+nhalo-ns2

           do_smooth_ij = ( ( (rr_halo(i,j,np)>1.0).AND.read_in_and_refine ).OR. new_smooth_topo )

           if (do_smooth_ij) THEN
           !-----------------------------------
           !  Set up conical kernel
           !----------------------------------- 
           wt1p(:,:)=0.
           wt1ps=0.
           ns2x = ns2 /rr_halo( i,j,np )
           do j2=-ns2x,ns2x
           do i2=-ns2x,ns2x
              jjx = j+j2
              iix = i+i2
              dalp = alph(iix)-alph(i)
              dbet = beta(jjx)-beta(j)
              diss = ggaa(i,j)*dalp*dalp + ggbb(i,j)*dbet*dbet + 2.*ggab(i,j)*dalp*dbet

                 diss0r = diss00 * rr_halo( i,j,np )
                 wt1p(i2,j2) = ( 1. - diss0r * sqrt( diss ) )*da_halo(iix,jjx,np)

              if (wt1p(i2,j2)<0.) wt1p(i2,j2)=0.
              wt1ps = wt1ps + wt1p(i2,j2)
           end do
           end do

           !------------------------------
           ! Now smooth over conical kernel
           !-------------------------------
           terr_halo_sm(i,j,np) = 0.
           do j2=-ns2x,ns2x
           do i2=-ns2x,ns2x
              jjx = j+j2
              iix = i+i2
              terr_halo_sm(i,j,np) = terr_halo_sm(i,j,np) + terr_halo(iix,jjx,np)*wt1p(i2,j2)/wt1ps
           end do
           end do
           ! end of smoothing with conical kernel
           !---------------------------------------
           ! Block above is functionally like the one-line smoothing command here:
           !  terr_halo_sm(i,j,np) = SUM( terr_halo_fx( i-ns2:i+ns2  ,  j-ns2:j+ns2   ,np ) )/ ((2.*ns2+1)**2)        

        end if

        END DO  !  i-loop   
        if (mod(j,1) ==0 ) write(*,900,advance='no') achar(13), J, Ncube+2*Nhalo, Np
        END DO  ! j-loop
      END DO  ! panel loop
#endif


      write(*,*) " end / clear "
   end if


900   format( a1, " Smoothed Row ",i4," out of ",i4," Panel=",i2 )

      deallocate( wt1p )
      deallocate( terr_patch )


END SUBROUTINE smooth_intermediate_topo_halo



END MODULE smooth_topo_cube_sph
