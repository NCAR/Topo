
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
                                    , command_line_arguments&
                                    , str_dir, str_source, smooth_topo_fname)

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
    LOGICAL, INTENT(IN)  :: lread_smooth_topofile    ! , lsmooth_topo_cubesph
    LOGICAL, INTENT(IN)  :: luse_prefilter, lstop_after_smoothing
    LOGICAL, INTENT(IN)  :: lregional_refinement
    CHARACTER(len=1024), INTENT(IN   )           :: str_dir, str_source
    CHARACTER(len=1024), INTENT(OUT)             :: ofname
    character(len=1024), INTENT(IN   )           :: command_line_arguments !for writing netCDF file
    CHARACTER(len=1024), INTENT(IN   ), optional :: smooth_topo_fname


    REAL (KIND=dbl_kind),                                            &
         DIMENSION(1-nhalo:ncube+nhalo, 1-nhalo:ncube+nhalo, 6) :: terr_halo
    REAL (KIND=dbl_kind),                                            &
         DIMENSION(1-nhalo:ncube+nhalo, 1-nhalo:ncube+nhalo, 6) :: terr_halo_sm, terr_halo_dev
    REAL (KIND=dbl_kind),                                            &
         DIMENSION(1-nhalo:ncube+nhalo, 1-nhalo:ncube+nhalo, 6) :: da_halo, rr_halo
    REAL (KIND=dbl_kind), &
            DIMENSION(ncube,ncube,6) :: daxx


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
      write(*,*) " Read precomputed filtered topography from ",trim(smooth_topo_fname)
      if (lregional_refinement) then
!check can not overwrite rrfac - I think we need merge from Julio
        !        call read_topo_smooth_data(smooth_topo_fname,ncube*ncube*6,terr_sm,terr_dev,rr_fac=rrfac)
      else
        call read_topo_smooth_data(smooth_topo_fname,ncube*ncube*6,terr_sm,terr_dev)
      end if
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
          "('_nc',i0.4,'_Co',i0.3,'_Fi',i0.3)" ) & 
          ncube, NSCL_c/2, NSCL_f/2
     ofname = 'topo_smooth_'//trim(str_source)//trim(ofname)
     if (lregional_refinement) then
       ofname= TRIM(str_dir)//'/'//trim(ofname)//'_VRtest.nc'
     else
       ofname= TRIM(str_dir)//'/'//trim(ofname)//'.nc'
     end if

     write(*,*) " Will do smoothing of topo on cubed sphere "
     write(*,*) " Output will go to: ",trim(ofname)

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

  
      if (lregional_refinement) then
!xxx        call wrtncdf_topo_smooth_data(ncube*ncube*6,terr_sm,terr_dev,ofname,command_line_arguments,rr_fac=rrfac)
      else
        call wrtncdf_topo_smooth_data(ncube*ncube*6,terr_sm,terr_dev,ofname,command_line_arguments)
      end if

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

subroutine wrtncdf_topo_smooth_data(n,terr_sm,terr_dev,output_fname,command_line_arguments,rr_fac)
  use shr_kind_mod, only: r8 => shr_kind_r8
  implicit none
  
#     include         <netcdf.inc>
    
  !
  ! Dummy arguments
  !
  integer, intent(in) :: n
  real(r8),dimension(n),           intent(in) :: terr_sm,terr_dev
  character(len=1024),             intent(in) :: output_fname
  real(r8),dimension(n), optional, intent(in) :: rr_fac
  character(len=1024),             intent(in) :: command_line_arguments

  integer            :: foutid     ! Output file id
  integer            :: lonvid
  integer            :: latvid
  integer            :: terr_smid, terr_devid, rr_fac_id
  integer            :: status
  character (len=8)  :: datestring
  integer, dimension(2) :: nid
    
  real(r8), parameter :: fillvalue = 1.d36
  integer, dimension(1) :: latdim
  character(len=1024) :: str
    
    
  !
  !  Create NetCDF file for output
  !
  print *,"Create NetCDF file for output: ",trim(output_fname)
  status = nf_create (trim(output_fname), NF_64BIT_OFFSET , foutid)
  if (status .ne. NF_NOERR) call handle_err(status)
  !
  ! Meta data for CESM compliance
  !
  !-data_summary		|	Short paragraph about the data.
  !-data_creator 		| 	Name and email of the person who created the dataset
  !-cesm_contact    	        |     	The liaison of the relevant WG
  !-creation_date    	        |     	Full date of dataset creation
  !-update_date    	        |     	Full date of most recent modification
  !-history    		        |     	Updates to changes made to the data.
  !-data_script    	        |     	script to generate data (will be available in the SVN repository ?)
  !-data_description_url 	|     	A web-page with a description if available  (this could be the climatedataguide webpage.)
  !-data_source_url    	        |     	The web page where the raw data can be downloaded
  !-data_reference    	        |     	Full reference for the dataset if available
  !-data_doi    		|     	If doi of data exists
  !-climo_years    	        |     	Year 1-year N of the climatological averaging period.
  !-data_mods    		|     	Any special substantive (non resolution) modifications that were made to the input data set purely for the purpose of using it in CESM. 
  !
  str = 'Smoothed topo data for quicker generation of topography data'
  status = nf_put_att_text (foutid,NF_GLOBAL,'data_summary',LEN(TRIM(str)), TRIM(str))
  if (status .ne. NF_NOERR) call handle_err(status)
  
  call DATE_AND_TIME(DATE=datestring)
  status = nf_put_att_text (foutid,NF_GLOBAL,'creation_date',8, TRIM(datestring) )
  if (status .ne. NF_NOERR) call handle_err(status)
  
  str = 'Peter Hjort Lauritzen and Julio Bacmeister'
  status = nf_put_att_text (foutid,NF_GLOBAL,'cesm_contact',LEN(TRIM(str)), TRIM(str))
  if (status .ne. NF_NOERR) call handle_err(status)
  
  str = 'https://github.com/NCAR/Topo.git'
  status = nf_put_att_text (foutid,NF_GLOBAL,'data_source',LEN(TRIM(str)), TRIM(str))
  if (status .ne. NF_NOERR) call handle_err(status)
  
  status = nf_put_att_text (foutid,NF_GLOBAL,'data_script',LEN(TRIM(command_line_arguments)), TRIM(command_line_arguments))
  if (status .ne. NF_NOERR) call handle_err(status)
  
  str = TRIM('Lauritzen, P. H. et al.: NCAR global model topography generation software for unstructured grids, '// &
       'Geosci. Model Dev., 8, 1-12, doi:10.5194/gmd-8-1-2015, 2015.')
  status = nf_put_att_text (foutid,NF_GLOBAL,'data_reference',LEN(TRIM(str)), TRIM(str))
  if (status .ne. NF_NOERR) call handle_err(status)
  
  
  
  !  status = nf_put_att_text (foutid,NF_GLOBAL,'data_script',LEN(TRIM(command_line_arguments)), TRIM(command_line_arguments))
  !  if (status .ne. NF_NOERR) call handle_err(status)
  !
  ! Create dimensions for output
  !
  status = nf_def_dim (foutid, 'ncol', n, nid(1))
  if (status .ne. NF_NOERR) call handle_err(status)
  
  print *,"Create variable for output: terr_sm"
  status = nf_def_var (foutid,'terr_sm', NF_DOUBLE, 1, nid(1), terr_smid)
  if (status .ne. NF_NOERR) call handle_err(status)

  status = nf_put_att_text (foutid,terr_smid,'long_name', 21, 'precomputed smoothed height field')
  status = nf_put_att_text (foutid,terr_smid,'units', 1, 'm')
  status = nf_put_att_double (foutid, terr_smid, 'missing_value', nf_double, 1, fillvalue)
  status = nf_put_att_double (foutid, terr_smid, '_FillValue'   , nf_double, 1, fillvalue)
  
  print *,"Create variable for output: terr_dev"
  status = nf_def_var (foutid,'terr_dev', NF_DOUBLE, 1, nid(1), terr_devid)
  if (status .ne. NF_NOERR) call handle_err(status)

  status = nf_put_att_text (foutid,terr_devid,'long_name', 82, &
       'precomputed difference between intermdiate cubed-sphere height and smoothed height')
  status = nf_put_att_text (foutid,terr_devid,'units', 1, 'm')
  status = nf_put_att_double (foutid, terr_devid, 'missing_value', nf_double, 1, fillvalue)
  status = nf_put_att_double (foutid, terr_devid, '_FillValue'   , nf_double, 1, fillvalue)

  if (present(rr_fac)) then
    print *,"Create variable for output: rr_fac"
    status = nf_def_var (foutid,'rr_fac', NF_DOUBLE, 1, nid(1), rr_fac_id)
    if (status .ne. NF_NOERR) call handle_err(status)    
  end if
  !
  ! End define mode for output file
  !
  status = nf_enddef (foutid)
  if (status .ne. NF_NOERR) call handle_err(status)
  
  print*,"writing terr",MINVAL(terr_sm),MAXVAL(terr_sm)
  status = nf_put_var_double (foutid, terr_smid, terr_sm)
  if (status .ne. NF_NOERR) call handle_err(status)
  
  print*,"writing terr_dev",MINVAL(terr_dev),MAXVAL(terr_dev)
  status = nf_put_var_double (foutid, terr_devid, terr_dev)
  if (status .ne. NF_NOERR) call handle_err(status)

  if (present(rr_fac)) then
    print*,"writing rr_fac",MINVAL(rr_fac),MAXVAL(rr_fac)
    status = nf_put_var_double (foutid, rr_fac_id, rr_fac)
    if (status .ne. NF_NOERR) call handle_err(status)
  end if

  
  print *,"close file"
  status = nf_close (foutid)
  if (status .ne. NF_NOERR) call handle_err(status)
end subroutine wrtncdf_topo_smooth_data

subroutine read_topo_smooth_data(fname,ncol,terr_sm,terr_dev,rr_fac)
  use shr_kind_mod, only: r8 => shr_kind_r8
  implicit none
#     include         <netcdf.inc>
  character(len=1024),      intent(in)  :: fname
  integer,                  intent(in)  :: ncol
  real(r8),dimension(ncol), intent(out) :: terr_sm,terr_dev
  real(r8),dimension(ncol), intent(out), optional :: rr_fac

  integer :: ncid,status, dimid, alloc_error, terr_sm_id,terr_dev_id,ncol_file, rr_fac_id

  status = nf_open(TRIM(fname) , 0, ncid)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR(STATUS)
  
  status = NF_INQ_DIMID(ncid,'ncol', dimid)
  IF (status .NE. NF_NOERR) CALL HANDLE_ERR(status)

  status = NF_INQ_DIMLEN(ncid, dimid, ncol_file)
  IF (status .NE. NF_NOERR) CALL HANDLE_ERR(status)
  
  if (ncol.ne.ncol_file) then
    write(*,*) "Mismatch between specified smoothed topo data dimension: ",ncol_file,ncol
    stop
  end if

  status = NF_INQ_VARID(ncid, 'terr_sm', terr_sm_id)
  IF (status .NE. NF_NOERR) CALL HANDLE_ERR(status)
  
  status = NF_GET_VAR_DOUBLE(ncid, terr_sm_id,terr_sm)
  IF (status .NE. NF_NOERR) CALL HANDLE_ERR(status)

  status = NF_INQ_VARID(ncid, 'terr_dev', terr_dev_id)
  IF (status .NE. NF_NOERR) CALL HANDLE_ERR(status)
  
  status = NF_GET_VAR_DOUBLE(ncid, terr_dev_id,terr_dev)
  IF (status .NE. NF_NOERR) CALL HANDLE_ERR(status)

  if (present(rr_fac)) then
    status = NF_GET_VAR_DOUBLE(ncid, rr_fac_id,rr_fac)
    IF (status .NE. NF_NOERR) CALL HANDLE_ERR(status)
  end if

  status = nf_close (ncid)
  if (status .ne. NF_NOERR) call handle_err(status)  
end subroutine read_topo_smooth_data



END MODULE smooth_topo_cube_sph
