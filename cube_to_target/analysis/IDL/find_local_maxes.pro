pro find_local_maxes,terr_dev_halo

  
    nhalo=0
    DO np = 1, 6
    DO j=1-nhalo,ncube+nhalo
    DO i=1-nhalo,ncube+nhalo
       terr_max_halo(i,j,np) = 0._r8
    END DO
    END DO
    END DO
    
  
    write(*,*) "performing a diff with 3x3 to find 'peaks' "

    DO np = 1, 6
    DO j=1-nhalo+1,ncube+nhalo-1
    DO i=1-nhalo+1,ncube+nhalo-1

      terr_dev_halox(i,j,np) =       &
         terr_dev_halo(i+1,j,np) -   &
         (  terr_dev_halo(i+1,j,np)+ &
            terr_dev_halo(i-1,j,np)+ &
            terr_dev_halo(i,j+1,np)+ &
            terr_dev_halo(i,j-1,np)+ & 
            terr_dev_halo(i+1,j-1,np)+ & 
            terr_dev_halo(i-1,j-1,np)+ & 
            terr_dev_halo(i+1,j+1,np)+ & 
            terr_dev_halo(i-1,j+1,np)+ & 
            terr_dev_halo(i,j,np) ) /9.

    END DO
    END DO
             print, " FACE = ",np
    END DO

    terr_dev_halo = terr_dev_halox

    DO np = 1, 6
    DO j=1-nhalo+1,ncube+nhalo-1
    DO i=1-nhalo+1,ncube+nhalo-1

      if ( ( terr_dev_halo(i,j,np) > terr_dev_halo(i+1,j,np)   ) .and. &
           ( terr_dev_halo(i,j,np) > terr_dev_halo(i-1,j,np)   ) .and. &
           ( terr_dev_halo(i,j,np) > terr_dev_halo(i,j+1,np)   ) .and. &
           ( terr_dev_halo(i,j,np) > terr_dev_halo(i,j-1,np)   ) .and. & 
           ( terr_dev_halo(i,j,np) > terr_dev_halo(i+1,j-1,np)   ) .and. & 
           ( terr_dev_halo(i,j,np) > terr_dev_halo(i-1,j-1,np)   ) .and. & 
           ( terr_dev_halo(i,j,np) > terr_dev_halo(i+1,j+1,np)   ) .and. & 
           ( terr_dev_halo(i,j,np) > terr_dev_halo(i-1,j+1,np)   ) .and. & 
           ( terr_dev_halo(i,j,np) > 0. )  ) then 

                  terr_max_halo(i,j,np) = terr_dev_halo(i,j,np)
       END IF

    END DO
    END DO
             write(*,*) " FACE = ",np
    END DO

    do np=1,6
       terr_max(1:ncube,1:ncube,np) = terr_max_halo(1:ncube,1:ncube,np )
    end do

     

    npeaks = count(  (terr_max > thsh) )
