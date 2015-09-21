module rotation
!  use shr_kind_mod, only: r8 => shr_kind_r8
private
public rotby
public rotby2
public rotby3

contains
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  FUNCTION ROTBY( AA,N,THETA ) RESULT(AR)
    implicit none
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  INTEGER,            intent(IN) ::  N

  REAL,               intent(IN) ::  AA(N,N),THETA
  REAL                           ::  AR(N,N)

  real :: x(n),y(n)
  real :: xp(n,n),yp(n,n)
  real :: r(n,n),wt(n,n)
  real :: THETRAD,PI,swt
  integer :: i,j,l,m,n2

  PI = 2*ACOS(0.0)
  THETRAD = theta*(PI/180.)

  n2=n/2
  do i=1,n
     x(i)=-n2+i*1.-.5
  end do
  n2=n/2
  do i=1,n
     y(i)=-n2+i*1.-.5
  end do

  do j=1,n
  do i=1,n

     xp(i,j)=x(i)*cos(thetrad) - y(j)*sin(thetrad)
     yp(i,j)=y(j)*cos(thetrad) + x(i)*sin(thetrad)
     

  end do
  end do

  do j=1,n
  do i=1,n
     
     if ( (xp(i,j)<x(1)).or.(xp(i,j)>x(n)).or.(yp(i,j)<y(1)).or.(yp(i,j)>y(n)) ) then
        ar(i,j)=-99999.9
     else
        ar(i,j)=1.
     endif

  end do
  end do

  do j=1,n
  do i=1,n
     
     r = (xp - x(i))**2 + (yp-y(j))**2

     where( r <= 2. )
          wt=2.-r
     elsewhere
          wt=0.
     endwhere

     swt = sum(wt)
     if (swt > 0.) ar(i,j) = sum( aa*wt )/swt

  end do
  end do


end FUNCTION ROTBY
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  FUNCTION ROTBY2( AA,N,THETA ) RESULT(AR)
    implicit none
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  INTEGER,            intent(IN) ::  N

  REAL,               intent(IN) ::  AA(N,N),THETA
  REAL                           ::  AR(N,N)

  real :: x(n),y(n)
  real :: xp(n,n),yp(n,n)
  real :: r(n,n),wt(n,n)
  real :: THETRAD,PI,swt
  integer :: i,j,l,m,n2,ii,jj,i1,j1,r00,r10,r01,r11

  PI = 2*ACOS(0.0)
  THETRAD = -theta*(PI/180.)

  n2=n/2
  do i=1,n
     x(i)=-n2+i*1. -1. - 0.5
  end do
  n2=n/2
  do i=1,n
     y(i)=-n2+i*1. -1. - 0.5
  end do

  do j=1,n
  do i=1,n

     xp(i,j)=x(i)*cos(thetrad) - y(j)*sin(thetrad)
     yp(i,j)=y(j)*cos(thetrad) + x(i)*sin(thetrad)
     

  end do
  end do

  do j=1,n
  do i=1,n
     
     if ( (xp(i,j)<x(1)).or.(xp(i,j)>x(n)).or.(yp(i,j)<y(1)).or.(yp(i,j)>y(n)) ) then
        ar(i,j)=-9999999.9
     else
        ii=count( x<=xp(i,j))
        jj=count( y<=yp(i,j))
        !!ar(i,j)=aa(ii,jj)
        i1=ii+1
        j1=jj+1
        if (ii==n) i1=n
        if (jj==n) j1=n
        r00 = 2.-(xp(i,j) - x(ii))**2 + (yp(i,j) - y(ii))**2
        r10 = 2.-(xp(i,j) - x(i1))**2 + (yp(i,j) - y(ii))**2
        r01 = 2.-(xp(i,j) - x(ii))**2 + (yp(i,j) - y(i1))**2
        r11 = 2.-(xp(i,j) - x(i1))**2 + (yp(i,j) - y(i1))**2
        ar(i,j) = ( r00*aa(ii,jj)+r10*aa(i1,jj)+r01*aa(ii,j1)+r11*aa(i1,j1) ) / &
                                   (r00+r01+r10+r11)


     endif

  end do
  end do


    


!write(111) aa
!write(111) ar
!write(111) xp
!write(111) yp

end FUNCTION ROTBY2

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  FUNCTION ROTBY3( AA,N,THETA ) RESULT(AR)
    implicit none
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  INTEGER,            intent(IN) ::  N

  REAL,               intent(IN) ::  AA(N,N),THETA
  REAL                           ::  AR(N,N)

  real :: x(n),y(n)
  real :: xp(n,n),yp(n,n)
  real :: r(n,n),wt(n,n)
  real :: THETRAD,PI,swt
  integer :: i,j,l,m,n2,ii,jj,i1,j1,r00,r10,r01,r11,ic,d00

  PI = 2*ACOS(0.0)
  THETRAD = -theta*(PI/180.)

  n2=n/2
  do i=1,n
     x(i)=-n2+i*1. - 0.5
  end do
  n2=n/2
  do i=1,n
     y(i)=-n2+i*1. - 0.5
  end do

  do j=1,n
  do i=1,n

     xp(i,j)=x(i)*cos(thetrad) - y(j)*sin(thetrad)
     yp(i,j)=y(j)*cos(thetrad) + x(i)*sin(thetrad)
     

  end do
  end do

  do j=1,n
  do i=1,n
     
     if ( (xp(i,j)<x(1)).or.(xp(i,j)>x(n)).or.(yp(i,j)<y(1)).or.(yp(i,j)>y(n)) ) then
        ar(i,j)=-9999999.9
     else

        do ic=1,n-1
           if ( x(ic+1) > xp(i,j) ) then
              ii=ic
              exit
           end if
        end do  
        do ic=1,n-1
           if ( y(ic+1) > yp(i,j) ) then
              jj=ic
              exit
           end if
        end do  

        !ii=count( x<=xp(i,j))
        !jj=count( y<=yp(i,j))
        !!ar(i,j)=aa(ii,jj)
        i1=ii+1
        j1=jj+1
        if (ii==n) i1=n
        if (jj==n) j1=n

        r00=0.
        r10=0.
        r01=0.
        r11=0.
        
        if ( ( abs( xp(i,j) - x(ii) ) <= 0.5 ) .and. ( abs( yp(i,j) - y(jj) ) <= 0.5 ) ) then
           r00=1.
        end if
        if ( ( abs( xp(i,j) - x(ii) ) > 0.5 ) .and. ( abs( yp(i,j) - y(jj) ) <= 0.5 ) ) then
           r10=1.
        end if
        if ( ( abs( xp(i,j) - x(ii) ) <= 0.5 ) .and. ( abs( yp(i,j) - y(jj) ) > 0.5 ) ) then
           r01=1.
        end if
         if ( ( abs( xp(i,j) - x(ii) ) > 0.5 ) .and. ( abs( yp(i,j) - y(jj) ) > 0.5 ) ) then
           r11=1.
        end if
           
        ar(i,j) = ( r00*aa(ii,jj)+r10*aa(i1,jj)+r01*aa(ii,j1)+r11*aa(i1,j1) ) / &
                                   (r00+r01+r10+r11)


     endif

  end do
  end do

end FUNCTION ROTBY3


end module rotation
