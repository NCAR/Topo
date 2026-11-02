program test_rnodes_v2
  ! Reads profiles.txt (line 1: nprof nsw ; then one profile per line),
  ! runs rnodes (old) and rnodes_v2 (new) on each, writes results to
  ! wedges_f90.txt :  ip  old(3)  new(3)  xlim(2)  rms  lok
  use shr_kind_mod, only: rpx => shr_kind_r8
  use ridge_utils,  only: rnodes, rnodes_v2
  implicit none
  integer :: nprof, nsw, ip, iu, io
  real(rpx), allocatable :: ridge(:), hnodes(:)
  integer,   allocatable :: xnodes(:)
  integer   :: xw_old(3), xw_new(3), xlim(2), dcenter, npeak, nvail, nnode
  real(rpx) :: hw_old(3), hw_new(3), rms
  logical   :: lok

  open(newunit=iu, file='profiles.txt', status='old', action='read')
  read(iu,*) nprof, nsw
  allocate( ridge(nsw+1), hnodes(nsw+1), xnodes(nsw+1) )
  open(newunit=io, file='wedges_f90.txt', status='replace', action='write')
  do ip = 1, nprof
     read(iu,*) ridge
     call rnodes   ( nsw, ridge, xnodes, hnodes, dcenter, xw_old, hw_old, npeak, nvail, nnode )
     call rnodes_v2( nsw, ridge, xw_new, hw_new, xlim, rms, lok )
     write(io,'(i6,3i4,2x,3i4,2x,2i4,f12.5,l3)') ip, xw_old, xw_new, xlim, rms, lok
  end do
  close(iu); close(io)
  print *, 'wrote wedges_f90.txt for', nprof, 'profiles, nsw =', nsw
end program test_rnodes_v2
