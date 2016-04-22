subroutine medianbinning2d(n,nx,ny,x0,dx,y0,dy,x,y,z,npb,binmed,info)
  implicit none
  ! arguments
  integer, intent(in) :: n, nx, ny
  real, intent(in) :: x0, dx, y0, dy, x(n), y(n), z(n)
  integer, intent(out) :: npb(nx,ny), info
  real, intent(out) :: binmed(nx,ny)
  ! local variables
  real :: zcopy(n)
  integer :: ixybin(n), ixbin(n), iybin(n), ord(n)
  integer :: i, j, k, i0, i1
  integer :: ix, iy, iz, ixy
  integer :: nrun

  ! initialise the output arrays
  info = 0
  npb = 0
  binmed = -999.0

  ! make a local copy
  do i=1,n
    zcopy(i) = z(i)
  end do

  ! assign each value a bin
  do i=1,n
    ! assume regularly spaced bins
    ix = int((x(i) - x0)/dx) + 1
    iy = int((y(i) - y0)/dy) + 1
    ! range check: ix
    if((ix .le. 0) .or. (ix .gt. nx)) then
      write(*,*) 'ix',ix,'out of bounds...'
      info = -1
      return
    end if
    ! range check: iy
    if((iy .le. 0) .or. (iy .gt. ny)) then
      write(*,*) 'iy',iy,'out of bounds...'
      info = -2
      return
    end if
    ! a single index that combines both bin indices
    ixy = (ix-1)*ny + iy
    !
    ixbin(i) = ix
    iybin(i) = iy
    ixybin(i) = ixy
  end do
  ! group together all values for individual bins
  call qsort_int(ixybin,ord,n)
  ! sort the indices and the values
  ixybin = ixybin(ord)
  ixbin = ixbin(ord)
  iybin = iybin(ord)
  zcopy  = zcopy(ord)
  ! 
  i1 = 0
  do while(i1 .lt. n) 
    ! find the values within this bin
    i0 = i1 + 1
    i1 = i0
    do j=i0+1,n
      if(ixybin(i0) .ne. ixybin(j)) then
        i1 = j-1
        exit
      end if
    end do

    ! see how many there are
    nrun = i1 - i0 + 1
    ! store the number of values
    npb(ixbin(i0), iybin(i0)) = nrun
    ! calculate the median
    binmed(ixbin(i0), iybin(i0)) = median(zcopy(i0:i1),nrun)
  end do

  Return
End Subroutine Medianbinning2d

function median(x,n) result(med)
  implicit none
  ! arguments
  integer, intent(in)  :: n
  real,    intent(in)  :: x(n)
  real                 :: med
  ! local variables
  integer :: ord(n)
  integer :: k
  
  ! easy cases:
  if(n .eq. 1) then
     med = x(1)
  else if(n .eq. 2) then
     med = (x(1) + x(2)) * 0.5
  else
     ! standard case (need to sort)

     ! sort this list of values
     call qsort_real(x, ord, n)
     ! two cases for the median
     if(mod(n,2) .eq. 0) then
        ! even cases: take the mean of the two middle values
        k =  n/2
        med = (x(ord(k)) + x(ord(k+1)))*0.5
     else 
        ! odd cases: take the sole middle value
        k = (n+1)/2
        med = x(ord(k))
     end if
  end if

  return
end function median
