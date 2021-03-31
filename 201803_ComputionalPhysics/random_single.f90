program dla
implicit none

  call random_single

end program dla

subroutine random_single
implicit none

  integer :: n,i,clock,Nin=0
  real :: x
  integer,allocatable :: seed(:)

  call SYSTEM_CLOCk(clock)
  call RANDOM_SEED(size=n)
  allocate(seed(n))
  do i=1,n
    seed(i)=clock+37*i
  end do
  call random_seed(PUT=seed)
  deallocate(seed)

end subroutine random_single
