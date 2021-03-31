program test

  use mpi
  implicit none

  integer :: ntasks,id,rc
  integer(8),allocatable :: status(:)

  call MPI_INIT(rc)
  call MPI_COMM_SIZE(MPI_COMM_WORLD, ntasks,rc)
  call MPI_COMM_RANK(MPI_COMM_WORLD, id, rc)
  Allocate(Status(MPI_STATUS_SIZE)) 

  call random_mpi
  
  deallocate(status)
  call mpi_finalize(rc)

end program test

subroutine random_mpi
 
  use mpi
  
  implicit none

  integer :: ntasks,id,rc
  integer :: n,i,clock,Nin=0
  real :: x
  integer,allocatable :: seed(:),status(:)

  call MPI_COMM_SIZE(MPI_COMM_WORLD, ntasks,rc)
  call MPI_COMM_RANK(MPI_COMM_WORLD, id, rc)
  Allocate(Status(MPI_STATUS_SIZE))

  if (id==0) then
    call SYSTEM_CLOCk(clock)
    call RANDOM_SEED(size=n)
    allocate(seed(n))
    do i=1,n
      seed(i)=clock+37*i
    end do
    call random_seed(PUT=seed)
    deallocate(seed)
    do i=1,ntasks-1
      call random_number(x)
      clock = clock+int(x*1000000)
      call MPI_SEnD(clock,1,MPI_LONG,i,i,MPI_COMM_WORLD,rc)
    end do
  else
    call MPI_RECV(clock,1,MPI_LONG,0,id,MPI_COMM_WORLD,status,rc)
    call random_seed(size=n)
    allocate(seed(n))
    do i=1,n
      seed(i)=clock+37*i
    end do
    call random_seed(PUT=seed)
    deallocate(seed)
  end if
  deallocate(status)

end subroutine random_mpi
