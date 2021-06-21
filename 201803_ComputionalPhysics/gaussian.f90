program test

  use mpi

  implicit none

  integer :: ntasks,id,rc
  integer(8),allocatable :: status(:)

  integer(8) :: i,u=100,n=1000000,rn
  real(8),parameter :: pi=4*atan(1.d0)
  real(8) :: v,w,interval=10,h,r
  integer(8),allocatable :: y(:),ysum(:)
  real(8),allocatable :: x(:),f(:)
  allocate(y(-u:u-1),x(-u:u-1),f(-u:u-1),ysum(-u:u-1))

  call MPI_INIT(rc)
  call MPI_COMM_SIZE(MPI_COMM_WORLD, ntasks,rc)
  call MPI_COMM_RANK(MPI_COMM_WORLD, id, rc)
  Allocate(Status(MPI_STATUS_SIZE)) 

  call random_mpi
  
  y=0
  h=interval/u
  do i=1,n
    call random_number(v)
    call random_number(w)
    r=sqrt(abs(-2*log(v)))*cos(2*pi*w)
    if ((-interval <= r) .and. (r < interval)) then
      rn=floor(r/h)
      y(rn)=y(rn)+1
    end if
  end do
  call mpi_reduce(y,ysum,4*u,mpi_integer,mpi_sum,0,mpi_comm_world,rc)
  if (id==0) then
     open(0,file='gaussian.data')
     do i=-u,u-1
       x(i)=h*(i+.5)
       f(i)=dble(ysum(i))/(h*n*ntasks)
       write(0,*) x(i),f(i),x(i)**2,log(f(i))
     end do
     close(0) 
  end if

  deallocate(y,ysum)  
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
