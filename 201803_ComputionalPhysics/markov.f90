program markov
use mpi
implicit none

  integer :: ntasks,id,rc
  integer(8),allocatable :: status(:)

  real(8) :: x1,x2,r,p,interval=10,h,integral=0,integral_sum
  integer(8) :: t=10000000,i,u=100,xn
  integer(8),allocatable :: y(:),ysum(:)
  real(8),allocatable :: x(:),f(:)
  real(8),parameter :: pi=4*atan(1.d0)
  allocate(y(-u:u-1),x(-u:u-1),f(-u:u-1),ysum(-u:u-1))

  call MPI_INIT(rc)
  call MPI_COMM_SIZE(MPI_COMM_WORLD, ntasks,rc)
  call MPI_COMM_RANK(MPI_COMM_WORLD, id, rc)
  Allocate(Status(MPI_STATUS_SIZE)) 
  
  call random_mpi

  y=0
  h=interval/u
  call random_number(r)
  x1=r*20-10
  do i=1,t
    call random_number(r)
    x2=r*20-10
    p=exp(x1**2-x2**2)
    call random_number(r)
    if (r<p) then
      x1=x2
    end if
    integral=integral+x1**2
    xn=floor(x1/h)
    y(xn)=y(xn)+1
  end do
  call mpi_reduce(y,ysum,4*u,mpi_integer,mpi_sum,0,mpi_comm_world,rc)
  call mpi_reduce(integral,integral_sum,1,mpi_double,mpi_sum,0,mpi_comm_world,rc)
  if (id==0) then
     integral_sum=integral_sum/(t*ntasks)*sqrt(pi)
     print *,'The final integral is',integral_sum
     open(0,file='markov.data')
     do i=-u,u-1
       x(i)=h*(i+.5)
       f(i)=dble(ysum(i))/(h*t*ntasks)
       write(0,*) x(i),f(i),x(i)**2,log(f(i))
     end do
     close(0)
  end if

  deallocate(y,x,ysum,f)
  deallocate(status)
  call mpi_finalize(rc)

end program markov

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
