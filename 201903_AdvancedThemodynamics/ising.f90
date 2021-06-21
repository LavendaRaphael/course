!==========================================================================================[README]
!---------------------------------------------------------------[VERSION]
!2021.06.10
!@TianFeifei
!---------------------------------------------------------------[NOTES]
!ising model
!---------------------------------------------------------------[Compile]
!mpiifort *.f90 -o *.x
!---------------------------------------------------------------[INFILE]
!---------------------------------------------------------------[USE]
!mpirun ./*.x <lattice_length>
!---------------------------------------------------------------[DATAFILE]
!---------------------------------------------------------------[OUTFILE]
![my.err]
!*.log
!*.out
!==========================================================================================[main]
program ising

  use mpi
  implicit none

  integer :: ntasks,id,rc
  integer(8),allocatable :: status(:)
  integer(8) :: side=100,i,warmup=1000000,working=1000000,x,y,t,times=1000,n=10,sample
  real(8) :: temperature,tmin=1,tmax=4,r,p,deltae,mag,sus
  real(8),allocatable :: map(:,:),mag_t(:,:),mag_t_0(:,:)
  character(len=20) :: arg

  call MPI_INIT(rc)
  call MPI_COMM_SIZE(MPI_COMM_WORLD, ntasks,rc)
  call MPI_COMM_RANK(MPI_COMM_WORLD, id, rc)
  Allocate(Status(MPI_STATUS_SIZE))

  call mpirandom
  
  if (id==0) then
    call get_command_argument(1,arg)
    read(arg,*) side
  end if
  call mpi_bcast(side,1,mpi_int,0,mpi_comm_world,rc)

  sample=working/n
  allocate(map(0:side-1,0:side-1),mag_t(times,2),mag_t_0(times,2))
!  call random_number(map)
!  map=sign(dble(1),map-.5)
  map=1
  mag=sum(map)
  mag_t=0
  do t=1,times
    temperature=tmin+t*(tmax-tmin)/times
    do i=1,working+warmup
      call random_number(r)
      x=floor(r*side)
      call random_number(r)
      y=floor(r*side)
      deltae=2*(map(modulo(x-1,side),y)+map(modulo(x+1,side),y)+map(x,modulo(y-1,side))+map(x,modulo(y+1,side)))*map(x,y)
      p=exp(-deltae/temperature)
      call random_number(r)
      if (r<p) then
        map(x,y)=-map(x,y)
        mag=mag+2*map(x,y)
      end if
      if ((i>warmup).and.(modulo(i,n)==0)) then
        mag_t(t,1)=mag_t(t,1)+abs(mag)
        mag_t(t,2)=mag_t(t,2)+mag**2
      end if
    end do
    mag_t(t,1)=mag_t(t,1)/sample/side**2
    mag_t(t,2)=mag_t(t,2)/sample/side**4
    print *,id,t
  end do

  call mpi_reduce(mag_t,mag_t_0,2*times,mpi_double,mpi_sum,0,mpi_comm_world,rc)
  if (id==0) then
    mag_t_0=mag_t_0/ntasks
    open(1,file='ising_'//trim(adjustl(arg))//'.data')
    do t=1,times
      temperature=tmin+t*(tmax-tmin)/times
      sus=(mag_t_0(t,2)-mag_t_0(t,1)**2)/temperature
      write(1,*) temperature,mag_t_0(t,1),sus
    end do
    close(1)
  end if
  deallocate(map,mag_t,mag_t_0)
 

  deallocate(status)
  call mpi_finalize(rc)

end program ising

subroutine mpirandom
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

end subroutine mpirandom
