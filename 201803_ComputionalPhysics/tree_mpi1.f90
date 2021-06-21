program test

  use mpi
  implicit none

  integer :: ntasks,id,rc
  integer(8),allocatable :: status(:)
  integer(8) :: xmax=1024,ymax=1024,t=1000,i,x0,y0,x,y,j
  integer(8),allocatable :: map(:,:),point(:,:)
  real(8) :: r

  call MPI_INIT(rc)
  call MPI_COMM_SIZE(MPI_COMM_WORLD, ntasks,rc)
  call MPI_COMM_RANK(MPI_COMM_WORLD, id, rc)
  Allocate(Status(MPI_STATUS_SIZE)) 
  
  call mpirandom
  
  allocate(map(0:xmax-1,0:ymax-1))
  allocate(point(0:ntasks-1,2))
  ! map initialize
  map=0
  point=0
  x0=xmax/2
  y0=0
  map(x0,y0)=1
  map(x0+1,y0)=1
  map(x0-1,y0)=1
  map(x0,y0+1)=1
  open(1,file='tree_mpi.data')
  write(1,*) x0,y0,int(0,8)
  do i=1,t
    ! boundary select start point
    call random_number(r)
    if (r<1.d0/3) then
      x=0
      y=floor(ymax*r)
    elseif ((1.d0/3<=r).and.(r<2.d0/3)) then
      x=xmax-1
      y=floor(ymax*r)
    else
      x=floor(xmax*r)
      y=ymax-1
    end if
    ! random walk
    do while (map(x,y)==0)
       call random_number(r)
       if (r<.25) then
         x=modulo(x+1,xmax)
       elseif ((.25<=r).and.(r<.5)) then
         x=modulo(x-1,xmax)
       elseif ((.5<=r).and.(r<.75)) then
         y=modulo(y-1,ymax)
       else
         y=ymax-1-abs(ymax-2-y)
       end if
    end do
    print *,x,y,i,id
    ! process correspondence
    point(id,1)=x
    point(id,2)=y
    do j=0,ntasks-1
      call mpi_bcast(point(j,:),4,mpi_int,j,mpi_comm_world,rc)
      map(point(j,1)+1,point(j,2))=1
      map(point(j,1)-1,point(j,2))=1
      map(point(j,1),point(j,2)+1)=1
      map(point(j,1),max(point(j,2)-1,0))=1
      write(1,*) point(j,1),point(j,2),i
    end do
  end do
  close(1)
 
  deallocate(map,point)
  deallocate(status)
  call mpi_finalize(rc)

end program test

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
