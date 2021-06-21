program dla
implicit none

  integer(8) :: side=50,i,j,warmup=1000000,x,y,t,sample,n=10,list(5)
  real(8) :: temperature=1,r,p,deltae,mag
  real(8),allocatable :: map(:,:),mag_i(:,:)
  character(len=20) :: arg

  call random_single

  call get_command_argument(1,arg)
  read(arg,*) temperature
  list=(/40,80,100,150,200/)
  sample=warmup/n
  allocate(mag_i(sample,5))
  do j=1,5
    side=list(j)
    allocate(map(0:side-1,0:side-1))
!    call random_number(map)
!    map=sign(dble(1),map-.5)
    map=1
    mag=sum(map)
    do i=1,warmup
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
      if (modulo(i,n)==0) then
        mag_i(i/n,j)=abs(mag)/side**2
      end if
    end do
    print *,side
    deallocate(map)
  end do
  open(1,file='warmup_'//trim(adjustl(arg))//'.data')
  write(1,*) 'warmup',list
  do i=1,sample
    write(1,*) i*n,mag_i(i,:)
  end do
  close(1)
  deallocate(mag_i)

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
