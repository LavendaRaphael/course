program dla
implicit none
  
  integer(8) :: xmax=1024,ymax=512,t=16000,i,x0,y0,x,y
  integer(8),allocatable :: map(:,:)
  real(8) :: r

  call singlerandom
  allocate(map(0:xmax-1,0:ymax-1))
  map=0
  x0=xmax/2
  y0=0
  map(x0,y0)=1
  map(x0+1,y0)=1
  map(x0-1,y0)=1
  map(x0,y0+1)=1
  open(1,file='tree.data')
  write(1,*) x0,y0,int(0,8)
  do i=1,t
    call random_number(r)
    if (r<1.d0/3) then
      x=0
      y=floor(ymax*r)
    elseif ((1.d0/3<=r).and.(r<2.d0/3)) then
      x=ymax-1
      y=floor(ymax*r)
    else
      x=floor(xmax*r)
      y=ymax-1
    end if
    do while (map(x,y)==0)
       call random_number(r)
       if (r<.25) then
         x=modulo(x+1,xmax)
       elseif ((.25<=r).and.(r<.5)) then
         x=modulo(x-1,xmax)
       elseif ((.5<=r).and.(r<.75)) then
         y=modulo(y-1,ymax)
       else
         !y=modulo(y+1,ymax)
         y=ymax-1-abs(ymax-2-y)
       end if
    end do
    map(x,y)=1
    map(x+1,y)=1
    map(x-1,y)=1
    map(x,y+1)=1
    map(x,max(y-1,0))=1
    print *,x,y,i
    write(1,*) x,y,i
  end do
  close(1)

end program dla

subroutine singlerandom
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

end subroutine singlerandom
