subroutine getdata
    implicit none
    integer::i
    integer::n,clock
    integer,allocatable::seed(:)
    real(8)::r1
    call SYSTEM_CLOCK(clock)
    call RANDOM_SEED(size=n)
    call RANDOM_SEED(put=seed)
    open(unit=1,file='rawdata.dat',status='unknown')
    do i=1,100
        call RANDOM_NUMBER(r1)
        write(1,'(2f12.6)') 0.5d0*(i-1),2.3d0+7.05d0*0.5d0*(i-1)+20.d0*r1
    end do
    close(1)
end subroutine getdata
program least_square_fit
    implicit none
    integer,parameter::dp=selected_real_kind(8)
    integer::i
    real(dp)::x(100),y(100),a,b,xsum,ysum,xsquaresum,xysum
    call getdata
    open(unit=1,file='rawdata.dat',status='old')
    do i=1,100
        read(1,*) x(i),y(i)
    end do
    close(1)
    xsquaresum=0.d0
    xsum=0.d0
    ysum=0.d0
    xysum=0.d0
    do i=1,100
        xsum=xsum+x(i)
        ysum=ysum+y(i)
        xsquaresum=xsquaresum+x(i)*x(i)
        xysum=xysum+x(i)*y(i)
    end do
    a=(ysum*xsquaresum-xsum*xysum)/(100.d0*xsquaresum-xsum*xsum)
    b=(100.d0*xysum-xsum*ysum)/(100.d0*xsquaresum-xsum*xsum)
    print*,'a,b=',a,b
    open(unit=1,file='fittedcure.dat',status='unknown')
    do i=1,100
        write(1,'(2f12.6)') 0.5d0*(i-1),a+b*0.5d0*(i-1)
    end do
    close(1)
end program least_square_fit