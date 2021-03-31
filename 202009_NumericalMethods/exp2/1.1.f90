!==========================================================================================
!===============================================================[VERSION]
!2020.11.28
!===============================================================[NOTES]
!===============================================================[USE]
!./*.x *.in
!===============================================================[INFILE]
!===============================================================[DATAFILE]
!===============================================================[OUTFILE]
![my.err]
!*.log
!*.out
!==========================================================================================
program main
implicit none

integer, parameter                  :: DP = 8
real (DP), parameter                :: pi = 3.14159265359d0, bohr2angstrom=0.529177210903d0, eang2debye=4.799d0
integer                             :: clock_start, clock_end, clock_rate
integer (DP)                        :: i, j, k, l, m, ios, temp_int0, input_tot
real (DP)                           :: temp_real0, temp_real (3)
character (len=100)                 :: temp_char0, temp_char (2), leng=" (65 ('='), ", infile, datafile, outfile="1.1.out", errfile='my.err',logfile="my.log"
integer, parameter                  :: errfile_unit=11, infile_unit=12, datafile_unit=13, outfile_unit=14
character (len=100), allocatable    :: input_list(:)

integer (DP)                        :: npiece=1000
!integer (DP), allocatable           :: 
!character (len=100), allocatable    :: 
!real (DP), allocatable              :: 
real (DP)                           :: x,f,ln, xmin=-1.d0, xmax=1.d0, piece
open (errfile_unit, file=errfile, status='replace')
call system_clock (clock_start)
!===============================================================[READ INFILE]
!===============================================================[OUTPUT]
write (errfile_unit, leng//"'[OUTPUT]: "//trim(outfile)//"')")
open (outfile_unit, file = outfile, status = 'replace')
write (outfile_unit, '(A1,A24,5A25)') "#","x","f","l2","l4","l8","l16"
piece=(xmax-xmin)/npiece

do i=0,npiece
    x=xmin+i*piece
    write (outfile_unit, '(6F25.10)') x,f(x),ln(xmin,xmax,x,2),ln(xmin,xmax,x,4),ln(xmin,xmax,x,8),ln(xmin,xmax,x,16)
enddo
close (outfile_unit)
!===============================================================[FINALIZE]
call system_clock (clock_end, clock_rate)
write (errfile_unit, ' (A25, F)') 'Elapsed time (s)', (clock_end-clock_start)/real(clock_rate)
close(errfile_unit)
call rename(errfile, logfile)
end program main

function f(x)
implicit none
integer, parameter                  :: DP = 8
real(DP)                            :: x
real(DP)                            :: f

f=1.d0/(1.d0+25.d0*x**2)

end function f

function ln(xmin,xmax,x,n)
implicit none

integer, parameter                  :: DP = 8
real(DP)                            :: x,xmin,xmax
integer                             :: n
real(DP)                            :: temp_real0
integer(DP)                         :: i,j,k
real(DP)                            :: lj,ln,f
real(DP),allocatable                :: xi(:),yi(:)

allocate(xi(0:n),yi(0:n))
temp_real0=(xmax-xmin)/n

do i=0,n
    xi(i)=xmin+temp_real0*i
    yi(i)=f(xi(i))
enddo
ln=0.d0
do j=0,n
    lj=1.d0
    do k=0,n
        if (k==j) cycle
        lj=lj*(x-xi(k))/(xi(j)-xi(k))
    enddo
    ln=ln+lj*yi(j)
enddo

end function 
