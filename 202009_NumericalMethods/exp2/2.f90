!==========================================================================================
!===============================================================[VERSION]
!2020.11.28
!===============================================================[NOTES]
!===============================================================[USE]
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
character (len=100)                 :: temp_char0, temp_char (2), leng=" (65 ('='), ", infile, datafile, outfile="2.out", errfile='my.err',logfile="2.log"
integer, parameter                  :: errfile_unit=11, infile_unit=12, datafile_unit=13, outfile_unit=14
character (len=100), allocatable    :: input_list(:)

integer (DP)                        :: intera,k0,k1,method
!integer (DP), allocatable           :: 
!character (len=100), allocatable    :: 
character (len=100)                 :: method_name(1:3)=(/'Jacobi','Gauss','Newton'/)
!real (DP), allocatable              :: 
real (DP)                           :: xk(0:1,2), norm, x0(1:2)=(/0.d0,0.d0/), myepsilon=1.d-6, xstar(1:2)=(/1.d0,1.d0/)
open (errfile_unit, file=errfile, status='replace')
call system_clock (clock_start)
!===============================================================[READ INFILE]
!===============================================================[OUTPUT]
do method=1,3
write (errfile_unit, leng//"'"//method_name(method)//"')")
intera=0
k1=0
xk(k1,1:2)=x0
norm=maxval(abs(xk(k1,1:2)-xstar(1:2)))
write (errfile_unit, "(5A25)") 'k1','intera',"x1","x2",'norm'
write (errfile_unit, "(I25,I25,3F25.10)") k1,intera,xk(k1,1:2),norm
do while (norm>=myepsilon)
    intera=intera+1
    k0=mod(intera-1,2)
    k1=mod(intera,2)
    select case(method)
    case(1)    
        call subr_jacobi (xk(k0,1:2), xk(k1,1:2))
    case(2)
        call subr_gauss (xk(k0,1:2), xk(k1,1:2))
    case(3)
        call subr_newton (xk(k0,1:2), xk(k1,1:2))
    end select
    norm = maxval (abs(xk(k1,1:2) -xstar(1:2)))
    write (errfile_unit, "(I25,I25,3F25.10)") k1,intera,xk(k1,1:2),norm
enddo
enddo
!===============================================================[FINALIZE]
call system_clock (clock_end, clock_rate)
write (errfile_unit, ' (A25, F)') 'Elapsed time (s)', (clock_end-clock_start)/real(clock_rate)
close(errfile_unit)
call rename(errfile, logfile)
end program main
!===============================================================[subr_f]
subroutine subr_jacobi(xk0,xk1)
implicit none
integer, parameter                  :: DP = 8
real(DP),intent(in)                 :: xk0(1:2)
real(DP),intent(out)                :: xk1(1:2)

xk1(1) = 0.1d0 *(xk0(1)**2 +xk0(2)**2 +8.d0)
xk1(2) = 0.1d0 *(xk0(1)*xk0(2)**2 +xk0(1) +8.d0)

end subroutine subr_jacobi
!===============================================================[subr_f]
subroutine subr_gauss(xk0,xk1)
implicit none
integer, parameter                  :: DP = 8
real(DP),intent(in)                 :: xk0(1:2)
real(DP),intent(out)                :: xk1(1:2)

xk1(1) = 0.1d0 *(xk0(1)**2 +xk0(2)**2 +8.d0)
xk1(2) = 0.1d0 *(xk1(1)*xk0(2)**2 +xk1(1) +8.d0)

end subroutine subr_gauss
!===============================================================[subr_f]
subroutine subr_newton(xk0,xk1)
implicit none
integer, parameter                  :: DP = 8
real(DP),intent(in)                 :: xk0(1:2)
real(DP),intent(out)                :: xk1(1:2)

real(DP)                            :: fx(1:2), fpx(1:2,1:2),deltax(1:2)

fx(1) = xk0(1)**2 -10.d0*xk0(1) +xk0(2)**2 +8.d0
fx(2) = xk0(1)*xk0(2)**2 +xk0(1) -10.d0*xk0(2) +8.d0

fpx(1,1) = 2.d0*xk0(1) -10.d0
fpx(1,2) = 2.d0*xk0(2)
fpx(2,1) = xk0(2)**2 +1.d0
fpx(2,2) = 2.d0*xk0(1)*xk0(2) -10.d0

deltax(1) = fx(1)*fpx(2,2) -fx(2)*fpx(1,2)
deltax(2) = -fx(1)*fpx(2,1) +fx(2)*fpx(1,1)
deltax = -deltax /(fpx(1,1)*fpx(2,2) -fpx(1,2)*fpx(2,1))

xk1 = xk0 + deltax

end subroutine subr_newton

