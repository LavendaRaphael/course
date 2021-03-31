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
character (len=100)                 :: temp_char0, temp_char (2), leng=" (65 ('='), ", infile, datafile, outfile="1.3.out", errfile='my.err',logfile="1.3.log"
integer, parameter                  :: errfile_unit=11, infile_unit=12, datafile_unit=13, outfile_unit=14
character (len=100), allocatable    :: input_list(:)

integer (DP)                        :: npiece=10000
!integer (DP), allocatable           :: 
!character (len=100), allocatable    :: 
!real (DP), allocatable              :: 
real (DP)                           :: xk0,xk1, xmin=-1.d0, xmax=1.d0, piece, se0,se1,se01,mse, f, p4
open (errfile_unit, file=errfile, status='replace')
call system_clock (clock_start)
!===============================================================[READ INFILE]
!===============================================================[OUTPUT]
write (errfile_unit, leng//"'[OUTPUT]: "//trim(outfile)//"')")
open (outfile_unit, file = outfile, status = 'replace')
write (outfile_unit, '(A1,A24,5A25)')
piece = (xmax-xmin)/npiece
mse = 0.d0
write (outfile_unit, '(A1,A24,3A25)') "#","x","f","p4","se"
do i = 0,npiece-1
    xk0 = xmin+i*piece
    xk1 = xk0+piece
    call subr_f(xk0,f)
    call subr_p4(xk0,p4)
    call subr_se(xk0,se0)
    call subr_se(xk1,se1)
    call subr_se((xk0+xk1)/2.d0,se01)
    temp_real0 = se0 + 4.d0*se01 + se1
    write (outfile_unit, '(4F25.10)') xk0, f, p4, se0
    mse = mse + temp_real0
enddo
close(outfile_unit)
mse = sqrt (mse*piece/6.d0)
write (errfile_unit, ' (A25, F25.10)') 'mse=',mse
!===============================================================[FINALIZE]
call system_clock (clock_end, clock_rate)
write (errfile_unit, ' (A25, F)') 'Elapsed time (s)', (clock_end-clock_start)/real(clock_rate)
close(errfile_unit)
call rename(errfile, logfile)
end program main
!===============================================================[subr_f]
subroutine subr_f(x,f)
implicit none
integer, parameter                  :: DP = 8
real(DP),intent(in)                 :: x
real(DP),intent(out)                :: f

f=1.d0/(1.d0+25.d0*x**2)

end subroutine subr_f
!===============================================================[subr_p4]
subroutine subr_p4(x,p4)
implicit none

integer, parameter                  :: DP = 8
real(DP), parameter                  :: a=0.66942, b=-2.30554, c=1.86886
real(DP),intent(in)                 :: x
real(DP),intent(out)                :: p4

p4=a+b*x**2+c*x**4

end subroutine subr_p4
!===============================================================[subr_se]
subroutine subr_se(x,se)
implicit none

integer, parameter                  :: DP = 8
real(DP),intent(in)                 :: x
real(DP),intent(out)                :: se
real(DP)                            :: f,p4

call subr_f(x,f)
call subr_p4(x,p4)
se=(f-p4)**2

end subroutine subr_se

