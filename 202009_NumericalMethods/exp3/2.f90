!===============================================================[VERSION]
!2020.12.13
!===============================================================[NOTES]
!===============================================================[USE]
!./*.x *.in
!===============================================================[INFILE]
!h      0.01
!tend   10.0
!===============================================================[DATAFILE]
!===============================================================[OUTFILE]
![my.err]
!*.log
!*.out
!==========================================================================================
program main
implicit none

integer, parameter                  :: q = 8
real (q), parameter                 :: pi = 3.14159265359d0, bohr2angstrom=0.529177210903d0, eang2debye=4.799d0
integer                             :: clock_start, clock_end, clock_rate
integer (q)                         :: i, j, k, l, m, ios, temp_int0, input_tot
real (q)                            :: temp_real0, temp_real (3)
character (len=100)                 :: temp_char0, temp_char (2), leng=" (65 ('='), ", infile, datafile, outfile="2.out", errfile='my.err',logfile="2.log"
integer, parameter                  :: errfile_unit=11, infile_unit=12, datafile_unit=13, outfile_unit=14
character (len=100), allocatable    :: input_list(:)

real(q), parameter                  :: t0=0.0_q, x0=3.0_q, y0=2.0_q
real (q)                            :: h, tend
real (q)                            :: tn, xn0(3), yn0(3), xn1(3), yn1(3)

open (errfile_unit, file=errfile, status='replace')
call system_clock (clock_start)
!===============================================================[READ INFILE]
call get_command_argument(1,infile)
write (errfile_unit, leng//"'[READ INFILE]: "//trim(infile)//"')")
open(infile_unit, file = infile, status = 'old')
input_tot = 2
allocate(input_list(input_tot))
input_list = (/'h','tend'/)
do i = 1, input_tot
do while (.true.)
    read (infile_unit, *, iostat = ios) temp_char0
    if (ios < 0) then
        write (*, *) "ERROR: see 'my.err' for details!"
        write (errfile_unit, *) "ERROR: parameter '"//trim(input_list(i))//"' not found in "//trim(infile)//"!"
        stop
    endif
    if (temp_char0 /= input_list(i)) cycle
    backspace(infile_unit)
    exit
enddo
select case (i)
case (1)
    read (infile_unit, *) temp_char0, temp_char(2)
    write (errfile_unit, "(2A25)") trim(temp_char0), trim(temp_char(2))
    read (temp_char(2),*) h
case (2)
    read (infile_unit, *) temp_char0, temp_char(2)
    write (errfile_unit, "(2A25)") trim(temp_char0), trim(temp_char(2))
    read (temp_char(2),*) tend
endselect
rewind (infile_unit)
enddo
close (infile_unit)
!===============================================================[OUTPUT]
write (errfile_unit, leng//"'[OUTPUT]: "//trim(outfile)//"')")
open (outfile_unit, file = outfile, status = 'replace')
write (outfile_unit, '(A1,A24,6A25)') "#","t","Trapezoid","","Euler","","Runge-Kutta",""
tn = t0
xn0 = x0
yn0 = y0
write (outfile_unit, '(7F25.10)') tn, xn0(1), yn0(1), xn0(2), yn0(2),xn0(3), yn0(3)
do while (tn<=tend)
    call sub_trapezoid(h,xn0(1),yn0(1),xn1(1),yn1(1))
    call sub_euler(h,xn0(2),yn0(2),xn1(2),yn1(2))
    call sub_runge(h,xn0(3),yn0(3),xn1(3),yn1(3))
    tn = tn+h
    xn0(:) = xn1(:)
    yn0(:) = yn1(:)
    write (outfile_unit, '(7F25.10)') tn, xn0(1), yn0(1), xn0(2), yn0(2),xn0(3), yn0(3)
enddo
close (outfile_unit)
!===============================================================[FINALIZE]
call system_clock (clock_end, clock_rate)
write (errfile_unit, ' (A25, F)') 'Elapsed time (s)', (clock_end-clock_start)/real(clock_rate)
close(errfile_unit)
call rename(errfile, logfile)
end program main
!===============================================================[]
subroutine sub_trapezoid(h,xn0,yn0,xn1,yn1)
implicit none
integer, parameter                  :: q = 8

real(q), intent(in)                 :: h, xn0, yn0
real(q), intent(out)                :: xn1,yn1
real(q)                             :: comatr(2,2), temp_real0

comatr(1,1) = 1.0_q-2.25_q*h**2
comatr(1,2) = 4.5_q*h
comatr(2,1) =-2.0_q*h
comatr(2,2) = comatr(1,1)
temp_real0  = 1.0_q+2.25_q*h**2
comatr = comatr/temp_real0

xn1 = xn0*comatr(1,1) + yn0*comatr(1,2)
yn1 = xn0*comatr(2,1) + yn0*comatr(2,2)

end subroutine sub_trapezoid
!===============================================================[]
subroutine sub_euler(h,xn0,yn0,xn1,yn1)
implicit none
integer, parameter                  :: q = 8

real(q), intent(in)                 :: h, xn0, yn0
real(q), intent(out)                :: xn1,yn1
real(q)                             :: func_f, func_g

xn1 = xn0 + h*func_f(yn0)
yn1 = yn0 + h*func_g(xn0)

end subroutine sub_euler
!===============================================================[]
subroutine sub_runge(h,xn0,yn0,xn1,yn1)
implicit none
integer, parameter                  :: q = 8

real(q), intent(in)                 :: h, xn0, yn0
real(q), intent(out)                :: xn1,yn1
real(q)                             :: l(4), k(4)
real(q)                             :: func_f, func_g

l(1) = func_f(yn0)
k(1) = func_g(xn0)
l(2) = func_f(yn0 +h/2.0_q*k(1))
k(2) = func_g(xn0 +h/2.0_q*l(1))
l(3) = func_f(yn0 +h/2.0_q*k(2))
k(3) = func_g(xn0 +h/2.0_q*l(2))
l(4) = func_f(yn0 +h*k(3))
k(4) = func_g(xn0 +h*l(3))

xn1 = xn0 + h/6.0_q*( l(1) +2.0_q*l(2) +2.0_q*l(3) +l(4))
yn1 = yn0 + h/6.0_q*( k(1) +2.0_q*k(2) +2.0_q*k(3) +k(4))

end subroutine sub_runge
!===============================================================[]
function func_f(y)
implicit none
integer, parameter                  :: q = 8

real(q)                             :: y, func_f

func_f = 4.5_q*y

end function func_f
!===============================================================[]
function func_g(x)
implicit none
integer, parameter                  :: q = 8

real(q)                             :: x, func_g

func_g = -2.0_q*x

end function func_g

