!==========================================================================================[HEAD]
!===============================================================[VERSION]
!2020.12.13
!===============================================================[NOTES]
!===============================================================[USE]
!./*.x *.in
!===============================================================[INFILE]
!h          0.1
!lambda     0.1
!===============================================================[DATAFILE]
!===============================================================[OUTFILE]
![my.err]
!*.log
!*.out
!==========================================================================================[MAIN]
program main
implicit none

integer, parameter                  :: q = 8
real (q), parameter                 :: pi = 3.14159265359d0, bohr2angstrom=0.529177210903d0, eang2debye=4.799d0
integer                             :: clock_start, clock_end, clock_rate
integer (q)                         :: i, j, k, l, m, ios, temp_int0, input_tot
real (q)                            :: temp_real0, temp_real (3)
character (len=100)                 :: temp_char0, temp_char (2), leng=" (65 ('='), ", infile, datafile, outfile="3.out", errfile='my.err',logfile="3.log"
integer, parameter                  :: errfile_unit=11, infile_unit=12, datafile_unit=13, outfile_unit=14
character (len=100), allocatable    :: input_list(:)

real (q)                            :: h, lambda, tau, tn
integer (q)                         :: maxj, n0, n1, n
real(q), parameter                  :: t0=0.0_q, tend=1.0_q
real(q), parameter                  :: x0=0.0_q, xend=1.0_q
real (q), allocatable               :: u(:,:), v(:,:), beta(:), xj(:), ustar(:,:)
real (q)                            :: a, func_ustar, error(2)

open (errfile_unit, file=errfile, status='replace')
call system_clock (clock_start)
!===============================================================[READ INFILE]
call get_command_argument(1,infile)
write (errfile_unit, leng//"'[READ INFILE]: "//trim(infile)//"')")
open(infile_unit, file = infile, status = 'old')
input_tot = 2
allocate(input_list(input_tot))
input_list = (/'h','lambda'/)
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
    read (temp_char(2),*) lambda
endselect
rewind (infile_unit)
enddo
close (infile_unit)
!===============================================================[]
maxj = int((xend-x0)/h)
write (errfile_unit, "(A25,I25)") "maxj",maxj
tau = lambda*h*h
write (errfile_unit, "(A25,F25.10)") "tau",tau
allocate (u(0:1,0:maxj))
allocate (xj(0:maxj))
allocate (ustar(0:1,0:maxj))
do j=0,maxj
    xj (j) = x0 +h*j
    ustar(0,j) = func_ustar( xj(j), t0 )
    ustar(1,j) = func_ustar( xj(j), tend)
enddo
u(0,:) = ustar(0,:)
u(1,:) = ustar(0,:)
allocate (v(0:1,0:maxj))
v = u
!===============================================================[]
a = -2.0_q-2.0_q/lambda
allocate (beta(0:maxj-1))
beta(0) = 0.0_q
do j=1, maxj-1
    beta(j) = 1.0_q/(a-beta(j-1))
enddo
!===============================================================[]
write (errfile_unit, leng//"'[COMPUTE]')")
n=0
tn = t0
write (errfile_unit, "(4A25)") "n","tn","n0","n1"
do while (tn<tend)
    n = n+1
    tn = tn + tau
    n0=mod(n,2)
    n1=1-n0
    call sub_forward(lambda, maxj, u(n0,0:maxj), u(n1,1:maxj-1))
    call sub_crank(lambda, maxj, beta(1:maxj-1), v(n0,0:maxj), v(n1,1:maxj-1))
    write (errfile_unit, "(I25,F25.10,2I25)") n,tn,n0,n1
enddo
error(1) = maxval(abs(u(n1,:)-ustar(1,:)))
error(2) = maxval(abs(v(n1,:)-ustar(1,:)))
!===============================================================[OUTPUT]
write (errfile_unit, leng//"'[OUTPUT]: "//trim(outfile)//"')")
open (outfile_unit, file = outfile, status = 'replace')
write (outfile_unit, "(A1,A24,2A25)") "#","x","Forward","Crank"
write (outfile_unit, "(A1,A24,2F25.10)") "#","Error",error(1),error(2)
do j=0,maxj
    write (outfile_unit, "(3F25.10)") xj(j), u(n1,j), v(n1,j)
enddo
close (outfile_unit)
!===============================================================[FINALIZE]
deallocate(u, v, beta, xj, ustar)
call system_clock (clock_end, clock_rate)
write (errfile_unit, ' (A25, F)') 'Elapsed time (s)', (clock_end-clock_start)/real(clock_rate)
close(errfile_unit)
call rename(errfile, logfile)
end program main
!==========================================================================================[function]
function func_ustar(x,t)
implicit none
integer, parameter                  :: q = 8
real (q), parameter                 :: pi = 3.14159265359_q

real (q)                            :: x, t
real (q)                            :: func_ustar

func_ustar = exp(-t*pi**2)*sin(pi*x)

endfunction func_ustar
!==========================================================================================[subroutine]
subroutine sub_forward(lambda, maxj, u0, u1)
implicit none
integer, parameter                  :: q = 8
    
real(q), intent(in)                 :: lambda
integer(q), intent(in)              :: maxj
real(q), intent(in)                 :: u0(0:maxj)

real(q), intent(out)                :: u1(1:maxj-1)

integer (q)                         :: j

do j=1, maxj-1
    u1(j) = u0(j) + lambda*( u0(j+1) -2.0_q*u0(j) +u0(j-1))
enddo

end subroutine sub_forward
!==========================================================================================[subroutine]
subroutine sub_crank(lambda, maxj, beta, u0, u1)
implicit none
integer, parameter                  :: q = 8

real(q), intent(in)                 :: lambda
integer(q), intent(in)              :: maxj
real(q), intent(in)                 :: beta(1:maxj-1)
real(q), intent(in)                 :: u0(0:maxj)

real(q), intent(out)                :: u1(1:maxj-1)

integer (q)                         :: j
real (q)                            :: b
real (q)                            :: y(0:maxj-1)

b = 2.0_q-2.0_q/lambda
y(0) = 0.0_q
do j=1, maxj-1
    y(j) = -u0(j-1) +b*u0(j) -u0(j+1)
    y(j) = (y(j) -y(j-1))*beta(j)
enddo

do j=maxj-1, 1, -1
    u1(j) = y(j) -beta(j)*u1(j+1)
enddo

end subroutine sub_crank
