!==========================================================================================
!===============================================================[VERSION]
!2020.12.12
!===============================================================[NOTES]
!===============================================================[USE]
!./*.x *.in
!===============================================================[INFILE]
!xend   10
!h      0.05
!===============================================================[OUTFILE]
![my.err]
!*.log
!*.out
!==========================================================================================
program main
implicit none

integer, parameter                  :: q = 8
real (q), parameter                :: pi = 3.14159265359d0, bohr2angstrom=0.529177210903d0, eang2debye=4.799d0
integer                             :: clock_start, clock_end, clock_rate
integer (q)                        :: i, j, k, l, m, ios, temp_int0, input_tot
real (q)                           :: temp_real0, temp_real (3)
character (len=100)                 :: temp_char0, temp_char (2), leng=" (65 ('='), ", infile, datafile, outfile="1.out", errfile='my.err',logfile="1.log"
integer, parameter                  :: errfile_unit=11, infile_unit=12, datafile_unit=13, outfile_unit=14
character (len=100), allocatable    :: input_list(:)

real (q)                           :: x0=0.0_q, y0=100.0_q
real (q)                           :: xn, y0n0, y0n1, y1n0, y1n1
real (q)                           :: xend, h

open (errfile_unit, file=errfile, status='replace')
call system_clock (clock_start)
!===============================================================[READ INFILE]
call get_command_argument(1,infile)
write (errfile_unit, leng//"'[READ INFILE]: "//trim(infile)//"')")
open(infile_unit, file = infile, status = 'old')
input_tot = 2
allocate(input_list(input_tot))
input_list = (/'xend','h'/)
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
    read (temp_char(2),*) xend
case (2)
    read (infile_unit, *) temp_char0, temp_char(2)
    write (errfile_unit, "(2A25)") trim(temp_char0), trim(temp_char(2))
    read (temp_char(2),*) h
endselect
rewind (infile_unit)
enddo
close (infile_unit)
!===============================================================[OUTPUT]
write (errfile_unit, leng//"'[OUTPUT]: "//trim(outfile)//"')")
open (outfile_unit, file = outfile, status = 'replace')
write (outfile_unit, '(A1,A24,2A25)') "#","x","ExplicitEuler","ImplicitEuler"
xn = x0
y0n0 = y0
y1n0 = y0
write (outfile_unit, '(3F25.10)') xn, y0n0,y1n0
do while (xn<=xend)
    call sub_euler(0_q,h,y0n0,y0n1)
    call sub_euler(1_q,h,y1n0,y1n1)
    xn = xn+h
    y0n0 = y0n1
    y1n0 = y1n1
    write (outfile_unit, '(3F25.10)') xn, y0n0,y1n0
enddo
close (outfile_unit)
!===============================================================[FINALIZE]
call system_clock (clock_end, clock_rate)
write (errfile_unit, ' (A25, F)') 'Elapsed time (s)', (clock_end-clock_start)/real(clock_rate)
close(errfile_unit)
call rename(errfile, logfile)
end program main
!===============================================================[]
subroutine sub_euler(method,h,yn0,yn1)
implicit none

integer, parameter                  :: q = 8

integer(q), intent(in)              :: method
real(q), intent(in)                 :: yn0
real(q), intent(in)                 :: h

real(q), intent(out)                :: yn1

if (method==0) then
    yn1 = (1.0_q-50.0_q*h)*yn0
elseif (method==1) then
    yn1 = 1.0_q/(1.0_q+50.0_q*h)*yn0
endif

end subroutine sub_euler
