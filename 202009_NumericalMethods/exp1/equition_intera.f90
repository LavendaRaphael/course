!==========================================================================================
!===============================================================[VERSION]
!2020.11.04
!===============================================================[NOTES]
!202009_NumericalMethods
!Experiment 1
!===============================================================[USE]
!./*.x *.in
!===============================================================[INFILE]
!method     0   #0 for Jacobi; 1 for Gauss-Seidel or SOR
!N          10
!myepsilon  1E-6
!omega      1   #1 for Gauss-Seidel; >1 for SOR
!===============================================================[DATAFILE]
!===============================================================[OUTFILE]
![my.err]
!equition_intera.log
!*.out
!==========================================================================================
program main
implicit none

integer, parameter                  :: DP = 8
real (DP), parameter                :: pi = 4.D0*ATAN(1.D0), bohr2angstrom=0.529177210903d0, eang2debye=4.799d0
integer                             :: clock_start, clock_end, clock_rate
integer (DP)                        :: i, j, k, l, m, ios, temp_int0, input_tot
real (DP)                           :: temp_real0, temp_real (3)
character (len=100)                 :: temp_char0, temp_char (2), leng=" (65 ('='), ", infile, datafile, outfile="*.out", errfile='my.err',logfile
integer, parameter                  :: errfile_unit=11, infile_unit=12, datafile_unit=13, outfile_unit=14
character (len=100), allocatable    :: input_list(:)

integer (DP)                        :: method, N, k0, k1, intera
!integer (DP), allocatable           ::
!character (len=100), allocatable    :: 
real (DP), allocatable              :: u(:,:,:), f_pri(:,:), u_star(:,:)
real (DP)                           :: h, myepsilon, omega, norm
open (errfile_unit, file=errfile, status='replace')
call system_clock (clock_start)
!===============================================================[READ INFILE]
call get_command_argument(1,infile)
write (errfile_unit, leng//"'[READ INFILE]: "//trim(infile)//"')")
open(infile_unit, file = infile, status = 'old')
input_tot = 4
allocate(input_list(input_tot))
input_list = (/'method','N','myepsilon','omega'/)
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
    read (temp_char(2),*) method
case (2)
    read (infile_unit, *) temp_char0, temp_char(2)
    write (errfile_unit, "(2A25)") trim(temp_char0), trim(temp_char(2))
    read (temp_char(2),*) N
case (3)
    read (infile_unit, *) temp_char0, temp_char(2)
    write (errfile_unit, "(2A25)") trim(temp_char0), trim(temp_char(2))
    read (temp_char(2),*) myepsilon
case (4)
    read (infile_unit, *) temp_char0, temp_char(2)
    write (errfile_unit, "(2A25)") trim(temp_char0), trim(temp_char(2))
    read (temp_char(2),*) omega
endselect
rewind (infile_unit)
enddo
close (infile_unit)
!===============================================================[READ DATAFILE]
!===============================================================[COMPUTE]
write (errfile_unit, leng//"'[COMPUTE]')")
h=1.d0/N
write (errfile_unit, "(A25,F25.10)") 'h=',h
allocate(u(0:1,0:N,0:N),f_pri(1:N-1,1:N-1),u_star(1:N-1,1:N-1))
u=0.d0
temp_real0=pi*h
do i=1,N-1
do j=1,i-1
    u_star(i,j)=u_star(j,i)
enddo
do j=i,N-1
    u_star(i,j)=sin(temp_real0*i)*sin(temp_real0*j)
enddo
enddo
f_pri=u_star*pi**2.d0*0.5d0*h**2.d0
if (method==1) then
    f_pri=f_pri*omega
endif
!===============================================================[INTERATION]
write (errfile_unit, leng//"'[INTERATION]')")
intera=0
k1=0
norm=maxval(abs(u(k1,1:N-1,1:N-1)-u_star(1:N-1,1:N-1)))
write (errfile_unit, "(3A25)") 'intera','k1','norm'
write (errfile_unit, "(I25,I25,F25.10)") intera,k1,norm
do while (norm>=myepsilon)
intera=intera+1
k0=mod(intera-1,2)
k1=mod(intera,2)
select case (method)
case (0)
    do i=1,N-1
    do j=1,N-1
        u(k1,i,j) = 0.25d0 *(u(k0,i,j-1) +u(k0,i-1,j) +u(k0,i+1,j) +u(k0,i,j+1)) +f_pri(i,j)
    enddo
    enddo
case (1)
    do i=1,N-1
    do j=1,N-1
        u(k1,i,j) = 0.25d0*omega*(u(k1,i,j-1) +u(k1,i-1,j) +u(k0,i+1,j) +u(k0,i,j+1))+(1.d0-omega)*u(k0,i,j) +f_pri(i,j)
    enddo
    enddo
case default
    write (*, *) "ERROR: see 'my.err' for details!"
    write (errfile_unit, *) "ERROR: parameter 'method' invalid in "//trim(infile)//"!"
    stop
endselect
norm=maxval(abs(u(1,1:N-1,1:N-1)-u(0,1:N-1,1:N-1)))
write (errfile_unit, "(I25,I25,F25.10)") intera,k1,norm
enddo
norm=maxval(abs(u(k1,1:N-1,1:N-1)-u_star(1:N-1,1:N-1)))
write (errfile_unit, "(A50,F25.10)") 'Difference to the exact solution:',norm
!===============================================================[FINALIZE]
call system_clock (clock_end, clock_rate)
write (errfile_unit, ' (A25, F)') 'Elapsed time (s)', (clock_end-clock_start)/real(clock_rate)
logfile=infile(:index(infile,'.in'))//'log'
write (errfile_unit, leng//"'[OUTPUT]: "//trim(logfile)//"')")
close(errfile_unit)
call rename(errfile, logfile)
end program main
