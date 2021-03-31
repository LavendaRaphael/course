!==========================================================================================
!===============================================================[VERSION]
!2020.10.10
!===============================================================[NOTES]
!===============================================================[USE]
!./dos.x dos.in
!===============================================================[INFILE]
!piece_tot      1000
!y_min          -8
!y_max          8
!y_tot          1000
!===============================================================[DATAFILE]
!===============================================================[OUTFILE]
![my.err]
!dos.log
!dos.out
!==========================================================================================
program main
use mpi
implicit none

integer                   :: ntasks, rank, ierr, stat (MPI_STATUS_SIZE)
integer, parameter               :: DP = 8
real (DP), parameter             :: pi = 3.14159265359d0, bohr2angstrom=0.529177210903d0, eang2debye=4.799d0
integer                          :: clock_start, clock_end, clock_rate
integer (DP)                     :: i, j, k, l, m, ios, temp_int0, input_tot
real (DP)                        :: temp_real0, temp_real (3)
character (len=100)              :: temp_char0, temp_char (2), leng=" (65 ('='), ", infile, datafile, outfile, errfile='my.err',logfile
integer, parameter               :: errfile_unit=11, infile_unit=12, datafile_unit=13, outfile_unit=14
character (len=100), allocatable :: input_list(:)

integer (DP)                     :: piece_tot, y_tot
!integer (DP), allocatable        :: 
!character (len=100), allocatable :: 
real (DP), allocatable           :: intensity_rank(:), intensity(:)
real (DP)                        :: y_min, y_max, dpiece, dx, piece_i, piece_j, piece_area, area, y

call MPI_INIT (ierr)
call MPI_COMM_SIZE (MPI_COMM_WORLD, ntasks, ierr)
call MPI_COMM_RANK (MPI_COMM_WORLD, rank, ierr)
if (rank == 0) then
open (errfile_unit, file=errfile, status='replace')
call system_clock (clock_start)
!===============================================================[READ INFILE]
call get_command_argument(1,infile)
write (errfile_unit, leng//"'[READ INFILE]: "//trim(infile)//"')")
open(infile_unit, file = infile, status = 'old')
input_tot = 4
allocate(input_list(input_tot))
input_list = (/'piece_tot', 'y_min', 'y_max', 'y_tot'/)
do i = 1, input_tot
do while (.true.)
        read (infile_unit, *, iostat = ios) temp_char0
        if (ios < 0) then
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
        read (temp_char(2),*) piece_tot
case (2)
        read (infile_unit, *) temp_char0, temp_char(2)
        write (errfile_unit, "(2A25)") trim(temp_char0), trim(temp_char(2))
        read (temp_char(2),*) y_min
case (3)
        read (infile_unit, *) temp_char0, temp_char(2)
        write (errfile_unit, "(2A25)") trim(temp_char0), trim(temp_char(2))
        read (temp_char(2),*) y_max
case (4)
        read (infile_unit, *) temp_char0, temp_char(2)
        write (errfile_unit, "(2A25)") trim(temp_char0), trim(temp_char(2))
        read (temp_char(2),*) y_tot
endselect
rewind (infile_unit)
enddo
close (infile_unit)
endif
!===============================================================[MPI BCAST]
call MPI_BCAST (piece_tot, 1, MPI_INTEGER8, 0, MPI_COMM_WORLD, ierr)
call MPI_BCAST (y_min, 1, MPI_REAL8, 0, MPI_COMM_WORLD, ierr)
call MPI_BCAST (y_max, 1, MPI_REAL8, 0, MPI_COMM_WORLD, ierr)
call MPI_BCAST (y_tot, 1, MPI_INTEGER8, 0, MPI_COMM_WORLD, ierr)
!===============================================================[COMPUTE]
if (rank==0) then
        write (errfile_unit, leng//"'[COMPUTE]')")
endif
dpiece = 1.d0/piece_tot
dx = (y_max - y_min)/y_tot
allocate (intensity_rank (y_tot), intensity (y_tot))
intensity_rank = 0.d0
do i = 1, piece_tot
if (mod(i,ntasks) /= rank) cycle
if (rank == 0) then
        write (errfile_unit,"(I25)") i
endif
piece_i = (i-0.5d0)*dpiece
do j = 1, piece_tot
        piece_j = (j-0.5d0)*dpiece
        y = sqrt(3.d0+2.d0*cos(2*pi*(piece_j-piece_i))+2*cos(2*pi*piece_i)+2*cos(2*pi*piece_j))
        temp_int0 = ceiling((y-y_min)/dx)
        intensity_rank (temp_int0) = intensity_rank (temp_int0) + 
enddo
enddo
call MPI_REDUCE (intensity_rank, intensity, y_tot, MPI_REAL8, MPI_SUM, 0, MPI_COMM_WORLD, ierr)
if (rank == 0) then
area = sum(intensity)
write(errfile_unit, "(A25,F22.10,A3)") "intensity_sum",area
area = area*dx
intensity = intensity/area
!===============================================================[OUTPUT]
outfile = 'dos.out'
write (errfile_unit, leng//"'[OUTPUT]: "//trim(outfile)//"')")
open (outfile_unit, file = outfile, status = 'replace')
write (outfile_unit, "(A,A24,A25)") "#","E","DOS"
do i = 1, y_tot
        write (outfile_unit, "(2F25.10)") y_min+dx*(i-0.5), intensity(i)
enddo
close (outfile_unit)
!===============================================================[FINALIZE]
call system_clock (clock_end, clock_rate)
write (errfile_unit, ' (A25, F)') 'Elapsed time (s)', (clock_end-clock_start)/real(clock_rate)
close(errfile_unit)
logfile='dos.log'
call rename(errfile, logfile)
endif
call mpi_finalize (ierr)
end program main
