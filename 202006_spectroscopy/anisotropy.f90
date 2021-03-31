!==========================================================================================
!===============================================================[VERSION]
!2020.06.23
!===============================================================[NOTES]
!===============================================================[USE]
!./anisotropy.x anisotropy.in
!===============================================================[INFILE]
!w_x            6
!w_y            6
!w_z            12
!piece_tot      1000
!w_min          5
!w_max          13
!w_tot          1000
!===============================================================[DATAFILE]
!===============================================================[OUTFILE]
![my.err]
!anisotropy.log
!anisotropy.out
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

integer (DP)                     :: piece_tot, w_tot
!integer (DP), allocatable        :: 
!character (len=100), allocatable :: 
real (DP), allocatable           :: intensity_rank(:), intensity(:)
real (DP)                        :: w_x, w_y, w_z, w_min, w_max, drad, dw, theta, phi, piece_area, area, w

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
input_tot = 7
allocate(input_list(input_tot))
input_list = (/'w_x','w_y','w_z', 'piece_tot', 'w_min', 'w_max', 'w_tot'/)
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
        read (temp_char(2),*) w_x
case (2)
        read (infile_unit, *) temp_char0, temp_char(2)
        write (errfile_unit, "(2A25)") trim(temp_char0), trim(temp_char(2))
        read (temp_char(2),*) w_y
case (3)
        read (infile_unit, *) temp_char0, temp_char(2)
        write (errfile_unit, "(2A25)") trim(temp_char0), trim(temp_char(2))
        read (temp_char(2),*) w_z
case (4)
        read (infile_unit, *) temp_char0, temp_char(2)
        write (errfile_unit, "(2A25)") trim(temp_char0), trim(temp_char(2))
        read (temp_char(2),*) piece_tot
case (5)
        read (infile_unit, *) temp_char0, temp_char(2)
        write (errfile_unit, "(2A25)") trim(temp_char0), trim(temp_char(2))
        read (temp_char(2),*) w_min
case (6)
        read (infile_unit, *) temp_char0, temp_char(2)
        write (errfile_unit, "(2A25)") trim(temp_char0), trim(temp_char(2))
        read (temp_char(2),*) w_max
case (7)
        read (infile_unit, *) temp_char0, temp_char(2)
        write (errfile_unit, "(2A25)") trim(temp_char0), trim(temp_char(2))
        read (temp_char(2),*) w_tot
endselect
rewind (infile_unit)
enddo
close (infile_unit)
endif
!===============================================================[MPI BCAST]
call MPI_BCAST (w_x, 1, MPI_REAL8, 0, MPI_COMM_WORLD, ierr)
call MPI_BCAST (w_y, 1, MPI_REAL8, 0, MPI_COMM_WORLD, ierr)
call MPI_BCAST (w_z, 1, MPI_REAL8, 0, MPI_COMM_WORLD, ierr)
call MPI_BCAST (piece_tot, 1, MPI_INTEGER8, 0, MPI_COMM_WORLD, ierr)
call MPI_BCAST (w_min, 1, MPI_REAL8, 0, MPI_COMM_WORLD, ierr)
call MPI_BCAST (w_max, 1, MPI_REAL8, 0, MPI_COMM_WORLD, ierr)
call MPI_BCAST (w_tot, 1, MPI_INTEGER8, 0, MPI_COMM_WORLD, ierr)
!===============================================================[COMPUTE]
if (rank==0) then
        write (errfile_unit, leng//"'[COMPUTE]')")
endif
drad = pi/2.d0/piece_tot
dw = (w_max - w_min)/w_tot
allocate (intensity_rank (w_tot), intensity (w_tot))
intensity_rank = 0.d0
do i = 1, piece_tot
if (mod(i,ntasks) /= rank) cycle
if (rank == 0) then
        write (errfile_unit,"(I25)") i
endif
theta = (i-0.5d0)*drad
piece_area = (cos((i-1)*drad)-cos(i*drad))*drad
do j = 1, piece_tot
        phi = (j-0.5d0)*drad
        w = (w_x*(cos(phi))**2+w_y*(sin(phi))**2)*(sin(theta))**2+w_z*(cos(theta))**2
        temp_int0 = ceiling((w-w_min)/dw)
        intensity_rank (temp_int0) = intensity_rank (temp_int0) + piece_area
enddo
enddo
call MPI_REDUCE (intensity_rank, intensity, w_tot, MPI_REAL8, MPI_SUM, 0, MPI_COMM_WORLD, ierr)
if (rank == 0) then
area = sum(intensity)
write(errfile_unit, "(A25,F22.10,A3)") "intensity_sum",area/pi,"pi"
area = area*dw
intensity = intensity/area
!===============================================================[OUTPUT]
outfile = 'anisotropy.out'
write (errfile_unit, leng//"'[OUTPUT]: "//trim(outfile)//"')")
open (outfile_unit, file = outfile, status = 'replace')
write (outfile_unit, "(A,A24,A25)") "#","w","intensity"
do i = 1, w_tot
        write (outfile_unit, "(2F25.10)") w_min+dw*(i-0.5), intensity(i)
enddo
close (outfile_unit)
!===============================================================[FINALIZE]
call system_clock (clock_end, clock_rate)
write (errfile_unit, ' (A25, F)') 'Elapsed time (s)', (clock_end-clock_start)/real(clock_rate)
close(errfile_unit)
logfile='anisotropy.log'
call rename(errfile, logfile)
endif
call mpi_finalize (ierr)
end program main
