module CAMS
  INTEGER, PARAMETER, public :: dp = SELECTED_REAL_KIND(12, 60)
  integer :: ncid

  integer, parameter :: NDIMS = 4, NRECS = 1
  integer, parameter :: NLVLS = 1, NLATS = 400, NLONS = 700
  character (len = *), parameter :: LVL_NAME = "level"
  character (len = *), parameter :: LAT_NAME = "latitude"
  character (len = *), parameter :: LON_NAME = "longitude"
  character (len = *), parameter :: TIME_NAME = "time"
  character (len = 10 ), dimension (6) :: chem2read = (/ &
    "co_conc   ",& 
    "no2_conc  ",&
    "o3_conc   ",& 
    "pm10_conc ",& 
    "pm2p5_conc",& 
    "so2_conc  " /)

  ! The start and count arrays will tell the netCDF library where to
  ! read our data.
  integer :: start(NDIMS), count(NDIMS)

  real :: lats(NLATS), lons(NLONS) 
  integer :: lon_varid, lat_varid, time_varid
  integer :: nrt,id

  character (len = 100) :: chem2analyse
  integer :: data_varid, AIQ, AIQMAX(49), polind(49)

  real, dimension(:,:,:,:), allocatable :: data2read_temp
  real, dimension(:,:,:,:,:), allocatable :: data2read
  real, dimension (:), allocatable :: times

 
  ! Loop indices
  integer :: lvl, lat, lon, rec, i, tim, ii, if,k,j
  real :: dummy
  real :: chemvalue
  integer :: ntime
end module CAMS



program macc_read_and_aiq
  use netcdf
  use cams
  implicit none
  real(dp) :: chem2out

  !
  call readfiles_and_prepare 
  !

  
  
  ! Check to make sure we got what we expected.
!  do lat = 1, NLATS
!     if (lats(lat) /= START_LAT + (lat - 1) * 5.0) stop 2
!      print*,lat,lats(lat)
!  end do
!  do lon = 1, NLONS
!     if (lons(lon) /= START_LON + (lon - 1) * 5.0) stop 2
!      print*,lon,lons(lon)
!  end do
!  do tim = 1, NTIME
!     if (lons(lon) /= START_LON + (lon - 1) * 5.0) stop 2
!      !print*,times(tim)
!  end do

  ! Get the varids of the pressure and temperature netCDF variables.

  ! Read 1 record of NLONS*NLATS*NLVLS values, starting at the beginning 
  ! of the record (the (1, 1, 1, rec) element in the netCDF file).
  !count = (/ NLONS, NLATS, NLVLS, NTIME /)
  !start = (/ 1, 1, 1, 1 /)

  ! Read the surface pressure and temperature data from the file, one
  ! record at a time.
  !do rec = 1, NRECS
     lat=1
     lon=1

    II=1
    do I=1,NLATS
       do j=1,NLONS
          do tim=1,ntime
            AIQMAX(tim)=1
            polind(tim)=0
            do k=1,6
              chemvalue=data2read(k,I,J,1,tim)
              forprint_conc(tim,k)=chemvalue
              !print*,chem2read(k)
              call AIQ_per_polluntant(chemvalue,chem2read(k), AIQ)
              if(AIQ > AIQMAX (tim))then
                AIQMAX(tim)=AIQ
                polind(tim)=k
              endif
              forprint_aiq(tim,k)=AIQ
              !print*,forprint_aiq(tim,k),forprint_conc(tim,k)
            enddo
          enddo
          !write(30,'(I8,150(";",F15.8,";",I1))')II,((forprint_conc(tim,k),forprint_aiq(tim,k),k=1,6),tim=1,25)
          write(30,'(I8,50(";",A1,"{",I2,",",I1,"}",A1))')II,(char(34),AIQMAX(tim),polind(tim),char(34),tim=1,ntime)
          ii=ii+1
    enddo
  enddo


stop


!  call check( nf90_close(ncid) )

contains
  subroutine AIQ_per_polluntant(chemvalue, chem2analyse, AIQ)
    implicit none
    real, intent (in) :: chemvalue
    character (len = 10) :: chem2analyse
    integer, intent (out) :: AIQ
    
    select case (adjustl(chem2analyse))
      case ("o3_conc")
      chem2out=chem2out*1E-3*24.45D0/48.00D0
      if ( chem2out <= 0.064 ) then
        AIQ=1
      else if (chem2out >= 0.065 .AND. chem2out <= 0.084) then
        AIQ=2
      else if (chem2out >= 0.085 .AND. chem2out <= 0.104) then
        AIQ=3
      else if (chem2out >= 0.105 .AND. chem2out <= 0.124) then
        AIQ=4
      else if (chem2out >= 0.125 .AND. chem2out <= 0.404) then
        AIQ=5
      else if (chem2out >= 0.405 .AND. chem2out <= 0.600) then
      AIQ=6
      endif
      !write(*,'(F7.3,1X,I1)')chem2out,AIQ
    case ("no2_conc")
      chem2out=chem2out*1E-3*24.45D0/46.01D0
      if ( chem2out <= 0.64 ) then
        AIQ=1
      else if (chem2out >= 0.65 .AND. chem2out <= 1.24) then
        AIQ=5
      else if (chem2out >= 1.25 .AND. chem2out <= 2.04) then
        AIQ=6
      endif               
      !write(*,'(F7.3,1X,I1)')chem2out,AIQ
    case ("so2_conc")
      chem2out=chem2out*1E-3*24.45D0/64.06D0
      if ( chem2out <= 0.03) then
        AIQ=1
      else if (chem2out > 0.03 .AND. chem2out <= 0.14) then
        AIQ=2
      else if (chem2out > 0.14 .AND. chem2out <= 0.22) then
        AIQ=3
      else if (chem2out > 0.22 .AND. chem2out <= 0.30) then
        AIQ=4
      else if (chem2out > 0.30 .AND. chem2out <= 0.60) then
        AIQ=5
      else if (chem2out >= 0.60 .AND. chem2out <= 1) then
        AIQ=6
      endif
      !write(*,'(F7.3,1X,I1)')chem2out,AIQ
    case ("co_conc")
      chem2out=chem2out*1E-3*24.45D0/28.01D0
      if ( chem2out <= 4.4 ) then
        AIQ=1
      else if (chem2out >= 4.5 .AND. chem2out <= 9.4) then
        AIQ=2
      else if (chem2out >= 9.5 .AND. chem2out <= 12.4) then
        AIQ=3
      else if (chem2out >= 12.5 .AND. chem2out <= 15.4) then
        AIQ=4
      else if (chem2out >= 15.5 .AND. chem2out <= 30.4) then
        AIQ=5
      else if (chem2out >= 30.5 .AND. chem2out <= 50.4) then
        AIQ=6
      endif
      !write(*,'(F7.3,1X,I1)')chem2out,AIQ
    case ("pm2p5_conc")
      chem2out=chem2out
      if ( chem2out <= 15 ) then
        AIQ=1
      else if (chem2out > 15 .AND. chem2out <= 40) then
        AIQ=2
      else if (chem2out > 40 .AND. chem2out <= 65) then
        AIQ=3
      else if (chem2out > 65 .AND. chem2out <= 150) then
        AIQ=4
      else if (chem2out > 150 .AND. chem2out <= 250) then
        AIQ=5
      else if (chem2out >= 250 .AND. chem2out <= 500) then
        AIQ=6
      endif
      !write(*,'(F7.3,1X,I1)')chem2out,AIQ
    case ("pm10_conc")
      chem2out=chem2out
      if ( chem2out <= 50 ) then
        AIQ=1
      else if (chem2out > 50 .AND. chem2out <= 150) then
        AIQ=2
      else if (chem2out >= 150 .AND. chem2out <= 250) then
        AIQ=3
      else if (chem2out > 250 .AND. chem2out <= 350) then
        AIQ=4
      else if (chem2out > 350 .AND. chem2out <= 420) then
        AIQ=5
      else if (chem2out >= 420 .AND. chem2out <= 600) then
        AIQ=6
      endif
      !write(*,'(F7.3,1X,I1)')chem2out,AIQ
    end select
  end subroutine AIQ_per_polluntant
  
  subroutine check(status)
    integer, intent ( in) :: status
    
    if(status /= nf90_noerr) then 
      print *, trim(nf90_strerror(status))
      stop 2
    end if
  end subroutine check  

  subroutine nearst(x,xinp,vecsize, xnear, i)
    integer, intent (out) :: i
    integer, intent (in) :: vecsize
    real(KIND=8), dimension (vecsize) :: xinp
    real(KIND=8) :: x, xnear

  end subroutine nearst 

  subroutine readfiles_and_prepare
    implicit none
    INTEGER  :: arg_count,reason
    CHARACTER(len=100) :: arg
    LOGICAL  :: file2read_exists=.FALSE.
    LOGICAL  :: file_exists=.FALSE.
    character(len=100), dimension(24) :: file2read
    character(len=100) :: var

      open(10,file="files.dat")
!      read(10,*)ntime
      allocate(data2read_temp(NLONS, NLATS, NLVLS, NTIME))
      allocate(data2read(25,NLONS, NLATS, NLVLS, NTIME))

       forprint_conc(25+24,6)
       forprint_aiq(25+24,6)
       allocate(times(ntime))


    count = (/ NLONS, NLATS, NLVLS, NTIME /)
    start = (/ 1, 1, 1, 1 /)


      do i=1,6
        read(10,'(A)')file2read(i)
        print*,file2read(i)
        var=adjustl(file2read(i))
        call check( nf90_open(var, nf90_nowrite, ncid) )

        if (i == 1)then
          call check( nf90_inq_varid(ncid, LAT_NAME, lat_varid) )
          call check( nf90_inq_varid(ncid, LON_NAME, lon_varid) )
          call check( nf90_inq_varid(ncid, TIME_NAME, time_varid) )
          call check( nf90_get_var(ncid, lat_varid, lats) )
          call check( nf90_get_var(ncid, lon_varid, lons) )
          call check( nf90_get_var(ncid, time_varid, times) )
          print*, time_varid,times
          k=1
          do ii=1,NLATS
            do j=1,NLONS
              if(lons(j) > 300 )then
                dummy=lons(j)-360
              else
                dummy=lons(j)
              endif
              write(20,'(I8,2(";",F11.7))')k,lats(ii),dummy
              k=k+1
            enddo
          enddo
        endif

        call check( nf90_inq_varid(ncid, chem2read(i), data_varid) )
        call check( nf90_get_var(ncid, data_varid,  data2read_temp, start = start, &
                               count = count) )
!        do ij=1,NLONS
!           do ik=1,NLATS
!              do il=1,NLVLS
!                do im=1,NTIME
!                data2read(i,:,:,:)=data2read_temp(ij,ik,il,)
!        print*,
                data2read(i,:,:,:,:)=data2read_temp(:,:,:,:)
      print*,data2read_temp(1,10,1,1)
      print*,data2read(i,1,10,1,1)
      enddo
      
  end subroutine readfiles_and_prepare
end program macc_read_and_aiq
