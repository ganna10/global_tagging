!----------------------------------------------------------------------
! A set of routines for handling dates and times.
!
! CONTENTS:
!   compare_times
!   eqtime
!   geth_idts
!   inctime
!   isleap
!   julday
!   month_days
!   month_num2strabrev
!   show_time
!   timediff
!   time2str_ddmmmyyyhhmm
!   utc_to_lst
!
!----------------------------------------------------------------------

module timeroutines
  implicit none

  type time
     integer :: yr,mn,dy,hr,mm
  end type time

contains

!----------------------------------------------------------------------
integer function compare_times(t1,t2)
! Compares two times. This version ignores minutes and seconds.
! Returns: -1 if t1<t2
!           0 if t1=t2
!           1 if t1>t2
!----------------------------------------------------------------------
  type(time),intent(in) :: t1,t2

  if( t1%yr > t2%yr ) then
     compare_times = 1
  else if( t1%yr==t2%yr ) then
     if( t1%mn > t2%mn ) then
        compare_times = 1
     else if( t1%mn==t2%mn ) then
        if( t1%dy > t2%dy ) then
           compare_times = 1
        else if( t1%dy==t2%dy ) then
           if( t1%hr > t2%hr ) then
              compare_times = 1
           else if( t1%hr==t2%hr ) then
              compare_times = 0
           else !t1%hr < t2%hr
              compare_times = -1
           end if
        else !t1%dy < t2%dy
           compare_times = -1
        end if
     else !t1%mn < t2%mn
        compare_times = -1
     end if
  else !t1%yr < t2%yr
     compare_times = -1
  end if
end function compare_times

!----------------------------------------------------------------------
subroutine eqtime(t1,t2)
! Sets t1=t2 where t1 and t2 are type(time)
!----------------------------------------------------------------------
  type(time),intent(in)  :: t2
  type(time),intent(out) :: t1

  t1%yr = t2%yr
  t1%mn = t2%mn
  t1%dy = t2%dy
  t1%hr = t2%hr
  t1%mm = t2%mm
end subroutine eqtime

!----------------------------------------------------------------------
SUBROUTINE geth_idts (ndate, odate, idts, leap)
! Computes the time difference in seconds between ndate and odate.
! This routine is from MM5's REGRID/regridder/src/module_date_pack.F.
! Note that ndate and odate are strings and not the time structure
! used by most routines in this module. The routine timediff can be
! used as a wrapper for this routine if your times are in the structure
! format.
!----------------------------------------------------------------------
  
  IMPLICIT NONE

  !  From 2 input mdates ('YYYY-MM-DD HH:MM:SS.ffff'), 
  !  compute the time difference.

  !  on entry     -  ndate  -  the new hdate.
  !                  odate  -  the old hdate.

  !  on exit      -  idts    -  the change in time in seconds.

  CHARACTER (LEN=*) , INTENT(INOUT) :: ndate, odate
  INTEGER           , INTENT(OUT)   :: idts
  logical           , optional      :: leap

  !  Local Variables

  !  yrnew    -  indicates the year associated with "ndate"
  !  yrold    -  indicates the year associated with "odate"
  !  monew    -  indicates the month associated with "ndate"
  !  moold    -  indicates the month associated with "odate"
  !  dynew    -  indicates the day associated with "ndate"
  !  dyold    -  indicates the day associated with "odate"
  !  hrnew    -  indicates the hour associated with "ndate"
  !  hrold    -  indicates the hour associated with "odate"
  !  minew    -  indicates the minute associated with "ndate"
  !  miold    -  indicates the minute associated with "odate"
  !  scnew    -  indicates the second associated with "ndate"
  !  scold    -  indicates the second associated with "odate"
  !  i        -  loop counter
  !  mday     -  a list assigning the number of days in each month

  CHARACTER (LEN=24) :: tdate
  INTEGER :: olen, nlen
  INTEGER :: yrnew, monew, dynew, hrnew, minew, scnew
  INTEGER :: yrold, moold, dyold, hrold, miold, scold
  INTEGER :: mday(12), i, newdys, olddys
  LOGICAL :: leapyr,npass, opass
  INTEGER :: isign

  if( present(leap) ) then
     leapyr = leap
  else
     leapyr = .true.
  end if

  IF (odate.GT.ndate) THEN
     isign = -1
     tdate=ndate
     ndate=odate
     odate=tdate
  ELSE
     isign = 1
  END IF

  !  Assign the number of days in a months

  mday( 1) = 31
  mday( 2) = 28
  mday( 3) = 31
  mday( 4) = 30
  mday( 5) = 31
  mday( 6) = 30
  mday( 7) = 31
  mday( 8) = 31
  mday( 9) = 30
  mday(10) = 31
  mday(11) = 30
  mday(12) = 31

  !  Break down old hdate into parts

  hrold = 0
  miold = 0
  scold = 0
  olen = LEN(odate)

  READ(odate(1:4),  '(I4)') yrold
  READ(odate(6:7),  '(I2)') moold
  READ(odate(9:10), '(I2)') dyold
  IF (olen.GE.13) THEN
     READ(odate(12:13),'(I2)') hrold
     IF (olen.GE.16) THEN
        READ(odate(15:16),'(I2)') miold
        IF (olen.GE.19) THEN
           READ(odate(18:19),'(I2)') scold
        END IF
     END IF
  END IF

  !  Break down new hdate into parts

  hrnew = 0
  minew = 0
  scnew = 0
  nlen = LEN(ndate)

  READ(ndate(1:4),  '(I4)') yrnew
  READ(ndate(6:7),  '(I2)') monew
  READ(ndate(9:10), '(I2)') dynew
  IF (nlen.GE.13) THEN
     READ(ndate(12:13),'(I2)') hrnew
     IF (nlen.GE.16) THEN
        READ(ndate(15:16),'(I2)') minew
        IF (nlen.GE.19) THEN
           READ(ndate(18:19),'(I2)') scnew
        END IF
     END IF
  END IF

  !  Check that the dates make sense.

  npass = .true.
  opass = .true.

  !  Check that the month of NDATE makes sense.

  IF ((monew.GT.12).or.(monew.LT.1)) THEN
     PRINT*, 'GETH_IDTS:  Month of NDATE = ', monew
     npass = .false.
  END IF

  !  Check that the month of ODATE makes sense.

  IF ((moold.GT.12).or.(moold.LT.1)) THEN
     PRINT*, 'GETH_IDTS:  Month of ODATE = ', moold
     opass = .false.
  END IF

  !  Check that the day of NDATE makes sense.

  IF (monew.ne.2) THEN
     ! ...... For all months but February
     IF ((dynew.GT.mday(monew)).or.(dynew.LT.1)) THEN
        PRINT*, 'GETH_IDTS:  Day of NDATE = ', dynew
        npass = .false.
     END IF
  ELSE IF (monew.eq.2) THEN
     ! ...... For February
!     IF ((dynew.GT.nfeb(yrnew)).OR.(dynew.LT.1)) THEN
     IF ((dynew.GT.month_days(2,yrnew,leapyr)).OR.(dynew.LT.1)) THEN
        PRINT*, 'GETH_IDTS:  Day of NDATE = ', dynew
        npass = .false.
     END IF
  END IF

  !  Check that the day of ODATE makes sense.

  IF (moold.ne.2) THEN
     ! ...... For all months but February
     IF ((dyold.GT.mday(moold)).or.(dyold.LT.1)) THEN
        PRINT*, 'GETH_IDTS:  Day of ODATE = ', dyold
        opass = .false.
     END IF
  ELSE IF (moold.eq.2) THEN
     ! ....... For February
!     IF ((dyold.GT.nfeb(yrold)).or.(dyold.LT.1)) THEN
     IF ((dyold.GT.month_days(2,yrold,leapyr)).or.(dyold.LT.1)) THEN
        PRINT*, 'GETH_IDTS:  Day of ODATE = ', dyold
        opass = .false.
     END IF
  END IF

  !  Check that the hour of NDATE makes sense.

  IF ((hrnew.GT.23).or.(hrnew.LT.0)) THEN
     PRINT*, 'GETH_IDTS:  Hour of NDATE = ', hrnew
     npass = .false.
  END IF

  !  Check that the hour of ODATE makes sense.

  IF ((hrold.GT.23).or.(hrold.LT.0)) THEN
     PRINT*, 'GETH_IDTS:  Hour of ODATE = ', hrold
     opass = .false.
  END IF

  !  Check that the minute of NDATE makes sense.

  IF ((minew.GT.59).or.(minew.LT.0)) THEN
     PRINT*, 'GETH_IDTS:  Minute of NDATE = ', minew
     npass = .false.
  END IF

  !  Check that the minute of ODATE makes sense.

  IF ((miold.GT.59).or.(miold.LT.0)) THEN
     PRINT*, 'GETH_IDTS:  Minute of ODATE = ', miold
     opass = .false.
  END IF

  !  Check that the second of NDATE makes sense.

  IF ((scnew.GT.59).or.(scnew.LT.0)) THEN
     PRINT*, 'GETH_IDTS:  SECOND of NDATE = ', scnew
     npass = .false.
  END IF

  !  Check that the second of ODATE makes sense.

  IF ((scold.GT.59).or.(scold.LT.0)) THEN
     PRINT*, 'GETH_IDTS:  Second of ODATE = ', scold
     opass = .false.
  END IF

  IF (.not. npass) THEN
     PRINT*, 'Screwy NDATE: ', ndate(1:nlen)
     STOP 'ndate_2'
  END IF

  IF (.not. opass) THEN
     PRINT*, 'Screwy ODATE: ', odate(1:olen)
     STOP 'odate_1'
  END IF

  !  Date Checks are completed.  Continue.

  !  Compute number of days from 1 January ODATE, 00:00:00 until ndate
  !  Compute number of hours from 1 January ODATE, 00:00:00 until ndate
  !  Compute number of minutes from 1 January ODATE, 00:00:00 until ndate

  newdys = 0
  DO i = yrold, yrnew - 1
!     newdys = newdys + (365 + (nfeb(i)-28))
     newdys = newdys + (365 + (month_days(2,i,leapyr)-28))
  END DO

  IF (monew .GT. 1) THEN
!     mday(2) = nfeb(yrnew)
     mday(2) = month_days(2,yrnew,leapyr)
     DO i = 1, monew - 1
        newdys = newdys + mday(i)
     END DO
     mday(2) = 28
  END IF

  newdys = newdys + dynew-1

  !  Compute number of hours from 1 January ODATE, 00:00:00 until odate
  !  Compute number of minutes from 1 January ODATE, 00:00:00 until odate

  olddys = 0

  IF (moold .GT. 1) THEN
!     mday(2) = nfeb(yrold)
     mday(2) = month_days(2,yrold,leapyr)
     DO i = 1, moold - 1
        olddys = olddys + mday(i)
     END DO
     mday(2) = 28
  END IF

  olddys = olddys + dyold-1

  !  Determine the time difference in seconds

  idts = (newdys - olddys) * 86400
  idts = idts + (hrnew - hrold) * 3600
  idts = idts + (minew - miold) * 60
  idts = idts + (scnew - scold)

  IF (isign .eq. -1) THEN
     tdate=ndate
     ndate=odate
     odate=tdate
     idts = idts * isign
  END IF

END SUBROUTINE geth_idts

!----------------------------------------------------------------------
subroutine inctime(tt,inchrs,incmin,leap)
! Increments the time, tt, by the given number of hours and minutes
! The basic methodology comes from module_date_pack.F in
! MM5/REGRID/regridder/src. If leap is present and true, leap years
! are considered.
!----------------------------------------------------------------------
  type(time), intent(inout) :: tt
  integer(kind=8),intent(in):: inchrs,incmin
  logical,    optional      :: leap

  integer               :: i
  integer               :: iyr,imn,idy,ihr,imm
  logical               :: leapyr

  if( present(leap) ) then
     leapyr = leap
  else
     leapyr = .true.
  end if
!
! Calc. the number of time units to increment
!
  idy = abs(inchrs*60 + incmin)/1440
  ihr = mod(abs(inchrs*60 + incmin),1440)/60
  imm = mod(abs(inchrs*60 + incmin),60)
!
! If positive increment
!
  if( (inchrs*1440 + incmin) > 0 ) then
     tt%mm = tt%mm + imm
     do
        if( tt%mm >= 60 ) then
           tt%mm = tt%mm - 60
           tt%hr = tt%hr + 1
        else 
           exit
        end if
     end do

     tt%hr = tt%hr + ihr
     do
        if( tt%hr > 23 ) then
           tt%hr = tt%hr - 24
           idy   = idy + 1
        else
           exit
        end if
     end do

     do i=1,idy
        tt%dy = tt%dy + 1
        if( tt%dy > month_days(tt%mn,tt%yr,leapyr) ) then
           tt%dy = tt%dy - month_days(tt%mn,tt%yr,leapyr)
           tt%mn = tt%mn + 1
           if( tt%mn > 12 ) then
              tt%mn = 1
              tt%yr = tt%yr + 1
           end if
        end if
     end do
!
! If negative increment
!
  else if( (inchrs*1440 + incmin) < 0 ) then
     tt%mm = tt%mm - imm
     do
        if( tt%mm < 0 ) then
           tt%mm = tt%mm + 60
           tt%hr = tt%hr - 1
        else
           exit
        end if
     end do

     tt%hr = tt%hr - ihr
     do
        if( tt%hr < 0 ) then
           tt%hr = tt%hr + 24
           idy   = idy + 1
        else
           exit
        end if
     end do

     !Now, handle a negative day adjustment.
!     do i=idy,-1
     do i=1,idy
        tt%dy = tt%dy - 1
        if( tt%dy == 0 ) then
           tt%mn = tt%mn - 1
           if( tt%mn == 0 ) then
              tt%mn = 12
              tt%yr = tt%yr - 1
           end if
           tt%dy = month_days(tt%mn,tt%yr,leapyr)
        end if
     end do
  end if

end subroutine inctime

!----------------------------------------------------------------------
logical function isleap(yr)
! Returns true if the given year is a leap year, otherwise returns
! false.
!
!----------------------------------------------------------------------
  integer, intent(in) :: yr

  if( mod(yr,4) /= 0 ) then
     isleap = .false.
  else if( mod(yr,400) == 0 )then
     isleap = .true.
  else if( mod(yr,100) == 0 ) then
     isleap = .false.
  else
     isleap = .true.
  end if
end function isleap

!----------------------------------------------------------------------
integer function julday(tm)
! Returns the julian day of the year (Jan. 1==1, Feb. 1==32, etc.)
!
! Dependencies: month_days
!
!----------------------------------------------------------------------
  type(time),intent(in) :: tm

  integer :: i

  julday = 0
  do i=1,tm%mn-1
     julday = julday + month_days(tm%mn,tm%yr)
  end do
  julday = julday + tm%dy
end function julday

!----------------------------------------------------------------------
integer function month_days(mn,yr,leap)
! Returns the number of days in the given month for the given year.
! If leap=true (default), leap years are considered, if false 
!then they are ignored.
!
! Dependencies: isleap
!----------------------------------------------------------------------
  integer, intent(in) :: mn,yr
  logical, optional   :: leap

  integer,dimension(12) :: mnlen
  logical :: leapyr

  if( present(leap) ) then
     leapyr = leap
  else
     leapyr = .true.
  end if

  data mnlen/31,28,31,30,31,30,31,31,30,31,30,31/
  month_days = mnlen(mn)
  if( leapyr .and. mn==2 ) then
     if( isleap(yr) ) month_days = 29
  end if
end function month_days

!----------------------------------------------------------------------
character(len=3) function month_num2strabrev(mn)
! Given the month number, returns the 3 letter month abreviation in
! upper case.
!----------------------------------------------------------------------
  integer, intent(in) :: mn

  select case(mn)
  case (1)
     month_num2strabrev = "JAN"
  case (2)
     month_num2strabrev = "FEB"
  case (3)
     month_num2strabrev = "MAR"
  case (4)
     month_num2strabrev = "APR"
  case (5)
     month_num2strabrev = "MAY"
  case (6)
     month_num2strabrev = "JUN"
  case (7)
     month_num2strabrev = "JUL"
  case (8)
     month_num2strabrev = "AUG"
  case (9)
     month_num2strabrev = "SEP"
  case (10)
     month_num2strabrev = "OCT"
  case (11)
     month_num2strabrev = "NOV"
  case (12)
     month_num2strabrev = "DEC"
  case default
     print*,"WARNING: month_num2strabrev cannot determine month name."
     month_num2strabrev = "   "
  end select
end function month_num2strabrev

!----------------------------------------------------------------------
subroutine show_time()
! Prints the current system date and time to standard output.
!----------------------------------------------------------------------
  character(len=8)     :: date
  character(len=10)    :: time
  character(len=5)     :: zone
  integer,dimension(8) :: values

  call date_and_time(date,time,zone,values)
  print*,"The current wall time is "// &
       date(1:4)//"-"//date(5:6)//"-"//date(7:8)//" "// &
       time(1:2)//":"//time(3:4)//":"//time(5:10)
end subroutine show_time

!----------------------------------------------------------------------
character(len=17) function time2str_ddmmmyyyhhmm(tt)
! Returns a string containing the the time tt in the format
! DD-MMM-YYYY hh:mm
!
!----------------------------------------------------------------------
  type(time), intent(in) :: tt

  time2str_ddmmmyyyhhmm = "DD-MMM-YYYY hh:mm"
  write(time2str_ddmmmyyyhhmm( 1: 2),'(i2.2)') tt%dy
  time2str_ddmmmyyyhhmm(4:6) = month_num2strabrev(tt%mn)
  write(time2str_ddmmmyyyhhmm( 8:11),'(i4.4)') tt%yr
  write(time2str_ddmmmyyyhhmm(13:14),'(i2.2)') tt%hr
  write(time2str_ddmmmyyyhhmm(16:17),'(i2.2)') tt%mm
end function time2str_ddmmmyyyhhmm

!----------------------------------------------------------------------
subroutine timediff(t1,t2,dmm,leapyr)
! Returns the time difference t1-t2 in minutes. This is a wrapper
! routine to convert from the time structure format to the string
! format used by MM5's geth_idts routine.
!
!----------------------------------------------------------------------
  type(time),      intent( in) :: t1, t2
  integer,         intent(out) :: dmm
  logical,         optional    :: leapyr

  integer :: idts
  character(len=24) :: ndate, odate
  logical :: leap

  if( present(leapyr) ) then
     leap = leapyr
  else
     leap = .true.
  end if

  !YYYY-MM-DD HH:MM:SS.ffff
  write(ndate,'(i4.4,"-",i2.2,"-",i2.2," ",i2.2,":",i2.2,":00.0000")') t1
  write(odate,'(i4.4,"-",i2.2,"-",i2.2," ",i2.2,":",i2.2,":00.0000")') t2
  call geth_idts (ndate, odate, idts, leap)
  dmm = anint(real(idts)/60.)
end subroutine timediff

!----------------------------------------------------------------------
subroutine utc_to_lst(lon,tm)
! Converts time in UTC to local solar time (LST). The longitude, lon,
! is assumed to be in the range -180 to 180 degE. The contents of tm
! are overwritten.
!
! The formulas are from "Radiative Energy in the Atmosphere and Ocean"
! by Steven A. Ackerman, Univ. of Wisconsin-Madison, Spring 1995. They
! were cross checked against  "General Solar Position Calculations"
! from http://www.srb.noaa.gov/highlights/sunrise/calcdetails.html.
!
! Dependencies: inctime, julday
!
!----------------------------------------------------------------------
  real,       intent(   in) :: lon
  type(time), intent(inout) :: tm

  real :: eqntime, gamma, toff

  !fractional year:
  gamma = 2.*acos(-1.)/365.*(real(julday(tm)) - 1. + (real(tm%hr)-12.)/24.)

  !equation of time in minutes:
  eqntime = 229.18*( 7.5e-5 + 1.868e-3*Cos(gamma) - 0.032077*Sin(gamma) &
            - 0.014615*Cos(2.*gamma) - 0.040849*Sin(2.*gamma) )

  !time offset, convert the longitude to degW:
  toff = eqntime - 4.*(-lon)

  !increment the time:
  call inctime(tm,0_8,int(anint(toff),8))
end subroutine utc_to_lst

end module timeroutines
