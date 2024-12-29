!
! Author: Angelo Graziosi
!
!   created   : Sep 08, 2015
!   last edit : Jul 24, 2016
!
!   Useful Julian Dates manipulation module
!

module julian_dates
  use kind_consts, only: WP

  implicit none
  private

  public :: date2jd, jd2date, cal2jd, jd2cal

contains

  function date2jd(year,month,day,julian_date) result (jdn)
    integer, intent(in) :: year, month
    real(WP), intent(in) :: day
    logical, intent(in), optional :: julian_date
    real(WP) :: jdn
    !
    ! Implementation of the algorithm described in
    !
    !   http://adsabs.harvard.edu/full/1984QJRAS..25...53H
    !
    ! It should be valid for all dates after March 1, -4712
    ! (JD 60 in Julian Calendar; JD 98 in Gregorian calendar)
    !
    integer :: A, M, A1, M1, N, D1, y, d, g
    real(WP) :: f

    A = year
    M = month
    D1 = int(day)

    ! F is the fraction of the day after  midnight (hours 0.0 of day D1)
    ! if YEAR >= 1925 OR the fraction of the day after the noon if YEAR < 1925
    f = day-D1

    if (A >= 1925) f = f-0.5_WP

    A1 = A-int((12-M)/10.0_WP)
    M1 = modulo((M-3),12)

    A = A1+int(M1/10.0_WP)
    M = modulo((M1+2),12)+1

    y = int(365.25_WP*(A1+4712))
    d = int(30.6_WP*M1+0.5_WP)
    N = y+d+D1+59

    if (present(julian_date) .and. julian_date) then
       g = 0
    else
       g = int(int((A1/100.0_WP)+49)*0.75_WP)-38
    end if

    jdn = (N-g)+f
  end function date2jd

  subroutine jd2date(jdn,year,month,day,julian_date)
    real(WP), intent(in) :: jdn
    integer, intent(out) :: year, month
    real(WP), intent(out) :: day
    logical, intent(in), optional :: julian_date
    !
    ! Implementation of the algorithm described in
    !
    !   http://adsabs.harvard.edu/full/1984QJRAS..25...53H
    !
    ! It should be valid for all dates after April 7, -4700 in
    ! Julian Calendar (JD 4480), i.e. March 1, -4700 in Gregorian calendar
    ! (same JD 4480)
    !
    integer :: N, g, d1, A, D, JD, M
    real(WP) :: f

    JD = int(jdn)

    ! The Julian Day start at the noon, so F is the fraction of the day
    ! after the noon!
    f = jdn-JD

    f = f+0.5_WP
    if (f >= 1) then
       f = f-1
       JD = JD+1
    end if

    if (present(julian_date) .and. julian_date) then
       g = 0
    else
       g = int(int((JD-4479.5_WP)/36524.25_WP)*0.75_WP+0.5_WP)-37
    end if

    N = JD+g

    A = int(N/365.25_WP)-4712
    d1 = int(modulo((N-59.25_WP),365.25_WP))

    M = modulo((int((d1+0.5_WP)/30.6_WP)+2),12)+1
    D = int(modulo((d1+0.5_WP),30.6_WP))+1

    year = A
    month = M

    if (A < 1925) f = f-0.5_WP
    day = D+f
  end subroutine jd2date

  subroutine cal2jd(y,m,d,gc,jdn)
    integer, intent(in) :: y, m, gc
    real(WP), intent(in) :: d
    real(WP), intent(out) :: jdn
    !
    ! Adapted from CALJD.BAS, appeared in Astronomical Computing,
    ! Sky and Telescope, May, 1984
    !
    ! For Julian Calendar, it seems valid after Jan. 01, -4708... NO!
    ! Instead for Gregorian Calendar, it seems valid after Jan. 01, 0000.
    !
    integer :: d1, jd, jd1, s, a
    real(WP) :: f

    if (gc < 0 .or. 1 < gc) error stop ': GC should be 0 or 1.'

    d1 = int(d)
    f = d-d1

    if (y >= 1925) f = f-0.5_WP

    jd = -int(7*(int((m+9)/12.0_WP)+y)/4.0_WP)

    ! To silence the compiler
    jd1 = 0

    if (gc == 1) then
       a = m-9

       ! Our SGN() function
       select case (a)
       case (:-1)
          s = -1
       case (0)
          s = 0
       case (1:)
          s = 1
       end select

       a = abs(a)

       jd1 = int(y+s*int(a/7.0_WP))
       jd1 = -int((int(jd1/100.0_WP)+1)*3/4.0_WP)
    end if

    jd = jd+int(275*m/9.0_WP)+d1+gc*jd1
    jd = jd+1721027+2*gc+367*y

    if (f < 0) then
       f = f+1
       jd = jd-1
    end if

    jdn = jd+f

  end subroutine cal2jd

  subroutine jd2cal(jdn,gc,y,m,d)
    real(WP), intent(in) :: jdn
    integer, intent(in) :: gc
    integer, intent(out) :: y, m
    real(WP), intent(out) :: d
    !
    ! Adapted from JDCAL.BAS, appeared in Astronomical Computing,
    ! Sky and Telescope, May, 1984
    !
    integer :: jd, a1, a, b, c, d1, e
    real(WP) :: f

    if (gc < 0 .or. 1 < gc) error stop ': GC should be 0 or 1.'

    jd = int(jdn)
    f = jdn-jd+0.5_WP

    if (f >= 1) then
       f = f-1
       jd = jd+1
    end if

    if (gc == 1) then
       a1 = int((jd/36524.25_WP)-51.12264_WP)
       a = jd+1+a1-int(a1/4.0_WP)
    else
       a = jd
    end if

    b = a+1524
    c = int((b/365.25_WP)-0.3343_WP)
    d1 = int(365.25_WP*c)
    e = int((b-d1)/30.61_WP)
    d = (b-d1-int(30.61_WP*e))+f
    m = e-1
    y = c-4716
    if (e > 13.5_WP) m = m-12
    if (m < 2.5_WP) y = y+1

    if (y < 1925) d = d-0.5_WP

  end subroutine jd2cal
end module julian_dates
