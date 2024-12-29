!
! Author: Angelo Graziosi
!
!   created   : Dec 29, 2015
!   last edit : Aug 09, 2018
!
!   Nice Numbers for Graph Labels module
!
! REFERENCES
!
!   1. Paul Heckbert, Nice Numbers for Graph Labels,
!      from "Graphics Gems", Academic Press, 1990
!
! NOTES
!
!   Adapted from: label.c: demonstrate nice graph labeling, Paul Heckbert,
!                 2 Dec 1988
!

module nicelabels
  use kind_consts, only: WP
  use math_consts, only: ONE => Z1, TWO => Z2, THREE => Z3, FIVE => Z5, &
       SEVEN => Z7, TEN => Z10, HALF => Q1_2, ONEHALF => Q3_2

  implicit none
  private

  public :: loose_label

contains

  function nicenum(x,round_flag) result(r)
    real(WP), intent(in) :: x
    logical, intent(in) :: round_flag
    real(WP) :: r
    !
    ! nicenum: find a "nice" number approximately equal to x.
    ! Round the number if round=.true., take ceiling if round=.false.
    !
    integer :: expv
    real(WP) :: f, nf

    expv = floor(log10(x))
    f = x/(TEN**expv)
    !print *, x, radix(x), f, fraction(x)

    if (round_flag) then
       if (f < ONEHALF) then
          nf = ONE
       else if (f < THREE) then
          nf = TWO
       else if (f < SEVEN) then
          nf = FIVE
       else
          nf = TEN
       end if
    else
       if (f <= ONE) then
          nf = ONE
       else if (f <= TWO) then
          nf = TWO
       else if (f <= FIVE) then
          nf = FIVE
       else
          nf = TEN
       end if
    end if

    r = nf * TEN**expv
  end function nicenum

  subroutine sort_ticks(n,tick_str)
    integer, intent(in) :: n
    character(len = *), intent(inout) :: tick_str(:)
    !
    ! A simple sorting/re-ordering routine
    !
    integer :: i, j, nm1
    real(WP) :: xi, xj

    ! Just a buffer to store partial values..
    character(len = 32) :: temp

    nm1 = n-1

    do i = 1, nm1
       read(tick_str(i),*) xi
       do j = i+1, n
          read(tick_str(j),*) xj
          if (xi > xj) then
             temp = ''
             temp = trim(adjustl(tick_str(i)))

             tick_str(i) = ''
             tick_str(i) = trim(adjustl(tick_str(j)))

             tick_str(j) = ''
             tick_str(j) = trim(adjustl(temp))
          end if
       end do
    end do
  end subroutine sort_ticks

  subroutine loose_label(amin,amax,ntick,tick_str)
    real(WP), intent(in) :: amin, amax
    integer, intent(inout) :: ntick
    character(len = *), intent(out) :: tick_str(:)

    !
    ! loose_label: demonstrate loose labeling of data range from amin to amax.
    ! (tight method is similar)
    !
    character(len = 12) :: fmt
    integer :: nfrac, n, ntick_max
    logical :: zero_in
    real(WP) :: d, graphmin, graphmax, range, x

    ntick_max = size(tick_str)

    ! We expect amin /= amax
    range = nicenum(amax-amin,.false.)
    d = nicenum(range/(ntick-1),.true.)
    graphmin = floor(amin/d)*d
    graphmax = ceiling(amax/d)*d

    zero_in = (graphmin < 0) .and. (graphmax > 0)

    ! # of fractional digits to show
    ! First, the "signed" # of fractional digit...
    nfrac = -floor(log10(d))

    ! Simplest axis labels
    if (abs(nfrac) > 6) then
       ! If the size of d is too big or too little (say, ~ 10 ** (+-6)),
       ! then use exponential format..
       nfrac = 2
       write(fmt,'("(es12.", i0, ")")') nfrac
    else
       ! ...else use fixed format..
       nfrac = max(nfrac,0)
       write(fmt,'("(f12.", i0, ")")') nfrac
       !write(fmt,'(a,i0,a)') "'(f6.",nfrac,")'"
    end if

    !write(*,'(a/)') trim(adjustl(fmt))

    !write(*,'(3(a,1pg12.5))') &
    !     'graphmin = ', graphmin, ' graphmax = ', graphmax, ' increment = ', d

    ! 'n' counts the true number of ticks used
    n = 0
    if (zero_in) then
       x = 0
    else
       x = graphmin
    end if
    do while (x < graphmax+HALF*d)
       n = n+1
       if (n > ntick_max) then
          write(*,*) 'N : ', n, ' > NTICK_MAX = ', ntick_max
          stop ': TOO MANY TICKS, N!'
       end if
       write(tick_str(n),fmt) x
       x = x+d
    end do

    ! If ZERO is in the interval, complete the work..
    if (zero_in) then
       x = 0-d
       do while (x > graphmin-HALF*d)
          n = n+1
          if (n > ntick_max) then
             write(*,*) 'N : ', n, ' > NTICK_MAX = ', ntick_max
             stop ': TOO MANY TICKS, N!'
          end if
          write(tick_str(n),fmt) x
          x = x-d
       end do
    end if

    ! 'Return' the ticks string an their number..
    call sort_ticks(n,tick_str)
    ntick = n
  end subroutine loose_label
end module nicelabels
