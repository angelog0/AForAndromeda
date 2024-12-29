!
! Author: ANGELO GRAZIOSI
!
!   created   : Sep 24, 2016
!   last edit : Aug 09, 2018
!
!   ForTran Timer class module
!
! NOTES
!
!   Adapted from: http://www.nocturnalaviationsoftware.com/blog/\
!                 using-type-bound-procedures.html
!
!   See also:
!
!     https://github.com/jlokimlin/array_passing_performance
!
!       src/type_CpuTimer.f90
!

module ft_timer_m
  use kind_consts, only: TP => WP
  use math_consts, only: ZERO => Z0

  implicit none
  private

  integer, parameter, public  :: TC_SECONDS = 0, TC_MINUTES = 1, TC_HOURS = 2

  type, public :: ft_timer_t
     private
     logical :: started = .false., stopped = .false.
     real(TP) :: start_time = ZERO
     real(TP) :: finish_time = ZERO
   contains
     private
     procedure, pass(self), public :: start => start_timer
     procedure, pass(self), public :: stop  => stop_timer
     procedure, pass(self), public :: elapsed_time
  end type ft_timer_t

contains

  subroutine start_timer(self)
    class(ft_timer_t), intent(inout) :: self
    self%started = .true.
    call cpu_time(self%start_time)
  end subroutine start_timer

  subroutine stop_timer(self)
    class(ft_timer_t), intent(inout) :: self
    call cpu_time(self%finish_time)
    self%stopped = .true.
  end subroutine stop_timer

  function elapsed_time(self,units) result(r)
    class(ft_timer_t), intent(inout) :: self
    integer, intent(in), optional :: units
    real(TP) :: r

    ! Return zero if the timer was never started
    if (.not.self%started) then
       r = ZERO
       return
    end if

    ! If the timer was not stopped, then return the current time elapsed
    if (.not.self%stopped) then
       call self%stop()
    end if

    r =  self%finish_time-self%start_time

    ! Convert to requested units if present
    if (present(units)) then

       select case (units)
       case (TC_MINUTES)
          r = r/60.0_TP
       case (TC_HOURS)
          r = r/3600.0_TP
       end select

    end if
  end function elapsed_time
end module ft_timer_m
