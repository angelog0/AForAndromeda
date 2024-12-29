!
! Author: Angelo Graziosi
!
!   created   : Mar 10, 2006
!   last edit : Jul 06, 2016
!
!   Useful KIND constants module
!

module kind_consts
  implicit none
  private

  integer, parameter, public :: I1B = selected_int_kind(2)
  !integer, parameter, public :: I8B = selected_int_kind(10)
  integer, parameter, public :: I8B = selected_int_kind(18)

  integer, parameter, public :: SP = selected_real_kind(6,30)

  !integer, parameter :: DP = kind(1.D0)
  ! DP: Following A. Miller definitions...
  integer, parameter, public :: DP = selected_real_kind(12,60)

  integer, parameter, public :: EP = selected_real_kind(18,90)
  integer, parameter, public :: QP = selected_real_kind(30,300)

  ! Working (FP) Precision
  integer, parameter, public :: WP = DP

end module kind_consts
