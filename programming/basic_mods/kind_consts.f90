!
! Author: Angelo Graziosi
!
!   created   : Mar 10, 2006
!   last edit : Apr 01, 2024
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

  !
  ! Are we sure that what suggested here:
  !
  !   https://fortran-lang.discourse.group/t/best-way-to-declare-a-double-precision-in-fortran/69/2
  !
  ! i.e.
  !
  !   use :: kind_consts, only: WP => DP
  !
  ! in the caller, is better of what we do here?
  !
  ! Changing precision with the above, you have to change its
  ! occurrence in each file of a library, for example, and rebuild the
  ! library. Instead with our method, you need to change just in this
  ! file the definition of WP and rebuild the library.
  !

  ! Working (FP) Precision
  integer, parameter, public :: WP = DP

end module kind_consts
