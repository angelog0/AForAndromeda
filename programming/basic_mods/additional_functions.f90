!
! Author: Angelo Graziosi
!
!   created   : Jul 28, 2018
!   last edit : Jul 26, 2019
!
!   Useful additional functions module
!

module additional_functions
  use kind_consts, only: WP
  use math_consts, only: PI

  implicit none
  private

  public :: delta_dirac, sqr_x, sqr_z, conjg_z

contains

  ! Y = DELTA(X,A), X == P(1), A = P(2) (A is related to the WIDTH)
  function delta_dirac(p) result(r)
    real(WP), intent(in) :: p(*)
    real(WP) :: r

    real(WP), parameter :: FAC = sqrt(PI)

    associate (x => p(1), a => p(2))
      r = x/a
      r = exp(-r*r)/(abs(a)*FAC)
    end associate
  end function delta_dirac

  function sqr_x(p) result (s)
    real(WP), intent(in) :: p(*)
    real(WP) :: s
    s = p(1)*p(1)
  end function sqr_x

  ! Really, the parser DOES HAVE the CONJ() function
  function conjg_z(p) result (s)
    complex(WP), intent(in) :: p(*)
    complex(WP) :: s
    s = conjg(p(1))
  end function conjg_z

  function sqr_z(p) result (s)
    complex(WP), intent(in) :: p(*)
    complex(WP) :: s
    s = p(1)*p(1)
  end function sqr_z
end module additional_functions
