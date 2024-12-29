!
! Author: Angelo Graziosi
!
!   created   : Mar 10, 2006
!   last edit : Jan 24, 2024
!
!   Useful mathematical constants module
!

module math_consts
  use kind_consts, only: WP

  implicit none
  private

  ! INTEGER constants
  real(WP), parameter, public :: Z0 = 0
  real(WP), parameter, public :: Z1 = 1
  real(WP), parameter, public :: Z2 = 2
  real(WP), parameter, public :: Z3 = 3
  real(WP), parameter, public :: Z4 = 4
  real(WP), parameter, public :: Z5 = 5
  real(WP), parameter, public :: Z6 = 6
  real(WP), parameter, public :: Z7 = 7
  real(WP), parameter, public :: Z8 = 8
  real(WP), parameter, public :: Z9 = 9
  real(WP), parameter, public :: Z10 = 10
  real(WP), parameter, public :: Z73 = 73
  real(WP), parameter, public :: Z180 = 180
  real(WP), parameter, public :: Z360 = 360

  ! RATIONAL constants
  real(WP), parameter, public :: Q1_2 = Z1/2
  real(WP), parameter, public :: Q1_3 = Z1/3
  real(WP), parameter, public :: Q1_4 = Z1/4
  real(WP), parameter, public :: Q1_6 = Z1/6
  real(WP), parameter, public :: Q1_8 = Z1/8
  real(WP), parameter, public :: Q3_2 = Z3/2
  real(WP), parameter, public :: Q4_3 = Z4/3
  real(WP), parameter, public :: Q3_16 = Z3/16

  ! PI constants
  real(WP), parameter, public :: PI      = acos(-Z1)
  real(WP), parameter, public :: TWO_PI  = Z2*PI
  real(WP), parameter, public :: FOUR_PI = Z4*PI
  real(WP), parameter, public :: E_NEPER = exp(Z1)
  real(WP), parameter, public :: DEG2RAD = PI/Z180
  real(WP), parameter, public :: RAD2DEG = Z180/PI

  ! COMPLEX constants
  complex(WP), parameter, public :: JJ = (0,1)  ! Imaginary unity
  complex(WP), parameter, public :: OO = (0,0)  ! The ORIGIN

  ! Euler-Mascheroni constant.
  ! From: https://wg5-fortran.org/N1901-N1950/N1921.pdf
  !
  ! See also: https://www.rosettacode.org/wiki/Euler%27s_constant_0.5772...
  real(WP), parameter, public :: EULER_GAMMA = &
       0.5772156649015328606065120900824024310421593359399235988057672348848677_WP

end module math_consts
