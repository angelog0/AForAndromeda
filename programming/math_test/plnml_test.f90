!
!   rm -rf *.mod; \
!     gfortran[-mp-X] [-g3 -fbacktrace -fcheck=all] [-march=native] \
!       -Wall -std=f2018 [-fmax-errors=1] -O3 \
!       plnml_test.f90 -o plnml_test$EXE $LIBS; \
!   rm -rf *.mod
!
! ./plnml_test$EXE
!
! For the build on GNU/Linux [OSX+MacPorts X server], is:
!
!   EXE = .out
!
! while for the build on MSYS2 is:
!
!   EXE = -$MSYSTEM (or EMPTY)
!
!   For a static build (run from Explorer), I have found usefull
!
!     LIBS = -static -lmingw32 -lws2_32 -ldinput8 \
!            -ldxguid -ldxerr8 -luser32 -lgdi32 -lwinmm -limm32 -lole32 \
!            -loleaut32 -lshell32 -lversion -luuid -lcomdlg32 -lhid -lsetupapi
!
!   In this case one should avoid to use '-march=native' flag because
!   it makes the binaries not portable: on another machine they crash
!   (abort).
!
! References
!
!   https://annapurna.ifj.edu.pl/~golec/data/research/files/cernlib.pdf
!
! CERNLIB is still alive!
!
!   https://cernlib.web.cern.ch/download/2024_source/README
!   https://arxiv.org/pdf/2303.07506 (CERNLIB Status)
!

module types

  implicit none
  private

  integer, parameter, public :: SP = selected_real_kind(6,30)
  integer, parameter, public :: DP = selected_real_kind(15,307)
  integer, parameter, public :: EP = selected_real_kind(18,90)
  integer, parameter, public :: QP = selected_real_kind(30,300)

  ! Working (FP) Precision
  integer, parameter, public :: WP = DP

end module types

module plnml_lib
  use types, only: WP

  implicit none
  private

  ! Overloading
  interface plnml
     module procedure plnml_real, plnml_complex
  end interface

  public :: plnml

contains

  ! Adapted from:
  !
  !   https://github.com/apc-llc/cernlib/blob/master/2006/src/mathlib/gen/b/dplnml.F
  !   https://www.rosettacode.org/wiki/Horner%27s_rule_for_polynomial_evaluation#Fortran
  !
  ! P-MODE: P_N(X) = A(0)+A(1)*X+A(2)*X**2+...+A(N)*X**N
  ! Q-MODE: Q_N(X) = A(0)*X**N+A(1)*X**(N-1)+...+A(N)
  !
  function plnml_real(a,x,p_mode) result (r)
    real(WP), intent(in) :: a(0:), x
    logical, optional, intent(in) :: p_mode
    real(WP) :: r

    integer :: n, i

    r = 0

    ! The degree of the plnml
    n = ubound(a,1)

    if (n < 0) then
       write(*,*) '*** PLNML ERROR ***'
       write(*,*) 'POLYNOMIAL DEGREE LESS THAN ZERO!!!'
       return
    end if

    if (present(p_mode) .and. (p_mode .eqv. .false.)) then
       ! It is the Q-mode
       r = a(0)
       do i = 1, n
          r = a(i)+x*r
       end do
    else
       ! It is the P-mode
       r = a(n)
       do i = n-1, 0, -1
          r = a(i)+x*r
       end do
    end if
  end function plnml_real

  function plnml_complex(a,z,p_mode) result (w)
    complex(WP), intent(in) :: a(0:), z
    logical, optional, intent(in) :: p_mode
    complex(WP) :: w

    integer :: n, i

    w = 0

    ! The degree of the plnml
    n = ubound(a,1)

    if (n < 0) then
       write(*,*) '*** PLNML ERROR ***'
       write(*,*) 'POLYNOMIAL DEGREE LESS THAN ZERO!!!'
       return
    end if

    if (present(p_mode) .and. (p_mode .eqv. .false.)) then
       ! It is the Q-mode
       w = a(0)
       do i = 1, n
          w = a(i)+z*w
       end do
    else
       ! It is the P-mode
       w = a(n)
       do i = n-1, 0, -1
          w = a(i)+z*w
       end do
    end if
  end function plnml_complex
end module plnml_lib

program plnml_test
  use types, only: WP
  use plnml_lib, only: plnml
  implicit none

  complex(WP), parameter :: I = (0,1)

  real(WP) :: a(3) = [ 1, -2, 1 ], b(0:2) = [ 1, -2, 1 ], c(5)
  complex(WP) :: d(3) = [ (1,0),(-2,0),(1,0) ]

  print *, 'Using A:', a
  print *, plnml(a,1.0_WP)
  print *, plnml(a,2.0_WP)
  print *, plnml(a,3.0_WP)
  print *
  print *, 'Using B:', b
  print *, plnml(b,1.0_WP)
  print *, plnml(b,2.0_WP)
  print *, plnml(b,3.0_WP)
  print *

  a(:) = [4,-4,1]
  b(0:2) = a(:)

  print *, 'Using A:', a(1), a(2), a(3)
  print *, plnml(a,1.0_WP)
  print *, plnml(a,0.5_WP)
  print *, plnml(a,1.0_WP,p_mode=.false.)
  print *, plnml(a,0.5_WP,p_mode=.false.)

  print *
  print *, 'Using B:', b
  print *, plnml(b,1.0_WP)
  print *, plnml(b,0.5_WP)
  print *, plnml(b,1.0_WP,p_mode=.false.)
  print *, plnml(b,0.5_WP,p_mode=.false.)

  c = [0,1,-4,4,0]

  print *
  print *, 'Using C:', c(2:4)
  print *, plnml(c(2:4),1.0_WP)
  print *, plnml(c(2:4),0.5_WP)
  print *, plnml(c(2:4),1.0_WP,p_mode=.false.)
  print *, plnml(c(2:4),0.5_WP,p_mode=.false.)

  print *
  print *, 'Using C:', c(4:2:-1)
  print *, plnml(c(4:2:-1),1.0_WP)
  print *, plnml(c(4:2:-1),0.5_WP)
  print *, plnml(c(4:2:-1),1.0_WP,p_mode=.false.)
  print *, plnml(c(4:2:-1),0.5_WP,p_mode=.false.)

  print *
  print *, 'Using D:', d
  print *, plnml(d,(1.0_WP,0.0_WP))
  print *, plnml(d,(2.0_WP,0.0_WP))
  print *, plnml(d,(3.0_WP,0.0_WP),.true.)
  print *, plnml(d,(3.0_WP,0.0_WP),.false.)

  d = [ (1,0),(0,0),(1,0) ]
  print *
  print *, 'Using D:', d
  print *, plnml(d,(0.0_WP,1.0_WP))
  print *, plnml(d,-(0.0_WP,1.0_WP))

  print *, plnml(d,I)
  print *, plnml(d,-I)

end program plnml_test
