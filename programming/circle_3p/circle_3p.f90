!
! From: https://math.stackexchange.com/questions/213658/get-the-equation-of-a-circle-when-given-3-points
! https://rosettacode.org/wiki/Centre_and_radius_of_a_circle_passing_through_3_points_in_a_plane#Python
! https://math.libretexts.org/Bookshelves/Geometry/Geometry_with_an_Introduction_to_Cosmic_Topology_(Hitchman)/03%3A_Transformations/3.01%3A_Basic_Transformations_of_Complex_Numbers
!
! HOW TO BUILD THE APP (MSYS2, GNU/Linux, macOS)
!
!   cd programming
!
!   git clone https://github.com/interkosmos/fortran-sdl2.git
!
!   cd fortran-sdl2
!
!   make FFLAGS='[-march=native] -Wall -std=f2018 -fmax-errors=1 $(SDL_CFLAGS) -O3' all examples
!   mv libfortran-sdl2.a ../lib/
!   mv c_util.mod glu.mod sdl2*.mod ../finclude/
!   make clean
!   cd ..
!
!   cd basic_mods
!
!   make FFLAGS='[-march=native] -Wall -std=f2018 -fmax-errors=1 -O3' all
!   mv *.a ../lib/
!   mv *.mod ../finclude/
!   make clean
!   cd ..
!
!   cd fortran-sdl2apps
!
!   make FFLAGS='[-march=native] -Wall -std=f2018 -fmax-errors=1 -O3' all
!   mv *.a ../lib/
!   mv *.mod ../finclude/
!   make clean
!   cd ..
!
!   cd circle_3p
!
!   rm -rf *.mod; \
!     gfortran[-mp-X] [-g3 -fbacktrace -fcheck=all] [-march=native] \
!       -Wall [-Wno-unused-dummy-argument] -std=f2018 [-fmax-errors=1] -O3 \
!       -I ../finclude [`sdl2-config --cflags`] \
!       circle_3p.f90 -o circle_3p$EXE \
!       -L ../lib -lbasic_mods -lfortran-sdl2apps -lfortran-sdl2 $LIBS; \
!   rm -rf *.mod
!
!   ./circle_3p$EXE
!
!   where, for the build on GNU/Linux [OSX+MacPorts X server], is:
!
!     EXE = .out
!
!   while for the build on MSYS2 is:
!
!     EXE = -$MSYSTEM (or EMPTY)
!
!   and
!
!     LIBS = `sdl2-config --libs`
!
!   Notice that the above definition for LIBS produces a pure Windows
!   app. This means that it will not show up a console/terminal for
!   input data. On these systems (Windows), the LIBS definition should
!   be:
!
!     LIBS = [-lSDL2main] -lSDL2 -lgdi32 -lcomdlg32 -luuid -loleaut32 -lole32
!
!   For a static build (run from Explorer), I have found usefull
!
!     LIBS = -static -lmingw32 [-lSDL2main] -lSDL2 -lws2_32 -ldinput8 \
!            -ldxguid -ldxerr8 -luser32 -lgdi32 -lwinmm -limm32 -lole32 \
!            -loleaut32 -lshell32 -lversion -luuid -lcomdlg32 -lhid -lsetupapi
!
!   In this case one should avoid to use '-march=native' flag because
!   it makes the binaries not portable: on another machine they crash
!   (abort).
!
!   See as references:
!
!     1. https://stackoverflow.com/questions/53885736/issues-when-statically-compiling-sdl2-program
!     2. https://groups.google.com/g/comp.lang.fortran/c/Usgys7Gww6o/m/CYEfzQfbhckJ
!

module circle_m
  use :: kind_consts, only: WP

  implicit none
  private

  type, public :: circle_t
     private
     real(WP) :: radius = 0
     complex(WP) :: center = 0

   contains
     private
     procedure, pass(self), public :: find
     procedure, pass(self), public :: get_radius
     procedure, pass(self), public :: get_center
  end type circle_t

contains

  subroutine find(self,z1,z2,z3)
    class(circle_t), intent(out) :: self
    complex(WP), intent(in) :: z1, z2, z3

    real(WP), parameter :: EPS = 1.0E-12_WP

    complex(WP) :: w

    associate (r => self%radius, c => self%center)

      if (z1 == z2 .or. z1 == z3 .or. z2 == z3) then
         write(*,*) ' DUPLICATED POINTS'
         stop
      end if

      ! Just as auxiliary variable
      c = z2-z1

      ! (Z3-Z1)/(Z2-Z1)
      w = (z3-z1)/c

      ! Just as auxiliary variable
      r = w%im

      if (abs(r) < EPS) then
         write(*,*) 'THE POINTS COULD BE COLLINEAR'
         stop
      end if

      ! (Z2-Z1)*(W-|W|**2)
      w = c*(w-abs(w)**2)

      ! W-conjg(W) = 2*J*W%IM = (0,2*W%IM)
      c = cmplx(0,2*r,KIND=WP)

      ! (Z2 - Z1)*(W - ABS(W)**2)/(2J*W%IMAG) + Z1
      c = z1+w/c
      r = abs(z1-c)
    end associate
  end subroutine find

  function get_radius(self) result(r)
    class(circle_t), intent(in) :: self
    real(WP) :: r

    r = self%radius
  end function get_radius

  function get_center(self) result(c)
    class(circle_t), intent(in) :: self
    complex(WP) :: c

    c = self%center
  end function get_center
end module circle_m

program circle_3p
  use :: kind_consts, only: WP
  use :: circle_m, only: circle_t

  implicit none

  character(len=*), parameter :: FMT = '(*(1x,g0))'
  real(WP), parameter :: U_MIN = -10, U_SIZE = 20, VIEW_SIZE = 1.05_WP*U_SIZE

  real(WP) :: u(2)
  complex(WP) :: z1 = (1,1), z2 = (2,4), z3 = (5,3)
  type(circle_t) :: my_circle

  ! Random points in a square of size SIZE and centered with O
  call random_number(u)
  u = U_MIN+U_SIZE*u
  z1 = cmplx(u(1),u(2),KIND=WP)

  call random_number(u)
  u = U_MIN+U_SIZE*u
  z2 = cmplx(u(1),u(2),KIND=WP)

  call random_number(u)
  u = U_MIN+U_SIZE*u
  z3 = cmplx(u(1),u(2),KIND=WP)

  write(*,FMT) 'Z1 = ', z1
  write(*,FMT) 'Z2 = ', z2
  write(*,FMT) 'Z3 = ', z3
  write(*,*)

  call my_circle%find(z1,z2,z3)

  write(*,FMT) 'R = ', my_circle%get_radius()
  write(*,FMT) 'C = ', my_circle%get_center()
  write(*,*)

  call display()

contains

  subroutine paint_screen()
    use sdl2app, only: draw_circle, fill_circle, refresh, set_color, clear_screen

    integer, parameter :: RED = int(z'FF0000FF')
    integer, parameter :: GREEN = int(z'FF00FF00')
    integer, parameter :: BLUE = int(z'FFFF0000')
    integer, parameter :: YELLOW = ior(RED,GREEN)
    integer, parameter :: CYAN = ior(GREEN,BLUE)
    integer, parameter :: WHITE = ior(YELLOW,BLUE)
    integer, parameter :: ORANGE = int(z'FF4F9DFD') ! As our cursor

    real(WP), parameter :: RADIUS = 0.005_WP*VIEW_SIZE

    ! Almost surely there is a call to a function for each c%re, c%im, r
    associate (r => my_circle%get_radius(), c => my_circle%get_center())
      call clear_screen()

      call set_color(ORANGE)
      call draw_circle(c%re,c%im,r)

      call set_color(RED)
      call fill_circle(z1%re,z1%im,RADIUS)

      call set_color(GREEN)
      call fill_circle(z2%re,z2%im,RADIUS)

      call set_color(BLUE)
      call fill_circle(z3%re,z3%im,RADIUS)

      call set_color(CYAN)
      call fill_circle(0.0_WP,0.0_WP,RADIUS)

      call set_color(WHITE)
      call fill_circle(c%re,c%im,RADIUS)

      call refresh()
    end associate
  end subroutine paint_screen

  subroutine display()
    use sdl2app, only: init_graphics, close_graphics, QUIT_EVENT, &
         get_event

    integer, parameter :: SCREEN_WIDTH = 700
    integer, parameter :: SCREEN_HEIGHT = 700

    real(WP), parameter :: X_MAX = VIEW_SIZE/2, X_MIN = -X_MAX, &
         Y_MIN = X_MIN, Y_MAX = X_MAX

    integer :: ievent = -1000

    call init_graphics('Circle Passing Through 3 Points - OO', &
         WIDTH=SCREEN_WIDTH,HEIGHT=SCREEN_HEIGHT, &
         X1=X_MIN,X2=X_MAX,Y1=Y_MIN,Y2=Y_MAX)

    do while (ievent /= QUIT_EVENT)
       call paint_screen()
       ievent = get_event()
    end do

    call close_graphics()
  end subroutine display

end program circle_3p
