!
! From: https://en.wikipedia.org/wiki/Logistic_map
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
!   cd maps_attractors
!
!   rm -rf *.mod; \
!     gfortran[-mp-X] [-g3 -fbacktrace -fcheck=all] [-march=native] \
!       -Wall [-Wno-unused-dummy-argument] -std=f2018 [-fmax-errors=1] -O3 \
!       -I ../finclude [`sdl2-config --cflags`] \
!       logistic_map.f90 -o logistic_map$EXE \
!       -L ../lib -lbasic_mods -lfortran-sdl2apps -lfortran-sdl2 $LIBS; \
!   rm -rf *.mod
!
!   ./logistic_map$EXE
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

program logistic_map
  use :: kind_consts, only: WP

  implicit none

  character(len=*), parameter :: FMT = '(*(1x,g0))'
  integer, parameter :: MAX_ITER = 5000, NPOINTS = 200
  real(WP), parameter :: R1 = 0.0_WP, R2 = 4.0_WP, RSTP = 0.001_WP

  write(*,FMT) 'MAX_ITER = ', MAX_ITER
  write(*,FMT) 'NPOINTS = ', NPOINTS
  write(*,*)
  write(*,FMT) 'R1 = ', R1
  write(*,FMT) 'R2 = ', R2
  write(*,FMT) 'RSTP = ', RSTP
  write(*,*)

  call display()

contains

  subroutine paint_screen()
    use sdl2app, only: draw_point, refresh, set_color, clear_screen

    integer, parameter :: ORANGE = int(z'FF4F9DFD') ! As our cursor

    integer  :: k, l, n_rstp
    real(WP) :: r, p, p0

    ! Initial population in (0,1)
    call random_number(p0)

    write(*,FMT) 'P0 = ', p0

    call clear_screen()
    call set_color(ORANGE)

    ! Number of r steps, rounding up
    n_rstp = 1+int((R2-R1)/RSTP)

    r = R1
    do l = 1, n_rstp
       p = p0
       do k = 1, MAX_ITER
          p = r*p*(1.0_WP-p)
       end do

       ! Now we assume having reached the "convergence", i.e. a fix,
       ! oscillating or chaotic limit. So we can plot at most npoints points
       do k = 1, NPOINTS
          p = r*p*(1.0_WP-p)

          ! Drawing point k...
          call draw_point(r,p)
       end do
       call refresh()

       r = r+RSTP
    end do
  end subroutine paint_screen

  subroutine display()
    use sdl2app, only: init_graphics, close_graphics, QUIT_EVENT, &
         get_event

    integer, parameter :: SCREEN_WIDTH = 1000
    integer, parameter :: SCREEN_HEIGHT = 700

    ! DX = (L-(R2-R1))/2, L = 1.05*(R2-R1)
    ! DY = (L-(P2-P1))/2, L = 1.05*(P2-P1), P1 = 0, P2 = 1
    real(WP), parameter :: DX = 0.025_WP*(R2-R1), &
         DY = 0.025_WP

    real(WP), parameter :: X_MIN = R1-DX, X_MAX = R2+DX, &
         Y_MIN = 0.0_WP-DY, Y_MAX = 1.0_WP+DY

    integer :: ievent = -1000

    call init_graphics('Logistic Map', &
         WIDTH=SCREEN_WIDTH,HEIGHT=SCREEN_HEIGHT, &
         X1=X_MIN,X2=X_MAX,Y1=Y_MIN,Y2=Y_MAX)

    do while (ievent /= QUIT_EVENT)
       call paint_screen()
       ievent = get_event()
    end do

    call close_graphics()
  end subroutine display

end program logistic_map
