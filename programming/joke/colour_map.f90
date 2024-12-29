!
! Author: ANGELO GRAZIOSI
!
!   created   : Dec 17, 2022
!   last edit : Dec 29, 2024
!
!   Colour Map.
!
! DESCRIPTION
!
!   Displaying a colour map. Just a joke reimplementing the
!   shade.kumac PAW macro in fortran-sdl2!
!
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
!   cd joke
!
!   rm -rf *.mod; \
!     gfortran[-mp-X] [-g3 -fbacktrace -fcheck=all] [-march=native] \
!       -Wall -std=f2018 [-fmax-errors=1] -O3 \
!       -I ../finclude [`sdl2-config --cflags`] \
!       colour_map.f90 -o colour_map$EXE \
!       -L ../lib -lbasic_mods -lfortran-sdl2apps -lfortran-sdl2 \
!       $LIBS; \
!   rm -rf *.mod
!
!   ./colour_map$EXE
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
!   app. This means that will not show up a console/terminal to input
!   data. On these systems, the LIBS definition should be:
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

program colour_map
  use :: kind_consts, only: WP
  use :: math_consts, only: ZERO => Z0, ONE => Z1, TWO_PI
  use :: shading_colors, only: MAX_COLOUR_INDEX, MAX_COLOURS, shading_setup, &
       get_shading_color, color_rgb_t
  use :: sdl2app, only: init_graphics, close_graphics, set_rgba_color, &
       draw_point, clear_screen, refresh, QUIT_EVENT, get_event, quit

  implicit none

  integer, parameter :: SCREEN_WIDTH = 600, SCREEN_HEIGHT = 600, NSTPS = 1000
  real(WP), parameter :: TP = 10.0_WP, OMEGA = TWO_PI/TP, DT = TP/NSTPS

  ! WC-DC tranformation parameters
  real(WP) :: x_min, x_max, y_min, y_max, x1s, x2s, y1s, y2s, sx, sy

  ! FCN parameters
  real(WP) :: f, df, f_min, f_max, x, y, t
  integer :: i, j, i_max, j_max, k, ievent = -1000

  type(color_rgb_t) :: c

  call shading_setup()

  i_max = SCREEN_WIDTH-1
  j_max = SCREEN_HEIGHT-1

  ! Setup the DC window
  x1s = ZERO
  x2s = real(i_max,WP)

  ! NB. the exchange
  y2s = ZERO
  y1s = real(j_max,WP)

  ! Setup the WC window
  x_min = -2.0_WP
  x_max = 2.0_WP
  y_min = -2.0_WP
  y_max = 2.0_WP

  ! Computing DC to WC transormation parameters
  sx = (x_max-x_min)/(x2s-x1s)
  sy = (y_max-y_min)/(y2s-y1s)

  ! Setup FCN colour mapping
  f_min = -ONE
  f_max = ONE

  df = (f_max-f_min)/MAX_COLOURS

  call init_graphics('Colour Map Demo', &
       width=SCREEN_WIDTH,height=SCREEN_HEIGHT)

  call clear_screen()

  do while (ievent /= QUIT_EVENT)

     t = ZERO
     do while (t < TP .and. .not. quit())

        do j = 0, j_max
           y = map_y(real(j,WP))
           do i = 0, i_max
              x = map_x(real(i,WP))

              f = fcn(x,y,t)
              k = int((f-f_min)/df)

              !print *, c

              ! A safe guard
              if (k < 0) k = 0
              if (k > MAX_COLOUR_INDEX) k = MAX_COLOUR_INDEX

              c = get_shading_color(k)

              call set_rgba_color(c%r,c%g,c%b)
              call draw_point(i,j)
           end do
        end do
        call refresh()

        t = t+DT
     end do

     write(*,*) 'Ok, this is done!'

     ! Wait for a key or mouse click
     ievent = get_event()
  end do

  call close_graphics()

contains

  function fcn(x,y,t) result(r)
    real(WP), intent(in) :: x, y, t
    real(WP) :: r

    r = sin(OMEGA*t)*exp(-(x**2 + (y-(-1))**2))
  end function fcn

  function map_x(xs) result (x)
    real(WP), intent(in) :: xs
    real(WP) :: x

    x = x_min+sx*(xs-x1s)
  end function map_x

  function map_y(ys) result (y)
    real(WP), intent(in) :: ys
    real(WP) :: y

    y = y_min+sy*(ys-y1s)
  end function map_y

end program colour_map
