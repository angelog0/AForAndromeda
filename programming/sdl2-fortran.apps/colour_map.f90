!
! Author: ANGELO GRAZIOSI
!
!   created   : Dec 17, 2022
!   last edit : Jul 04, 2023
!
!   Colour Map.
!
! DESCRIPTION
!
!   Displaying a colour map. Just a joke reimplementing the
!   shade.kumac PAW macro in SDL2-Fortran!
!
!
! HOW TO BUILD THE APP
!
!   cd sdl2-fortran.apps
!
!   git clone https://github.com/interkosmos/fortran-sdl2.git
!
!   rm -rf *.mod; \
!     gfortran[-mp-X] -std=f2018 -O3 -Wall [`sdl2-config --cflags`] \
!       ../basic-modules/{{kind,math}_consts,nicelabels}.f90 \
!       $SDL2F90 SDL2_{app,shading}.f90 \
!       colour_map.f90 \
!       $LIBS -o colour_map$EXE; \
!   rm -rf *.mod
!
!   ./colour_map$EXE
!
!   where, for the build on GNU/Linux [OSX+MacPorts X server], is:
!
!     EXE = .out
!
!   while for the build on MSYS2/MINGW64 is:
!
!     EXE = -$MSYSTEM (or EMPTY)
!
!   and (all platform):
!
!     B = ../..
!     S = ..
!
!     SDL2F90 = fortran-sdl2/src/{c_util,sdl2/{sdl2_stdinc,sdl2_audio,\
!       sdl2_blendmode,sdl2_cpuinfo,sdl2_gamecontroller,sdl2_error,\
!       sdl2_events,sdl2_filesystem,sdl2_hints,sdl2_joystick,sdl2_keyboard,\
!       sdl2_log,sdl2_messagebox,sdl2_rect,sdl2_pixels,sdl2_platform,\
!       sdl2_scancode,sdl2_surface,sdl2_render,sdl2_keycode,sdl2_mouse,\
!       sdl2_rwops,sdl2_thread,sdl2_timer,sdl2_version,sdl2_video,\
!       sdl2_opengl},sdl2}.f90
!
!
!     LIBS = `sdl2-config --libs`
!
!   Notice that the above definition for LIBS produces a pure Windows
!   app on MSYS2/MINGW64. This means that it will not show up a
!   console/terminal for input data. On these systems, the LIBS
!   definition should be:
!
!     LIBS = [-lSDL2main] -lSDL2 -lgdi32 -lcomdlg32 -luuid -loleaut32 -lole32
!
!   For a static build (run from Explorer), I have found usefull
!
!     LIBS = -static -lmingw32 -lSDL2main -lSDL2 -lws2_32 -ldinput8 \
!            -ldxguid -ldxerr8 -luser32 -lgdi32 -lwinmm -limm32 -lole32 \
!            -loleaut32 -lshell32 -lversion -luuid -lcomdlg32 -lhid -lsetupapi
!
!   See as references:
!
!     1. https://stackoverflow.com/questions/53885736/issues-when-statically-compiling-sdl2-program
!     2. https://groups.google.com/g/comp.lang.fortran/c/Usgys7Gww6o/m/CYEfzQfbhckJ
!
!
! NOTE FOR WINDOWS
!
!   On Windows the application _hangs_ (NOT RESPONDING) when its
!   window has focus (i.e. is selected) so the best way to launch it
!   is from CMD or Explorer. From the MSYS2/MINGW64 shell one should
!   use:
!
!     open colour_map$EXE
!
!   being:
!
!     alias open='start'
!
!   Maybe the same considerations hold for GNU/Linux and macOS.
!

program colour_map
  use kind_consts, only: WP
  use math_consts, only: ZERO => Z0, ONE => Z1, TWO_PI
  use SDL2_shading, only: MAX_COLOUR_INDEX, MAX_COLOURS, shading_setup, &
       get_shading_color, color_rgb_t
  use SDL2_app, only: init_graphics, close_graphics, set_rgba_color, &
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
