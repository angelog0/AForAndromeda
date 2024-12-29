!
! From: https://en.wikipedia.org/wiki/Henon_map
!
! RUN EXAMPLES
!
!   MAX_ITER = 100000/1000000, C = (0.5,0.2), D = (0.0625, 0.0625)
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
!       henon_map.f90 -o henon_map$EXE \
!       -L ../lib -lbasic_mods -lfortran-sdl2apps -lfortran-sdl2 $LIBS; \
!   rm -rf *.mod
!
!   ./henon_map$EXE
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

program henon_map
  use :: kind_consts, only: WP
  use :: sdl2app, only: init_graphics, close_graphics, QUIT_EVENT, &
       get_event, set_map_window, select_map, set_map_viewport

  implicit none

  character(len=*), parameter :: FMT = '(*(1x,g0))'

  integer, parameter :: SCREEN_WIDTH = 700
  integer, parameter :: SCREEN_HEIGHT = 700

  integer, parameter :: IMAX = SCREEN_WIDTH-1
  integer, parameter :: JMAX = SCREEN_HEIGHT-1

  integer :: ievent = -1000
  real(WP) :: xc = 0, yc = 0, half_deltax = 2, half_deltay = 2
  real(WP) :: x1 = 0, x2 = 0, y1 = 0, y2 = 0

  call init_graphics('Henon Map/Attractor', &
       WIDTH=SCREEN_WIDTH,HEIGHT=SCREEN_HEIGHT)

  ! The SCREEN; TEMP usage
  x1 = 0
  x2 = IMAX
  y1 = 0
  y2 = JMAX
  call set_map_viewport(1,x1,x2,y1,y2)
  call select_map(1)

  ievent = -1000
  do while (ievent /= QUIT_EVENT)

     write(*,*)
     write(*,FMT) 'Click mouse left to ZOOM IN'
     write(*,FMT) 'Click mouse right to ZOOM OUT'

     write(*,*)
     write(*,FMT) 'CENTER   = ', xc, yc
     write(*,FMT) 'DELTA    = ', 2*half_deltax, 2*half_deltay
     write(*,*)

     x1 = xc-half_deltax
     y1 = yc-half_deltay

     x2 = xc+half_deltax
     y2 = yc+half_deltay

     call set_map_window(1,x1,x2,y1,y2)

     call paint_screen()

     ievent = get_event()
     call process_event(ievent)
  end do

  call close_graphics()

contains

  subroutine paint_screen()
    use :: sdl2app, only: draw_point, refresh, set_color, clear_screen, quit

    !integer, parameter :: NSOUT = 5000
    integer, parameter :: ORANGE = int(z'FF4F9DFD') ! As our cursor
    real(WP), parameter :: A = 1.4_WP, B = 0.3_WP

    integer  :: k
    real(WP) :: x, y, xn

    call clear_screen()
    call set_color(ORANGE)

    ! Init point
    x = 0
    y = 0

    k = 0

    !
    ! x(n+1) = 1+y(n)-a*x(n)**2
    ! y(n+1) = b*x(n)
    !
    do while (.not. quit())

       ! Draw only visible points at current step. It is much faster!
       if (((x1 < x) .and. (x < x2)) .and. ((y1 < y) .and. (y < y2))) then
          call draw_point(x,y)
          call refresh()
       end if

       ! if (mod(k,NSOUT) == 0) &
       !      write(*,FMT) 'CURRENT STEP            : ', k

       ! Update the step
       k = k+1
       xn = x
       x = 1+y-A*xn**2
       y = B*xn

    end do

    ! Draw only visible points at the last step. It is much faster!
    if (((x1 < x) .and. (x < x2)) .and. ((y1 < y) .and. (y < y2))) then
       call draw_point(x,y)
       call refresh()
    end if

    write(*,FMT) 'MAX_ITER = ', k
    write(*,FMT) 'P        = ', x, y
  end subroutine paint_screen

  subroutine process_event(ievent)
    use :: sdl2, only: SDL_BUTTON_LEFT, SDL_BUTTON_RIGHT
    use :: sdl2app, only: get_mouse_x, get_mouse_y

    integer, intent(in) :: ievent

    real(WP) :: dx, dy


    ! The current increments per pixel
    dx = (x2-x1)/IMAX
    dy = (y2-y1)/JMAX

    !
    ! NOTICE that 'get_mouse_x()' and 'get_mouse_y()' return the
    ! clicked point in absolute coordinate. We need to map it in
    ! the output window and with right orientation (Y pixels axis
    ! pointing bottom)
    !
    !   i = (get_mouse_x()-i_left), j = (get_mouse_y()-j_top)
    !
    !   x = x1+i*dx, y = y2-j*dy
    !

    ! Process the event
    select case (ievent)
    case (SDL_BUTTON_LEFT)

       ! Set the center point and zoom IN
       xc = x1+(get_mouse_x()-0)*dx
       yc = y2-(get_mouse_y()-0)*dy
       half_deltax = half_deltax/2
       half_deltay = half_deltay/2

    case (SDL_BUTTON_RIGHT)

       ! Set the center point and zoom OUT
       xc = x1+dx*(get_mouse_x()-0)
       yc = y2-dy*(get_mouse_y()-0)
       half_deltax = half_deltax*2
       half_deltay = half_deltay*2

    end select

  end subroutine process_event

end program henon_map
