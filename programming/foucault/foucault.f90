!
! Author: ANGELO GRAZIOSI
!
!   created   : Jan 23, 2024
!   last edit : Dec 28, 2024
!
!   Foucault pendulum trajectories.
!
! DESCRIPTION
!
!   Simplified Foucault pendulum in two dimension. We plot the
!   trajectory of the pendulum projected on the tangent plane to the
!   point of a given latitude.
!
! HISTORICAL DATA
!
!   LAMBDA = 48 deg 52' N (48.87 deg), LENP = 67 m, MASS = 28 kg, D = 17 cm
!   R = 23 h 56'/sin(LAMBDA) ~ 31.8 h (31 h 50 min),
!   T = 2*PI*sqrt(l/g) = 16.4 s, THETA = 2.1 deg
!
! REFERENCES
!
!   1. http://www.ph.unito.it/ccl/docenti/menichetti/Meccanica/Foucault.pdf
!
!   2. https://www.researchgate.net/publication/304782028_Le_equazioni_di_moto_del_Pendolo_di_Foucault_The_equations_of_motion_of_the_Foucault_Pendulum
!
!   3. http://wwwinfo.jinr.ru/programs/cern/cernlib.pdf (D203)
!
! SOME TEST RUN
!
!   LENP = 67 m, OMEGA0 = 180, 1800, 3600 deg/h, LAMBDA0 = 90 deg
!   NSOUT 7680, H = 1/128, H = 5E-3 s, VX = 0.01, 0.015 m/s
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
!   cd foucault
!
!   rm -rf *.mod; \
!     gfortran[-mp-X] [-g3 -fbacktrace -fcheck=all] [-march=native] \
!       -Wall [-Wno-unused-dummy-argument] -std=f2018 [-fmax-errors=1] -O3 \
!       -I ../finclude [`sdl2-config --cflags`] \
!       foucault.f90 -o foucault$EXE \
!       -L ../lib -lbasic_mods -lfortran-sdl2apps -lfortran-sdl2 $LIBS; \
!   rm -rf *.mod
!
!   ./foucault$EXE
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

module foucault_lib
  use :: kind_consts, only: WP
  use :: math_consts, only: R2 => Q1_2, R6 => Q1_6, R8 => Q1_8, DEG2RAD, &
       TWO_PI
  use :: getdata, only: get
  use :: shading_colors, only: LRED, LGREEN, LBLUE, YELLOW, WHITE, &
       BORLAND_PALETTE
  use :: sdl2app, only: init_graphics, close_graphics, QUIT_EVENT, &
       get_event, quit, draw_point, draw_circle, draw_ellipse, set_color, &
       clear_screen, refresh, fill_circle

  implicit none
  private

  real(WP), parameter :: GACC = 9.81_WP

  character(len=*), parameter :: TITLE = 'The Foucault Pendulum'
  character(len=*), parameter :: FMT = '(*(g0,1x))'

  ! DATA
  !
  integer :: screen_width = 900, screen_height = 900, &
       nsout = 128*60

  ! omega0 in deg/h, lambda0 in deg, theta0 in deg, lenp in m
  real(WP) :: xsize = 10, h = 1.0_WP/128, v0(2) = [0, 0], &
       omega0 = 15, lambda0 = 48.87_WP, lenp = 67, theta0 = 2.1_WP

  ! AUXILIARY DATA
  !
  integer :: istep, ival

  ! lambda in rad, omega in rad/s
  real(WP) :: sinl, t, q(2), v(2), h2, h6, hh2, hh6, hh8, &
       lambda, omega, aa, bb, x_min, x_max, y_min, y_max, ysize, rval, &
       work_space(2,6), rotation_time, elong

  public :: app_menu

contains

  ! System of autonomous equations
  subroutine field(q,v,f)
    real(WP), intent(in) :: q(2), v(2)
    real(WP), intent(out) :: f(2)

    f(1) = aa*q(1)+bb*v(2)
    f(2) = aa*q(2)-bb*v(1)

    ! q = [0, 2.5], v = [0.5, 0.25]
    !f = -q/dot_product(q,q)

    ! q = [0, 2.5], v = [0.9, 0]
    !f = -q/(norm2(q))**3

  end subroutine field

  subroutine setup_params()
    ! H constants
    h2 = R2*h
    h6 = R6*h

    hh2 = h*h2
    hh6 = h*h6
    hh8 = R8*h*h

    istep = 0
    t = 0

    ! THETA in radians and computing the elongation a t = 0
    rval = theta0*DEG2RAD
    elong = lenp*sin(rval)

    q = [0.0_WP, elong]
    v = v0

    lambda = lambda0*DEG2RAD
    omega = omega0*DEG2RAD/3600  ! In rad/s

    sinl = sin(lambda)

    aa = -GACC/lenp    ! Omega_p**2 = g/l, T = 2*PI/Omega_p = 2*PI*sqrt(l/g)
    bb = 2*omega*sinl

    ! Computing params for initial positions
    x_max = xsize/2
    x_min = -x_max

    ysize = (xsize*screen_height)/screen_width
    y_max = ysize/2
    y_min = -y_max

  end subroutine setup_params

  subroutine show_params()
    ! R = T/sin(lambda) = (2*PI/omega)/sin(lambda)
    !   = (360/omega0)/sin(lambda), in hours if omega0 in deg/h
    rotation_time = (360/omega0)/sinl

    write(*,*) 'Historical parameters:'
    write(*,*)
    write(*,*) 'LENP (m) = 67'
    write(*,*) 'MASS (kg) = 28'
    write(*,*) 'DIAMETER (cm) = 17'
    write(*,*) 'THETA (deg) = 2.1'
    write(*,*) 'LAMBDA (deg) = 48.87'
    write(*,*) 'R (h) = 31.8'
    write(*,*) 'T (s) = 16.4'
    write(*,*)
    write(*,*) 'Current parameters:'
    write(*,*)
    write(*,*) 'LENP (m) = ', lenp
    write(*,*) 'THETA (deg) = ', theta0
    write(*,*) 'ELONG (m) = ', elong
    write(*,*) 'V0 (m/s) = ', v0
    write(*,*)
    write(*,*) 'OMEGA (deg/h)  = ', omega0
    write(*,*) 'LAMBDA (deg)  = ', lambda0
    write(*,*)
    write(*,*) 'NSOUT  = ', nsout
    write(*,*)
    write(*,*) 'R (h) = ', rotation_time
    write(*,*) 'T (s) = ', TWO_PI/sqrt(abs(aa))
    write(*,*) 'H (s) = ', h
    write(*,*)
    write(*,*) 'XSIZE (m) = ', xsize
    write(*,*) 'YSIZE (m) = ', ysize
    write(*,*)
    write(*,*) 'SCREEN_WIDTH  = ', screen_width
    write(*,*) 'SCREEN_HEIGHT = ', screen_height
    write(*,*)
  end subroutine show_params

  subroutine draw_logo()
    integer, save :: icolor = 0

    call set_color(LRED)
    call draw_ellipse(screen_width/2,screen_height/2,250,100)
    call set_color(LGREEN)
    call draw_ellipse(screen_width/2,screen_height/2,100,250)
    call set_color(LBLUE)
    call draw_circle(screen_width/2,screen_height/2,250)
    call set_color(WHITE)
    call draw_circle(screen_width/2,screen_height/2,100)

    call set_color(BORLAND_PALETTE(icolor))
    call fill_circle(screen_width/2,screen_height/2,98)
    call refresh()

    icolor = mod(icolor+1,16)
  end subroutine draw_logo

  subroutine paint_screen()

    do while (.not. quit())
       ! Draw pendulum at current time (ISTEP)
       call display_position()

       if (mod(istep,nsout) == 0) &
            write(*,FMT) 'CURRENT STEP: ', istep, 'TIME (s): ', t, &
            'Q (m): ', q

       ! t + h
       istep = istep+1
       t = istep*h
       call update_position()
    end do

    ! Draw pendulum at the last step executed
    call display_position()

  contains

    subroutine display_position()

      call draw_point(q(1),q(2))
      call refresh()
    end subroutine display_position

    ! Advance the position from t to t+h: CERNLIB-like. See
    !
    !   https://github.com/apc-llc/cernlib/blob/master/2006/src/mathlib/gen/d/rknys64.F
    !
    subroutine update_position()
      associate (w1 => work_space(:,1), w2 => work_space(:,2), &
           w3 => work_space(:,3), w4 => work_space(:,4), &
           qp => work_space(:,5), vp => work_space(:,6))
        call field(q,v,w1)   ! K1

        !tp = t+h2
        w4 = q+h2*v
        qp = w4+hh8*w1       ! QP = Q+H2*V+HH8*K1
        vp = v+h2*w1         ! VP =V+H2*K1
        call field(qp,vp,w2) ! K2

        vp = v+h2*w2         ! VP = V+H2*K2
        w1 = w1+w2           ! W1 = K1+K2
        w2 = w1+w2           ! W2 = K1+K2+K2 = K1+2*K2
        call field(qp,vp,w3)

        !t = t+h
        w4 = w4+h2*v         ! W4 = Q+H2*V+H2*V = Q+H*V
        qp = w4+hh2*w3       ! QP = Q+H*V+HH2*K3
        vp = v+h*w3          ! VP = V+H*K3
        w1 = w1+w3           ! W1 = K1+K2+K3
        w2 = w2+2*w3         ! W2 = K1+2*K2+2*K3
        call field(qp,vp,w3) ! K4

        q = w4+hh6*w1        ! Q = Q+H*V+HH6*(K1+K2+K3)
        v = v+h6*(w2+w3)     ! V = V+H6*(K1+2*K2+2*K3+K4)
      end associate
    end subroutine update_position
  end subroutine paint_screen

  subroutine run()
    integer :: ievent

    ! First the PARAMS ...
    call setup_params()
    call show_params()

    call init_graphics(TITLE, &
         WIDTH=screen_width,HEIGHT=screen_height, &
         X1=x_min,X2=x_max,Y1=y_min,Y2=y_max)

    ! We need to reset IEVENT if we want to restart the run
    ievent = -1000
    call draw_logo()
    ievent = get_event()

    call clear_screen()
    call set_color(YELLOW)

    if (ievent /= QUIT_EVENT) then

       ! We need to reset IEVENT if we want to restart the run
       ievent = -1000
       do while (ievent /= QUIT_EVENT)
          call paint_screen()

          ievent = get_event()
       end do

    end if

    call close_graphics()
  end subroutine run

  ! ================
  !    M  E  N  U
  ! ================

  subroutine set_lenp()
    rval= lenp
    call get('LENP (m) =',rval)
    if (rval > 0) then
       lenp = rval
    else
       write(*,*) 'LENP <= 0! UNCHANGED...'
    end if
  end subroutine set_lenp

  subroutine set_theta()
    rval= theta0
    call get('THETA (deg) =',rval)
    if (0 <= rval .and. rval <= 30) then
       theta0 = rval
    else
       write(*,*) '|THETA| > 30 DEG! UNCHANGED...'
    end if
  end subroutine set_theta

  subroutine set_velocity()
    call get('VX (m/s) =',v0(1))
    call get('VY (m/s) =',v0(2))
  end subroutine set_velocity

  subroutine set_omega()
    rval= omega0
    call get('OMEGA (deg/h) =',rval)
    if (rval >= 0) then
       omega0 = rval
    else
       write(*,*) 'OMEGA < 0! UNCHANGED...'
    end if
  end subroutine set_omega

  subroutine set_lambda()
    rval= lambda0
    call get('LAMBDA (deg) =',rval)
    if (0 <= rval .and. rval <= 90) then
       lambda0 = rval
    else
       write(*,*) '|LAMBDA| > 90! UNCHANGED...'
    end if
  end subroutine set_lambda

  subroutine set_nsout()
    ival= nsout
    call get('NSOUT =',ival)
    if (ival > 0) then
       nsout = ival
    else
       write(*,*) 'NSOUT <= 0! UNCHANGED...'
    end if
  end subroutine set_nsout

  subroutine set_step()
    rval= h
    call get('H (s) =',rval)
    if (rval > 0) then
       h = rval
    else
       write(*,*) 'H <= 0! UNCHANGED...'
    end if
  end subroutine set_step

  subroutine set_xsize()
    rval= xsize
    call get('XSIZE (m) =',rval)
    if (rval > 0) then
       xsize = rval
    else
       write(*,*) 'XSIZE <= 0! UNCHANGED...'
    end if
  end subroutine set_xsize

  subroutine set_screen_size()
    ival= screen_width
    call get('SCREEN_WIDTH =',ival)
    if (ival > 0) then
       screen_width = ival
    else
       write(*,*) 'SCREEN_WIDTH <= 0! UNCHANGED...'
    end if

    ival= screen_height
    call get('SCREEN_HEIGHT =',ival)
    if (ival > 0) then
       screen_height = ival
    else
       write(*,*) 'SCREEN_HEIGHT <= 0! UNCHANGED...'
    end if
  end subroutine set_screen_size

  subroutine show_menu()
    write(*,*) 'Choose item:'
    write(*,*)
    write(*,*) 'Pendulum:'
    write(*,*) '  L : Length'
    write(*,*) '  T : THETA'
    write(*,*) '  V : Velocity'
    write(*,*)
    write(*,*) 'Earth:'
    write(*,*) '  W : Angular speed'
    write(*,*) '  B : Lambda'
    write(*,*)
    write(*,*) 'Integration:'
    write(*,*) '  O : Output Rate'
    write(*,*) '  H : Time Step'
    write(*,*)
    write(*,*) 'Screen:'
    write(*,*) '  G : Region'
    write(*,*) '  S : Screen Size'
    write(*,*)
    write(*,*) '  R : RUN'
    write(*,*) '  Q : QUIT'
  end subroutine show_menu

  subroutine process_menu(key)
    character, intent(in) :: key

    select case (key)
    case ('L')
       call set_lenp()
    case ('T')
       call set_theta()
    case ('V')
       call set_velocity()
    case ('W')
       call set_omega()
    case ('B')
       call set_lambda()
    case ('O')
       call set_nsout()
    case ('H')
       call set_step()
    case ('G')
       call set_xsize()
    case ('S')
       call set_screen_size()

    case ('R')
       call run()
    end select
  end subroutine process_menu

  subroutine app_menu()
    character :: key

    do
       call show_menu()

       ! Default PROMPT
       key = 'R'
       call get('Choice :',key)

       ! Convert in upcase if not
       if ('a' <= key .and. key <= 'z') key = char(ichar(key)-32)

       if (key == 'Q') exit

       write(*,*)

       call process_menu(key)
    end do
  end subroutine app_menu
end module foucault_lib

program foucault
  use :: foucault_lib

  implicit none

  call app_menu()
end program foucault
