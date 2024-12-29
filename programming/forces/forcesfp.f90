!
! Author: ANGELO GRAZIOSI
!
!   created   : Apr 20, 2024
!   last edit : Dec 27, 2024
!
!   Particle trajectories.
!
! DESCRIPTION
!
!   We plot the trajectory of a particle subject to a force on the
!   plane.
!
! REFERENCES
!
!   1. http://wwwinfo.jinr.ru/programs/cern/cernlib.pdf (D203)
!
! SOME TEST RUN
!
!   1. F(t,Q,V) = -k(t)*Q-b*V
!
!     A. k(t) = 16, b = 0, Q = [0,3], V = [12,0]
!     B. k(t) = 16, b = 0.05, Q = [0,3], V = [12,0]
!     C. k(t) = 16+2*sin(t), b = 0.05, Q = [0,3], V = [12,0]
!
!   2. F(t,Q,V) = -k(t,r)*Q-b*V, r = norm2(Q) : Central Field Motion
!
!     A. k(r,t) = r**2, b = 0, Q = [0,3], V = [12,0]
!     B. k(r,t) = sqrt(r), b = 0, Q = [0,3], V = [12,0]
!     C. k(r,t) = log(r), b = 0, Q = [0,3], V = [12,0]
!     D. k(r,t) = log(r)+CC*sin(t), b = 0, CC = 1, Q = [0,3], V = [12,0]
!
!   3. F(t,Q,V) = MU*(1-Q(1)**2)*SGMZ*V-Q+[AA,BB]*sin(WW*t) : Van der Pol
!
!     A. MU = 2, Q = [0,0], V = [12,0], AA = BB = 0
!     B. MU = 8.53, Q = [-1,0], V = [0,0], AA = 1.2, BB = 0, WW = 2*PI/10,
!        XSIZE = 300, YSIZE = 5, SCREEN_W = 1500, SCREEN_H = 300
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
!   wget -q http://warp.povusers.org/FunctionParser/fparser4.5.2.zip
!   aunpack -q fparser4.5.2.zip -X fparser-4.5.2/ > /dev/null
!   rm -rf fparser4.5.2.zip
!
!   cd fortran-fparser
!
!   make FFLAGS='[-march=native] -Wall -std=f2018 -fmax-errors=1 -O3' all
!   mv *.a ../lib/
!   mv *.mod ../finclude/
!   make clean
!   cd ..
!
!   cd forces
!
!   rm -rf *.mod; \
!     gfortran[-mp-X] [-g3 -fbacktrace -fcheck=all] [-march=native] \
!       -Wall [-Wno-unused-dummy-argument] -std=f2018 [-fmax-errors=1] -O3 \
!       -I ../finclude [`sdl2-config --cflags`] \
!       forcesfp.f90 -o forcesfp$EXE \
!       -L ../lib -lfortran-fparser -lbasic_mods -lfortran-sdl2apps -lfortran-sdl2 \
!       -lfpc++ -lstdc++ $LIBS; \
!   rm -rf *.mod
!
!   ./forcesfp$EXE
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

module forces_lib
  use :: kind_consts, only: WP
  use :: math_consts, only: R2 => Q1_2, R6 => Q1_6, R8 => Q1_8, &
       PI, E_NEPER
  use :: fparser_dp, only: function_parser=> FunctionParser_type, &
       new_parser => NewParser, parse_function => Parse, &
       error_msg => ErrorMsg, get_parse_error => GetParseErrorType, &
       delete_parser => DeleteParser, eval_function => Eval, &
       add_constant => AddConstant, optimize_parser => Optimize
  use :: getdata, only: get
  use :: shading_colors, only: LRED, LGREEN, LBLUE, YELLOW, WHITE, &
       BORLAND_PALETTE
  use :: sdl2app, only: init_graphics, close_graphics, QUIT_EVENT, &
       get_event, quit, draw_point, draw_circle, draw_ellipse, set_color, &
       clear_screen, refresh, fill_circle

  implicit none
  private

  integer, parameter :: MAX_FCN_BUF = 128

  ! Useful constants
  integer, parameter :: X_Y_PLOT  = 1
  integer, parameter :: X_VX_PLOT = 2
  integer, parameter :: Y_VY_PLOT = 3
  integer, parameter :: T_X_PLOT  = 4
  integer, parameter :: T_Y_PLOT  = 5

  ! To strip '  1 : ' use PLOT(1)(7:) and so on...
  character(len=*), parameter :: PLOT(5) = [ &
       '  1 : X - Y ', &
       '  2 : X - VX', &
       '  3 : Y - VY', &
       '  4 : T - X ', &
       '  5 : T - Y ' ]

  character(len=*), parameter :: TITLE = 'F O R C E S'
  character(len=*), parameter :: FMT = '(*(g0,1x))'

  ! DATA
  !
  integer :: screen_width = 900, screen_height = 900, &
       nsout = 2**10, plot_type = X_VX_PLOT

  real(WP) :: xsize = 25, ysize = 25, h = 1.0_WP/(2**10), &
       q0(2) = [-3,1], v0(2) = [0, 0]

  character(len=MAX_FCN_BUF) :: &
       fx_buffer = '(853/100)*(1-x*x)*u-x+(12/10)*sin(pi*t/5)', &
       fy_buffer = '0' !'-(853/100)*(1-x*x)*v-y'

  ! AUXILIARY DATA
  !
  integer :: istep, ival

  real(WP) :: tp, h2, h6, hh2, hh6, hh8, &
       x_min, x_max, y_min, y_max, rval, &
       work_space(2,6)

  real(WP), target :: t, q(2), v(2)
  real(WP), pointer :: xx => q(1), yy => q(2)

  type(function_parser) :: fpx, fpy

  public :: app_menu

contains

  ! System of equations (uppercase: vectors)
  subroutine field(t,q,v,f)
    real(WP), intent(in) :: t, q(2), v(2)
    real(WP), intent(out) :: f(2)

    f(1) = eval_function(fpx,[t,q(1),q(2),v(1),v(2)])
    f(2) = eval_function(fpy,[t,q(1),q(2),v(1),v(2)])

  end subroutine field

  ! Create the PARSER
  !
  ! About Fortran C interface to string coul be useful:
  ! https://stackoverflow.com/questions/9972743/creating-a-fortran-interface-to-a-c-function-that-returns-a-char
  !
  subroutine create_parser(fcn_buffer,fcn_vars,fp)
    character(len=*), intent(in) :: fcn_buffer, fcn_vars
    type(function_parser), intent(out) :: fp

    integer :: ierr = 0

    ! Create the fparser
    call new_parser(fp)

    if (add_constant(fp,'pi',PI) <= 0) &
         stop ': ADD_CONSTANT(PI) error...'

    if (add_constant(fp,'e',E_NEPER) <= 0) &
         stop ': ADD_CONSTANT(E) error...'

    ierr = parse_function(fp,fcn_buffer,fcn_vars)

    if (ierr >= 0) then
       write(*,*) 'Failure creating FCN parser ...'
       write(*,*)

       ! Notice, 6 is the number of characters in 'FCN = '...
       write(*,'(a)') 'FCN = '//trim(fcn_buffer)
       write(*,'(a)') repeat(' ',ierr+6)//'^'

       ! Remember : error_msg() is an array of characters...
       write(*,*) error_msg(fp)
       write(*,*) 'Error type: ',get_parse_error(fp)
       stop ': FParser creation failure (CREATE_PARSER).'
    end if

    call optimize_parser(fp)
  end subroutine create_parser

  subroutine setup_params()
    ! H constants
    h2 = R2*h
    h6 = R6*h

    hh2 = h*h2
    hh6 = h*h6
    hh8 = R8*h*h

    istep = 0
    t = 0

    q = q0
    v = v0

    ! Computing params for initial positions: default
    x_max = xsize/2
    x_min = -x_max

    y_max = ysize/2
    y_min = -y_max

    select case(plot_type)
    case(X_Y_PLOT)
       xx => q(1)
       yy => q(2)
    case(X_VX_PLOT)
       xx => q(1)
       yy => v(1)
    case(Y_VY_PLOT)
       xx => q(2)
       yy => v(2)
    case(T_X_PLOT)
       xx => t
       yy => q(1)
       ! Correct the computing params for initial positions
       x_min = 0
       x_max = xsize
    case(T_Y_PLOT)
       xx => t
       yy => q(2)
       ! Correct the computing params for initial positions
       x_min = 0
       x_max = xsize

    case default
       ! X - Y Plot
       xx => q(1)
       yy => q(2)
    end select

  end subroutine setup_params

  subroutine show_params()
    write(*,*) 'Current parameters:'
    write(*,*)
    write(*,*) 'FX(t,x,y,u,v) = '//trim(fx_buffer)
    write(*,*) 'FY(t,x,y,u,v) = '//trim(fy_buffer)
    write(*,*)
    write(*,*) 'Q0 (m) = ', q0
    write(*,*) 'V0 (m/s) = ', v0
    write(*,*)
    write(*,*) 'NSOUT  = ', nsout
    write(*,*)
    write(*,*) 'H (s) = ', h
    write(*,*)
    write(*,*) 'USING '//trim(PLOT(plot_type)(7:))//' PLOT'
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

       ! On exit istep = istep+1, t = t+h
       call update_position()
    end do

    ! Draw pendulum at the last step executed
    call display_position()

  contains

    subroutine display_position()

      call draw_point(xx,yy)
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
        call field(t,q,v,w1)    ! K1

        tp = t+h2
        w4 = q+h2*v
        qp = w4+hh8*w1          ! QP = Q+H2*V+HH8*K1
        vp = v+h2*w1            ! VP =V+H2*K1
        call field(tp,qp,vp,w2) ! K2

        vp = v+h2*w2            ! VP = V+H2*K2
        w1 = w1+w2              ! W1 = K1+K2
        w2 = w1+w2              ! W2 = K1+K2+K2 = K1+2*K2
        call field(tp,qp,vp,w3)

        istep = istep+1
        t = istep*h
        w4 = w4+h2*v            ! W4 = Q+H2*V+H2*V = Q+H*V
        qp = w4+hh2*w3          ! QP = Q+H*V+HH2*K3
        vp = v+h*w3             ! VP = V+H*K3
        w1 = w1+w3              ! W1 = K1+K2+K3
        w2 = w2+2*w3            ! W2 = K1+2*K2+2*K3
        call field(t,qp,vp,w3)  ! K4

        q = w4+hh6*w1           ! Q = Q+H*V+HH6*(K1+K2+K3)
        v = v+h6*(w2+w3)        ! V = V+H6*(K1+2*K2+2*K3+K4)
      end associate
    end subroutine update_position
  end subroutine paint_screen

  subroutine run()
    integer :: ievent

    ! First the PARAMS ...
    call setup_params()
    call show_params()

    ! CREATE PARSERS
    call create_parser(fx_buffer,'t,x,y,u,v',fpx)
    call create_parser(fy_buffer,'t,x,y,u,v',fpy)

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

    ! DESTROY PARSERS
    call delete_parser(fpy)
    call delete_parser(fpx)
  end subroutine run

  ! ================
  !    M  E  N  U
  ! ================

  subroutine set_field()
    call get('FX(t,x,y,u,v) = ',fx_buffer)
    call get('FY(t,x,y,u,v) = ',fy_buffer)
  end subroutine set_field

  subroutine set_position()
    call get('X (m) =',q0(1))
    call get('Y (m) =',q0(2))
  end subroutine set_position

  subroutine set_velocity()
    call get('VX (m/s) =',v0(1))
    call get('VY (m/s) =',v0(2))
  end subroutine set_velocity

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

  subroutine set_plot_type()
    integer :: i, m

    m = size(PLOT)

    write(*,*) 'Choose the plot:'
    do i = 1, m
       write(*,*) PLOT(i)
    end do

    call get('PLOT_TYPE =',PLOT_type)
    write(*,*)

    ! Correction so that PLOT_TYPE is always in [1,M]
    ! Remember that MOD(N,M) is in [-(M-1),0] if N < 0!
    if (plot_type <= 0) then
       plot_type = 1
    else
       plot_type = mod(plot_type-1,m)+1
    end if
  end subroutine set_plot_type

  subroutine set_view_size()
    rval= xsize
    call get('XSIZE (m) =',rval)
    if (rval > 0) then
       xsize = rval
    else
       write(*,*) 'XSIZE <= 0! UNCHANGED...'
    end if

    rval= ysize
    call get('YSIZE (m) =',rval)
    if (rval > 0) then
       ysize = rval
    else
       write(*,*) 'YSIZE <= 0! UNCHANGED...'
    end if
  end subroutine set_view_size

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
    write(*,*) 'Field:'
    write(*,*) '  F : Field'
    write(*,*)
    write(*,*) 'Particle:'
    write(*,*) '  P : Position'
    write(*,*) '  V : Velocity'
    write(*,*)
    write(*,*) 'Integration:'
    write(*,*) '  O : Output Rate'
    write(*,*) '  H : Time Step'
    write(*,*)
    write(*,*) 'Screen:'
    write(*,*) '  T : Plot'
    write(*,*) '  W : View Size'
    write(*,*) '  S : Screen Size'
    write(*,*)
    write(*,*) '  R : RUN'
    write(*,*) '  Q : QUIT'
  end subroutine show_menu

  subroutine process_menu(key)
    character, intent(in) :: key

    select case (key)
    case ('F')
       call set_field()
    case ('P')
       call set_position()
    case ('V')
       call set_velocity()
    case ('O')
       call set_nsout()
    case ('H')
       call set_step()
    case ('T')
       call set_plot_type()
    case ('W')
       call set_view_size()
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
end module forces_lib

program forces
  use :: forces_lib

  implicit none

  call app_menu()
end program forces
