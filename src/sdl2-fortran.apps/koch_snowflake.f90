!
! Author: ANGELO GRAZIOSI
!
!   created   : Jun 20, 2023
!   last edit : Jun 22, 2023
!
!   Koch Curve
!
! DESCRIPTION
!
!   We draw Koch curve using SDL2-Fortran interface. Inspired by liceo
!   "Landi" WhatsApp chat
!
! REFERENCES
!
!   https://rosettacode.org/wiki/Koch_curve#C
!   https://it.wikipedia.org/wiki/Curva_di_Koch
!   https://en.wikipedia.org/wiki/Koch_snowflake
!
! CITATION
!
! Stainhaus: "Ci avviciniamo alla realtà, considerando che la maggior
! parte degli archi che s'incontrano nella natura sono non
! rettificabili. Questa affermazione contrasta con la credenza che gli
! archi non rettificabili siano un'invenzione dei matematici, e che
! gli archi naturali siano rettificabili: si verifica invece il
! contrario"
!
! COMPUTING THE TRANSFORMATION FROM WC TO DC
!
!   WC = (X1,X2) x (Y1,Y2), DC = (0,IMAX) x (JMAX,0)
!
!   DC is defined as (X1S,X2S) x (Y1S,Y2S), i.e.:
!
!     X1S = 0
!     X2S = IMAX
!     Y1S = JMAX
!     Y2S = 0
!
!   Units per pixel:
!
!     DX = (X2-X1)/IMAX, DY = (Y2-Y1)/JMAX
!
!   Transformation from DC to WC:
!
!     X = X1+I*DX,  Y = Y2-J*DY
!
!   Tranformation from WC to DC:
!
!     I = (X-X1)/DX = DPX*(X-X1),  J = (Y2-Y)/DY = DPY*(Y2-Y)
!
!   where:
!
!     DPY = (1/DX) = IMAX/(X2-X1), DPY = (1/DY) = JMAX/(Y2-Y1)
!
!   are the pixel per unit in X and Y
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
!       $SDL2F90 SDL2_app.f90 koch_snowflake.f90 \
!       $LIBS -o koch_snowflake$EXE; \
!   rm -rf *.mod
!
!   ./koch_snowflake$EXE NITER
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
!     open koch_snowflake$EXE NITER
!
!   being:
!
!     alias open='start'
!
!   Maybe the same considerations hold for GNU/Linux and macOS.
!

program koch_snowflake
  use kind_consts, only: WP

  use SDL2_app, only: QUIT_EVENT, &
       clear_screen, close_graphics, get_event, init_graphics, &
       refresh, set_rgba_color

  implicit none

  integer, parameter :: NUMARGS = 1, &
       SCREEN_WIDTH = 900, SCREEN_HEIGHT = 900, &
       IMAX = SCREEN_WIDTH-1, JMAX = SCREEN_HEIGHT-1

  ! Snowflake centered; L = 1, HH = SIN60*L/3
  real(WP), parameter :: SIN60 = sqrt(3.0_WP)/2, &
       COS60 = 0.5_WP, LL = 0.5_WP, HH = SIN60/3, H = 2*HH

  ! Stored by columns
  real(WP), parameter :: ROT60(2,2) = &
       reshape([COS60, SIN60, -SIN60, COS60],[2,2])

  ! Region, pixel per unit (in X and Y)
  real(WP), parameter :: X_MIN = -H, X_MAX = H, &
       Y_MIN = -H, Y_MAX = H, &
       DPX = IMAX/(X_MAX-X_MIN), DPY = JMAX/(Y_MAX-Y_MIN)

  character(len=*), parameter :: FMT = '(a,g0.7)'

  integer :: nargs, ievent = -1000
  real(WP) :: args_val
  character(len=80) :: args

  integer :: num_iter
  real(WP) :: p(2), q(2)

  nargs = command_argument_count()

  if (nargs /= NUMARGS) stop ': USAGE: ./koch_snowflake <NUM_ITER>'

  ! Reading the first argument
  call get_command_argument(1,args)
  read(args,*) args_val

  ! ARGS_VAL is NUM_ITER...
  num_iter = nint(args_val)
  if (num_iter < 0) num_iter = 0
  if (num_iter > 12) num_iter = 12

  write(*,'(a,i0)') 'Running with NUM_ITER = ', num_iter
  write(*,*)
  write(*,FMT) 'NUMBER OF SIDES: ', 3 * 4**num_iter
  write(*,FMT) 'LENGTH OF SIDES: ', (1.0_WP/3)**num_iter
  write(*,FMT) 'PERIMETER      : ', 3*(4.0_WP/3)**num_iter
  write(*,*)
  write(*,FMT) 'DELTA          : ', SIN60/2
  write(*,FMT) 'AREA/DELTA     : ', (8-3*(4.0_WP/9)**num_iter)/5
  write(*,*)
  ! log_3(4) = ln(4)/ln(3) = log(4)/log(3)
  write(*,FMT) 'FRACTAL DIM.   : ', log(4.0_WP)/log(3.0_WP)

  call init_graphics('The Koch Snowflake', &
       width=SCREEN_WIDTH,height=SCREEN_HEIGHT)

  call clear_screen()

  ! Our default color: LIGHTCYAN
  call set_rgba_color(0,255,255)

  p = [-LL,HH]
  q = [LL,HH]

  call koch(p,q,num_iter)

  p = q+matmul(ROT60,p-q)
  call koch(q,p,num_iter)

  q = p+matmul(ROT60,q-p)
  call koch(p,q,num_iter)

  call refresh()

  do while (ievent /= QUIT_EVENT)

     ievent = get_event()
  end do

  call close_graphics()

contains

  subroutine plot_line(a,b)
    use SDL2_app, only: draw_line
    real(WP), intent(in) :: a(2), b(2)

    integer :: ia, ja, ib, jb

    ! Tranformation WC to DC for the first point: A
    ia = nint(DPX*(a(1)-X_MIN))
    ja = nint(DPY*(Y_MAX-a(2)))

    ! Tranformation WC to DC for the second point: B
    ib = nint(DPX*(b(1)-X_MIN))
    jb = nint(DPY*(Y_MAX-b(2)))

    ! Drawing A--B line
    call draw_line(ia,ja,ib,jb)
  end subroutine plot_line

  recursive subroutine koch(p1,p2,niter)
    real(WP), intent(in) :: p1(2), p2(2)
    integer, intent(in) :: niter

    real(WP) :: p3(2), p4(2), p5(2)

    if (niter > 0) then
       p3 = (2*p1+p2)/3
       p5 = (2*p2+p1)/3
       p4 = p3+matmul(ROT60,p5-p3)

       call koch(p1,p3,niter-1)
       call koch(p3,p4,niter-1)
       call koch(p4,p5,niter-1)
       call koch(p5,p2,niter-1)
    else
       call plot_line(p1,p2)
    end if
  end subroutine koch
end program koch_snowflake
