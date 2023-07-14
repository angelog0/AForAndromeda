!
! Author: ANGELO GRAZIOSI
!
!   created   : Jul 05, 2023
!   last edit : Jul 14, 2023
!
!   Implicit schemes for heat diffusion equation in one dimension.
!
! DESCRIPTION
!
!   We solve the Dirichlet problem for diffusion equation in one
!   dimension with implicit schemes.
!   The equation is
!
!     Ut = Uxx+S(x,t)
!
!   where Ut is the 1st partial derivative w.r.t. t of U(x,t) and Uxx
!   is the 2nd partial derivative w.r.t. x of U(x,t); S(x,t) is a
!   source function. We can think of it as the diffusion of heat, that
!   is, temperature, along a bar.
!
!   The bar is divided in NDIV parts and we assume a square grid so
!   that we have
!
!     NITER = NSOUT*NDIV
!     TT = NITER*DT
!
!   being NSOUT the rate of the output, DT the time step and TT the
!   total time.
!
! REFERENCES
!
!   Koonin S.E., Computational Physics, Addison-Wesley
!   Karlen D., Computational Physics, Carleton University
!   Press W.H., Numerical Recipes, C.U.P
!
!   https://www.uni-muenster.de/imperia/md/content/physik_tp/lectures/ws2016-2017/num_methods_i/heat.pdf
!
! TESTS
!
!   NSOUT = 1, NDIV = 300, DT = 0.0005, D = 1, DU = (1-0)
!   U0T = UNT = 0
!   UX0 = exact(x,0), 4*x*(1-x), SXT = 0
!
!   NSOUT = 1, NDIV = 300, DT = 0.005, D = 1, DU = (1.7-0.5) or DU = (2-0.5)
!   U0T = TWO, UNT = 0.5 UX0 = 2-1.5*x+sin(pi*x), SXT = 0
!
!   NSOUT = 1, NDIV = 600, DT = 0.0025, D = 1, DU = (2-0.5)
!   U0T = TWO, UNT = 0.5 UX0 = 2-1.5*x+sin(pi*x), SXT = 0
!
!   NSOUT = 10, NDIV = 600, DT = 0.00025, D = 1, DU = (2-0.5)
!   U0T = TWO, UNT = 0.5 UX0 = 2-1.5*x+sin(pi*x), SXT = 0
!
!   NSOUT = 10, NDIV = 900, DT = 0.00015, D = 1, DU = (2-0.5)
!   U0T = TWO, UNT = 0.5 UX0 = 2-1.5*x+sin(pi*x), SXT = 0
!
!   NSOUT = 1, NDIV = 500, DT = 0.0005, D = 1, DU = (0.3-0)
!   U0T = 0, UNT = 0 UX0 = 0, SXT = 5*(theta(x-0.4)-theta(x-0.6))
!   (SXT = delta(x-0.5,0.05))
!
!   NSOUT = 10, NDIV = 800, DT = 0.0005, D = 1, DU = (1-(-1))
!   U0T = sin((2*pi/2.5)*t), UNT = 0 (-sin((2*pi/2.5)*t)) UX0 = 0, SXT = 0
!
!   NSOUT = 1, NDIV = 500, DT = 0.0005, D = 1 (0.5), DU = (1-0)
!   U0T = 0 (exp(-t/k), k = 0.01, 0.1), UNT = 0  UX0 = exact(x,0), SXT = 0
!
!   NSOUT = 10, NDIV = 500, DT = 0.0005, D = 1 (0.5), DU = (1-0)
!   U0T = 0 (exp(-t/k), k = 0.01, 0.1; exp(-t)*sin(2*pi*t/0.25)), UNT = 0
!   UX0 = 4*x*(1-x), SXT = 0
!
!
! HOW TO BUILD THE APP (macOS, MSYS2. MSYS2-MINGW64, GNU/Linux)
!
!   cd sdl2-fortran.apps
!
!   wget http://warp.povusers.org/FunctionParser/fparser4.5.2.zip
!   aunpack fparser4.5.2.zip -X fparser-4.5.2/
!   cd fparser-4.5.2
!
!   g++[-mp-X] -DFP_SUPPORT_FLOAT_TYPE [-DFP_USE_STRTOLD] \
!     -DFP_SUPPORT_LONG_DOUBLE_TYPE \
!     -DFP_SUPPORT_LONG_INT_TYPE -DFP_SUPPORT_COMPLEX_DOUBLE_TYPE \
!     -DFP_SUPPORT_COMPLEX_FLOAT_TYPE -DFP_SUPPORT_COMPLEX_LONG_DOUBLE_TYPE \
!     -DFP_USE_THREAD_SAFE_EVAL -DFP_USE_THREAD_SAFE_EVAL_WITH_ALLOCA \
!     -c fparser.cc
!
!   g++[-mp-X] -DFP_SUPPORT_FLOAT_TYPE [-DFP_USE_STRTOLD] \
!     -DFP_SUPPORT_LONG_DOUBLE_TYPE \
!     -DFP_SUPPORT_LONG_INT_TYPE -DFP_SUPPORT_COMPLEX_DOUBLE_TYPE \
!     -DFP_SUPPORT_COMPLEX_FLOAT_TYPE -DFP_SUPPORT_COMPLEX_LONG_DOUBLE_TYPE \
!     -DFP_USE_THREAD_SAFE_EVAL -DFP_USE_THREAD_SAFE_EVAL_WITH_ALLOCA \
!     -c fpoptimizer.cc
!
!   mv *.o ../../fparser-fortran/
!
!   cd ../../fparser-fortran
!   g++[-mp-X] -I ../sdl2-fortran.apps/fparser4.5.2 -c cwrapper_fparser.cc
!   ar rcs libFParser.a fparser.o fpoptimizer.o cwrapper_fparser.o
!
!   rm -rf *.o *.mod
!
!   cd ../sdl2-fortran.apps
!
!   git clone https://github.com/interkosmos/fortran-sdl2.git
!
!   cd heat1D_solver
!
!   rm -rf *.mod; \
!     gfortran[-mp-X] -std=f2018 -O3 -Wall \
!       [`sdl2-config --cflags`] \
!       $B/basic-modules/{{kind,math}_consts,additional_functions,\
!         utilities,getdata,nicelabels}.f90 \
!       $B/fparser-fortran/fparser_dp.f90 \
!       $SDL2F90 $S/SDL2_{app,shading}.f90 heat1D_solver.f90 \
!       $LIBS -L $B/fparser-fortran -lFParser -lstdc++ \
!       -o heat1D_solver$EXE; \
!   rm -rf {*.mod,$B/modules/*}
!
!   ./heat1d_solver$EXE
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
!     SDL2F90 = $S/fortran-sdl2/src/{c_util,sdl2/{sdl2_stdinc,sdl2_audio,\
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
!     open poisson2D_solver$EXE
!
!   being:
!
!     alias open='start'
!
!   Maybe the same considerations hold for GNU/Linux and macOS.
!

module heat1D_solver_lib
  use kind_consts, only: WP
  use math_consts, only: ZERO => Z0, ONE => Z1, TWO => Z2, PI, E_NEPER, &
       HF => Q1_2
  use additional_functions, only: delta => delta_dirac
  use fparser_dp, only: function_parser_t => FunctionParser_type, &
       new_parser => NewParser, parse_function => Parse, &
       error_msg => ErrorMsg, get_parse_error_type => GetParseErrorType, &
       delete_parser => DeleteParser, eval_function => Eval, &
       add_constant => AddConstant, add_function => AddFunction, &
       optimize_parser => Optimize

  implicit none
  private

  integer, parameter :: MAX_FCN_BUF = 128

  character(len=*), parameter :: FMT = '(*(g0,1x))'

  ! Input data
  logical :: print_time = .false.
  integer :: ndiv = 500, nsout = 1
  real(WP) :: dt = 0.0005_WP, dcoeff = ONE, &
       u_min = ZERO, u_max = ONE
  character(len=MAX_FCN_BUF) :: ux0_buf = '4*x*(1-x)', &
       u0t_buf = '0', uNt_buf = '0', sxt_buf = '0'

  ! Auxiliary data
  integer :: ndm1, niter, iter, i, ierr = 0
  real(WP) :: tt, h, dth, ap, az, du, t

  ! Dynamic memory...
  real(WP), allocatable :: alpha(:), beta(:), ggamma(:), u(:)

  ! Function parsers
  type(function_parser_t) :: ux0p, u0tp, uNtp, sxtp

  public :: app_menu

contains

  !
  ! Functions data base
  !

  ! General functions
  function gaussian(x,t) result(r)
    real(WP), intent(in) :: x, t
    real(WP) :: r

    r = 1+80*t
    r = exp(-20.0_WP*(x-0.5_WP)**2/r)/sqrt(r)
  end function gaussian

  ! Y = EXACT(X,T), X == P(1), T == P(2)
  ! P() must be ASSUMED-SIZE array
  function exact(p) result(r)
    real(WP), intent(in) :: p(*)
    real(WP) :: r

    associate (x => p(1), t => p(2))
      r = gaussian(x,t)-gaussian(x-1.0_WP,t)-gaussian(x+1.0_WP,t)
    end associate
  end function exact

  ! Y = THETA(X), X == P(1)
  ! P() must be ASSUMED-SIZE array
  function theta(p) result (r)
    real(WP), intent(in) :: p(*)
    real(WP) :: r

    associate (x => p(1))
      if (x > ZERO) then
         r = ONE
      else
         r = ZERO
      end if
    end associate
  end function theta

  ! Create the PARSER
  subroutine create_parser(fcn_buffer,fcn_vars,fp)
    character(len=*), intent(in) :: fcn_buffer, fcn_vars
    type(function_parser_t), intent(out) :: fp

    ! Create the fparser
    call new_parser(fp)

    if (add_constant(fp,'hf',HF) <= 0) &
         stop ': ADD_CONSTANT(HF) error...'

    if (add_constant(fp,'pi',PI) <= 0) &
         stop ': ADD_CONSTANT(PI) error...'

    if (add_constant(fp,'e',E_NEPER) <= 0) &
         stop ': ADD_CONSTANT(E) error...'

    if (add_function(fp,'delta',delta,2) <= 0) &
         stop ': ADD_FUNCTION(DELTA) error...'

    if (add_function(fp,'exact',exact,2) <= 0) &
         stop ': ADD_FUNCTION(EXACT) error...'

    if (add_function(fp,'theta',theta,1) <= 0) &
         stop ': ADD_FUNCTION(THETA) error...'

    ierr = parse_function(fp,fcn_buffer,fcn_vars)

    if (ierr >= 0) then
       write(*,*) 'Failure creating FCN parser ...'
       write(*,*)

       ! Notice, 6 is the number of characters in 'FCN = '...
       write(*,'(a)') 'FCN = '//trim(fcn_buffer)
       write(*,'(a)') repeat(' ',ierr+6)//'^'

       ! Remember : error_msg() is an array of characters...
       write(*,*) error_msg(fp)
       write(*,*) 'Error type: ',get_parse_error_type(fp)
       stop ': FParser creation failure (CREATE_PARSER).'
    end if

    call optimize_parser(fp)
  end subroutine create_parser

  !
  ! App routines
  !
  subroutine lattice_on()

    call create_parser(ux0_buf,'x',ux0p)
    call create_parser(u0t_buf,'t',u0tp)
    call create_parser(uNt_buf,'t',uNtp)
    call create_parser(sxt_buf,'x,t',sxtp)

    allocate(alpha(0:ndiv), STAT=ierr)
    if (ierr /= 0) stop ': Allocation failure for ALPHA(:).'

    allocate(beta(0:ndiv), STAT=ierr)
    if (ierr /= 0) stop ': Allocation failure for BETA(:).'

    allocate(ggamma(0:ndiv), STAT=ierr)
    if (ierr /= 0) stop ': Allocation failure for GGAMMA(:).'

    allocate(u(0:ndiv), STAT=ierr)
    if (ierr /= 0) stop ': Allocation failure for U(:).'

  end subroutine lattice_on

  subroutine lattice_off()

    if (allocated(u)) deallocate(u,STAT=ierr)
    if (ierr /= 0) stop ': Deallocation failure for U(:).'

    if (allocated(ggamma)) deallocate(ggamma,STAT=ierr)
    if (ierr /= 0) stop ': Deallocation failure for GGAMMA(:).'

    if (allocated(beta)) deallocate(beta,STAT=ierr)
    if (ierr /= 0) stop ': Deallocation failure for BETA(:).'

    if (allocated(alpha)) deallocate(alpha,STAT=ierr)
    if (ierr /= 0) stop ': Deallocation failure for ALPHA(:).'

    ! DESTROY PARSERS
    call delete_parser(sxtp)
    call delete_parser(uNtp)
    call delete_parser(u0tp)
    call delete_parser(ux0p)

  end subroutine lattice_off

  subroutine setup_params()
    use SDL2_shading, only: MAX_COLOURS

    ndm1 = ndiv-1
    h = ONE/ndiv    ! Spatial step

    ! DTH = D*DT/H**2
    dth = (dcoeff*dt)/h**2

    niter = nsout*ndiv
    tt = niter*dt

    du = (u_max-u_min)/MAX_COLOURS

    write(*,*)
    write(*,FMT) ' NSOUT = ', nsout
    write(*,FMT) ' NDIV  = ', ndiv
    write(*,FMT) ' NITER = ', niter
    write(*,FMT) ' TTIME = ', tt
    write(*,FMT) ' DT    = ', dt
    write(*,FMT) ' H     = ', h
    write(*,FMT) ' D     = ', dcoeff
    write(*,*)
  end subroutine setup_params

  subroutine solve()

    ! Initial conditions
    t = ZERO
    iter = 0

    !
    ! Setup the lattice
    !
    u(0) = u0t_fcn(t)     ! Left boundary condition
    u(ndiv) = uNt_fcn(t)  ! Right boundary condition

    ! Innner points initialization at time 0
    do i = 1, ndm1
       u(i) = ux0_fcn(i*h)
    end do

    ! Display solution a t = 0
    call display()

    !
    ! Computing ALPHA and GGAMMA, just once
    !
    ap = -dth
    az = ONE+TWO*dth

    ! Starting value
    alpha(ndm1) = ZERO
    ggamma(ndm1) = -ONE/az

    ! Backward sweep
    do i = ndm1, 1, -1
       alpha(i-1) = ggamma(i)*ap
       ggamma(i-1) = -1/(az+ap*alpha(i-1))
    end do

    ! Time loop
    do iter = 1, niter
       t = iter*dt

       ! Find BETA at this time
       beta(ndm1) = u(ndiv)

       ! Backward sweep
       do i = ndm1, 1, -1
          beta(i-1) = ggamma(i)*(ap*beta(i)-u(i)-sxt_fcn(i*h,t)*dt)
       end do

       !
       ! Find new U
       !

       ! Value at the left boundary
       u(0) = u0t_fcn(t)

       ! Forward sweep
       do i = 0, ndm1-1
          u(i+1) = alpha(i)*u(i)+beta(i)
       end do

       ! Value at the left boundary
       u(ndiv) = uNt_fcn(t)

       ! Output every 10th time step
       if (mod(iter,nsout) == 0) call display()
    end do

  contains

    function ux0_fcn(x) result(r)
      real(WP), intent(in) :: x
      real(WP) :: r

      r = eval_function(ux0p,[x])
    end function ux0_fcn

    function u0t_fcn(t) result(r)
      real(WP), intent(in) :: t
      real(WP) :: r

      r = eval_function(u0tp,[t])
    end function u0t_fcn

    function uNt_fcn(t) result(r)
      real(WP), intent(in) :: t
      real(WP) :: r

      r = eval_function(uNtp,[t])
    end function uNt_fcn

    function sxt_fcn(x,y) result(r)
      real(WP), intent(in) :: x, y
      real(WP) :: r

      r = eval_function(sxtp,[x,y])
    end function sxt_fcn

    subroutine display()
      use SDL2_shading, only: MAX_COLOUR_INDEX, get_shading_color, color_rgb_t
      use SDL2_app, only: set_rgba_color, draw_point, refresh

      integer :: k = 0, j = 0
      type(color_rgb_t) :: c = color_rgb_t(0,0,0)

      if (print_time) write(*,FMT) ' TIME: ', t

      ! YS position increases toward the bottom, so we have to invert
      j = ndiv-(iter/nsout)

      do i = 0, ndiv
         k = int((u(i)-u_min)/du)

         ! A safe guard
         if (k < 0) then
            k = 0
         else if (k > MAX_COLOUR_INDEX) then
            k = MAX_COLOUR_INDEX
         end if

         c = get_shading_color(k)

         ! .. and plotting by rows
         call set_rgba_color(c%r,c%g,c%b)
         call draw_point(i,j)
      end do
      call refresh()
    end subroutine display

  end subroutine solve

  subroutine run()
    use SDL2_shading, only: shading_setup
    use SDL2_app, only: init_graphics, close_graphics, clear_screen, &
         QUIT_EVENT, get_event

    integer :: ievent = -1000

    ! Switch ON the lattice...
    call lattice_on()
    call setup_params()

    call shading_setup()

    call init_graphics('Heat 1D Solver', &
         WIDTH=ndiv+1,HEIGHT=ndiv+1)

    call clear_screen()

    ! We need to reset IEVENT if we want to restart the run
    ievent = -1000
    do while (ievent /= QUIT_EVENT)

       call solve()

       ievent = get_event()
    end do

    call close_graphics()

    ! Switch OFF the lattice...
    call lattice_off()
  end subroutine run

  !
  ! Menu routines
  !

  subroutine show_params()
    write(*,*) 'Current parameters:'
    write(*,*)
    write(*,*) 'U(X,0) = '//trim(ux0_buf)
    write(*,*) 'U(0,T) = '//trim(u0t_buf)
    write(*,*) 'U(N,T) = '//trim(uNt_buf)
    write(*,*) 'S(X,T) = '//trim(sxt_buf)
    write(*,*)
    write(*,*) 'NSOUT    = ', nsout
    write(*,*) 'NDIV     = ', ndiv
    write(*,*)
    write(*,*) 'DT     = ', dt
    write(*,*) 'DCOEFF = ', dcoeff
    write(*,*)
    write(*,*) 'U_MIN = ', u_min
    write(*,*) 'U_MAX = ', u_max
    write(*,*)
    write(*,*) 'PRINT_TIME =', print_time
    write(*,*)
  end subroutine show_params

  subroutine show_menu()
    write(*,*) 'Choose item:'
    write(*,*) '  0 : U(X,0)'
    write(*,*) '  L : U(0,T)'
    write(*,*) '  H : U(N,T)'
    write(*,*) '  S : S(X,T)'
    write(*,*)
    write(*,*) '  O : NSOUT'
    write(*,*) '  N : NDIV'
    write(*,*)
    write(*,*) '  T : DT'
    write(*,*) '  D : DCOEFF'
    write(*,*)
    write(*,*) '  U : U_MIN/MAX'
    write(*,*)
    write(*,*) '  P : PRINT_TIME'
    write(*,*)
    write(*,*) '  R : RUN'
    write(*,*) '  Q : QUIT'
  end subroutine show_menu

  subroutine process_menu(ikey)
    use getdata, only: get

    integer, intent(in) :: ikey

    integer :: ival
    real(WP) :: x, y

    select case (ikey)
    case (ichar('0'))
       call get('U(x,0) = ',ux0_buf)
    case (ichar('L'))
       call get('U(0,t) = ',u0t_buf)
    case (ichar('H'))
       call get('U(N,t) = ',uNt_buf)
    case (ichar('S'))
       call get('S(x,t) = ',sxt_buf)
    case (ichar('O'))
       ival = nsout
       call get('NSOUT = ',ival)
       if (ival > 0) then
          nsout = ival
       else
          write(*,*) 'NSOUT <= 0! UNCHANGED...'
       end if
    case (ichar('N'))
       ival = ndiv
       call get('NDIV = ',ival)
       if (ival > 0) then
          ndiv = ival
       else
          write(*,*) 'NDIV <= 0! UNCHANGED...'
       end if
    case (ichar('T'))
       x = dt
       call get('DT = ',x)
       if (ZERO < x .and. x < ONE) then
          dt = x
       else
          write(*,*) 'DT not in (0,1)! UNCHANGED...'
       end if
    case (ichar('D'))
       call get('DCOEFF = ',dcoeff)
    case (ichar('U'))
       x = u_min
       y = u_max
       call get('U_MIN = ',x)
       call get('U_MAX = ',y)
       if (x < y) then
          u_min = x
          u_max = y
       else
          write(*,*) 'U_MIN >= U_MAX! UNCHANGED...'
       end if
    case (ichar('P'))
       call get('PRINT_TIME = ',print_time)

    case (ichar('R'))
       call run()
    end select

    write(*,*)
  end subroutine process_menu

  subroutine app_menu()
    use getdata, only: get

    character :: key = 'R'
    integer :: ikey = ichar('R') ! Default

    do while (ikey /= ichar('Q'))
       call show_params()
       call show_menu()

       call get('Choice :',key)

       ! Convert in upcase if not
       if ('a' <= key .and. key <= 'z') then
          ikey = ichar(key)-32
       else
          ikey = ichar(key)
       end if

       write(*,*)

       call process_menu(ikey)
    end do
    write(*,*) 'All done.'
  end subroutine app_menu
end module heat1D_solver_lib

program heat1D_solver
  use heat1D_solver_lib

  call app_menu()
end program heat1D_solver
