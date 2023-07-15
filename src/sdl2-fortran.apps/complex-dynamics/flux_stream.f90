!
! Author: ANGELO GRAZIOSI
!
!   created   : Jul 26, 2018
!   last edit : Jul 15, 2023
!
!   Equipotentials and Streamlines Plots
!
! DESCRIPTION
!
!   Given the transformation (complex potential):
!
!     W = F(Z),  W = PHI(X,Y) + i*PSI(X,Y), Z = X + i*Y
!
!   this program tries to plot the equipotentials
!
!     PHI(X,Y) = C
!
!   and streamlines
!
!     PSI(X,Y) = C
!
!   of the above complex potential.
!
! REFERENCES
!
!   https://en.wikipedia.org/wiki/Contour_line
!   http://paulbourke.net/papers/conrec
!   http://www-users.math.umn.edu/~olver/ln_/cml.pdf
!   https://en.wikipedia.org/wiki/Joukowsky_transform
!   http://www.diam.unige.it/~irro/conformi.html
!   http://web.math.unifi.it/users/dionisi/corsoanalisi3/Spadini.pdf
!   http://www.dic.unipi.it/e.buffoni/Idrodinamica/capitolo%208.pdf
!
!   A. Ghizzetti, F. Mazzarella, A. Ossicini - Lezioni di complementi
!   di matematica, Veschi
!
!   M. Picone, G. Fichera - Corso di analisi matematica, Vol. I e II,
!   Veschi
!
!   QFA1 p. 162-164
!
! EXAMPLES
!
!   f(z) = z*z, sqrt(z), z*cos(z), cos(z), sin(z), exp(z), cosh(z), sinh(z),
!          ((z*z-1)*sqr(z-2-i))/(z*z+2+2*i), log((z-1.1)/(z+1.1)), tan(z),
!          tanh(z), z*sin(z), exp(z)*sin(z), 1/z, 1/bar(z), z*log(z),
!          sin(z)/z, log(z*z-2), z^(1/3), z^(3/2)
!
! HOW TO BUILD THE APP (MSYS2/MINGW64, GNU/Linux, macOS)
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
!   g++[-mp-X] -I ../sdl2-fortran.apps/fparser-4.5.2 -c cwrapper_fparser.cc
!   ar rcs libFParser.a fparser.o fpoptimizer.o cwrapper_fparser.o
!
!   rm -rf *.o *.mod
!
!   cd ../sdl2-fortran.apps
!
!   git clone https://github.com/interkosmos/fortran-sdl2.git
!
!   cd complex-dynamics
!
!   rm -rf *.mod; \
!     gfortran[-mp-X] --std=f2018 -O3 -Wall [-Wno-surprising] \
!       [-fmax-stack-var-size=20971520] [`sdl2-config --cflags`] \
!       $B/basic-modules/{{kind,math}_consts,additional_functions,\
!         ft_timer_m,utilities,getdata,nicelabels,contour_plots}.f90 \
!       $B/fparser-fortran/fparser_cd.f90 \
!       $SDL2F90 $S/SDL2_app.f90 \
!       flux_stream.f90 $LIBS -L $B/fparser-fortran -lFParser -lstdc++ \
!       -o flux_stream$EXE; \
!   rm -rf *.mod
!
!   ./flux_stream$EXE
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
!     SDL2F90 = $B/sdl2-fortran/src/{c_util,sdl2/{sdl2_stdinc,sdl2_audio,\
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
!   app on MSYS2/MINGW64. This means that will not show up a
!   console/terminal to input data. On these systems, the LIBS
!   definition should be:
!
!     LIBS = -lSDL2main -lSDL2 -lgdi32 -lcomdlg32 -luuid -loleaut32 -lole32
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
!     open PROGNAME
!
!   being:
!
!     alias open='start'
!
!   Maybe the same considerations hold for GNU/Linux and macOS.
!

module flux_stream_lib
  use kind_consts, only: WP
  use math_consts, only: ZERO => Z0, PI, JJ
  use additional_functions, only: sqr => sqr_z, bar => conjg_z
  use fparser_cd, only: function_parser_cd_t => FunctionParser_cd_type, &
       new_parser => NewParser, parse_function => Parse, &
       error_msg => ErrorMsg, get_parse_error_type => GetParseErrorType, &
       delete_parser => DeleteParser, eval_function => Eval, &
       add_constant => AddConstant, add_function => AddFunction, &
       optimize_parser => Optimize
  use sdl2, only: sdl_rect

  implicit none
  private

  integer, parameter :: D1 = 20, D2 = 600

  ! Output window layout
  integer, parameter :: SCREEN_WIDTH = 3*D1+2*D2, &
       SCREEN_HEIGHT = 2*D1+D2

  integer, parameter :: W_MAP = 1, Z_MAP = 2

  ! The above map viewports, aka the bounding rectangles
  type(sdl_rect) :: vp1 = sdl_rect(D1,D1,D2,D2), &
       vp2 = sdl_rect(2*D1+D2,D1,D2,D2)

  ! Number of lines in the drawing
  integer :: nlines = 20

  logical :: phi_plot = .false., psi_plot = .false., axis_plot = .false.

  ! W = PHI + i*PSI = F(Z) and Z = X + i*Y regions
  real(WP) :: phi_min = -5, phi_max = 5, psi_min = -5, psi_max = 5, &
       x_min = -5, x_max = 5, y_min = -5, y_max = 5

  real(WP), allocatable :: d(:,:), g(:,:), x(:), y(:), phi(:), psi(:)

  character(len=128) :: fz_buffer = 'z*z'
  type(function_parser_cd_t) :: fp

  public :: app_on, app_run, app_off

contains

  subroutine input_data()
    use getdata, only: get

    ! Number of points in the drawing
    integer :: npoints = 500

    integer :: i, j, id_plot, ierr
    real(WP) :: dphi, dpsi, dx, dy

    ! The complex function string
    call get('F(z) = ',fz_buffer)
    write(*,*)

    ! Create the fparser for F(z)
    call new_parser(fp)

    if (add_constant(fp,'i',JJ) <= 0) then
       stop ': add_constant(i) error...'
    end if

    if (add_constant(fp,'pi',(PI,ZERO)) <= 0) then
       stop ': add_constant(pi) error...'
    end if

    if (add_function(fp,'sqr',sqr,1) <= 0) then
       stop ': add_function(sqr) error (complex Fortran function)...'
    end if

    if (add_function(fp,'bar',bar,1) <= 0) then
       stop ': add_function(bar) error (complex Fortran function)...'
    end if

    ierr = parse_function(fp,fz_buffer,'z')

    if (ierr >= 0) then
       write(*,*) 'Failure creating FP parser ...'
       write(*,*)
       ! Notice, 7 is the number of characters in 'F(z) = '...
       write(*,'(A)') 'F(z) = '//trim(fz_buffer)
       write(*,'(A)') repeat(' ',ierr+7)//'^'

       ! Remember : error_msg() is an array of characters...
       write(*,*) error_msg(fp)
       write(*,*) 'Error type: ',get_parse_error_type(fp)
       stop ': FParser creation failure (INPUT_DATA).'
    end if

    call optimize_parser(fp)

    ! Number of points/lines in the drawing
    call get('NPOINTS =',npoints)
    call get('NLINES =',nlines)
    write(*,*)

    allocate(d(0:npoints,0:npoints),stat=ierr)
    if (ierr /= 0) stop ': Allocation failure for D(:,:) (INPUT_DATA).'

    allocate(g(0:npoints,0:npoints),stat=ierr)
    if (ierr /= 0) stop ': Allocation failure for G(:,:) (INPUT_DATA).'

    allocate(x(0:npoints),stat=ierr)
    if (ierr /= 0) stop ': Allocation failure for X(:) (INPUT_DATA).'

    allocate(y(0:npoints),stat=ierr)
    if (ierr /= 0) stop ': Allocation failure for Y(:) (INPUT_DATA).'

    allocate(phi(0:nlines),stat=ierr)
    if (ierr /= 0) stop ': Allocation failure for PHI(:) (INPUT_DATA).'

    allocate(psi(0:nlines),stat=ierr)
    if (ierr /= 0) stop ': Allocation failure for PSI(:) (INPUT_DATA).'

    ! W-region
    call get('PHI_MIN =',phi_min)
    call get('PHI_MAX =',phi_max)
    write(*,*)

    call get('PSI_MIN =',psi_min)
    call get('PSI_MAX =',psi_max)
    write(*,*)

    ! Filling the LEVELS
    dphi = (phi_max-phi_min)/nlines
    dpsi = (psi_max-psi_min)/nlines

    do i = 0, nlines
       phi(i) = phi_min+i*dphi
       psi(i) = psi_min+i*dpsi
    end do

    ! Z-region
    call get('X_MIN =',x_min)
    call get('X_MAX =',x_max)
    write(*,*)

    call get('Y_MIN =',y_min)
    call get('Y_MAX =',y_max)
    write(*,*)

    write(*,'(A)',advance='NO') 'Filling the grid ...'

    ! Filling the grid
    dx = (x_max-x_min)/npoints
    dy = (y_max-y_min)/npoints

    do i = 0, npoints
       x(i) = x_min+i*dx
       do j = 0, npoints
          y(j) = y_min+j*dy
          d(i,j) = phi_fcn(x(i),y(j))
          g(i,j) = psi_fcn(x(i),y(j))
       end do
    end do

    write(*,*) 'done!'
    write(*,*)

    id_plot = 0 ! Default
    write(*,*) 'Choose WHAT to plot:'
    write(*,*) '  0 : BOTH'
    write(*,*) '  1 : PHI(X,Y) = C'
    write(*,*) '  2 : PSI(X,Y) = C'
    call get('ID_PLOT =',id_plot)
    write(*,*)

    ! For the sake of completeness...
    phi_plot = .false.
    psi_plot = .false.

    select case(id_plot)
    case (1)
       phi_plot = .true.
    case (2)
       psi_plot = .true.
    case default
       phi_plot = .true.
       psi_plot = .true.
    end select

    id_plot = 0 ! Default
    write(*,*) 'Choose WHAT to plot:'
    write(*,*) '  0 : NO Axis'
    write(*,*) '  1 : BOTH Axes'
    call get('ID_PLOT =',id_plot)
    write(*,*)

    ! For the sake of completeness...
    axis_plot = .false.

    select case(id_plot)
    case (1)
       axis_plot = .true.
    case default
       axis_plot = .false.
    end select

  contains

    function phi_fcn(x,y) result(r)
      real(WP), intent(in) :: x, y
      real(WP) :: r

      complex(WP) :: z = (0,0), w =(0,0)

      z = x+JJ*y
      w = eval_function(fp,[z])

      r = real(w)
    end function phi_fcn

    function psi_fcn(x,y) result(r)
      real(WP), intent(in) :: x, y
      real(WP) :: r

      complex(WP) :: z = (0,0), w = (0,0)

      z = x+JJ*y
      w = eval_function(fp,[z])

      r = aimag(w)
    end function psi_fcn

  end subroutine input_data

  subroutine app_on()
    use SDL2_app, only: set_map_window, set_map_viewport, init_graphics

    real(WP) :: dx, dy, dphi, dpsi, u(2), v(2)

    call input_data()
    call init_graphics('SDL2 Equipotentials and Streamlines Plots', &
         WIDTH=SCREEN_WIDTH,HEIGHT=SCREEN_HEIGHT)

    ! Margins of 5%
    dphi = 0.05_WP*(phi_max-phi_min)
    dpsi = 0.05_WP*(psi_max-psi_min)
    dx = 0.05_WP*(x_max-x_min)
    dy = 0.05_WP*(y_max-y_min)

    ! Defining map W_MAP
    u = [ vp1%x, vp1%x+vp1%w-1 ]
    v = [ vp1%y, vp1%y+vp1%h-1 ]
    call set_map_window(W_MAP,phi_min-dphi,phi_max+dphi, &
         psi_min-dpsi,psi_max+dpsi)
    call set_map_viewport(W_MAP,u(1),u(2),v(1),v(2))

    ! For the moment, in SDL2, we CANNOT draw axes and text!

    ! Defining map Z_MAP
    u = [ vp2%x, vp2%x+vp2%w-1 ]
    v = [ vp2%y, vp2%y+vp2%h-1 ]
    call set_map_window(Z_MAP,x_min-dx,x_max+dx,y_min-dy,y_max+dy)
    call set_map_viewport(Z_MAP,u(1),u(2),v(1),v(2))

    ! For the moment, in SDL2, we CANNOT draw axes and text!

    ! Now vp1 and vp2 are the bounding rectangles
    vp1%x = vp1%x-2
    vp1%y = vp1%y-2
    vp1%w = vp1%w+4
    vp1%h = vp1%h+4

    vp2%x = vp2%x-2
    vp2%y = vp2%y-2
    vp2%w = vp2%w+4
    vp2%h = vp2%h+4

  end subroutine app_on

  subroutine app_off()
    use SDL2_app, only: close_graphics

    integer :: ierr = 0

    !
    ! WE 'SWITCH OFF' THINGS IN INVERSE ORDER: G, PSI, PHI, Y, X, G, D, PARSER
    !

    ! G
    call close_graphics()

    ! PSI, PHI, Y, X, G, D
    if (allocated(psi)) deallocate(psi,stat=ierr)
    if (ierr /= 0) stop ': Deallocation failure for PSI(:) (APP_OFF).'

    if (allocated(phi)) deallocate(phi,stat=ierr)
    if (ierr /= 0) stop ': Deallocation failure for PHI(:) (APP_OFF).'

    if (allocated(y)) deallocate(y,stat=ierr)
    if (ierr /= 0) stop ': Deallocation failure for Y(:) (APP_OFF).'

    if (allocated(x)) deallocate(x,stat=ierr)
    if (ierr /= 0) stop ': Deallocation failure for X(:) (APP_OFF).'

    if (allocated(g)) deallocate(g,stat=ierr)
    if (ierr /= 0) stop ': Deallocation failure for G(:,:) (APP_OFF).'

    if (allocated(d)) deallocate(d,stat=ierr)
    if (ierr /= 0) stop ': Deallocation failure for D(:,:) (APP_OFF).'

    ! PARSER
    call delete_parser(fp)
  end subroutine app_off

  subroutine app_run()
    use ft_timer_m, only: ft_timer_t
    use SDL2_app, only: select_map, &
         QUIT_EVENT, clear_screen, draw_line, draw_rect, get_event, &
         refresh, set_rgba_color, draw_axis_x, draw_axis_y
    use contour_plots, only: conrec

    integer :: ievent = -1000
    integer :: i
    complex(WP) :: c_min, p, q, phi_step, psi_step
    type(ft_timer_t) :: run_timer

    ! For the moment, in SDL2, we CANNOT draw F(Z) as text!

    ! The Bottom-Left corner
    c_min = phi_min+JJ*psi_min

    call clear_screen()

    call set_rgba_color(0,255,255)     ! LIGHTCYAN
    call draw_rect(vp1)

    call set_rgba_color(0,255,0)       ! LIGHTGREEN
    call draw_rect(vp2)

    do while (ievent /= QUIT_EVENT)

       write(*,'(A)',advance='NO') 'Please wait, we are working ... '
       call run_timer%start()

       ! Start 'run_app()'...

       if (phi_plot) then

          !
          ! Plotting PHI V-lines
          !

          phi_step = ((phi_max-phi_min)/nlines)+JJ*0
          psi_step = 0+JJ*(psi_max-psi_min)

          call set_rgba_color(255,0,0)      ! LIGHTRED

          call select_map(W_MAP)
          do i = 0, nlines
             p = c_min+i*phi_step
             q = p+psi_step
             call draw_line(real(p),aimag(p),real(q),aimag(q))
          end do

          ! Plotting PHI level lines
          call select_map(Z_MAP)
          call conrec(d,x,y,phi,conrec_line)
       end if

       if (psi_plot) then

          !
          ! Plotting PSI H-lines
          !

          phi_step = (phi_max-phi_min)+JJ*0
          psi_step = 0+JJ*((psi_max-psi_min)/nlines)

          call set_rgba_color(255,255,0)    ! YELLOW

          call select_map(W_MAP)
          do i = 0, nlines
             p = c_min+i*psi_step
             q = p+phi_step
             call draw_line(real(p),aimag(p),real(q),aimag(q))
          end do

          ! Plotting PSI level lines
          call select_map(Z_MAP)
          call conrec(g,x,y,psi,conrec_line)
       end if

       if (axis_plot) then
          call set_rgba_color(255,255,255)  ! WHITE

          call select_map(W_MAP)
          call draw_axis_x(phi_min,phi_max,psi_min)
          call draw_axis_y(psi_min,psi_max,phi_min)

          call select_map(Z_MAP)
          call draw_axis_x(x_min,x_max,y_min)
          call draw_axis_y(y_min,y_max,x_min)
       end if

       ! Stop 'run_app()'...

       call run_timer%stop()
       write(*,*) 'done!'

       write(*,*)
       write(*,'(A,F0.3,A)') 'Completed in ',run_timer%elapsed_time(), &
            ' seconds!'

       call refresh()
       ievent = get_event()
    end do

  contains

    subroutine conrec_line(x1,y1,x2,y2,psi)
      real(WP), intent(in) :: x1, y1, x2, y2
      real(WP), intent(in) :: psi

      if (psi > 0.0_WP) then
         ! NOTHING
      end if

      call draw_line(x1,y1,x2,y2)
    end subroutine conrec_line

  end subroutine app_run
end module flux_stream_lib

program flux_stream
  use flux_stream_lib

  call app_on()
  call app_run()
  call app_off()
end program flux_stream
