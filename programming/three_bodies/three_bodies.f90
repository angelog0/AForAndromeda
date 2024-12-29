!
! Author: Angelo Graziosi
!
!   created   : Sep 12, 2015
!   last edit : Jul 24, 2023
!
!   Complete Solution of a General Problem of Three Bodies
!
! DESCRIPTION
!
!   Solving the dimensionless form of equations of motion of 3-bodies.
!
!   The 'dimensionless' means that the same result of computation
!   holds if we change the unit of measure. For example, the
!   trajectories computed for bodies of M1 = 3g, M2 = 4g, M3 = 5g,
!   P1(1,3,0) cm, V1(0,0,0) cm/T, ... , T ~= 1.08 h, ... , are the
!   same for stars of M1 = 3 MS, M2 = 4 MS, M3 = 5 MS, P1(1,3,0) pc,
!   V1(0,0,0) pc/T, ... , T ~= 1.4 x 10**7 yr (pc ~= 3 x 10**18, MS ~=
!   2 x 10**33). The numbers DO NOT change, what change is their
!   interpretation based on the System of measurement units.
!
! MORE ABOUT THE UNITS
!
!   The equations are scaled so that T**2 * G * M / L**3 == 1 (see
!   ref. 1) This means that if G = 6.67E-8 cm**3/(g * s**2), M = 1 g,
!   L = 1 cm, the unit of time is
!
!     T = sqrt(L**3/GM) = 1/sqrt(6.67E-8) = 3872.01546331183148544422 s
!
!   about 1.07555985091995319040 h ~ 1.08 h.
!
!   If the units were M = MSUN = 2E33 g and L = 1 pc = 3E18 cm, T would be
!
!     T = sqrt(L**3/GM) sqrt((3E18)**3 / (6.67E-8 * 2E33)) =
!       = 449887542169929.56197057955566577711 s =
!       = 14265840.37829558479105084841 yr ~ 1.43E7 yr
!
! REFERENCES
!
!   1. V. Szebehely and C. F. Peters, Complete Solution of a General
!      Problem of Three Bodies, The Astronomical Journal, vol. 72,
!      p. 876 (1967)
!      http://adsabs.harvard.edu/full/1967AJ.....72..876S
!
!   2. A. Graziosi, Logbook di questioni digitali 02 (LQD02), p. 45
!
! HOW TO BUILD THE APP (MSYS2/MINGW64, GNU/Linux, macOS)
!
!   cd N-Body
!
!   git clone https://github.com/interkosmos/fortran-sdl2.git
!
!   cd three_bodies
!
!   rm -rf *.mod; \
!   gfortran[-mp-X] -std=f2008 -O3 [-march=native -funroll-loops] -Wall \
!     -Wno-unused-dummy-argument [`sdl2-config --cflags`] \
!     $B/basic-modules/{{kind,math}_consts,ft_timer_m,getdata,nicelabels,\
!       camera_view_m}.f90 $B/ode-modules/{everhart,ode}_integrator.f90 \
!     $SDL2F90 $B/sdl2-fortran.apps/SDL2_{app,shading}.f90 \
!     three_bodies.f90 $LIBS -o three_bodies$EXE; \
!   rm -rf {*.mod,../modules/*}
!
!   ./three_bodies$EXE
!
!   where, for the build on GNU/Linux [OSX+MacPorts X server], is:
!
!     EXE = .out
!
!   while for the build on MINGW{32,64} is:
!
!     EXE = -$MSYSTEM (or EMPTY)
!
!   and (all platforms):
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
!     open PROGNAME
!
!   being:
!
!     alias open='start'
!
!   Maybe the same considerations hold for GNU/Linux and macOS.
!

module three_bodies_lib
  use kind_consts, only: WP
  use camera_view_m, only: camera_view_t
  use SDL2_shading, only: color_rgb_t

  implicit none
  private

  integer, parameter :: NDIM = 3, NBODY = 3, NEQ = 2*NDIM*NBODY

  type(color_rgb_t), parameter :: BODY_COLOR(NBODY) = [ &
       color_rgb_t(255,255,0), &   ! YELLOW
       color_rgb_t(255,0,255), &   ! LIGHTMAGENTA
       color_rgb_t(0,255,255) ]    ! LIGHTCYAN

  ! The dimensionless form of the equations implies that if M = 1 cgs(g),
  ! L = 1 cgs(cm), then T ~ 3872 sec ~ 1.08 hours.
  !
  ! H0 = 1.953125 x 10**(-3). Suggested for RK4: H0 = 1/(2**14) =
  ! 6.103515625E-5
  !
  real(WP) :: t0 = 0, t1 = 10, h0 = 1.0_WP/512, m(NBODY) = [ 3, 4, 5 ]

  !
  ! We adopt the variables with this meaning
  !
  !
  !   y(1:3)   = q1(1:3)
  !   y(4:6)   = q2(1:3)
  !   y(7:9)   = q3(1:3)
  !
  !   y(10:12) = v1(1:3)
  !   y(13:15) = v2(1:3)
  !   y(16:18) = v3(1:3)
  !
  ! Notice that the first index sequences,
  !
  !   1   4   7
  !
  ! can be produced with
  !
  !   3*i-2,   i = 1,2,...,NBODY
  !
  ! i.e.
  !
  !   NDIM*i-(NDIM-1) = 1 + (i-1)*NDIM
  !
  ! The default problem is the Pythagorean configuration (ref. 1)
  !
  real(WP) :: y0(NEQ) = [ &
       1, 3, 0,  -2, -1, 0,  1, -1, 0, &
       0, 0, 0,   0,  0, 0,  0,  0, 0 ]

  integer :: id_method = 2, ll = 12

  integer :: screen_width = 900, screen_height = 900
  logical :: axis_draw = .true.

  real(WP) :: k_view = 10, phi = 270, theta = 0, alpha = 45, &
       u_min = 0, u_max = 0, v_min = 0, v_max = 0, kq_clip = 10**2, &
       axis_len = 10*10
  type(camera_view_t) :: my_view

  public :: app_menu

contains

  subroutine run()
    use ft_timer_m, only: ft_timer_t
    use ode_integrator, only: ode_on, ode_integrate, ode_off
    use SDL2_app, only: close_graphics, init_graphics, &
         clear_screen, draw_axis_x, draw_axis_y, draw_line, &
         set_rgba_color, quit

    real(WP) :: u0, v0, u, v
    type(ft_timer_t) :: run_timer

    ! Initialize ODE before graphics, if you can...
    call ode_on(NEQ,sub,display_data,id_method,ACCURACY=ll,STOP_RUN=quit)

    ! Margins of 5%, BUT ONLY after APP_MENU!
    u0 = 0.05_WP*(u_max-u_min)
    v0 = 0.05_WP*(v_max-v_min)

    call init_graphics('General Problem of Three Bodies', &
         WIDTH=screen_width,HEIGHT=screen_height, &
         X1=u_min-u0,X2=u_max+u0,Y1=v_min-v0,Y2=v_max+v0)

    call clear_screen()

    if (axis_draw) then
       call set_rgba_color(255,255,255)  ! WHITE
       call draw_axis_x(u_min,u_max,v_min)
       call draw_axis_y(v_min,v_max,u_min)
       call draw_axis_x(u_min,u_max,v_max)
       call draw_axis_y(v_min,v_max,u_max)
    end if

    ! Project the origin of reference system
    call my_view%do_projection([ 0.0_WP, 0.0_WP, 0.0_WP ],u0,v0)

    ! Plot X axis
    call set_rgba_color(255,0,0)         ! LIGHTRED
    call my_view%do_projection([ axis_len, 0.0_WP, 0.0_WP ],u,v)

    ! Clip the result
    if (u < u_min) then
       u = u_min
    else if (u > u_max) then
       u = u_max
    end if

    if (v < v_min) then
       v = v_min
    else if (v > v_max) then
       v = v_max
    end if

    call draw_line(u0,v0,u,v)

    ! Plot Y axis
    call set_rgba_color(0,255,0)         ! LIGHTGREEN
    call my_view%do_projection([ 0.0_WP, axis_len, 0.0_WP ],u,v)

    ! Clip the result
    if (u < u_min) then
       u = u_min
    else if (u > u_max) then
       u = u_max
    end if

    if (v < v_min) then
       v = v_min
    else if (v > v_max) then
       v = v_max
    end if

    call draw_line(u0,v0,u,v)

    ! Plot Z axis
    call set_rgba_color(0,0,255)         ! LIGHTBLUE
    call my_view%do_projection([ 0.0_WP, 0.0_WP, axis_len ],u,v)

    ! Clip the result
    if (u < u_min) then
       u = u_min
    else if (u > u_max) then
       u = u_max
    end if

    if (v < v_min) then
       v = v_min
    else if (v > v_max) then
       v = v_max
    end if

    call draw_line(u0,v0,u,v)

    ! Just to start from a new line...
    write(*,*) 'Please wait, we are working ...'
    write(*,*)
    call run_timer%start()

    ! Start 'run_app()'...
    call ode_integrate(t0,t1,y0,h0)
    ! Stop 'run_app()'...

    call run_timer%stop()

    write(*,*)
    write(*,'(A,F8.3,A)') 'Completed in ',run_timer%elapsed_time(), &
         ' seconds!'

    write(*,*) 'Press a Q/ESC or click X to quit'
    do while (.not. quit())
       ! BLA BLA BLA ...
       !call refresh()
    end do

    call close_graphics()

    ! It was initialized before graphics...
    call ode_off()

  contains

    subroutine sub(t,y,f)
      real(WP), intent(in) :: t, y(:)
      real(WP), intent(out) :: f(:)
      !
      !   y(1:3)   = q1(1:3)
      !   y(4:6)   = q2(1:3)
      !   y(7:9)   = q3(1:3)
      !
      !   y(10:12) = v1(1:3)
      !   y(13:15) = v2(1:3)
      !   y(16:18) = v3(1:3)
      !
      real(WP), save :: d1(NDIM), d2(NDIM), d3(NDIM)

      associate (q1 => y(1:3), q2 => y(4:6), q3 => y(7:9), &
           v1 => y(10:12), v2 => y(13:15), v3 => y(16:18), &
           dq1 => f(1:3), dq2 => f(4:6), dq3 => f(7:9), &
           dv1 => f(10:12), dv2 => f(13:15), dv3 => f(16:18))

        ! d1 = (q3-q2)/|q3-q2|**3
        d1 = q3-q2
        d1 = d1/norm2(d1)**3

        ! d2 = (q1-q3)/|q1-q3|**3
        d2 = q1-q3
        d2 = d2/norm2(d2)**3

        ! d3 = (q2-q1)/|q2-q1|**3
        d3 = q2-q1
        d3 = d3/norm2(d3)**3

        ! Now computing the field (dq1 == dq_1/dt, ...)
        dq1 = v1
        dq2 = v2
        dq3 = v3

        dv1 = m(2)*d3-m(3)*d2
        dv2 = m(3)*d1-m(1)*d3
        dv3 = m(1)*d2-m(2)*d1
      end associate
    end subroutine sub

    subroutine display_data(t,y)
      use SDL2_app, only: draw_point, refresh

      real(WP), intent(in) :: t, y(:)

      integer, save :: k, kb
      real(WP), save :: dq, d(3)

      ! Plotting bodies at current position
      do k = 1, NBODY
         kb = 1+NDIM*(k-1)

         ! We could associate d => y(kb:kb+2), and then dq => dot_product(d,d)
         ! instead of using d, dq variables
         d = y(kb:kb+2)
         dq = dot_product(d,d)

         ! We draw only points inside the clipping sphere...
         if (dq < kq_clip) then
            call my_view%do_projection(d,u,v)

            ! Clipping with respect to the frame in U-V space
            if ((u_min < u .and. u < u_max) .and. &
                 (v_min < v .and. v < v_max)) then
               call set_rgba_color(BODY_COLOR(k)%r,BODY_COLOR(k)%g, &
                    BODY_COLOR(k)%b)
               call draw_point(u,v)
            end if
         end if
      end do

      call refresh()

      !write(*,'(5f16.12)') t, y(:)
    end subroutine display_data
  end subroutine run

  subroutine init_view_params()
    real(WP) :: r

    call my_view%setup(k_view,phi,theta,alpha)

    r = my_view%get_radius()

    u_min = -r
    u_max = r
    v_min = -r
    v_max = r

    ! We clip everything that is outside the sphere of radius K_VIEW
    ! (for convenience we use its square)
    kq_clip = k_view**2

    axis_len = 10*k_view
  end subroutine init_view_params

  subroutine show_params()

    character(len=*), parameter :: METHOD(4) = [ 'RK4', 'GBS', 'RKM', 'R15' ]

    write(*,*) 'Current parameters:'
    write(*,*)
    write(*,*) 'USING '//METHOD(id_method)//' METHOD'
    write(*,*)
    write(*,*) 'T0 = ', t0
    write(*,*) 'T1 = ', t1
    write(*,*) 'H0 = ', h0
    write(*,*)

    if (id_method /= 1) then
       write(*,*) 'LL = ', ll, '(', 10.0_WP ** (-ll), ')'
       write(*,*)
    end if

    write(*,*) 'M1 = ', m(1)
    write(*,*) 'M2 = ', m(2)
    write(*,*) 'M3 = ', m(3)
    write(*,*)

    associate (q1 => y0(1:3), q2 => y0(4:6), q3 => y0(7:9))
      write(*,*) 'X1 = ', q1(1)
      write(*,*) 'Y1 = ', q1(2)
      write(*,*) 'Z1 = ', q1(3)
      write(*,*)
      write(*,*) 'X2 = ', q2(1)
      write(*,*) 'Y2 = ', q2(2)
      write(*,*) 'Z2 = ', q2(3)
      write(*,*)
      write(*,*) 'X3 = ', q3(1)
      write(*,*) 'Y3 = ', q3(2)
      write(*,*) 'Z3 = ', q3(3)
      write(*,*)
    end associate

    ! The speeds have an offset of 9: v1(1) == y0(9+1) ...
    associate (v1 => y0(10:12), v2 => y0(13:15), v3 => y0(16:18))
      write(*,*) 'VX1 = ', v1(1)
      write(*,*) 'VY1 = ', v1(2)
      write(*,*) 'VZ1 = ', v1(3)
      write(*,*)
      write(*,*) 'VX2 = ', v2(1)
      write(*,*) 'VY2 = ', v2(2)
      write(*,*) 'VZ2 = ', v2(3)
      write(*,*)
      write(*,*) 'VX3 = ', v3(1)
      write(*,*) 'VY3 = ', v3(2)
      write(*,*) 'VZ3 = ', v3(3)
      write(*,*)
    end associate

    write(*,*) 'K_VIEW      = ', k_view
    write(*,*) 'PHI   (DEG) = ', phi
    write(*,*) 'THETA (DEG) = ', theta
    write(*,*) 'ALPHA (DEG) = ', alpha
    write(*,*)
    write(*,*) 'SCREEN WIDTH = ', screen_width
    write(*,*) 'SCREEN HEIGHT = ', screen_height
    write(*,*)
    write(*,*) 'U_MIN = ', u_min
    write(*,*) 'U_MAX = ', u_max
    write(*,*) 'V_MIN = ', v_min
    write(*,*) 'V_MAX = ', v_max
    write(*,*)
    write(*,*) 'Draw Axes = ', axis_draw
    write(*,*)
  end subroutine show_params

  subroutine show_menu()
    write(*,*) 'Choose item:'
    write(*,*) '  D : Method'
    write(*,*) '  T : Time Interval'
    write(*,*) '  H : Step'

    if (id_method /= 1) then
       write(*,*) '  L : ACCURACY (LL)'
    end if

    write(*,*) '  M : Mass'
    write(*,*) '  P : Position'
    write(*,*) '  V : Speed'
    write(*,*) '  K : KView Params'
    write(*,*) '  S : Screen Size'
    write(*,*) '  B : U-V Boundaries'

    if (axis_draw) then
       write(*,*) '  X : Hide Axes'
    else
       write(*,*) '  X : Draw Axes'
    end if

    write(*,*) '  R : RUN'
    write(*,*) '  Q : QUIT'
  end subroutine show_menu

  subroutine process_menu(ikey)
    use getdata, only: get

    integer, intent(in) :: ikey

    integer :: id_body, kb

    select case (ikey)
    case (ichar('D'))
       write(*,*) 'Choose the method:'
       write(*,*) '  1 : RK4'
       write(*,*) '  2 : GBS'
       write(*,*) '  3 : RKM'
       write(*,*) '  4 : R15'
       call get('ID_METHOD =',id_method)

       ! Correction so that ID_METHOD is always in [1,4]
       id_method = mod(id_method-1,4)+1
    case (ichar('T'))
       call get('T0 = ',t0)
       call get('T1 = ',t1)
    case (ichar('H'))
       call get('H0 = ',h0)
    case (ichar('M'))
       call get('M1 = ',m(1))
       call get('M2 = ',m(2))
       call get('M3 = ',m(3))
    case (ichar('P'))
       id_body = 1
       write(*,*) 'Choose the body:'
       write(*,*) '  1'
       write(*,*) '  2'
       write(*,*) '  3'
       call get('ID_BODY =',id_body)

       ! Correction so that ID_BODY is always in [1,3]
       id_body = mod(id_body-1,3)+1
       kb = 1+NDIM*(id_body-1)

       associate (q => y0(kb:kb+2))
         call get('X = ',q(1))
         call get('Y = ',q(2))
         call get('Z = ',q(3))
       end associate
    case (ichar('V'))
       id_body = 1
       write(*,*) 'Choose the body:'
       write(*,*) '  1'
       write(*,*) '  2'
       write(*,*) '  3'
       call get('ID_BODY =',id_body)

       ! Correction so that ID_BODY is always in [1,3]
       id_body = mod(id_body-1,3)+1

       ! The speeds have an offset of 9: v1(1) == y0(9+1) ...
       kb = 9+(1+NDIM*(id_body-1))

       associate (v => y0(kb:kb+2))
         call get('VX = ',v(1))
         call get('VY = ',v(2))
         call get('VZ = ',v(3))
       end associate
    case (ichar('K'))
       call get('K_VIEW = ',k_view)
       call get('PHI (DEG) = ',phi)
       call get('THETA (DEG) = ',theta)
       call get('ALPHA (DEG) = ',alpha)

       call init_view_params()
    case (ichar('S'))
       call get('SCREEN WIDTH  (pixels) =',screen_width)
       call get('SCREEN HEIGHT (pixels) =',screen_height)
    case (ichar('B'))
       call get('U_MIN =',u_min)
       call get('U_MAX =',u_max)
       call get('V_MIN =',v_min)
       call get('V_MAX =',v_max)
    case (ichar('X'))
       axis_draw = .not. axis_draw

    case (ichar('R'))
       call run()
    end select

    if (id_method /= 1) then
       if (ikey == ichar('L')) call get('LL = ',ll)
       if ((id_method /= 4 .and. ll > 13) .or. &
            (id_method == 4 .and. ll > 20)) &
            stop ': LL too big parameter (PROCESS_MENU).'
    end if

    write(*,*)
  end subroutine process_menu

  subroutine app_menu()
    use getdata, only: get

    character :: key = 'R'
    integer :: ikey = ichar('R') ! Default

    call init_view_params()

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
end module three_bodies_lib

program three_bodies
  use three_bodies_lib

  implicit none

  call app_menu()
end program three_bodies
