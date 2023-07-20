!
! Author: ANGELO GRAZIOSI
!
!   created   : May 02, 2015
!   last edit : Jul 20, 2023
!
!   Lorenz Attractor
!
! DESCRIPTION
!   Butterfly effect with Lorenz attractor.
!
! REFERENCES
!   http://it.wikipedia.org/wiki/Effetto_farfalla and related links.
!   http://ithaca.unisalento.it/nr-15_2020/articolo_IIp_12.pdf
!
! HOW TO BUILD THE APP (MSYS2/MINGW64, GNU/Linux, macOS)
!
!   cd sdl2-fortran.apps
!
!   git clone https://github.com/interkosmos/fortran-sdl2.git
!
!   cd lorenz
!
!   rm -rf *.mod \
!     gfortran[-mp-X] -std=f2018 -O3 -Wall [-Wno-unused-dummy-argument] \
!       [`sdl2-config --cflags`] \
!       $B/basic-modules/{{kind,math}_consts,getdata,nicelabels}.f90 \
!       $B/ode-modules/{everhart,ode}_integrator.f90 $SDL2F90 \
!       $S/SDL2_{app,shading}.f90 \
!       lorenz_attractor.f90 $LIBS -o lorenz_attractor$EXE; \
!   rm -rf *.mod
!
!   ./lorenz_attractor$EXE
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
!   app on MSYS2/MINGW64. This means that will not show up a
!   console/terminal to input data. On these systems, the LIBS
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

module lorenz_attractor_lib
  use kind_consts, only: WP
  use sdl2, only: sdl_rect

  implicit none
  private

  integer, parameter :: NEQ = 3

  ! Output window/region layout
  integer :: d1 = 20, d2 = 500

  integer :: id_method = 2, ll = 12

  ! H0 = 1/2**10 = 9.765625E-4; suggested also H0 = 1/2**11 =
  ! 4.8828125E-4, H0 = 6.103515625E-5 = 1/2**14 (remember we use
  ! Cavalieri transformation...)
  real(WP) :: t0 = 0.0_WP, t1 = 30.0_WP, h0 = 1.0_WP/1024, &
       dz0 = 1E-5_WP, &
       z1 = -35.0_WP, z2 = 35.0_WP, &
       u_min = -35.0_WP, u_max = 35.0_WP, &
       v_min = -5.0_WP, v_max = 65.0_WP, &
       y0(NEQ) = [ 0.0_WP, 1.0_WP, 1.05_WP ]

  ! The viewports, aka the bounding rectangles, for tz-plot, uv-P plot
  ! and uv-Q plot (remember we use Cavalieri transformation...)
  type(sdl_rect) :: vp1 = sdl_rect(0,0,0,0), vp2 = sdl_rect(0,0,0,0), &
       vp3 = sdl_rect(0,0,0,0)

  public :: app_menu

contains

  subroutine setup_graphics()
    use SDL2_app, only: set_map_window, set_map_viewport, init_graphics

    integer :: screen_width, screen_height
    real(WP) :: dt, du, dv, u(2), v(2)

    ! 'SCREEN' initialization...
    screen_width = 4*d1+3*d2
    screen_height = 2*d1+d2

    vp1 = sdl_rect(d1,d1,d2,d2)
    vp2 = sdl_rect(2*d1+d2,d1,d2,d2)
    vp3 = sdl_rect(3*d1+2*d2,d1,d2,d2)

    call init_graphics('Lorenz Attractor', &
         WIDTH=screen_width,HEIGHT=screen_height)

    ! Margins of 5%
    dt = 0.05_WP*(t1-t0)
    dv = 0.05_WP*(z2-z1)

    ! Defining tz-plot map:
    u = [ vp1%x, vp1%x+vp1%w-1 ]
    v = [ vp1%y, vp1%y+vp1%h-1 ]
    call set_map_window(1,t0-dt,t1+dt,z1-dv,z2+dv)
    call set_map_viewport(1,u(1),u(2),v(1),v(2))

    du = 0.05_WP*(u_max-u_min)
    dv = 0.05_WP*(v_max-v_min)

    ! Defining uv-P plot map
    u = [ vp2%x, vp2%x+vp2%w-1 ]
    v = [ vp2%y, vp2%y+vp2%h-1 ]
    call set_map_window(2,u_min-du,u_max+du,v_min-dv,v_max+dv)
    call set_map_viewport(2,u(1),u(2),v(1),v(2))

    ! Defining uv-Q plot map
    u = [ vp3%x, vp3%x+vp3%w-1 ]
    v = [ vp3%y, vp3%y+vp3%h-1 ]
    call set_map_window(3,u_min-du,u_max+du,v_min-dv,v_max+dv)
    call set_map_viewport(3,u(1),u(2),v(1),v(2))

    ! Now vp1, vp2 and vp3 are the bounding rectangles
    vp1%x = vp1%x-2
    vp1%y = vp1%y-2
    vp1%w = vp1%w+4
    vp1%h = vp1%h+4

    vp2%x = vp2%x-2
    vp2%y = vp2%y-2
    vp2%w = vp2%w+4
    vp2%h = vp2%h+4

    vp3%x = vp3%x-2
    vp3%y = vp3%y-2
    vp3%w = vp3%w+4
    vp3%h = vp3%h+4
  end subroutine setup_graphics

  ! Lorenz model
  subroutine field(t,q,f)
    real(WP), intent(in) :: t, q(:)
    real(WP), intent(out) :: f(:)

    real(WP), parameter :: SIGMA= 10.0_WP, RHO = 28.0_WP, &
         BETA = 8.0_WP/3.0_WP

    associate (x => q(1), y => q(2), z => q(3))
      f(1) = SIGMA*(y-x)
      f(2) = x*(RHO-z)-y
      f(3) = x*y-BETA*z
    end associate
  end subroutine field

  ! Cavalieri transformation R3 => R2
  subroutine cavalieri(p,u,v)
    real(WP), intent(in) :: p(:)
    real(WP), intent(out) :: u, v
    !
    !  u = (-1/2)*x+y
    !  v = (-1/2)*x+z
    !
    associate (x => p(1), y => p(2), z => p(3))
      v = -0.5_WP*x
      u = v+y
      v = v+z
    end associate
  end subroutine cavalieri

  subroutine solve()
    use ode_integrator, only: ode_step => step
    use SDL2_app, only: quit
    use SDL2_shading, only: color_rgb_t

    real(WP), parameter :: DZ_BIFURCATION = 0.1_WP

    ! P(:) is the solution plotted in uv-P plot map, Q(:) is the
    ! solution plotted in uv-Q plot map. They differ, initially, by
    ! 1E-5 (more precisely in Z(t=0))
    real(WP) :: h, t, t_save, tp, p(NEQ), q(NEQ), delta_z
    logical :: last, not_bifurcation

    type(color_rgb_t) :: color1 = color_rgb_t(0,0,0), &
         color2 = color_rgb_t(0,0,0), color3 = color_rgb_t(0,0,0)

    ! Initial conditions
    last = .false.
    t = t0
    p = y0
    q = y0
    h = sign(h0,t1-t0)

    ! The difference between P(:) and Q(:) is ONLY DZ0 <= 1E-5 in Z(t=0)
    p(3) = q(3)+dz0

    ! AKA, delta_z = p(3)-q(3) at TIME = t0
    delta_z = dz0

    ! i.e. BIFURCATION = .false.
    not_bifurcation = .true.

    ! By defaulta RED, BLUE and BROWN, in dark version to be used before
    ! bifurcation
    color1 = color_rgb_t(128,0,0)
    color2 = color_rgb_t(0,0,128)
    color3 = color_rgb_t(128,128,0)

    do
       ! Plot current state
       call display()

       if (last) exit

       if (quit()) then
          write(*,*)
          write(*,*) 'Stopped at T : ', t
          write(*,*)
          exit
       end if

       tp = t+h

       ! Adjust last time step
       last = ((h >= 0.0_WP .and. tp >= t1) .or. (h < 0.0_WP .and. tp <= t1))

       if (last) h = t1-t

       ! We need to save t, to compute the next step...
       t_save = t
       call ode_step(h,t,p)
       call ode_step(h,t_save,q)
       delta_z = p(3)-q(3)

       ! The first time that DELTA_Z > DZ_BIFURCATION there is
       ! bifurcation (i.e. NOT_BIFURCATION = FALSE)
       if (not_bifurcation .and. abs(delta_z) > DZ_BIFURCATION) then
          write(*,*)
          write(*,*) 'Bifurcation at about T = ', t
          write(*,*)

          ! i.e. BIFURCATION = .true.
          not_bifurcation = .false.

          ! RED, BLUE and BROWN, in light version to be used after
          ! bifurcation
          color1 = color_rgb_t(255,0,0)
          color2 = color_rgb_t(0,0,255)
          color3 = color_rgb_t(255,255,0)
       end if
    end do

  contains

    subroutine display()
      use SDL2_app, only: select_map, draw_point, set_rgba_color, refresh
      real(WP) :: u = 0, v = 0

      call select_map(1)
      call set_rgba_color(color1%r,color1%g,color1%b)
      call draw_point(t,delta_z)

      call cavalieri(p,u,v)
      call select_map(2)
      call set_rgba_color(color2%r,color2%g,color2%b)
      call draw_point(u,v)

      call cavalieri(q,u,v)
      call select_map(3)
      call set_rgba_color(color3%r,color3%g,color3%b)
      call draw_point(u,v)

      call refresh()
    end subroutine display

  end subroutine solve

  subroutine run()
    use ode_integrator, only: ode_on, ode_off
    use SDL2_app, only: clear_screen, close_graphics, draw_rect, &
         set_rgba_color, QUIT_EVENT, get_event, quit, select_map

    integer :: ievent = -1000

    ! Initialize ODE before graphics, if you can...
    call ode_on(NEQ,field,METHOD=id_method,ACCURACY=ll)

    call setup_graphics()

    ! We need to reset IEVENT if we want to restart the run
    ievent = -1000
    do while (ievent /= QUIT_EVENT)
       call clear_screen()
       call set_rgba_color(0,255,255)     ! LIGHTCYAN

       call select_map(0)
       call draw_rect(vp1)
       call draw_rect(vp2)
       call draw_rect(vp3)

       call solve()

       write(*,*)
       write(*,'(A)',ADVANCE='NO') &
            'Press a Q/ESC or click X to quit graphics when done ... '

       ievent = get_event()
    end do

    call close_graphics()

    ! It was initialized before graphics...
    call ode_off()
  end subroutine run

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
    write(*,*) 'DZ0 = ', dz0
    write(*,*)

    if (id_method /= 1) then
       write(*,*) 'LL = ', ll, '(', 10.0_WP ** (-ll), ')'
       write(*,*)
    end if

    write(*,*) 'X0 = ', y0(1)
    write(*,*) 'Y0 = ', y0(2)
    write(*,*) 'Z0 = ', y0(3)
    write(*,*)
    write(*,*) 'U_MIN = ', u_min
    write(*,*) 'U_MAX = ', u_max
    write(*,*) 'V_MIN = ', v_min
    write(*,*) 'V_MAX = ', v_max
    write(*,*)
    write(*,*) 'Z1 = ', z1
    write(*,*) 'Z2 = ', z2

    write(*,*) 'D1 = ', d1
    write(*,*) 'D2 = ', d2
    write(*,*)
  end subroutine show_params

  subroutine show_menu()
    write(*,*) 'Choose item:'
    write(*,*) '  M : Method'
    write(*,*) '  T : Time Interval'
    write(*,*) '  H : Step'
    write(*,*) '  D : Z(0) Difference'

    if (id_method /= 1) then
       write(*,*) '  L : ACCURACY (LL)'
    end if

    write(*,*) '  Y : Y(0) Condition'
    write(*,*) '  B : U-V Boundaries'
    write(*,*) '  Z : Z Interval in T-Z Plot'
    write(*,*) '  S : Screen Size'
    write(*,*) '  R : RUN'
    write(*,*) '  Q : QUIT'
  end subroutine show_menu

  subroutine process_menu(ikey)
    use getdata, only: get

    integer, intent(in) :: ikey

    select case (ikey)
    case (ichar('M'))
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
    case (ichar('D'))
       call get('DZ0 = ',dz0)
       if (dz0 > 1E-5_WP) stop ': DZ0 too big parameter (PROCESS_MENU).'
    case (ichar('Y'))
       call get('X0 = ',y0(1))
       call get('Y0 = ',y0(2))
       call get('Z0 = ',y0(3))
    case (ichar('B'))
       call get('U_MIN =',u_min)
       call get('U_MAX =',u_max)
       call get('V_MIN =',v_min)
       call get('V_MAX =',v_max)
    case (ichar('Z'))
       call get('Z1 =',z1)
       call get('Z2 =',z2)
    case (ichar('S'))
       call get('D1 (pixel) = ',d1)
       call get('D2 (pixel) = ',d2)

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
end module lorenz_attractor_lib

program lorenz_attractor
  use lorenz_attractor_lib

  call app_menu()
end program lorenz_attractor
