!
! Author: ANGELO GRAZIOSI
!
!   created   : May 02, 2015
!   last edit : Dec 28, 2024
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
!   cd ode_mods
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
!   cd lorenz_attractor
!
!   rm -rf *.mod; \
!     gfortran[-mp-X] [-g3 -fbacktrace -fcheck=all] [-march=native] \
!       -Wall [-Wno-unused-dummy-argument] -std=f2018 [-fmax-errors=1] -O3 \
!       -I ../finclude [`sdl2-config --cflags`] \
!       lorenz_attractor.f90 -o lorenz_attractor$EXE \
!       -L ../lib -lbasic_mods -lode_mods -lfortran-sdl2apps -lfortran-sdl2 \
!       $LIBS; \
!   rm -rf *.mod
!
!   ./lorenz_attractor$EXE
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

module lorenz_attractor_lib
  use :: kind_consts, only: WP
  use :: getdata, only: get
  use :: ode_integrator, only: ode_step => step, ode_on, ode_off
  use :: sdl2, only: sdl_rect
  use :: sdl2app, only: QUIT_EVENT, quit, get_event, &
       select_map, set_map_window, set_map_viewport, &
       init_graphics, close_graphics, clear_screen, &
       draw_point, draw_rect, set_color, refresh
  use :: shading_colors, only: RED, GRAY, BROWN, LRED, LGRAY, LCYAN, YELLOW

  implicit none
  private

  integer, parameter :: NEQ = 3

  character(len=*), parameter :: TITLE = 'Lorenz Attractor'

  ! DATA
  !
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

  ! AUXILIARY DATA
  !

  ! The viewports, aka the bounding rectangles, for tz-plot, uv-P plot
  ! and uv-Q plot (remember we use Cavalieri transformation...)
  type(sdl_rect) :: vp1 = sdl_rect(0,0,0,0), vp2 = sdl_rect(0,0,0,0), &
       vp3 = sdl_rect(0,0,0,0)

  integer, target :: colors_dark(3) = [RED, GRAY, BROWN]
  integer, target :: colors_light(3) = [LRED, LGRAY, YELLOW]
  integer, pointer :: color(:) => null()

  public :: app_menu

contains

  ! =========================
  !    U  T  I  L  I  T  Y
  ! =========================

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

  ! ========================
  !    T  H  E    A  P  P
  ! ========================

  subroutine setup_graphics()
    integer :: screen_width, screen_height
    real(WP) :: dt, du, dv, u(2), v(2)

    ! 'SCREEN' initialization...
    screen_width = 4*d1+3*d2
    screen_height = 2*d1+d2

    vp1 = sdl_rect(d1,d1,d2,d2)
    vp2 = sdl_rect(2*d1+d2,d1,d2,d2)
    vp3 = sdl_rect(3*d1+2*d2,d1,d2,d2)

    call init_graphics(TITLE, &
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

  subroutine solve()
    real(WP), parameter :: DZ_BIFURCATION = 0.1_WP

    ! P(:) is the solution plotted in uv-P plot map, Q(:) is the
    ! solution plotted in uv-Q plot map. They differ, initially, by
    ! 1E-5 (more precisely in Z(t=0))
    real(WP) :: h, t, t_save, tp, p(NEQ), q(NEQ), delta_z
    logical :: last, not_bifurcation

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

    ! By defaulta RED, GRAY and BROWN, in dark version to be used before
    ! bifurcation
    color => colors_dark

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

          ! RED, GRAY and BROWN, in light version to be used after
          ! bifurcation
          color => colors_light
       end if
    end do

  contains

    subroutine display()
      real(WP) :: u = 0, v = 0

      call select_map(1)
      call set_color(color(1))
      call draw_point(t,delta_z)

      call cavalieri(p,u,v)
      call select_map(2)
      call set_color(color(2))
      call draw_point(u,v)

      call cavalieri(q,u,v)
      call select_map(3)
      call set_color(color(3))
      call draw_point(u,v)

      call refresh()
    end subroutine display

  end subroutine solve

  subroutine run()
    integer :: ievent

    ! Initialize ODE before graphics, if you can...
    call ode_on(NEQ,field,METHOD=id_method,ACCURACY=ll)

    call setup_graphics()
    call show_params()

    ! We need to reset IEVENT if we want to restart the run
    ievent = -1000
    do while (ievent /= QUIT_EVENT)
       call clear_screen()
       call set_color(LCYAN)

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

  ! ================
  !    M  E  N  U
  ! ================

  subroutine show_menu()
    write(*,*) 'Choose item:'
    write(*,*) '  M : Method'
    write(*,*)
    write(*,*) '  T : Time Interval'
    write(*,*) '  H : Step'
    write(*,*)
    write(*,*) '  D : Z(0) Difference'
    write(*,*)

    if (id_method /= 1) then
       write(*,*) '  L : ACCURACY (LL)'
       write(*,*)
    end if

    write(*,*) '  Y : Y(0) Condition'
    write(*,*)
    write(*,*) '  B : U-V Boundaries'
    write(*,*) '  Z : Z Interval in T-Z Plot'
    write(*,*)
    write(*,*) '  S : Screen Size'
    write(*,*)
    write(*,*) '  R : RUN'
    write(*,*) '  Q : QUIT'
  end subroutine show_menu

  subroutine process_menu(key)
    character, intent(in) :: key

    select case (key)
    case ('M')
       write(*,*) 'Choose the method:'
       write(*,*) '  1 : RK4'
       write(*,*) '  2 : GBS'
       write(*,*) '  3 : RKM'
       write(*,*) '  4 : R15'
       call get('ID_METHOD =',id_method)

       ! Correction so that ID_METHOD is always in [1,4]
       id_method = mod(id_method-1,4)+1
    case ('T')
       call get('T0 = ',t0)
       call get('T1 = ',t1)
    case ('H')
       call get('H0 = ',h0)
    case ('D')
       call get('DZ0 = ',dz0)
       if (dz0 > 1E-5_WP) stop ': DZ0 too big parameter (PROCESS_MENU).'
    case ('Y')
       call get('X0 = ',y0(1))
       call get('Y0 = ',y0(2))
       call get('Z0 = ',y0(3))
    case ('B')
       call get('U_MIN =',u_min)
       call get('U_MAX =',u_max)
       call get('V_MIN =',v_min)
       call get('V_MAX =',v_max)
    case ('Z')
       call get('Z1 =',z1)
       call get('Z2 =',z2)
    case ('S')
       call get('D1 (pixel) = ',d1)
       call get('D2 (pixel) = ',d2)

    case ('R')
       call run()
    end select

    if (id_method /= 1) then
       if (key == 'L') call get('LL = ',ll)
       if ((id_method /= 4 .and. ll > 13) .or. &
            (id_method == 4 .and. ll > 20)) &
            stop ': LL too big parameter (PROCESS_MENU).'
    end if

    write(*,*)
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
end module lorenz_attractor_lib

program lorenz_attractor
  use :: lorenz_attractor_lib

  implicit none

  call app_menu()
end program lorenz_attractor
