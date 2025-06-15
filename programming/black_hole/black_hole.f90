!
! Author: ANGELO GRAZIOSI
!
!   created   : Aug 20, 2024
!   last edit : Jun 16, 2025
!
!   Particle motion around Schwarzschild black hole.
!
! DESCRIPTION
!
!   We plot the trajectory of a particle subject to the gravitational
!   field of a Schwarzschild black hole.
!
!   NB. RG = GM/C**2 is the Gravitational Radius while RS == REH = 2*RG
!       is the Schwarzschild radius or the Event Horizon
!
!   Here we use scaled equations:
!
!     length unit: RG = G*M/C**2, the gravitational radius
!     speed unit : C, the light speed in the vacuum
!
!     time unit  : T = RG/C, the time to travel the gravitational
!                  radius at the speed of light
!
!   The black hole mass is given in MSUN units.
!
!   The scaled equations do not depend on the black hole mass, so the
!   run time is the same for every BH mass.
!
! REFERENCES
!
!   1. E. Tejeda and S.Rosswog, An accurate Newtonian description of
!      particle motion around a Schwarzschild black hole, MNRAS 433,
!      1930-1940 (2013) [https://arxiv.org/pdf/1303.4068]
!
!   2. https://profoundphysics.com/black-hole-orbits
!      https://profoundphysics.com/can-light-orbit-a-black-hole-the-physics-explained
!      https://galileo-unbound.blog/2021/08/28/surfing-on-a-black-hole-accretion-disk-death-spiral
!
!   3. http://wwwinfo.jinr.ru/programs/cern/cernlib.pdf (D203)
!
!   4. A. Graziosi, QFA3 "Oxford", p. 210
!
! SOME TEST RUN
!
!   R0 = [ 40, 0, 0 ] RG, V0 = [-0.01, 0.095, 0]
!
!   R0 = [ 40, 0, 0 ] RG, V0 = [-0.01, 0.1, 0] T-X, T-Y,
!   XSIZE = 1000 (10000), YSIZE = 100, MBH = 1, 10
!
!   R0 = [ 40, 0, 0 ] RG, V0 = [-0.01, 0.09, 0] X-VX, Y-VY,
!   XSIZE = 100, YSIZE = 2, MBH = 1, 10
!
!   R0 = [ 40, 0, 0 ] RG, V0 = [-0.01, 0.0909, 0], VY 0.09091 ...
!   XSIZE = 100, YSIZE = 100
!
!   R0 = [ 40, 0, 0 ] RG, V0 = [-0.01, 0.09, 0.02], Z-VZ etc.
!   XSIZE = 100, YSIZE = 100
!
!   R0 = [ 40, 0, 0 ] RG, V0 = [0, 0.2, 0],
!   XSIZE = 500, YSIZE = 500, MBH = 1, 10
!
!   R0 = [ 40, 0, 0 ] RG, V0 = [0, 0.0909, 0],
!   XSIZE = 100, YSIZE = 100, MBH = 1, 10
!
!   As above with VY = 0.091, 0.092 ... M_BH = 1, 5, 10
!
!   R0 = [ 100, 0, 0 ] RG, V0 = [-0.141, 0.04, 0], or VY = 0.05 etc.
!   XSIZE = 1000, YSIZE = 1000, MBH = 1, 10
!
!   Mercury test
!
!     Mercury data (from https://it.wikipedia.org/wiki/Mercurio_(astronomia))
!
!       RG(SUN)         = 1.482 km
!
!       Rp = 4.6E7 km   = 31E6 RG
!       Ra = 6.982E7 km = 47.11E6 RG
!       Vp = 58.98 km/s = 1.966E-4 C (= Vmax)
!       Va = 38.86 km/s = 1.295E-4 C (= Vmin)
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
!   cd black_hole
!
!   rm -rf *.mod; \
!     gfortran[-mp-X] [-g3 -fbacktrace -fcheck=all] [-march=native] \
!       -Wall -Wno-unused-dummy-argument -std=f2018 [-fmax-errors=1] -O3 \
!       -I ../finclude [`sdl2-config --cflags`] \
!       black_hole.f90 -o black_hole$EXE \
!       -L ../lib -lbasic_mods -lfortran-sdl2apps -lfortran-sdl2 $LIBS; \
!   rm -rf *.mod
!
!   ./black_hole$EXE
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

module black_hole_lib
  use :: kind_consts, only: WP
  use :: math_consts, only: ZERO => Z0, ONE => Z1, TWO => Z2, THREE => Z3, &
       R2 => Q1_2, R6 => Q1_6, R8 => Q1_8, R3_2 => Q3_2
  use :: getdata, only: get
  use :: shading_colors, only: LRED, LGREEN, LBLUE, YELLOW, WHITE, &
       BORLAND_PALETTE
  use :: sdl2app, only: init_graphics, close_graphics, QUIT_EVENT, &
       get_event, quit, draw_point, draw_circle, draw_ellipse, set_color, &
       clear_screen, refresh, fill_circle

  implicit none
  private

  ! Useful constants
  integer, parameter :: NEQ = 3
  integer, parameter :: X_Y_PLOT  = 1
  integer, parameter :: X_Z_PLOT  = 2
  integer, parameter :: Y_Z_PLOT  = 3
  integer, parameter :: X_VX_PLOT = 4
  integer, parameter :: Y_VY_PLOT = 5
  integer, parameter :: Z_VZ_PLOT = 6
  integer, parameter :: T_X_PLOT  = 7
  integer, parameter :: T_Y_PLOT  = 8
  integer, parameter :: T_Z_PLOT  = 9

  ! To strip '  1 : ' use PLOT(1)(7:) and so on...
  character(len=*), parameter :: PLOT(9) = [ &
       '  1 : X - Y ', &
       '  2 : X - Z ', &
       '  3 : Y - Z ', &
       '  4 : X - VX', &
       '  5 : Y - VY', &
       '  6 : Z - VZ', &
       '  7 : T - X ', &
       '  8 : T - Y ', &
       '  9 : T - Z ' ]

  character(len=*), parameter :: UX(9) = [ &
       'RG', &
       'RG', &
       'RG', &
       'RG', &
       'RG', &
       'RG', &
       'T ', &
       'T ', &
       'T ' ]

  character(len=*), parameter :: UY(9) = [ &
       'RG', &
       'RG', &
       'RG', &
       'C ', &
       'C ', &
       'C ', &
       'RG', &
       'RG', &
       'RG' ]

  character(len=*), parameter :: TITLE = 'B L A C K  H O L E'
  character(len=*), parameter :: FMT = '(*(g0,1x))'

  real(WP), parameter :: MSUN = 2.0E30_WP    ! kg
  real(WP), parameter :: CLIGHT = 3.0E8_WP   ! m/s
  real(WP), parameter :: CLIGHT2 = 9.0E16_WP ! m^2 / s^2
  real(WP), parameter :: GNEW = 6.67E-11_WP  ! N x m^2 / kg^2

  ! DATA
  !
  integer :: screen_width = 900, screen_height = 900, &
       nsout = 100*2**4, plot_type = X_Y_PLOT

  ! M_BH = 1E6 MSUN, supermassive BH
  ! R0, X_SIZE ... in RG, V0 in CLIGHT, M_BH in MSUN
  real(WP) :: xsize = 100, ysize = 100, h = ONE/(2**4), &
       r0(NEQ) = [40,0,0], v0(NEQ) = [ZERO,0.1_WP,ZERO], m_bh = 1

  ! AUXILIARY DATA
  !
  integer :: istep, ival

  real(WP) :: tp, h2, h6, hh2, hh6, hh8, &
       x_min, x_max, y_min, y_max, rval, &
       work_space(NEQ,6), mu, rg, r_eh, ut

  real(WP), target :: t, r(NEQ), v(NEQ)
  real(WP), pointer :: xx => r(1), yy => r(2)

  public :: app_menu

contains

  ! subroutine field(t,r,v,f)
  !   real(WP), intent(in) :: t, r(NEQ), v(NEQ)
  !   real(WP), intent(out) :: f(NEQ)

  !   real(WP), save :: a, b, c, d

  !   ! F = R x V: X = YZ-ZY, Y = ZX-XZ, Z = XY-YX
  !   f(1) = r(2)*v(3)-r(3)*v(2)
  !   f(2) = r(3)*v(1)-r(1)*v(3)
  !   f(3) = r(1)*v(2)-r(2)*v(1)

  !   a = dot_product(f,f)  ! (R x V)**2
  !   c = dot_product(r,r)  ! r**2
  !   d = sqrt(c)           ! r
  !   b = d-TWO             ! (r-2)

  !   a = THREE*a           ! 3*(R x V)**2
  !   a = b*b+a             ! (r-2)**2+3*(R x V)**2

  !   c = ONE/c             ! 1/r**2
  !   a = -a*c              ! -[(r-2)**2+3*(R x V)**2]/r**2
  !   a = a/d               ! -[(r-2)**2+3*(R x V)**2]/r**3

  !   d = dot_product(r,v)  ! R . V

  !   b = TWO*d/b           ! 2*(R . V)/(r-2)

  !   f = c*(a*r+b*v)

  ! end subroutine field

  subroutine field(t,r,v,f)
    real(WP), intent(in) :: t, r(NEQ), v(NEQ)
    real(WP), intent(out) :: f(NEQ)

    real(WP), save :: a, b, c, d

    ! F = R x V: X = YZ-ZY, Y = ZX-XZ, Z = XY-YX
    f(1) = r(2)*v(3)-r(3)*v(2)
    f(2) = r(3)*v(1)-r(1)*v(3)
    f(3) = r(1)*v(2)-r(2)*v(1)

    c = dot_product(r,r)        ! r**2
    d = sqrt(c)                 ! r
    b = d-TWO                   ! (r-2)
    a = THREE*dot_product(f,f)  ! 3*(R x V)**2
    c = ONE/c                   ! 1/r**2
    a = -(b*b+a)*c/d            ! -[(r-2)**2+3*(R x V)**2]/r**3
    b = TWO*dot_product(r,v)/b  ! 2*(R . V)/(r-2)

    f = c*(a*r+b*v)

  end subroutine field

  subroutine setup_params()
    ! H constants
    h2 = R2*h
    h6 = R6*h

    hh2 = h*h2
    hh6 = h*h6
    hh8 = R8*h*h

    mu = GNEW*(m_bh*MSUN)
    rg = mu/CLIGHT2
    r_eh = 2*rg           ! Event Horizon
    ut = rg/CLIGHT        ! About 5 ms for the Sun

    istep = 0
    t = 0

    r = r0
    v = v0

    ! Computing params for initial positions: default
    x_max = xsize/2
    x_min = -x_max

    y_max = ysize/2
    y_min = -y_max

    select case(plot_type)
    case(X_Y_PLOT)
       xx => r(1)
       yy => r(2)
    case(X_Z_PLOT)
       xx => r(1)
       yy => r(3)
    case(Y_Z_PLOT)
       xx => r(2)
       yy => r(3)
    case(X_VX_PLOT)
       xx => r(1)
       yy => v(1)
    case(Y_VY_PLOT)
       xx => r(2)
       yy => v(2)
    case(Z_VZ_PLOT)
       xx => r(3)
       yy => v(3)
    case(T_X_PLOT)
       xx => t
       yy => r(1)
       ! Correct the computing params for initial positions
       x_min = 0
       x_max = xsize
    case(T_Y_PLOT)
       xx => t
       yy => r(2)
       ! Correct the computing params for initial positions
       x_min = 0
       x_max = xsize
    case(T_Z_PLOT)
       xx => t
       yy => r(3)
       ! Correct the computing params for initial positions
       x_min = 0
       x_max = xsize

    case default
       ! X - Y Plot
       xx => r(1)
       yy => r(2)
    end select

  end subroutine setup_params

  subroutine show_params()
    write(*,*) 'Current parameters:'
    write(*,*)
    write(*,*) 'RG (m) = ', rg
    write(*,*) 'T (s)  = ', ut
    write(*,*)
    write(*,*) 'R0 (RG) = ', r0
    write(*,*) 'V0 (C) = ', v0
    write(*,*) 'MBH (MSUN) = ', m_bh
    write(*,*)
    write(*,*) 'NSOUT  = ', nsout
    write(*,*)
    write(*,*) 'H (T) = ', h
    write(*,*)
    write(*,*) 'USING '//trim(PLOT(plot_type)(7:))//' PLOT'
    write(*,*)
    write(*,*) 'XSIZE ('//trim(ux(plot_type))//') = ', xsize
    write(*,*) 'YSIZE ('//trim(uy(plot_type))//') = ', ysize
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
       ! Draw body position at current time (ISTEP)
       call display_position()

       if (mod(istep,nsout) == 0) &
            write(*,FMT) 'CURRENT STEP: ', istep, 'TIME (T): ', t, &
            'R (RG): ', norm2(r), 'V (C): ', norm2(v)

       ! On exit istep = istep+1, t = t+h
       call update_position()
    end do

    ! Draw body position at the last step executed
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
           rp => work_space(:,5), vp => work_space(:,6))
        call field(t,r,v,w1)    ! K1

        tp = t+h2
        w4 = r+h2*v
        rp = w4+hh8*w1          ! RP = R+H2*V+HH8*K1
        vp = v+h2*w1            ! VP = V+H2*K1
        call field(tp,rp,vp,w2) ! K2

        vp = v+h2*w2            ! VP = V+H2*K2
        w1 = w1+w2              ! W1 = K1+K2
        w2 = w1+w2              ! W2 = K1+K2+K2 = K1+2*K2
        call field(tp,rp,vp,w3)

        istep = istep+1
        t = istep*h
        w4 = w4+h2*v            ! W4 = R+H2*V+H2*V = R+H*V
        rp = w4+hh2*w3          ! RP = R+H*V+HH2*K3
        vp = v+h*w3             ! VP = V+H*K3
        w1 = w1+w3              ! W1 = K1+K2+K3
        w2 = w2+2*w3            ! W2 = K1+2*K2+2*K3
        call field(t,rp,vp,w3)  ! K4

        r = w4+hh6*w1           ! R = R+H*V+HH6*(K1+K2+K3)
        v = v+h6*(w2+w3)        ! V = V+H6*(K1+2*K2+2*K3+K4)
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

    if (plot_type < 4) then
       call set_color(LRED)
       call fill_circle(ZERO,ZERO,TWO)  ! r_eh/rg == 2

       call set_color(WHITE)
       call fill_circle(ZERO,ZERO,ONE)  ! rg/rg
    else
       call set_color(LRED)
       call fill_circle(screen_width/2,screen_height/2,5)
    end if

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

  subroutine set_position()
    call get('X (RG) =',r0(1))
    call get('Y (RG) =',r0(2))
    call get('Z (RG) =',r0(3))
  end subroutine set_position

  subroutine set_velocity()
    call get('VX (C) =',v0(1))
    call get('VY (C) =',v0(2))
    call get('VZ (C) =',v0(3))
  end subroutine set_velocity

  subroutine set_bh_mass()
    call get('MBH (MSUN) =',m_bh)
  end subroutine set_bh_mass

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
    call get('H (T) =',rval)
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

    call get('PLOT_TYPE =',plot_type)
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
    write(*,*) 'USING '//trim(PLOT(plot_type)(7:))//' PLOT'
    write(*,*)

    rval= xsize
    call get('XSIZE ('//trim(ux(plot_type))//') = ',rval)
    if (rval > 0) then
       xsize = rval
    else
       write(*,*) 'XSIZE <= 0! UNCHANGED...'
    end if

    rval= ysize
    call get('YSIZE ('//trim(uy(plot_type))//') = ',rval)
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
    write(*,*) 'Particle:'
    write(*,*) '  P : Position'
    write(*,*) '  V : Velocity'
    write(*,*) '  M : BH Mass'
    write(*,*)
    write(*,*) 'Integration:'
    write(*,*) '  O : Output Rate'
    write(*,*) '  H : Time Step'
    write(*,*)
    write(*,*) 'Screen:'
    write(*,*) '  L : Plot'
    write(*,*) '  W : View Size'
    write(*,*) '  S : Screen Size'
    write(*,*)
    write(*,*) '  R : RUN'
    write(*,*) '  Q : QUIT'
  end subroutine show_menu

  subroutine process_menu(key)
    character, intent(in) :: key

    select case (key)
    case ('P')
       call set_position()
    case ('V')
       call set_velocity()
    case ('M')
       call set_bh_mass()
    case ('O')
       call set_nsout()
    case ('H')
       call set_step()
    case ('L')
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
end module black_hole_lib

program black_hole
  use :: black_hole_lib

  implicit none

  call app_menu()
end program black_hole
