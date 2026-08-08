!
! Author: ANGELO GRAZIOSI
!
!   created   : Sep 05, 2025
!   last edit : Aug 08, 2026
!
!   Particle motion around a static ellipsoid of given density, f(x,y,z).
!
! DESCRIPTION
!
!   We plot the trajectory of a particle subject to the gravitational
!   field of a static ellipsoid of given density, f(x,y,z).
!
! REFERENCES
!
!   1. http://www.typnet.net/Essays/EarthGrav.htm
!   2. https://en.wikipedia.org/wiki/Earth_radius
!   3. https://en.wikipedia.org/wiki/Preliminary_reference_Earth_model
!   4. https://fisica2at.jimdofree.com/lezioni-2018-1/ottobre-2018-moto-composto-mcu-gravitazione/prima-velocit%C3%A0-cosmica/
!
! TEST
!
!   1. Units: (km, m/s, min); R0(13,0,0); V0(0,2.0/2.1/2.05,0);
!      semiaxes (6,4,2); NSOUT (960); H (0.625 s = 1.0417E-2 min);
!      DENSITY (1-5, not 6); XY size (30).
!
!   2. Units: (km, m/s, min); R0(13,0,0); V0(0,2.0-4.1,0); semiaxes
!      (6,6,6); NSOUT (960); H (0.625 s = 1.0417E-2 min);
!      DENSITY (1-5, not 6); XY size (30).
!
!   3. FIRST COSMIC SPEED TEST. Units: (km, km/s, h); R0(6378.1,0,0);
!      V0(0,7/7.7-7.9,0); semiaxes (6378.1,6378.1,6356.8); NSOUT
!      (1440); H (0.625 s = 0.17361E-3 h); DENSITY (1 / 6); XY size
!      (63710). With V0Y = 11/11.1/11.2 you will have the ESCAPE SPEED
!      TEST. With V0Y = 11.2 the trajectory is still open (looks as an
!      hyperbole branch) up to TIME (h): 267.00+, R (km): 0.10932E+7,
!      V (km/s): 0.70437.
!
! HOW TO BUILD THE APP (MSYS2, GNU/Linux, macOS)
!
!   cd programming
!
!   git clone https://github.com/interkosmos/fortran-sdl2.git
!
!   cd fortran-sdl2
!
!   make LIBGL='-lopengl32' LIBGLU='-lglu32' FFLAGS='[-march=native] \
!     -Wall -std=f2018 -fmax-errors=1 -O3 `sdl2-config --cflags`' all examples
!
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
!   cd ellipsoid_gravitation
!
!   rm -rf *.mod; \
!     gfortran[-mp-X] [-g3 -fbacktrace -fcheck=all] [-march=native] \
!       -Wall [-Wno-unused-dummy-argument] -std=f2018 [-fmax-errors=1] -O3 \
!       -I ../finclude [`sdl2-config --cflags`] ellipsoid_gravitation.f90 \
!       ellipsoid_orbits.f90 -o ellipsoid_orbits$EXE \
!       -L ../lib -lbasic_mods -lfortran-sdl2apps -lfortran-sdl2 $LIBS; \
!   rm -rf *.mod
!
!   ./ellipsoid_orbits$EXE
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

module density_lib
  use :: kind_consts, only: WP
  use :: ellipsoid_gravitation, only: density_fcn

  implicit none
  private

  type :: density_proc
     procedure(density_fcn), pointer, nopass :: f => null()
  end type density_proc

  real(WP) :: a, b, c

  type(density_proc) :: densities(6)

  public :: densities, density_setup

contains

  subroutine density_setup(aa, bb, cc)
    real(WP), intent(in) :: aa, bb, cc

    logical, save :: first = .true.

    a = aa
    b = bb
    c = cc

    ! We only need this the first time
    if (first) then
       first = .false.

       densities(1)%f => f1
       densities(2)%f => f2
       densities(3)%f => f3
       densities(4)%f => f4
       densities(5)%f => f5
       densities(6)%f => f6
    end if
  end subroutine density_setup

  ! Constant density
  function f1(p) result(fp)
    real(WP), intent(in) :: p(:)
    real(WP) :: fp

    associate (x => p(1), y => p(2), z => p(3))

      fp = 5513.0_WP  ! kg/m^3

    end associate

  end function f1

  ! Linear density: linear approximation of
  ! http://www.typnet.net/Essays/EarthGravGraphics/Plot1.png
  function f2(p) result(fp)
    real(WP), intent(in) :: p(:)
    real(WP) :: fp

    real(WP) :: r_norm

    associate (x => p(1), y => p(2), z => p(3))

      r_norm = sqrt((x/a)**2 + (y/b)**2 + (z/c)**2)

      ! An approximation. See http://www.typnet.net/Essays/EarthGrav.htm
      !  rho(r) = rho1*r+rho0*(1-r), rho0 = 1.3088E4 SIU, rho1 ~ 3.0E3 SIU
      fp = 1.3088E4_WP * (1.0_WP - r_norm)+3.0E3_WP*r_norm  ! kg/m^3

      if (fp < 0.0_WP) fp = 0.0_WP

    end associate

  end function f2

  ! Linear density: ChatGPT suggestion
  function f3(p) result(fp)
    real(WP), intent(in) :: p(:)
    real(WP) :: fp

    real(WP) :: r_norm

    associate (x => p(1), y => p(2), z => p(3))

      r_norm = sqrt((x/a)**2 + (y/b)**2 + (z/c)**2)

      ! Maximum density in the center, decreasing outward
      fp = 5513.0_WP * (1.0_WP - r_norm)  ! kg/m^3

      if (fp < 0.0_WP) fp = 0.0_WP

    end associate

  end function f3

  ! Quartic density
  function f4(p) result(fp)
    real(WP), intent(in) :: p(:)
    real(WP) :: fp

    real(WP) :: r_norm

    associate (x => p(1), y => p(2), z => p(3))

      r_norm = sqrt((x/a)**2 + (y/b)**2 + (z/c)**2)

      ! Maximum density in the center, decreasing outward
      fp = 5513.0_WP * (1.0_WP - r_norm**4)  ! kg/m^3

      if (fp < 0.0_WP) fp = 0.0_WP

    end associate

  end function f4

  ! Power 100 density, almost uniform (constant)
  function f5(p) result(fp)
    real(WP), intent(in) :: p(:)
    real(WP) :: fp

    real(WP) :: r_norm

    associate (x => p(1), y => p(2), z => p(3))

      r_norm = sqrt((x/a)**2 + (y/b)**2 + (z/c)**2)

      ! Maximum density in the center, decreasing outward
      fp = 5513.0_WP * (1.0_WP - r_norm**100)  ! kg/m^3

      if (fp < 0.0_WP) fp = 0.0_WP

    end associate

  end function f5

  ! PREM fit model (http://www.typnet.net/Essays/EarthGrav.htm)
  function f6(p) result(fp)
    real(WP), intent(in) :: p(:)
    real(WP) :: fp

    real(WP), parameter :: HH(0:11) = [ 0.0_WP, 1.2215E6_WP, 3.4800E6_WP, &
         3.6300E6_WP, 5.7010E6_WP, 5.7710E6_WP, 5.9710E6_WP, 6.1510E6_WP, &
         6.3466E6_WP, 6.3560E6_WP, 6.3680E6_WP, 6.3710E6_WP ]
    real(WP), parameter :: AA(11) = [ -2.1773E-10_WP, -2.1773E-10_WP, &
         0.0_WP, -3.0922E-11_WP, 0.0_WP, 0.0_WP, 0.0_WP, 0.0_WP, &
         0.0_WP, 0.0_WP, 0.0_WP ]
    real(WP), parameter :: BB(11) = [ 1.9110E-8_WP, 1.3976E-4_WP, &
         -5.0007E-4_WP, -2.4441E-4_WP, -2.3286E-4_WP, -1.2603E-3_WP, &
         -5.9706E-4_WP, 1.0869E-4_WP, 0.0_WP, 0.0_WP, 0.0_WP ]
    real(WP), parameter :: CC(11) = [ 1.3088E4_WP, 1.2346E4_WP, &
         7.3067E3_WP, 6.7823E3_WP, 5.3197E3_WP, 1.1249E4_WP, &
         7.1083E3_WP, 2.6910E3_WP, 2.9000E3_WP, 2.6000E3_WP, 1.0200E3_WP ]

    integer :: k = 0, i = 0
    real(WP) :: r_norm = 0

    r_norm = norm2(p)

    i = -1
    do k = 1, 11
       if ((HH(k-1) < r_norm) .and. (r_norm <= HH(k))) i = k
    end do

    if (i > 0) then
       fp = r_norm*(AA(i)*r_norm+BB(i))+CC(i)  ! kg/m^3
    else
       fp = 0.0_WP
    end if

  end function f6

end module density_lib

module ellipsoid_orbits_lib
  use :: kind_consts, only: WP
  use :: math_consts, only: ZERO => Z0, ONE => Z1, TWO => Z2, THREE => Z3, &
       R2 => Q1_2, R6 => Q1_6, R8 => Q1_8
  use :: density_lib, only: densities, density_setup
  use :: ellipsoid_gravitation, only: ellipsoid_setup, ellipsoid_gfield
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

  ! To strip '  1 : ' use PLOT(1)(7:) and so on...
  character(len=*), parameter :: PLOT(3) = [ &
       '  1 : X - Y', &
       '  2 : X - Z', &
       '  3 : Y - Z' ]

  ! To strip '  1 : ' use DENSITYSTR(1)(7:) and so on...
  character(len=*), parameter :: DENSITYSTR(6) = [ &
       '  1 : CONST   ', &
       '  2 : LINEAR  ', &
       '  3 : CHATGPT ', &
       '  4 : QUARTIC ', &
       '  5 : POWER100', &
       '  6 : PREM    ' ]

  ! To strip '  1 : ' use ULSTR(1)(7:) and so on...
  character(len=*), parameter :: ULSTR(3) = [ &
       '  1 : m ', &
       '  2 : km', &
       '  3 : R1' ]

  ! To strip '  1 : ' use UVSTR(1)(7:) and so on...
  character(len=*), parameter :: UVSTR(3) = [ &
       '  1 : m/s ', &
       '  2 : km/s', &
       '  3 : km/h' ]

  ! To strip '  1 : ' use UTSTR(1)(7:) and so on...
  character(len=*), parameter :: UTSTR(4) = [ &
       '  1 : s  ', &
       '  2 : min', &
       '  3 : h  ', &
       '  4 : d  ' ]

  character(len=*), parameter :: TITLE = 'E L L I P S O I D    O R B I T S'
  character(len=*), parameter :: FMT = '(*(G0.5,:,1X))'

  integer, parameter :: NX = 50, NY = 50, NZ = 50
  integer, parameter :: ECOLOR(3) = [ LRED, LGREEN, LBLUE ]

  ! R1 is the mean radius (ref. 2). All SI units
  real(WP), parameter :: REQUAT = 6.3781370E6_WP, RPOLAR = 6.3567523142E6_WP, &
       R1 = 6.3710087714E6_WP

  ! Conversion factors
  real(WP), parameter :: UL(3) = [ ONE, 1000.0_WP, R1 ], &
       UV(3) = [ ONE, 1000.0_WP, ONE/3.6_WP ], &
       UT(4) = [ ONE, 60.0_WP, 3600.0_WP, 86400.0_WP ]

  ! DATA
  !
  integer :: screen_width = 900, screen_height = 900, &
       nsout = 100*2**4, id_plot = 1, id_density = 6, &
       id_ul = 3, id_uv = 2, id_ut = 1

  ! R0, XSIZE ..., V0, H, Semiaxes in SI units
  real(WP) :: xsize = 10*UL(3), ysize = 10*UL(3), &
       h = UT(1)*10.0_WP/(2**4), &
       r0(NEQ) = [ 4, 0, 0 ]*UL(3), &
       v0(NEQ) = [ ZERO, 2.0_WP, ZERO ]*UV(2), &
       a = REQUAT, b = REQUAT, c = RPOLAR

  ! AUXILIARY DATA
  !
  integer :: istep, ival

  ! Times in UT, X_MIN... in UL, RVAL for the input of real values,
  ! AS, BS ellipse semiaxes for the screen
  real(WP) :: tp, h2, h6, hh2, hh6, hh8, &
       x_min, x_max, y_min, y_max, rval, &
       work_space(NEQ,6), as, bs

  ! SI units. We do not need T and V(:) as TARGET unless we want to
  ! plot vs them
  real(WP), target :: t, r(NEQ), v(NEQ)
  real(WP), pointer :: xx => r(1), yy => r(2)

  public :: app_menu

contains

  subroutine setup_params()
    ! H constants
    h2 = R2*h
    h6 = R6*h

    hh2 = h*h2
    hh6 = h*h6
    hh8 = R8*h*h

    istep = 0
    t = 0

    ! R, V in SIU units
    r = r0
    v = v0

    ! Computing params for initial positions: default
    x_max = xsize/2
    x_min = -x_max

    y_max = ysize/2
    y_min = -y_max

    select case(id_plot)
    case(1)
       ! X-Y plot
       xx => r(1)
       yy => r(2)
       as = a
       bs = b
    case(2)
       ! X-Z plot
       xx => r(1)
       yy => r(3)
       as = a
       bs = c
    case(3)
       ! Y-Z plot
       xx => r(2)
       yy => r(3)
       as = b
       bs = c

    case default
       ! X - Y Plot
       xx => r(1)
       yy => r(2)
    end select

    call density_setup(a, b, c)
    call ellipsoid_setup(NX, NY, NZ, a, b, c, densities(id_density)%f)

  end subroutine setup_params

  subroutine show_params()
    write(*,*) 'Current parameters:'
    write(*,*)
    write(*,FMT) 'UL (m)  = ', UL(id_ul)
    write(*,FMT) 'UV (m/s) = ', UV(id_uv)
    write(*,FMT) 'UT (s)    = ', UT(id_ut)
    write(*,*)
    write(*,FMT) 'R0 ('//trim(ULSTR(id_ul)(7:))//') = ', r0/UL(id_ul)
    write(*,FMT) 'V0 ('//trim(UVSTR(id_uv)(7:))//') = ', v0/UV(id_uv)
    write(*,*)
    write(*,FMT) 'A ('//trim(ULSTR(id_ul)(7:))//') = ', a/UL(id_ul)
    write(*,FMT) 'B ('//trim(ULSTR(id_ul)(7:))//') = ', b/UL(id_ul)
    write(*,FMT) 'C ('//trim(ULSTR(id_ul)(7:))//') = ', c/UL(id_ul)
    write(*,*)
    write(*,FMT) 'NSOUT  = ', nsout
    write(*,*)
    write(*,FMT) 'H ('//trim(UTSTR(id_ut)(7:))//') = ', h/UT(id_ut)
    write(*,*)
    write(*,*) 'DENSITY: '//trim(DENSITYSTR(id_density)(7:))//' DENSITY'
    write(*,*)
    write(*,*) 'USING '//trim(PLOT(id_plot)(7:))//' PLOT'
    write(*,*)
    write(*,FMT) 'XSIZE ('//trim(ULSTR(id_ul)(7:))//') = ', xsize/UL(id_ul)
    write(*,FMT) 'YSIZE ('//trim(ULSTR(id_ul)(7:))//') = ', ysize/UL(id_ul)
    write(*,*)
    write(*,FMT) 'SCREEN_WIDTH  = ', screen_width
    write(*,FMT) 'SCREEN_HEIGHT = ', screen_height
    write(*,*)
  end subroutine show_params

  subroutine draw_logo()
    integer, save :: icolor = 0

    call clear_screen()

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
            write(*,FMT) 'CURRENT STEP: ', istep, &
            '  TIME ('//trim(UTSTR(id_ut)(7:))//'): ', t/UT(id_ut), &
            '  R ('//trim(ULSTR(id_ul)(7:))//'): ', norm2(r)/UL(id_ul), &
            '  V ('//trim(UVSTR(id_uv)(7:))//'): ', norm2(v)/UV(id_uv)

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
        call ellipsoid_gfield(r, w1)  ! K1

        tp = t+h2
        w4 = r+h2*v
        rp = w4+hh8*w1                ! RP = R+H2*V+HH8*K1
        vp = v+h2*w1                  ! VP = V+H2*K1
        call ellipsoid_gfield(rp, w2) ! K2

        vp = v+h2*w2                  ! VP = V+H2*K2
        w1 = w1+w2                    ! W1 = K1+K2
        w2 = w1+w2                    ! W2 = K1+K2+K2 = K1+2*K2
        call ellipsoid_gfield(rp, w3)

        istep = istep+1
        t = istep*h
        w4 = w4+h2*v                  ! W4 = R+H2*V+H2*V = R+H*V
        rp = w4+hh2*w3                ! RP = R+H*V+HH2*K3
        vp = v+h*w3                   ! VP = V+H*K3
        w1 = w1+w3                    ! W1 = K1+K2+K3
        w2 = w2+2*w3                  ! W2 = K1+2*K2+2*K3
        call ellipsoid_gfield(rp, w3) ! K4

        r = w4+hh6*w1                 ! R = R+H*V+HH6*(K1+K2+K3)
        v = v+h6*(w2+w3)              ! V = V+H6*(K1+2*K2+2*K3+K4)
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
    !ievent = -1000
    call draw_logo()
    ievent = get_event()

    call clear_screen()

    ! Ellipsoid profile
    call set_color(ECOLOR(id_plot))
    call draw_ellipse(ZERO,ZERO,as,bs)

    call set_color(YELLOW)

    if (ievent /= QUIT_EVENT) then

       ! We need to reset IEVENT if we want to restart the run
       !ievent = -1000
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

  subroutine set_item(menu_items,item)
    character(len=*), intent(in) :: menu_items(:)
    integer, intent(inout) :: item

    integer :: i, m

    m = size(menu_items)

    write(*,*) 'Choose item:'
    do i = 1, m
       write(*,*) menu_items(i)
    end do

    call get('Item =',item)
    write(*,*)

    ! Correction so that ITEM is always in [1,M]
    ! Remember that MOD(N,M) is in [-(M-1),0] if N < 0!
    if (item <= 0) then
       item = 1
    else
       item = mod(item-1,m)+1
    end if
  end subroutine set_item

  subroutine set_position()
    ! In UL units
    r0 = r0/UL(id_ul)

    call get('X ('//trim(ULSTR(id_ul)(7:))//') =',r0(1))
    call get('Y ('//trim(ULSTR(id_ul)(7:))//') =',r0(2))
    call get('Z ('//trim(ULSTR(id_ul)(7:))//') =',r0(3))

    ! Now in m
    r0 = r0*UL(id_ul)
  end subroutine set_position

  ! Speed is the magnitude (scalar) of velocity (vector)
  subroutine set_velocity()
    ! In UV
    v0 = v0/UV(id_uv)

    call get('VX ('//trim(UVSTR(id_uv)(7:))//') =',v0(1))
    call get('VY ('//trim(UVSTR(id_uv)(7:))//') =',v0(2))
    call get('VZ ('//trim(UVSTR(id_uv)(7:))//') =',v0(3))

    ! Now in m/s
    v0 = v0*UV(id_uv)
  end subroutine set_velocity

  subroutine set_semiaxes()
    ! In UL units
    a = a/UL(id_ul)
    b = b/UL(id_ul)
    c = c/UL(id_ul)

    call get('A ('//trim(ULSTR(id_ul)(7:))//') =',a)
    call get('B ('//trim(ULSTR(id_ul)(7:))//') =',b)
    call get('C ('//trim(ULSTR(id_ul)(7:))//') =',c)

    ! Now in m
    a = a*UL(id_ul)
    b = b*UL(id_ul)
    c = c*UL(id_ul)
  end subroutine set_semiaxes

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

    ! In UT
    rval= h/UT(id_ut)

    call get('H ('//trim(UTSTR(id_ut)(7:))//') =',rval)
    if (rval > 0) then
       ! In s
       h = rval*UT(id_ut)
    else
       write(*,*) 'H <= 0! UNCHANGED...'
    end if
  end subroutine set_step

  subroutine set_view_size()
    write(*,*) 'USING '//trim(PLOT(id_plot)(7:))//' PLOT'
    write(*,*)

    ! In UL units
    rval= xsize/UL(id_ul)

    call get('XSIZE ('//trim(ULSTR(id_ul)(7:))//') = ',rval)
    if (rval > 0) then
       ! In m
       xsize = rval*UL(id_ul)
    else
       write(*,*) 'XSIZE <= 0! UNCHANGED...'
    end if

    ! In UL units
    rval= ysize/UL(id_ul)

    call get('YSIZE ('//trim(ULSTR(id_ul)(7:))//') = ',rval)
    if (rval > 0) then
       ! In m
       ysize = rval*UL(id_ul)
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
    write(*,*) 'Units:'
    write(*,*) '  N : Length'
    write(*,*) '  E : Speed'
    write(*,*) '  T : Time'
    write(*,*)
    write(*,*) 'Particle:'
    write(*,*) '  P : Position'
    write(*,*) '  V : Velocity'
    write(*,*) '  X : Semiaxes'
    write(*,*)
    write(*,*) 'Integration:'
    write(*,*) '  O : Output Rate'
    write(*,*) '  H : Time Step'
    write(*,*) '  D : Density'
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
    case ('N')
       call set_item(ULSTR,id_ul)
    case ('E')
       call set_item(UVSTR,id_uv)
    case ('T')
       call set_item(UTSTR,id_ut)
    case ('P')
       call set_position()
    case ('V')
       call set_velocity()
    case ('X')
       call set_semiaxes()
    case ('O')
       call set_nsout()
    case ('H')
       call set_step()
    case ('D')
       call set_item(DENSITYSTR,id_density)
    case ('L')
       call set_item(PLOT,id_plot)
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
end module ellipsoid_orbits_lib

program ellipsoid_orbits
  use :: ellipsoid_orbits_lib

  implicit none

  call app_menu()
end program ellipsoid_orbits
