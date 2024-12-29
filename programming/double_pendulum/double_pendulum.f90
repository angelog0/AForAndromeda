!
! Author: Angelo Graziosi
!
!   created   : Sep 12, 2015
!   last edit : Dec 28, 2024
!
!   Double Pendulum
!
! DESCRIPTION
!
!   We solve IVP for double pendulum system. The ODE_INTEGRATOR is used,
!   it allows for different methods of integration.
!
!   We adopt the equation found in
!
!     http://www.physicsandbox.com/projects/double-pendulum.html
!
!   which are equivalent to those found in
!
!     http://www.myphysicslab.com/dbl_pendulum.html
!
! (see also QFA3 "Oxford", p. 157).
!
! EXAMPLES
!
!   t in [0,25], h = 0.0005, m1 = 1.5, m2 = 1, l1 = 1, l2 = 0.5,
!   phi1 = phi2 = 90, omega1 = omega2 = 0
!
! To test:
!
!   t in [0,1000], h = 1/1024 = 9.765625E-4, rate = 1024, phi1 = 0,
!   phi2 = 90, omega1 = omega2 = 0, ll = 12.
!
! Suggested: H0 = 1/2**10 = 9.765625E-4; H0 = 1/2**11 = 4.8828125E-4,
! H0 = 6.103515625E-5 = 1/2**14
!
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
!   cd double_pendulum
!
!   rm -rf *.mod; \
!     gfortran[-mp-X] [-g3 -fbacktrace -fcheck=all] [-march=native] \
!       -Wall [-Wno-unused-dummy-argument] -std=f2018 [-fmax-errors=1] -O3 \
!       -I ../finclude [`sdl2-config --cflags`] \
!       double_pendulum.f90 -o double_pendulum$EXE \
!       -L ../lib -lbasic_mods -lode_mods -lfortran-sdl2apps -lfortran-sdl2 \
!       $LIBS; \
!   rm -rf *.mod
!
!   ./double_pendulum$EXE
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

module double_pendulum_lib
  use :: kind_consts, only: WP
  use :: math_consts, only: DEG2RAD, PI, Q1_3, Q4_3
  use :: sdl2, only: sdl_rect

  implicit none
  private

  abstract interface
     subroutine field_fcn(x,y,f)
       import :: WP
       real(WP), intent(in) :: x, y(:)
       real(WP), intent(out) :: f(:)
     end subroutine field_fcn
  end interface

  integer, parameter :: NEQ = 4
  real(WP), parameter :: KGRAV = 9.81_WP

  ! Units for input data:
  !
  !   length in meters
  !   time   in second
  !   mass   in kg
  !   angle  in deg
  !   omg    in deg/sec
  !
  integer :: id_method = 2, rate = -1, ll = 12, counter = -1
  real(WP) :: m1 = 0.8_WP, m2 = 1.2_WP, &
       rho1 = 10.49_WP, rho2 = 19.3_WP, & ! Ag, Au: in g/cm**3
       l1 = 0.5_WP, l2 = 1.1_WP
  logical :: new_field = .true.

  !
  ! Here:
  !
  !   y(1) = phi1, y(2) = phi2, y(3) = omega1, y(4) = omega2
  !
  ! (OMEGA = Dot(PHI)) i.e. the system to be integrated IS
  !
  !   y'(1) = y(3)
  !   y'(2) = y(4)
  !   y'(3) = a1(y(1:2))
  !   y'(4) = a2(y(1:2))
  !
  real(WP) :: t0 = 0.0_WP, t1 = 25.0_WP, h0 = 1.0_WP/1024, &
       y0(NEQ) = [ 60.0_WP, 100.0_WP, 0.0_WP, 0.0_WP ]

  ! Auxiliary parameters/variables
  real(WP) :: mu, kgrav_mu, msum, msum1, r1, r2
  type(sdl_rect) :: vp1 = sdl_rect(0,0,0,0), vp2 = sdl_rect(0,0,0,0)

  procedure(field_fcn), pointer :: field => null()

  public:: app_menu

contains

  subroutine setup_graphics()
    use :: sdl2app, only: select_map, set_map_window, set_map_viewport, &
         init_graphics

    integer, parameter :: D1 = 20, D2 = 500, D3 = 500, &
         SCREEN_WIDTH = 3*D1+2*D2, SCREEN_HEIGHT = 2*D1+D3

    real(WP) :: u(2), v(2)

    !
    ! Graphics initialization
    !

    call init_graphics('Double Pendulum', &
         WIDTH=SCREEN_WIDTH,HEIGHT=SCREEN_HEIGHT)

    vp1 = sdl_rect(D1,D1,D2,D3)
    vp2 = sdl_rect(2*D1+D2,D1,D2,D3)

    ! Defining map 1
    u = [ vp1%x, vp1%x+vp1%w-1 ]
    v = [ vp1%y, vp1%y+vp1%h-1 ]
    call set_map_window(1,-2.0_WP,2.0_WP,-2.0_WP,2.0_WP)
    call set_map_viewport(1,u(1),u(2),v(1),v(2))

    ! Defining map 2
    u = [ vp2%x, vp2%x+vp2%w-1 ]
    v = [ vp2%y, vp2%y+vp2%h-1 ]
    call set_map_window(2,-2.0_WP,2.0_WP,-2.0_WP,2.0_WP)
    call set_map_viewport(2,u(1),u(2),v(1),v(2))

    ! Now vp1 and vp2 are the bounding rectangles
    vp1%x = vp1%x-2
    vp1%y = vp1%y-2
    vp1%w = vp1%w+4
    vp1%h = vp1%h+4

    vp2%x = vp2%x-2
    vp2%y = vp2%y-2
    vp2%w = vp2%w+4
    vp2%h = vp2%h+4
  end subroutine setup_graphics

  ! Implementation of the equations written as in
  ! http://www.myphysicslab.com/dbl_pendulum.html
  ! the first implementation we did
  subroutine field_old(x,y,f)
    real(WP), intent(in) :: x, y(:)
    real(WP), intent(out) :: f(:)

    real(WP), save :: a, b, c, d, s, s2

    associate (phi1 => y(1), phi2 => y(2), omega1 => y(3), omega2 => y(4))
      d = phi1-phi2

      c = cos(d)           ! c = cos(phi1-phi2)
      s2 = 2.0_WP*sin(d)   ! s2 = 2*sin(phi1-phi2)
      s = sin(d-phi2)      ! s = sin(phi1-2*phi2)

      ! s = msum1-m2*cos(2*phi1-2*phi2)
      d = msum1-m2*cos(2.0_WP*d)

      a = (omega1**2) * l1
      b = (omega2**2) * l2

      ! Now computing the field
      f(1) = omega1
      f(2) = omega2
      f(3) = (-KGRAV*(msum1*sin(phi1)+m2*s)-m2*s2*(b+a*c))/(l1*d)
      f(4) = (s2*(msum*(a+KGRAV*cos(phi1))+b*m2*c))/(l2*d)
    end associate
  end subroutine field_old

  ! Implementation of the equations written as in
  ! http://www.physicsandbox.com/projects/double-pendulum.html
  ! the second implementation we did
  subroutine field_new(t,y,f)
    real(WP), intent(in) :: t, y(:)
    real(WP), intent(out) :: f(:)

    real(WP), save :: a, b, c, d, s, s1, s2

    associate (phi1 => y(1), phi2 => y(2), omega1 => y(3), omega2 => y(4))
      d = phi1-phi2

      c = cos(d)
      s = sin(d)
      s1 = sin(phi1)
      s2 = sin(phi2)

      d = mu-c**2

      a = (omega1**2) * l1
      b = (omega2**2) * l2

      ! Now computing the field to be returned
      f(1) = omega1
      f(2) = omega2
      f(3) = (KGRAV*(c*s2-mu*s1)-(b+a*c)*s) / (l1*d)
      f(4) = (kgrav_mu*(s1*c-s2)+(mu*a+b*c)*s) / (l2*d)
    end associate
  end subroutine field_new

  subroutine setup_params()
    ! Masses
    mu = 1+(m1/m2)
    kgrav_mu = KGRAV*mu

    msum = m1+m2
    msum1 = 2*m1+m2

    ! Calculation of radii corresponding to mass densities.
    !
    ! The mass m is given in kg while the density is in g/cm**3, so we
    ! need the conversion m(g) = m(kg)*1000. This gives radius in cm
    ! or radius/100 in m.
    !
    r1 = (((m1*1000/rho1)/(Q4_3*PI))**Q1_3)/100
    r2 = (((m2*1000/rho2)/(Q4_3*PI))**Q1_3)/100

    ! The counter for the output rate, if requested..
    !
    ! Count the number of calls to display_data() and MUST be
    ! initialized at each run.
    ! COUNTER is initialized to -1 and it is reset to 0 (ZERO), so that:
    !
    !   counter = 0     t = t0
    !   counter = 1     t = t0+h
    !   counter = 2     t = t0+2*h
    !     ...              ...
    !   counter = 1000  t = t0+1000*h == t1000
    !   counter = 1     t = t1000+h
    !   counter = 2     t = t1000+2*h
    !     ...              ...
    !
    counter = -1

    ! The pointers to the equations field
    if (new_field) then
       field => field_new
    else
       field => field_old
    end if
  end subroutine setup_params

  subroutine draw_pendulums(x1,y1,x2,y2)
    use :: sdl2app, only: draw_line, fill_circle, set_rgba_color
    real(WP), intent(in) :: x1, y1, x2, y2

    ! Draw arms positions
    call set_rgba_color(128,128,0)   ! BROWN
    call draw_line(0.0_WP,0.0_WP,x1,y1)
    call draw_line(x1,y1,x2,y2)

    ! Draw the origin
    call set_rgba_color(128,0,0)     ! RED
    call fill_circle(0.0_WP,0.0_WP,0.02_WP)

    ! Draw the position of first pendulum
    call set_rgba_color(255,255,255) ! WHITE
    call fill_circle(x1,y1,r1)

    ! Draw the position of second pendulum
    call set_rgba_color(255,255,0)   ! YELLOW
    call fill_circle(x2,y2,r2)
  end subroutine draw_pendulums

  subroutine draw_position(x1,y1,x2,y2)
    use :: sdl2app, only: draw_point, set_rgba_color
    real(WP), intent(in) :: x1, y1, x2, y2

    ! Draw the position of first pendulum
    call set_rgba_color(255,255,255) ! WHITE
    call draw_point(x1,y1)

    ! Draw the position of second pendulum
    call set_rgba_color(255,255,0)   ! YELLOW
    call draw_point(x2,y2)
  end subroutine draw_position

  subroutine display_data(t,y)
    use :: sdl2app, only: select_map, clear_viewport, refresh
    real(WP), intent(in) :: t, y(:)

    real(WP), save :: x1, y1, x2, y2, v1q, v2q, e_tot

    ! First pendulum: conversion from generalized to cartesian coordinates
    x1 = l1*sin(y(1))
    y1 = -l1*cos(y(1))

    ! Second pendulum: conversion from generalized to cartesian coordinates
    x2 = x1+l2*sin(y(2))
    y2 = y1-l2*cos(y(2))

    ! Draw the pendulums on this off screen view
    call select_map(1)
    call clear_viewport()
    call draw_pendulums(x1,y1,x2,y2)

    call select_map(2)
    call draw_position(x1,y1,x2,y2)

    call refresh()

    ! A negative or null value suppresses the energy output
    if (rate <= 0) return

    counter = counter+1

    ! See: http://scienceworld.wolfram.com/physics/DoublePendulum.html
    !
    ! You can see that:
    !
    !   x1_dot = -omega1*y1
    !   y1_dot = omega1*x1
    !   x2-dot = x1_dot+omega2*(y1-y2)
    !   y2_dot = y1_dot+omega2*(x2-x1)
    !
    !   v1**2 = omega1**2 * l1**2 = (omega1*l1)**2
    !   v2**2 = [x1_dot+omega2*(y1-y2)]**2 + [y1_dot+omega2*(x2-x1)]**2 =
    !         = [-omega1*y1+omega2*(y1-y2)]**2 + [omega1*x1+omega2*(x2-x1)]**2
    !
    if (counter == rate) then
       ! Reset
       counter = 0

       v1q = -y(3)*y1+y(4)*(y1-y2)  ! x2_dot
       v2q = y(3)*x1+y(4)*(x2-x1)   ! y2_dot


       v2q = v1q**2+v2q**2
       v1q = (y(3)*l1)**2

       e_tot = 0.5_WP*(m1*v1q+m2*v2q)+KGRAV*(m1*y1+m2*y2)
       write(*,'(2f18.10)') t, e_tot
    end if
  end subroutine display_data

  subroutine run()
    use :: ode_integrator, only: ode_on, ode_integrate, ode_off
    use :: sdl2app, only: clear_screen, close_graphics, draw_rect, &
         set_rgba_color, QUIT_EVENT, get_event, quit, select_map

    integer :: ievent = -1000

    call setup_params()

    ! Y(0) is in DEG, we need RAD.  Notice, ODE_ON called out of
    ! graphics and it assigns QUIT(), a graphical function here, to
    ! STOP_RUN...
    call ode_on(NEQ,field,display_data,id_method,ACCURACY=ll,STOP_RUN=quit)

    call setup_graphics()

    ! We need to reset IEVENT if we want to restart the run
    ievent = -1000
    do while (ievent /= QUIT_EVENT)

       call clear_screen()
       call set_rgba_color(0,255,255)     ! LIGHTCYAN

       call select_map(0)
       call draw_rect(vp1)
       call draw_rect(vp2)

       call ode_integrate(t0,t1,y0*DEG2RAD,h0)

       write(*,*)
       write(*,'(A)',advance='NO') &
            'Press a Q/ESC or click X to quit graphics when done ... '

       ievent = get_event()
    end do

    call close_graphics()
    call ode_off()

    ! Clean the pointer, if needed
    if (associated(field)) nullify(field)
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

    if (id_method /= 1) then
       write(*,*) 'LL = ', ll, '(', 10.0_WP ** (-ll), ')'
       write(*,*)
    end if

    write(*,*) 'M1 (kg) = ', m1
    write(*,*) 'M2 (kg) = ', m2
    write(*,*)
    write(*,*) 'RHO1 (g/cm**3) = ', rho1
    write(*,*) 'RHO2 (g/cm**3) = ', rho2
    write(*,*)
    write(*,*) 'L1 (m) = ', l1
    write(*,*) 'L2 (m) = ', l2
    write(*,*)
    write(*,*) 'PHI1 (deg) = ', y0(1)
    write(*,*) 'PHI2 (deg) = ', y0(2)
    write(*,*)
    write(*,*) 'OMEGA1 (deg/s) = ', y0(3)
    write(*,*) 'OMEGA2 (deg/s) = ', y0(4)
    write(*,*)
    write(*,*) 'RATE = ', rate
    write(*,*)
    write(*,*) 'New FIELD = ', new_field
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

    write(*,*) '  M : Masses'
    write(*,*) '  I : Density'
    write(*,*) '  N : Length'
    write(*,*) '  P : PHI(0)'
    write(*,*) '  O : OMEGA(0)'
    write(*,*) '  A : RATE'

    if (new_field) then
       write(*,*) '  F : Old FIELD'
    else
       write(*,*) '  F : New FIELD'
    end if

    write(*,*) '  R : RUN'
    write(*,*) '  Q : QUIT'
  end subroutine show_menu

  subroutine process_menu(ikey)
    use :: getdata, only: get

    integer, intent(in) :: ikey

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
       call get('M1 (kg) = ',m1)
       call get('M2 (kg) = ',m2)
    case (ichar('I'))
       call get('RHO1 (g/cm**3) = ',rho1)
       call get('RHO2 (g/cm**3) = ',rho2)
    case (ichar('N'))
       call get('L1 (m) = ',l1)
       call get('L2 (m) = ',l2)
    case (ichar('P'))
       call get('PHI1 (deg) = ',y0(1))
       call get('PHI2 (deg) = ',y0(2))
    case (ichar('O'))
       call get('OMEGA1 (deg/s) = ',y0(3))
       call get('OMEGA2 (deg/s) = ',y0(4))
    case (ichar('A'))
       call get('RATE = ',rate)
    case (ichar('F'))
       new_field = .not. new_field

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
    use :: getdata, only: get

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
end module double_pendulum_lib

program double_pendulum
  use :: double_pendulum_lib

  implicit none

  call app_menu()
end program double_pendulum
