!
! Author: ANGELO GRAZIOSI
!
!   created   : Dec 21, 2022
!   last edit : Dec 24, 2022
!
!   Boundary Value Problem for Poisson Equation
!
! DESCRIPTION
!
!   We solve the Dirichlet problem for Poisson equation in two
!   dimension with overrelaxation of Gauss-Seidel method.
!   The equation is
!
!     Uxx+Uyy = -S(x,y)
!
!   where Uxx (Uyy) is the 2nd partial derivative w.r.t. x (y) of U(x,y),
!   the potential. -S(x,y) is the charge density.
!
!   This is a "remake" in SDL2-Fortran of 'poisson2D.f90', a
!   Windows-Fortran app (i.e. an app written in Fortran interfacing
!   Windows API).
!
! REFERENCES
!
!   Press W.H., Numerical Recipes, C.U.P
!   Karlen D., Computational Physics, Carleton University
!   Koonin S.E., Computational Physics, Addison-Wesley
!
! HOW TO BUILD THE APP
!
!   cd sdl2-fortran.apps
!
!   git clone https://github.com/interkosmos/fortran-sdl2.git
!
!   cd poisson2D_solver
!
!   rm -rf *.mod; \
!     gfortran[-mp-X] -std=f2018 -O3 -Wall \
!       [`sdl2-config --cflags`] \
!       ../../basic-modules/{{kind,math}_consts,getdata,nicelabels}.f90 \
!       $SDL2F90 ../SDL2_{app,shading}.f90 \
!       poisson2D_solver.f90 \
!       $LIBS -o poisson2D_solver$EXE; \
!   rm -rf *.mod
!
!   ./poisson2D_solver$EXE
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
!     SDL2F90 = ../fortran-sdl2/src/{c_util,sdl2/{sdl2_stdinc,sdl2_audio,\
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

module poisson2D_solver_lib
  use kind_consts, only: WP
  use math_consts, only: ZERO => Z0, ONE => Z1, TWO => Z2, PI, HF => Q1_2

  implicit none
  private

  ! Useful constants
  integer, parameter :: ONE_BOX         = 1
  integer, parameter :: TWO_BOX         = 2
  integer, parameter :: CONDENSER       = 3
  integer, parameter :: THREE_CHARGES   = 4
  integer, parameter :: CHARGED_LINE    = 5
  integer, parameter :: POTENTIAL_FIELD = 1
  integer, parameter :: GRADIENT_FIELD  = 2

  ! To strip '  1 : ' use CONFIG(1)(7:) and so on...
  character(len=*), parameter :: CONFIG(5) = [ &
       '  1 : ONE BOX      ', &
       '  2 : TWO BOX      ', &
       '  3 : CONDENSER    ', &
       '  4 : THREE CHARGES', &
       '  5 : CHARGED LINE ' ]

  character(len=*), parameter :: FIELD(2) = [ &
       '  1 : POTENTIAL', &
       '  2 : GRADIENT ' ]

  ! Input data
  integer :: ndiv = 100, max_iter = 100, nsout = 10, &
       config_type = ONE_BOX, field_type = POTENTIAL_FIELD
  real(WP) :: eps = 0.0001_WP, omega = 1.8_WP, &
       u_left = ONE, u_right = ZERO, &
       u_bottom = ZERO, u_top = ZERO, &
       phi_min = ZERO, phi_max = ONE

  ! Auxiliary data
  logical :: converg = .false.
  integer :: count_iter = 0, i1 = 0, i2 = 0, j1 = 0, j2 = 0
  real(WP) :: dphi = ZERO, h = ZERO, hq = ZERO, &
       omega1 = ZERO, omega4 = ZERO, &
       energy = ZERO, energy_old = ZERO

  ! Dynamic memory...
  real(WP), target, allocatable :: u(:,:),f(:,:)
  real(WP), allocatable :: s(:,:)
  real(WP), pointer :: phi(:,:) => null()
  logical, allocatable :: b(:,:)

  public :: app_menu

contains

  subroutine grid_on()
    integer :: ierr = 0

    allocate(u(0:ndiv,0:ndiv),stat=ierr)
    if (ierr /= 0) stop ': Allocation failure for U(:,:).'

    allocate(s(0:ndiv,0:ndiv),stat=ierr)
    if (ierr /= 0) stop ': Allocation failure for S(:,:).'

    allocate(f(0:ndiv,0:ndiv),stat=ierr)
    if (ierr /= 0) stop ': Allocation failure for F(:,:).'

    allocate(b(0:ndiv,0:ndiv),stat=ierr)
    if (ierr /= 0) stop ': Allocation failure for B(:,:).'

  end subroutine grid_on

  subroutine grid_off()
    integer :: ierr = 0

    if (allocated(b)) deallocate(b,stat=ierr)
    if (ierr /= 0) stop ': Deallocation failure for B(:,:).'

    if (allocated(f)) deallocate(f,stat=ierr)
    if (ierr /= 0) stop ': Deallocation failure for F(:,:).'

    if (allocated(s)) deallocate(s,stat=ierr)
    if (ierr /= 0) stop ': Deallocation failure for S(:,:).'

    if (allocated(u)) deallocate(u,stat=ierr)
    if (ierr /= 0) stop ': Deallocation failure for U(:,:).'

    ! Now that all is deallocated, we can deassociate the pointer
    nullify(phi)
  end subroutine grid_off

  subroutine fcn_two_box(i,j,u,s,b)
    integer, intent(in) :: i, j
    real(WP), intent(out) :: u, s
    logical, intent(out) :: b
    !
    ! A box inside a box without charge. The inner box is
    ! (0.25,0.25) - (0.75,0.75), but the boundary conditions are assigned
    ! ONLY on its perimeter, NOT on its inner points!!!
    ! (We have a kind of square ring...)
    !

    s = ZERO

    if (((i == i1 .or. i == i2) .and. (j1 <= j .and. j <= j2)) .or. &
         ((j == j1 .or. j == j2) .and. (i1 <= i .and. i <= i2))) then
       u = ONE
       b = .true.
    else
       u = ZERO
       b = .false.
    end if
  end subroutine fcn_two_box

  subroutine fcn_condenser(i,j,u,s,b)
    integer, intent(in) :: i, j
    real(WP), intent(out) :: u, s
    logical, intent(out) :: b
    !
    ! A Condenser inside a box without charge
    ! The condenser plates are at 0.25 and 0.75
    !

    s = ZERO

    if ((i == i1) .and. (j1 <= j .and. j <= j2)) then
       ! Left plate
       u = ONE
       b = .true.
    else if ((i == i2) .and. (j1 <= j .and. j <= j2)) then
       ! Right plate
       u = -ONE
       b = .true.
    else
       u = ZERO
       b = .false.
    end if
  end subroutine fcn_condenser

  function delta(x,hwhm) result (r)
    real(WP), intent(in) :: x, hwhm
    real(WP) :: r

    r = hwhm/(PI*(hwhm*hwhm+x*x))
  end function delta

  function theta(x) result (r)
    real(WP), intent(in) :: x
    real(WP) :: r

    if (x > ZERO) then
       r = ONE
    else
       r = ZERO
    end if
  end function theta

  subroutine fcn_three_charges(x,y,u,s,b)
    real(WP), intent(in) :: x, y
    real(WP), intent(out) :: u, s
    logical, intent(out) :: b

    real(WP), parameter :: HWHM = 0.0005_WP

    u = ZERO
    b = .false.
    s = 0.1_WP*(delta(x-0.25_WP,HWHM)*delta(y-0.25_WP,HWHM) &
         +delta(x-0.75_WP,HWHM)*delta(y-0.25_WP,HWHM) &
         -delta(x-HF,HWHM)*delta(y-0.75_WP,HWHM))
  end subroutine fcn_three_charges

  subroutine fcn_charged_line(x,y,u,s,b)
    real(WP), intent(in) :: x, y
    real(WP), intent(out) :: u, s
    logical, intent(out) :: b

    real(WP), parameter :: HWHM = 0.0005_WP

    u = ZERO
    b = .false.

    s = delta(x-HF,HWHM)*(theta(y-0.25_WP)-theta(y-0.75_WP))
  end subroutine fcn_charged_line

  subroutine setup_aux_params
    use SDL2_shading, only: MAX_COLOURS

    ! Setup some aux. params
    h = ONE/ndiv
    hq = h*h
    omega1 = ONE-omega
    omega4 = 0.25_WP*omega
    dphi = (phi_max-phi_min)/MAX_COLOURS

    ! Params for TWO box etc. problems
    i1 = int(0.25_WP/h)
    i2 = int(0.75_WP/h)

    j1 = i1
    j2 = i2

    ! Now that all is allocated and the field is chosen, we can
    ! associate the pointer
    if (field_type == POTENTIAL_FIELD) then
       phi => u
    else
       phi => f      ! ...the GRADIENT_FIELD
    end if
  end subroutine setup_aux_params

  subroutine setup_grid()
    real(WP) :: x, y
    integer :: i, j

    ! Grid initialization
    ! The method, as you can see, NEVER uses the values of density s(:,:) on
    ! boundaries! This is used as a trick in computing the energy: we
    ! set s(:,:) on boundary to ZERO.
    ! b(i,j) == true if in x = i*h, y = j*h there is a boundary condition
    !
    ! First the bottom and top side...
    do i = 0, ndiv
       u(i,0) = u_bottom
       u(i,ndiv) = u_top
       s(i,0) = ZERO
       s(i,ndiv) = ZERO
       f(i,0) = ZERO
       f(i,ndiv) = ZERO
       b(i,0) = .true.
       b(i,ndiv) = .true.
    end do

    ! ...then the left and right side...
    do j = 0, ndiv
       u(0,j) = u_left
       u(ndiv,j) = u_right
       s(0,j) = ZERO
       s(ndiv,j) = ZERO
       f(0,j) = ZERO
       f(ndiv,j) = ZERO
       b(0,j) = .true.
       b(ndiv,j) = .true.
    end do

    ! ...then the inner nodes
    do i = 1, ndiv-1
       x = i*h
       do j = 1, ndiv-1
          y = j*h
          select case(config_type)
          case(ONE_BOX)
             u(i,j) = ZERO
             s(i,j) = ZERO
             b(i,j) = .false.
          case(TWO_BOX)
             call fcn_two_box(i,j,u(i,j),s(i,j),b(i,j))
          case(CONDENSER)
             call fcn_condenser(i,j,u(i,j),s(i,j),b(i,j))
          case(THREE_CHARGES)
             call fcn_three_charges(x,y,u(i,j),s(i,j),b(i,j))
          case(CHARGED_LINE)
             call fcn_charged_line(x,y,u(i,j),s(i,j),b(i,j))

          case default
             ! ONE_BOX
             u(i,j) = ZERO
             s(i,j) = ZERO
             b(i,j) = .false.
          end select

          s(i,j) = s(i,j)*hq
          f(i,j) = ZERO
       end do
    end do
  end subroutine setup_grid

  function get_grid_energy() result (r)
    real(WP) :: r
    !
    ! Koonin's formula (6.7):
    !
    ! E = 0.5*Sum(i=1,N)Sum(j=1,N)[(u(i,j)-u(i-1,j))**2+(u(i,j)-u(i,j-1))**2]
    !   -h*h*Sum(i=1,N-1)Sum(j=1,N-1)[S(i,j)*u(i,j)]
    !
    integer :: i = 0, j = 0
    real(WP) :: sum1 = ZERO, sum2 = ZERO, ff = ZERO

    sum1 = ZERO
    sum2 = ZERO
    ff = ZERO

    do i = 1, ndiv
       do j = 1, ndiv

          ! The (length of the) gradient
          ff = hypot(u(i,j)-u(i-1,j),u(i,j)-u(i,j-1))

          f(i,j) = ff

          ! The total sum of the gradient squared
          sum1 = sum1+ff*ff

          ! On i == ndiv, j == ndiv we have s == 0: this means summing
          ! for i = 1,N-1, j = 1,N-1
          sum2 = sum2+s(i,j)*u(i,j)
       end do
    end do

    ! Completing the calculus of the field.
    ! We assume the continuity of the field
    f(1:ndiv,0) = f(1:ndiv,1)
    f(0,1:ndiv) = f(1,1:ndiv)
    f(0,0) = f(1,1)

    ! s(i,j) = hq*S(i*h,j*h)
    r = HF*sum1-sum2
  end function get_grid_energy

  subroutine update_grid()
    integer :: i = 0, j = 0

    do i = 1, ndiv-1
       do j = 1, ndiv-1
          if (b(i,j)) cycle
          u(i,j) = omega1*u(i,j) &
               +omega4*(u(i+1,j)+u(i-1,j)+u(i,j+1)+u(i,j-1)+s(i,j))
       end do
    end do
  end subroutine update_grid

  subroutine display_grid()
    use SDL2_shading, only: MAX_COLOUR_INDEX, get_shading_color, color_rgb_t
    use SDL2_app, only: set_rgba_color, draw_point, refresh

    character(len=*), parameter :: FMT = '(a,i6,2(a,1pg14.7),a,i6,a,1pg14.7)'
    ! https://zmoon.github.io/FortranTipBrowser/tips/041.html
    ! character(len=*), parameter :: FMT = "(*(g0.7, :, ','))"

    ! Implicit saving
    integer :: i = 0, j = 0, k = 0
    type(color_rgb_t) :: c = color_rgb_t(0,0,0)

    write(*,FMT) 'NDIV = ',ndiv,'   OMEGA = ',omega,'   EPS = ',eps, &
         '   COUNT = ',count_iter,'   E = ',energy

    do j = 0, ndiv
       do i = 0, ndiv
          !
          ! Remember that PHI(:,:), on the first row, stores the left
          ! side of the plane while the last row stores the right.  On
          ! the first column, it stores the bottom and on the last
          ! column it stores the top of the plane...
          !
          ! We scan PHI(:,:) by columns starting from the last (i.e
          ! from the top of the plane)..
          !
          k = int((phi(i,ndiv-j)-phi_min)/dphi)

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
    end do
    call refresh()
  end subroutine display_grid

  subroutine solve()
    ! Switch ON the grid...
    call grid_on()

    call setup_aux_params()
    call setup_grid()

    ! Initialization of energy for convergence flags
    energy = get_grid_energy()

    converg = .false.
    do count_iter = 1, max_iter
       energy_old = energy        ! Save current energy

       call update_grid()         ! ...then UPDATE the grid
       energy = get_grid_energy() ! Get the energy for the updated grid

       ! Set convergenze flag
       if (abs(energy-energy_old) <= eps) converg = .true.

       if (mod(count_iter,nsout) == 0 .or. converg) then
          call display_grid()
       end if

       ! Convergence test
       if (converg) exit
    end do

    ! Switch OFF the grid...
    call grid_off()
  end subroutine solve

  subroutine run()
    use SDL2_shading, only: shading_setup
    use SDL2_app, only: init_graphics, close_graphics, clear_screen, &
         QUIT_EVENT, get_event

    integer :: ievent = -1000

    call shading_setup()

    call init_graphics('Poisson 2D Solver', &
         width=ndiv+1,height=ndiv+1)

    call clear_screen()

    ! We need to reset IEVENT if we want to restart the run
    ievent = -1000
    do while (ievent /= QUIT_EVENT)

       call solve()

       ievent = get_event()
    end do

    call close_graphics()

  end subroutine run

  subroutine show_params()
    write(*,*) 'Current parameters:'
    write(*,*)
    write(*,*) 'USING '//trim(CONFIG(config_type)(7:))//' CONFIGURATION'
    write(*,*) 'USING '//trim(FIELD(field_type)(7:))//' FIELD'
    write(*,*)
    write(*,*) 'NDIV     = ', ndiv
    write(*,*) 'MAX_ITER = ', max_iter
    write(*,*) 'NSOUT    = ', nsout
    write(*,*)
    write(*,*) 'EPS   = ', eps
    write(*,*) 'OMEGA = ', omega
    write(*,*)
    write(*,*) 'U_LEFT  = ', u_left
    write(*,*) 'U_RIGHT = ', u_right
    write(*,*)
    write(*,*) 'U_BOTTOM = ', u_bottom
    write(*,*) 'U_TOP    = ', u_top
    write(*,*)
    write(*,*) 'PHI_MIN = ', phi_min
    write(*,*) 'PHI_MAX = ', phi_max
    write(*,*)
  end subroutine show_params

  subroutine show_menu()
    write(*,*) 'Choose item:'
    write(*,*) '  C : CONFIG_TYPE'
    write(*,*) '  F : FIELD_TYPE'
    write(*,*)
    write(*,*) '  D : NDIV'
    write(*,*) '  I : MAX_ITER'
    write(*,*) '  N : NSOUT'
    write(*,*)
    write(*,*) '  E : EPS'
    write(*,*) '  O : OMEGA'
    write(*,*)
    write(*,*) '  L : U_LEFT/RIGHT'
    write(*,*) '  B : U_BOTTOM/TOP'
    write(*,*)
    write(*,*) '  P : PHI_MIN/MAX'
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
    case (ichar('C'))
       call set_config_type()
    case (ichar('F'))
       call set_field_type()
    case (ichar('D'))
       ival = ndiv
       call get('NDIV = ',ival)
       if (ival > 0) then
          ndiv = ival
       else
          write(*,*) 'NDIV <= 0! UNCHANGED...'
       end if
    case (ichar('I'))
       ival = max_iter
       call get('MAX_ITER = ',ival)
       if (ival > 0) then
          max_iter = ival
       else
          write(*,*) 'MAX_ITER <= 0! UNCHANGED...'
       end if
    case (ichar('N'))
       ival = nsout
       call get('NSOUT = ',ival)
       if (ival > 0) then
          nsout = ival
       else
          write(*,*) 'NSOUT <= 0! UNCHANGED...'
       end if
    case (ichar('E'))
       x = eps
       call get('EPS = ',x)
       if (ZERO < x .and. x < ONE) then
          eps = x
       else
          write(*,*) 'EPS not in (0,1)! UNCHANGED...'
       end if
    case (ichar('O'))
       x = omega
       call get('OMEGA = ',x)
       if (ZERO < x .and. x < TWO) then
          omega = x
       else
          write(*,*) 'OMEGA not in (0,2)! UNCHANGED...'
       end if
    case (ichar('L'))
       call get('U_LEFT = ',u_left)
       call get('U_RIGHT = ',u_right)
    case (ichar('B'))
       call get('U_BOTTOM = ',u_bottom)
       call get('U_TOP = ',u_top)
    case (ichar('P'))
       x = phi_min
       y = phi_max
       call get('PHI_MIN = ',x)
       call get('PHI_MAX = ',y)
       if (x < y) then
          phi_min = x
          phi_max = y
       else
          write(*,*) 'PHI_MIN >= PHI_MAX! UNCHANGED...'
       end if

    case (ichar('R'))
       call run()
    end select

    write(*,*)

  contains

    subroutine set_config_type()
      integer :: i, m

      m = size(CONFIG)

      write(*,*) 'Choose the problem:'
      do i = 1, m
         write(*,*) CONFIG(i)
      end do

      call get('CONFIG_TYPE =',config_type)
      write(*,*)

      ! Correction so that CONFIG_TYPE is always in [1,M]
      ! Remember that MOD(N,M) is in [-(M-1),0] if N < 0!
      if (config_type <= 0) then
         config_type = 1
      else
         config_type = mod(config_type-1,m)+1
      end if
    end subroutine set_config_type

    subroutine set_field_type()
      integer :: i, m

      m = size(FIELD)

      write(*,*) 'Choose the field:'
      do i = 1, m
         write(*,*) FIELD(i)
      end do

      call get('FIELD_TYPE =',field_type)
      write(*,*)

      ! Correction so that FIELD_TYPE is always in [1,M]
      ! Remember that MOD(N,M) is in [-(M-1),0] if N < 0!
      if (field_type <= 0) then
         field_type = 1
      else
         field_type = mod(field_type-1,m)+1
      end if
    end subroutine set_field_type

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
end module poisson2D_solver_lib

program poisson2D_solver
  use poisson2D_solver_lib

  call app_menu()
end program poisson2D_solver
