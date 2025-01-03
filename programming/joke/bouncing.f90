!
! Author: Angelo Graziosi
!
!   created   : Feb 10, 2014
!   last edit : Dec 29, 2024
!
!   Bouncing ball: a remake with SDL2 of BOUNCE_PLUS, an app base on
!   WIN32-FORTRAN interface, and BALLS_SIM[ULATION], apps based on
!   WBGI-FORTRAN interface.
!
!   This program is used also to test some primitive of SDL2_app module.
!
! DESCRIPTION
!
!   We integrate the equations of motion for N balls colliding
!   elastically (Hooke Low). The integration is very "crude":
!   Euler-Cromer 1st order integration algorithm.
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
!   cd joke
!
!   rm -rf *.mod; \
!     gfortran[-mp-X] [-g3 -fbacktrace -fcheck=all] [-march=native] \
!       -Wall -std=f2018 [-fmax-errors=1] -O3 \
!       -I ../finclude [`sdl2-config --cflags`] \
!       bouncing.f90 -o bouncing$EXE \
!       -L ../lib -lbasic_mods -lfortran-sdl2apps -lfortran-sdl2 \
!       $LIBS; \
!   rm -rf *.mod
!
!   ./bouncing$EXE
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

module bouncing_lib
  use :: kind_consts, only: WP
  use :: math_consts, only: ZERO => Z0, ONE => Z1, HF => Q1_2, Q1_3, PI
  use :: shading_colors, only: color_rgb_t

  implicit none
  private

  type ball_t
     type(color_rgb_t) :: col = color_rgb_t(0,0,0)
     real(WP) :: mass = ZERO, &
          density = ZERO, &
          radius = ZERO
     real(WP), dimension(2) :: force = ZERO, &
          acc = ZERO, &
          vel = ZERO, &
          pos = ZERO
  end type ball_t

  ! Data
  integer :: nballs = 12, nsout = 1, screen_width = 1200, screen_height = 900
  real(WP) :: x_size = 1200, density = 0.01_WP, stiffnes = 5E5_WP, &
       m0 = 400.0_WP, m1 = 8000.0_WP, &
       tstep = ONE/512  ! 1.953125E-03 = 0.000000001_2

  ! Auxiliary data
  integer :: ierr = 0
  real(WP) :: x_min = ZERO, x_max = ZERO, y_min = ZERO, y_max = ZERO, &
       tstop = ZERO

  ! Dynamic memory...
  type(ball_t), allocatable :: ball(:)

  public:: app_menu

contains

  subroutine setup_params()
    associate (h2 => x_max)

      ! IT IS HALF the HEIGHT!!!
      h2 = HF*((x_size*screen_height)/screen_width)

      y_max = h2
      y_min = -h2
    end associate

    x_max = x_size/2
    x_min = -x_max

    ! Initialization
    tstop = ZERO
  end subroutine setup_params

  subroutine create_balls()
    real(WP), parameter :: SPHERE_FAC = 0.75_WP/PI ! 3/(4*PI) = 1/(4*PI/3)
    real(WP) :: u(9)
    integer  :: i

    ! Create balls
    allocate(ball(nballs),STAT=ierr)
    if (ierr /= 0) stop ': Allocation failure for BALL(:).'

    ! Set startup conditions of elastic balls
    do i = 1, nballs

       call random_number(u)

       ball(i)%col = color_rgb_t(int(64+u(1)*192), &
            int(64+u(2)*192), &
            int(64+u(3)*192))

       ball(i)%mass = m0+(i-1)*(m1-m0)/(nballs-1)
       ball(i)%density = density

       ball(i)%radius = ((ball(i)%mass/ball(i)%density)*SPHERE_FAC)**Q1_3

       ball(i)%pos = [ (ONE-u(4))*(x_min+ball(i)%radius) &
            +u(4)*(x_max-ball(i)%radius), &
            (ONE-u(5))*(y_min+ball(i)%radius) &
            +u(5)*(y_max-ball(i)%radius) ]

       ball(i)%vel = 200*[ u(6)-u(7), u(8)-u(9) ]
    end do
  end subroutine create_balls

  subroutine destroy_balls()
    if (allocated(ball)) deallocate(ball,STAT=ierr)
    if (ierr /= 0) stop ': Deallocation failure for BALL(:).'
  end subroutine destroy_balls

  subroutine draw_ball(p,r,c)
    use :: sdl2app, only: set_rgba_color, draw_circle

    real(WP), intent(in) :: p(:), r
    type(color_rgb_t), intent(in) :: c

    call set_rgba_color(c%r,c%g,c%b)

    call draw_circle(p(1),p(2),r)
    call draw_circle(p(1),p(2),r-HF)
    call draw_circle(p(1),p(2),r-ONE)
  end subroutine draw_ball

  subroutine paint_screen()
    use :: sdl2app, only: quit

    character(len=*), parameter :: FMT = '(*(g0,1x))'

    integer :: istep
    real(WP) :: t

    istep = 0
    t = tstop

    ! Display initial position
    call display_balls()

    do while (.not. quit())
       istep = istep+1
       t = tstop+istep*tstep

       call update_balls()

       if (mod(istep,nsout) == 0) call display_balls()

    end do

    tstop = t
    write(*,*)
    write(*,FMT) 'Stopped at T = ', tstop
    write(*,*)

  contains

    subroutine display_balls()
      use :: sdl2app, only: clear_screen, refresh

      integer :: i

      write(*,FMT) 'T = ', t

      call clear_screen()
      do i = 1, nballs
         call draw_ball(ball(i)%pos,ball(i)%radius,ball(i)%col)
      end do

      call refresh()
    end subroutine display_balls

    subroutine update_balls()
      ! We use SAVE just to save something at each call
      real(WP), save :: force(2), ball_distance, dist_min, dist(2)
      integer, save :: i, j

      ! We DO NOT set BALL()%FORCE = 0 because it is already
      ! initialized. See below.

      ! Test all elastic balls against each other.
      ! Calculate forces if they touch.
      do i = 1, nballs-1
         do j = i+1, nballs
            ! Distance between elastic balls (Pythagoras' theorem)
            dist = ball(j)%pos-ball(i)%pos
            ball_distance = norm2(dist)
            dist_min = ball(i)%radius+ball(j)%radius
            if (ball_distance < dist_min) then

               ! Cosine and sine to the angle between ball i and j
               ! (trigonometry): here 'force' is a unit vector!
               force = dist/ball_distance

               ! Spring force (Hooke's law of elasticity)
               ! Here 'force' is the total force of 'i' on 'j' :
               ! (All capital letters are vectors)
               !
               !   F(i -> j) = -k * S = -k*(Bd-Dm) = -k*(|Bd|-|Dm|)*U
               !   U = Bd/|Bd|
               force = -stiffnes*(ball_distance-dist_min)*force

               !
               ! Notice that ball()%force IS ALREADY INITIALIZED to ZERO
               ! in the definition of ball_t ...
               !
               ! F(i) = F(i)+F(j,i) = F(i)-F(i,j), F(j) = F(j)+F(i,j)
               ! being F(i,j) the force of 'i' on 'j'
               ball(i)%force = ball(i)%force-force
               ball(j)%force = ball(j)%force+force
            end if
         end do
      end do

      ! Update acceleration, velocity, and position of elastic balls
      ! (using the Euler-Cromer 1st order integration algorithm)
      do i = 1, nballs
         ! Accelerate balls (acceleration = force / mass)
         ball(i)%acc = ball(i)%force/ball(i)%mass

         ! ... and here we reset force vector for the next computation
         ball(i)%force = ZERO

         ! Update velocity
         ! delta velocity = acceleration * delta time
         ! new velocity = old velocity + delta velocity
         ball(i)%vel = ball(i)%vel+ball(i)%acc*tstep

         ! Update position
         ! delta position = velocity * delta time
         ! new position = old position + delta position
         ball(i)%pos = ball(i)%pos+ball(i)%vel*tstep

         ! Now keep elastic balls within screen boundaries
         ! Right
         if (ball(i)%pos(1) > x_max-ball(i)%radius) then
            ball(i)%vel(1) = -ball(i)%vel(1)
            ball(i)%pos(1) = x_max-ball(i)%radius
         end if

         ! Left
         if (ball(i)%pos(1) < x_min+ball(i)%radius) then
            ball(i)%vel(1) = -ball(i)%vel(1)
            ball(i)%pos(1) = x_min+ball(i)%radius
         end if

         ! Top
         if (ball(i)%pos(2) > y_max-ball(i)%radius) then
            ball(i)%vel(2) = -ball(i)%vel(2)
            ball(i)%pos(2) = y_max-ball(i)%radius
         end if

         ! Bottom
         if (ball(i)%pos(2) < y_min+ball(i)%radius) then
            ball(i)%vel(2) = -ball(i)%vel(2)
            ball(i)%pos(2) = y_min+ball(i)%radius
         end if
      end do
    end subroutine update_balls
  end subroutine paint_screen

  subroutine run()
    use :: sdl2app, only: init_graphics, close_graphics, QUIT_EVENT, &
         get_event, set_rgba_color, draw_ellipse, fill_ellipse, refresh

    integer, parameter :: RED = int(z'FF0000FF')
    integer, parameter :: GREEN = int(z'FF00FF00')
    integer, parameter :: BLUE = int(z'FFFF0000')
    integer, parameter :: MAGENTA = ior(RED,BLUE)
    integer, parameter :: CYAN = ior(GREEN,BLUE)

    integer :: ievent = -1000

    ! First the PARAMS ...
    call setup_params()

    ! ... then the BALLS and GRAPHICS
    call create_balls()

    call init_graphics('Bouncing Balls...', &
         WIDTH=screen_width,HEIGHT=screen_height, &
         X1=x_min,X2=x_max,Y1=y_min,Y2=y_max)

    ! Testing DRAW_ELLIPSE()
    call set_rgba_color(255,255,0)
    call draw_ellipse(screen_width/2,screen_height/2,250,100)
    call draw_ellipse(screen_width/2,screen_height/2,100,250,RED)
    call draw_ellipse(screen_width/2,screen_height/2,250,250,BLUE)
    call set_rgba_color(0,255,0)
    call draw_ellipse(300.0_WP,-200.0_WP,50.0_WP,25.0_WP)
    call draw_ellipse(-300.0_WP,-200.0_WP,50.0_WP,25.0_WP,MAGENTA)
    call set_rgba_color(0,255,255)
    call fill_ellipse(300,200,100,50)
    call fill_ellipse(0.0_WP,300.0_WP,50.0_WP,25.0_WP)
    call draw_ellipse(0.0_WP,300.0_WP,50.0_WP,25.0_WP,MAGENTA)
    call draw_ellipse(300,200,100,50)
    call fill_ellipse(900,200,100,50,CYAN)
    call draw_ellipse(900,200,100,50,MAGENTA)
    call refresh()

    ievent = get_event()

    if (ievent /= QUIT_EVENT) then

       ! We need to reset IEVENT if we want to restart the run
       ievent = -1000
       do while (ievent /= QUIT_EVENT)

          call paint_screen()

          ievent = get_event()
       end do

    end if

    call close_graphics()

    call destroy_balls()
  end subroutine run

  subroutine show_params()
    write(*,*) 'Current parameters:'
    write(*,*)
    write(*,*) 'NBALLS    = ', nballs
    write(*,*) 'NSOUT     = ', nsout
    write(*,*)
    write(*,*) 'DENSITY (g/cm**3) = ', density
    write(*,*) 'STIFFNES (dyn/cm) = ', stiffnes
    write(*,*)
    write(*,*) 'M0 (g) = ', m0
    write(*,*) 'M1 (g) = ', m1
    write(*,*)
    write(*,*) 'TSTEP (s) = ', tstep
    write(*,*)
    write(*,*) 'SCREEN_WIDTH  (pixels) = ', screen_width
    write(*,*) 'SCREEN_HEIGHT (pixels) = ', screen_height
    write(*,*)
    write(*,*) 'X_SIZE (cm) =', x_size
    write(*,*)
  end subroutine show_params

  subroutine show_menu()
    write(*,*) 'Choose item:'
    write(*,*) '  N : Number of balls'
    write(*,*) '  O : Output Rate'
    write(*,*) '  D : Ball Density'
    write(*,*) '  K : Elastic Constant'
    write(*,*) '  M : Mass Bounds'
    write(*,*) '  T : Time Step'
    write(*,*) '  S : Screen Size'
    write(*,*) '  X : X Region Size'
    write(*,*) '  R : RUN'
    write(*,*) '  Q : QUIT'
  end subroutine show_menu

  subroutine process_menu(ikey)
    use :: getdata, only: get

    integer, intent(in) :: ikey

    integer :: ival
    real(WP) :: x, y

    select case (ikey)
    case (ichar('N'))
       ival = nballs
       call get('NBALLS =',ival)
       if (ival > 1) then
          nballs = ival
       else
          write(*,*) 'NBALLS < 2! UNCHANGED...'
       end if
    case (ichar('O'))
       ival = nsout
       call get('NSOUT =',ival)
       if (ival > 0) then
          nsout = ival
       else
          write(*,*) 'NSOUT < 1! UNCHANGED...'
       end if
    case (ichar('D'))
       x = density
       call get('DENSITY (g/cm**3) =',x)
       if (x > ZERO) then
          density = x
       else
          write(*,*) 'DENSITY <= 0! UNCHANGED...'
       end if
    case (ichar('K'))
       x = stiffnes
       call get('STIFFNES (dyn/cm) =',x)
       if (x > ZERO) then
          stiffnes = x
       else
          write(*,*) 'STIFFNES <= 0! UNCHANGED...'
       end if
    case (ichar('M'))
       x = m0
       y = m1
       call get('M0 (g) =',x)
       call get('M1 (g) =',y)
       if (x <= y) then
          m0 = x
          m1 = y
       else
          write(*,*) 'M0 > M1! UNCHANGED...'
       end if
    case (ichar('T'))
       x = tstep
       call get('TSTEP (s) =',x)
       if (x > ZERO) then
          tstep = x
       else
          write(*,*) 'TSTEP <= 0! UNCHANGED...'
       end if
    case (ichar('S'))
       ival = screen_width
       call get('SCREEN_WIDTH (pixels) =',ival)
       if (ival > 0) then
          screen_width = ival
       else
          write(*,*) 'SCREEN_WIDTH <= 0! UNCHANGED...'
       end if
       ival = screen_height
       call get('SCREEN_HEIGHT (pixels) =',ival)
       if (ival > 0) then
          screen_height = ival
       else
          write(*,*) 'SCREEN_HEIGHT <= 0! UNCHANGED...'
       end if
    case (ichar('X'))
       x = x_size
       call get('X_SIZE (cm3) =',x)
       if (x > ZERO) then
          x_size = x
       else
          write(*,*) 'X_SIZE <= 0! UNCHANGED...'
       end if

    case (ichar('R'))
       call run()
    end select

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
end module bouncing_lib

program bouncing
  use :: bouncing_lib

  ! Initialize the rnd generator (maybe we do not need this anymore in
  ! recent version of GFortran)
  call random_init(.false., .false.)

  call app_menu()
end program bouncing
