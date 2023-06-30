!
! Author: ANGELO GRAZIOSI
!
!   created   : Jun 26, 2023
!   last edit : Jun 30, 2023
!
!   Star Walk in 2D of NPOINTS
!
! DESCRIPTION
!
!   We draw the 2D star walk using SDL2-Fortran interface. The stars
!   are initially distribuited at random in a square region then the
!   gravitational field is computed at the position of each star and
!   the star moves one step (of unit size) in the direction and the
!   verse indicated by the field. We assume for all stars
!
!     mu(i) = G*M(i) == 1
!
!   The units are arbitrary.
!
! REFERENCES
!
!   https://towardsdatascience.com/random-walks-with-python-8420981bc4bc
!   S. Chandrashekhar Stochastic Problems in Physics and Astronomy,
!   Rev. Mod. Phys. 15, 1 – Published 1 January 1943
!
! SOME TEST RUN
!
!   ./star_walk_2d-static.out 1000 5000 50 -1
!
!   Try also with VIEW_SIDE 25, 15, 10
!
! COMPUTING THE TRANSFORMATION FROM WC TO DC
!
!   WC = (X1,X2) x (Y1,Y2), DC = (0,IMAX) x (JMAX,0)
!
!   DC is defined as (X1S,X2S) x (Y1S,Y2S), i.e.:
!
!     X1S = 0
!     X2S = IMAX
!     Y1S = JMAX
!     Y2S = 0
!
!   Units per pixel:
!
!     DX = (X2-X1)/IMAX, DY = (Y2-Y1)/JMAX
!
!   Transformation from DC to WC:
!
!     X = X1+I*DX,  Y = Y2-J*DY
!
!   Tranformation from WC to DC:
!
!     I = (X-X1)/DX = DPX*(X-X1),  J = (Y2-Y)/DY = DPY*(Y2-Y)
!
!   where:
!
!     DPY = (1/DX) = IMAX/(X2-X1), DPY = (1/DY) = JMAX/(Y2-Y1)
!
!   are the pixel per unit in X and Y
!
! HOW TO BUILD THE APP
!
!   cd sdl2-fortran.apps
!
!   git clone https://github.com/interkosmos/fortran-sdl2.git
!
!   rm -rf *.mod; \
!     gfortran[-mp-X] -std=f2018 -O3 -Wall \
!       [`sdl2-config --cflags`] -J ../modules \
!       ../basic-modules/{{kind,math}_consts,nicelabels}.f90 \
!       $SDL2F90 SDL2_app.f90 star_walk_2d.f90 \
!       $LIBS -o star_walk_2d$EXE; \
!   rm -rf *.mod
!
!   ./star_walk_2d$EXE 1000 5000 50 30
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
!     SDL2F90 = fortran-sdl2/src/{c_util,sdl2/{sdl2_stdinc,sdl2_audio,\
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
!     open star_walk_2d$EXE NSTEPS
!
!   being:
!
!     alias open='start'
!
!   Maybe the same considerations hold for GNU/Linux and macOS.
!

program star_walk_2d
  use kind_consts, only: WP

  use SDL2_app, only: QUIT_EVENT, &
       clear_screen, close_graphics, draw_point, get_event, init_graphics, &
       refresh, set_rgba_color, clear_viewport

  implicit none

  integer, parameter :: NUMARGS = 4, &
       SCREEN_WIDTH = 900, SCREEN_HEIGHT = 900, &
       IMAX = SCREEN_WIDTH-1, JMAX = SCREEN_HEIGHT-1

  ! Region, pixel per unit (in X and Y)
  real(WP) :: x_min, x_max, y_min, y_max, dpx, dpy

  character(len=*), parameter :: FMT = '(*(g0,1x))'  !'(a,g0.7)'

  integer :: nargs, ievent = -1000
  real(WP) :: args_val(NUMARGS)
  character(len=80) :: args

  integer :: nsteps, npoints, i, j, ierr
  real(WP) :: rms_d, f(2), side, view_side, mean_x, mean_y, sigma_x, sigma_y
  real(WP), allocatable :: p(:,:)

  nargs = command_argument_count()

  if (nargs /= NUMARGS) &
       stop ': USAGE: ./star_walk_2d <NSTEPS> <NPOINTS> <SIDE> <VSIDE>'

  ! Reading the arguments
  do i = 1, nargs
     call get_command_argument(i,args)
     read(args,*) args_val(i)
  end do

  ! ARGS_VAL(1) is NSTEPS...
  nsteps = nint(args_val(1))
  if (nsteps < 1) nsteps = 1000

  ! The rms displacement is sqrt(nsteps)
  rms_d = nsteps
  rms_d = sqrt(rms_d)

  ! ARGS_VAL(2) is NPOINTS...
  npoints = nint(args_val(2))
  if (npoints < 1) npoints = 1000

  ! First row of P: X coordinate
  ! Second row of P: Y coordinate
  allocate(p(2,npoints),stat=ierr)
  if (ierr /= 0) stop ': Allocation failure for P(:,:).'

  ! ARGS_VAL(3) is SIDE...
  side = args_val(3)
  if (side < 0) side = 6*rms_d

  ! ARGS_VAL(4) is VIEW_SIDE...
  view_side = args_val(4)
  if (view_side < 0) view_side = side

  ! Computing params for initial positions
  x_max = side/2
  x_min = -x_max

  y_max = x_max
  y_min = -y_max

  ! Initial positions in the square [x_min,x_max] x [y_min,y_max]
  do j = 1, npoints
     call random_number(f)
     associate (q => p(:,j))
       q = x_min+(x_max-x_min)*f
     end associate
  end do

  ! Computing params for WC to DC mapping
  x_max = view_side/2
  x_min = -x_max

  y_max = x_max
  y_min = -y_max

  dpx = IMAX/(x_max-x_min)
  dpy = JMAX/(y_max-y_min)

  ! ... or ALL points at the origin O
  ! x = 0
  ! y = 0

  write(*,*)
  write(*,FMT) 'Running with NSTEPS     : ', nsteps
  write(*,FMT) 'Running with NPOINTS    : ', npoints
  write(*,FMT) 'Running with RMS_D      : ', rms_d
  write(*,FMT) 'Running with REGION SIDE: ', side
  write(*,FMT) 'Running with VIEW SIDE  : ', view_side
  write(*,*)

  call init_graphics('The Star Walk in 2D', &
       width=SCREEN_WIDTH,height=SCREEN_HEIGHT)

  do i = 1, nsteps
     call clear_screen()

     ! YELLOW: we have to set the color after calling CLEAR_SCREEN()
     ! because... CLEAR_SCREEN() IS TOO PRIMITIVE!!!
     call set_rgba_color(255,255,0)

     do j = 1, npoints
        associate (q => p(:,j))
          call field(q,f)
          f = f/norm2(f)

          ! The diameter of the final cluster is determined by the
          ! step size F. Try:
          !
          !   q = q+k*f
          !
          ! with k = 2, 3, ..., 0.1, 0.5, ...
          !
          q = q+f

          call draw_point(x2s(q(1)),y2s(q(2)))
        end associate
     end do

     call refresh()

     if (mod(i,100) == 0) write(*,FMT) 'CURRENT STEP            : ', i
  end do

  ! Computing mean and sigma for final cluster(s)
  ! See: https://www.programming-idioms.org/idiom/203/calculate-mean-and-standard-deviation/3468/fortran
  !
  associate (x => p(1,:), y => p(2,:))
    mean_x = sum(x)/npoints
    mean_y = sum(y)/npoints

    sigma_x = sqrt(sum(x**2 )/npoints-mean_x**2 )
    sigma_y = sqrt(sum(y**2 )/npoints-mean_y**2 )
  end associate

  write(*,*)
  write(*,FMT) 'FINAL CLUSTER(S) MEAN-X : ', mean_x
  write(*,FMT) 'FINAL CLUSTER(S) MEAN-Y : ', mean_y
  write(*,*)
  write(*,FMT) 'FINAL CLUSTER(S) SIGM-X : ', sigma_x
  write(*,FMT) 'FINAL CLUSTER(S) SIGM-Y : ', sigma_y

  associate (r => sqrt(p(1,:)**2+p(2,:)**2), &
       mean_r => mean_x, sigma_r => sigma_x)
    mean_r = sum(r)/npoints

    sigma_r = sqrt(sum(r**2 )/npoints-mean_r**2 )

    write(*,*)
    write(*,FMT) 'FINAL CLUSTER(S) MEAN-R : ', mean_r
    write(*,FMT) 'FINAL CLUSTER(S) SIGM-R : ', sigma_r
    write(*,*)
  end associate

  do while (ievent /= QUIT_EVENT)

     ievent = get_event()
  end do

  call close_graphics()

  if (allocated(p)) deallocate(p,stat=ierr)
  if (ierr /= 0) stop ': Deallocation failure for P(:,:).'

contains

  function x2s(x) result (r)
    real(WP), intent(in) :: x
    integer :: r

    r = nint(dpx*(x-x_min))
  end function x2s

  function y2s(y) result (r)
    real(WP), intent(in) :: y
    integer :: r

    r = nint(dpy*(y_max-y))
  end function y2s

  subroutine field(q,f)
    real(WP), intent(in) :: q(2)
    real(WP), intent(out) :: f(2)

    integer, save :: i
    real(WP), save :: d(2), r

    ! G(P) = -SUM_i mu(i)*(r(P)-r(i))/|r(P)-r(i)|**3, with i /= P
    !
    !   mu(i) = G*m(i)
    !
    ! Here mu(i) = 1
    !

    f = 0
    do i = 1, npoints
       d = p(:,i)-q         ! p(:,i)-q = -(q-p(:,i)): this avoids f = -f
       r = norm2(d)

       if (r == 0) cycle    ! i /= P ...

       d = d/r**3
       f = f+d
    end do

  end subroutine field

end program star_walk_2d
