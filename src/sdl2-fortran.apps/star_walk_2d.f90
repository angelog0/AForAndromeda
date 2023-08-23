!
! Author: ANGELO GRAZIOSI
!
!   created   : Jun 26, 2023
!   last edit : Aug 23, 2023
!
!   Star Walk in 2D of NSTARS
!
! DESCRIPTION
!
!   We draw the 2D star walk using SDL2-Fortran interface. The stars
!   are initially distribuited at random in a rectangular region then
!   the gravitational field is computed at the position of each star
!   and the star moves one STEP in the direction and the verse
!   indicated by the field. We assume for all stars
!
!     mu(i) = G*M(i) == 1
!
!   The units are arbitrary.
!
!   If these units were UA and D (Day), for the SUN it would be
!
!     mu(SUN) = G*M(SUN) = k**2 = 2.95...E-4 UA**3/D**2
!
!   this means that G*M == 1 ==> M = M(SUN)/mu(SUN) ~ 3390*M(SUN).  If
!   the step is 1, i.e. 1 UA, assuming s == 1 UA = (1/2)g*t**2, it
!   would be t = sqrt(2/g). With 5000 points in a square of side 1000
!   (UA), we would have sqrt(5000) in a side of 1000 (UA), i.e. about
!   1000/sqrt(5000) ~ 14 UA between two near points, and this would
!   give g = GM/r**2 ~ 1/14**2, i.e. t = sqrt(2/g) ~ sqrt(2*14**2) ~
!   20 D. In this model 1 step (1 UA) is done in about 20 D.
!
!   Just a few divagations...
!
! MORE ABOUT THE UNITS
!
!   We can think of the units as being chosen as in ref. 3. Indeed our
!   equations of motion are the same. We have here all the stars with
!   the same unit mass: M = 1. So if M = 1 g and L = 1 cm, T = 1.08 h;
!   if M = 1 MSUN and L = 1 pc, T = 1.43E7 yr, and so on.
!
! REFERENCES
!
!   1. https://towardsdatascience.com/random-walks-with-python-8420981bc4bc
!
!   2. S. Chandrashekhar Stochastic Problems in Physics and Astronomy,
!      Rev. Mod. Phys. 15, 1 -- Published 1 January 1943
!
!   3. V. Szebehely and C. F. Peters, Complete Solution of a General
!      Problem of Three Bodies, The Astronomical Journal, vol. 72,
!      p. 876 (1967)
!      http://adsabs.harvard.edu/full/1967AJ.....72..876S
!      SEE also the program THREE_BODIES.F90
!
! SOME TEST RUN
!
!   NSTARS = 5000, NSOUT = 100, XSIZE = VIEW_XSIZE = 50, STEP = 1
!
!   Try also with XSIZE = 500, VIEW_XSIZE = 600, 25, 15, 10 and/or
!   XSIZE 100, 1000..., i.e.
!
! and let it run for more than 50000 steps (83610).
!
! With STEP = 1.5 the final cluster has radius ~ 0.8 and the time for
! its formation is smaller. It is as if the stars were put in a
! smaller region.
!
! With STEP = 0.5 the final cluster has radius ~ 0.3 and the time for
! its formation is bigger. It is as if the stars were put in a larger
! region.
!
! HOW TO BUILD THE APP (MSYS2/MINGW64, GNU/Linux, macOS)
!
!   cd sdl2-fortran.apps
!
!   git clone https://github.com/interkosmos/fortran-sdl2.git
!
!   rm -rf *.mod; \
!     gfortran[-mp-X] [-g3 -fbacktrace -fcheck=all] [-march=native] \
!       -Wall -std=f2018 [-fmax-errors=1] \
!       [-I ...] -O3 [`sdl2-config --cflags`] -J ../../modules \
!       ../../basic-modules/{{kind,math}_consts,getdata,nicelabels}.f90 \
!       $SDL2F90 ../SDL2_{app,shading}.f90 \
!       star_walk_2d.f90 -o star_walk_2d$EXE $LIBS; \
!   rm -rf *.mod
!
!   ./star_walk_2d$EXE
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
!     LIBS = -static -lmingw32 [-lSDL2main] -lSDL2 -lws2_32 -ldinput8 \
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

module star_walk_2d_lib
  use :: kind_consts, only: WP
  use :: getdata, only: get
  use :: SDL2_app, only: init_graphics, close_graphics, QUIT_EVENT, &
       get_event, quit, draw_point, draw_circle, draw_ellipse, set_color, &
       clear_screen, refresh, fill_circle
  use :: SDL2_shading, only: LRED, LGREEN, LBLUE, YELLOW, WHITE, &
       BORLAND_PALETTE

  implicit none
  private

  character(len=*), parameter :: TITLE = 'The Star Walk in 2D'
  character(len=*), parameter :: FMT = '(*(g0,1x))'

  ! DATA
  !
  integer :: screen_width = 900, screen_height = 900, &
       nsout = 100, nstars = 5000
  real(WP) :: xsize = 50, view_xsize = 50, step = 1

  ! AUXILIARY DATA
  !
  integer :: ival, i, istep, ierr = 0

  ! DENSITY = NSTARS/(XSIZE*YSIZE); ysize = xsize etc.
  real(WP) :: x_min, x_max, y_min, y_max, f(2), &
       ysize = 50, view_ysize = 50, &
       rms_d, mean_x, mean_y, sigma_x, sigma_y, density = 5E3_WP/50**2, rval

  ! MEMORY HANDLERS
  real(WP), allocatable :: p(:,:)

  public :: app_menu

contains

  subroutine setup_params()
    ! First row of P  : X coordinate
    ! Second row of P : Y coordinate
    allocate(p(2,nstars),STAT=ierr)
    if (ierr /= 0) stop ': Allocation failure for P(:,:) (SETUP_PARAMS).'

    ! Computing params for initial positions
    x_max = xsize/2
    x_min = -x_max

    ysize = (xsize*screen_height)/screen_width
    y_max = ysize/2
    y_min = -y_max

    density = nstars/(xsize*ysize)

    ! Initial positions in the square [x_min,x_max] X [y_min,y_max]
    istep = 0
    do i = 1, nstars
       call random_number(f)
       associate (q => p(:,i))
         q(1) = x_min+xsize*f(1)
         q(2) = y_min+ysize*f(2)
       end associate
    end do

    ! Computing params for WC to DC mapping
    x_max = view_xsize/2
    x_min = -x_max

    view_ysize = (view_xsize*screen_height)/screen_width
    y_max = view_ysize/2
    y_min = -y_max

  end subroutine setup_params

  subroutine shutdown_params()
    if (allocated(p)) deallocate(p,STAT=ierr)
    if (ierr /= 0) stop ': Deallocation failure for P(:,:) (SHUTDOWN_PARAMS).'
  end subroutine shutdown_params

  subroutine show_params()
    write(*,*) 'Current parameters:'
    write(*,*)
    write(*,*) 'NSTARS = ', nstars
    write(*,*) 'NSOUT  = ', nsout
    write(*,*)
    write(*,*) 'STEP = ', step
    write(*,*)
    write(*,*) 'XSIZE = ', xsize
    write(*,*) 'YSIZE = ', ysize
    write(*,*)
    write(*,*) 'VIEW_XSIZE = ', view_xsize
    write(*,*) 'VIEW_YSIZE = ', view_ysize
    write(*,*)
    write(*,*) 'DENSITY = ', density
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
       ! Draw stars at current time (ISTEP)
       call display_stars()

       if (mod(istep,nsout) == 0) &
            write(*,FMT) 'CURRENT STEP            : ', istep

       istep = istep+1
       call update_stars()
    end do

    ! Draw stars at the last step executed
    call display_stars()

    call print_results()

  contains

    subroutine display_stars()
      call clear_screen()
      call set_color(YELLOW)

      do i = 1, nstars
         associate (q => p(:,i))
           call draw_point(q(1),q(2))
         end associate
      end do
      call refresh()
    end subroutine display_stars

    ! Total "force" of the remaining NSTARS-1 satrs on star at position Q(:)
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
      do i = 1, nstars
         d = p(:,i)-q         ! p(:,i)-q = -(q-p(:,i)): this avoids f = -f
         r = norm2(d)

         if (r == 0) cycle    ! i /= P ...

         d = d/r**3
         f = f+d
      end do
    end subroutine field

    subroutine update_stars()
      do i = 1, nstars
         associate (q => p(:,i))
           call field(q,f)

           ! The diameter of the final cluster is determined by the
           ! step size. Try:
           !
           !   step = 2, 3, ..., 0.1, 0.5, ...
           !
           !f = step*(f/norm2(f)) ! TWO divisions and TWO multiplications
           f = (step/norm2(f))*f  ! ONE divisions and TWO multiplications

           q = q+f
         end associate
      end do
    end subroutine update_stars

    subroutine print_results()
      associate (nsteps => istep)
        ! The rms displacement is sqrt(nsteps)
        rms_d = nsteps
        rms_d = sqrt(rms_d)

        write(*,*)
        write(*,FMT) 'Executed with NSTEPS    : ', nsteps
        write(*,FMT) 'Executed with RMS_D     : ', rms_d
      end associate

      ! Computing mean and sigma for final cluster(s)
      ! See: https://www.programming-idioms.org/idiom/203/calculate-mean-and-standard-deviation/3468/fortran
      !
      associate (x => p(1,:), y => p(2,:))
        mean_x = sum(x)/nstars
        mean_y = sum(y)/nstars

        sigma_x = sqrt(sum(x**2 )/nstars-mean_x**2 )
        sigma_y = sqrt(sum(y**2 )/nstars-mean_y**2 )
      end associate

      write(*,*)
      write(*,FMT) 'FINAL CLUSTER(S) MEAN-X : ', mean_x
      write(*,FMT) 'FINAL CLUSTER(S) MEAN-Y : ', mean_y
      write(*,*)
      write(*,FMT) 'FINAL CLUSTER(S) SIGM-X : ', sigma_x
      write(*,FMT) 'FINAL CLUSTER(S) SIGM-Y : ', sigma_y

      associate (r => norm2(p,1), mean_r => mean_x, sigma_r => sigma_x)
        mean_r = sum(r)/nstars

        sigma_r = sqrt(sum(r**2 )/nstars-mean_r**2 )

        write(*,*)
        write(*,FMT) 'FINAL CLUSTER(S) MEAN-R : ', mean_r
        write(*,FMT) 'FINAL CLUSTER(S) SIGM-R : ', sigma_r
        write(*,*)
      end associate
    end subroutine print_results
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

    if (ievent /= QUIT_EVENT) then

       ! We need to reset IEVENT if we want to restart the run
       ievent = -1000
       do while (ievent /= QUIT_EVENT)
          call paint_screen()

          ievent = get_event()
       end do

    end if

    call close_graphics()
    call shutdown_params()
  end subroutine run

  ! ================
  !    M  E  N  U
  ! ================

  subroutine set_nstars()
    ival= nstars
    call get('NSTARS =',ival)
    if (ival > 0) then
       nstars = ival
    else
       write(*,*) 'NSTARS <= 0! UNCHANGED...'
    end if
  end subroutine set_nstars

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
    rval= step
    call get('STEP =',rval)
    if (rval > 0) then
       step = rval
    else
       write(*,*) 'STEP <= 0! UNCHANGED...'
    end if
  end subroutine set_step

  subroutine set_xsize()
    rval= xsize
    call get('XSIZE =',rval)
    if (rval > 0) then
       xsize = rval
    else
       write(*,*) 'XSIZE <= 0! UNCHANGED...'
    end if
  end subroutine set_xsize

  subroutine set_view_xsize()
    rval= view_xsize
    call get('VIEW_XSIZE =',rval)
    if (rval > 0) then
       view_xsize = rval
    else
       write(*,*) 'VIEW_XSIZE <= 0! UNCHANGED...'
    end if
  end subroutine set_view_xsize

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
    write(*,*) '  N : Number of Stars'
    write(*,*) '  O : Output Rate'
    write(*,*)
    write(*,*) '  P : Step Length'
    write(*,*)
    write(*,*) '  X : Region X-Size'
    write(*,*) '  W : View X-Size'
    write(*,*)
    write(*,*) '  S : Screen Size'
    write(*,*)
    write(*,*) '  R : RUN'
    write(*,*) '  Q : QUIT'
  end subroutine show_menu

  subroutine process_menu(key)
    character, intent(in) :: key

    select case (key)
    case ('N')
       call set_nstars()
    case ('O')
       call set_nsout()
    case ('P')
       call set_step()
    case ('X')
       call set_xsize()
    case ('W')
       call set_view_xsize()
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
end module star_walk_2d_lib

program star_walk_2d
  use :: star_walk_2d_lib

  implicit none

  ! Initialize the rnd generator (maybe we do not need this anymore in
  ! recent version of GFortran)
  call random_init(.false., .false.)

  call app_menu()
end program star_walk_2d
