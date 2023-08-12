!
! Author: Angelo Graziosi
!
!   created   : Aug 08, 2023
!   last edit : Aug 13, 2023
!
! Just a joke. It plots an ellipse for wich F0, F1 and F2 are ALWAYS
! the center and two vector in the directions of coniugate diameters.
!
! Building on MSYS2/MINGW64:
!
! rm -rf {*.mod,../../modules/*}; \
!   gfortran -march=native -Wall -std=f2018 -fmax-errors=1 -O3 \
!   -J ../../modules \
!   ../../basic-modules/{{kind,math}_consts,getdata,nicelabels}.f90 \
!   ../../sdl2-fortran/src/{c_util,sdl2/{sdl2_stdinc,sdl2_audio,\
!     sdl2_blendmode,sdl2_cpuinfo,sdl2_gamecontroller,sdl2_error,\
!     sdl2_events,sdl2_filesystem,sdl2_hints,sdl2_joystick,sdl2_keyboard,\
!     sdl2_log,sdl2_messagebox,sdl2_rect,sdl2_pixels,sdl2_platform,\
!     sdl2_scancode,sdl2_surface,sdl2_render,sdl2_keycode,sdl2_mouse,\
!     sdl2_rwops,sdl2_thread,sdl2_timer,sdl2_version,sdl2_video,\
!     sdl2_opengl},sdl2}.f90 ../SDL2_app.f90 \
!   plot_ellipse.f90 -o plot_ellipse-static.out -static \
!   -lmingw32 -lSDL2main -lSDL2 -lws2_32 -ldinput8 -ldxguid -ldxerr8 \
!   -luser32 -lgdi32 -lwinmm -limm32 -lole32 -loleaut32 -lshell32 \
!   -lversion -luuid -lcomdlg32 -lhid -lsetupapi;  \
! rm -rf {*.mod,../modules/*}
!
! Building on WSL:
!
! rm -rf {*.mod,../../modules/*}; \
!   gfortran -march=native -Wall -std=f2018 -fmax-errors=1 -O3 \
!   -J ../../modules \
!   ../../basic-modules/{{kind,math}_consts,getdata,nicelabels}.f90 \
!   ../../sdl2-fortran/src/{c_util,sdl2/{sdl2_stdinc,sdl2_audio,\
!     sdl2_blendmode,sdl2_cpuinfo,sdl2_gamecontroller,sdl2_error,\
!     sdl2_events,sdl2_filesystem,sdl2_hints,sdl2_joystick,sdl2_keyboard,\
!     sdl2_log,sdl2_messagebox,sdl2_rect,sdl2_pixels,sdl2_platform,\
!     sdl2_scancode,sdl2_surface,sdl2_render,sdl2_keycode,sdl2_mouse,\
!     sdl2_rwops,sdl2_thread,sdl2_timer,sdl2_version,sdl2_video,\
!     sdl2_opengl},sdl2}.f90 ../SDL2_app.f90 \
!   plot_ellipse.f90 -o plot_ellipse-wsl.out -lSDL2; \
! rm -rf {*.mod,../modules/*}
!
! Try as independent vectors: A(1,3), B(-1,1) and so on ...
!
! REFERENCES
!
!   1. https://en.wikipedia.org/wiki/Ellipse
!
!   2. https://math.stackexchange.com/questions/3127984/whats-the-parametric-equation-of-a-partial-ellipse-in-3d-space-with-given-major
!
!   3. https://math.stackexchange.com/questions/3994666/parametric-equation-of-an-ellipse-in-the-3d-space
!
!   4. https://math.stackexchange.com/questions/339126/how-to-draw-an-ellipse-if-a-center-and-3-arbitrary-points-on-it-are-given
!

module app_lib
  use :: kind_consts, only: WP
  use :: math_consts, only: ZERO => Z0
  use :: getdata, only: get

  implicit none
  private

  abstract interface
     function ellipse_fcn(v0,v1,v2,t) result(r)
       import :: WP
       real(WP), intent(in) :: v0(:), v1(size(v0)), v2(size(v0)), t
       real(WP) :: r(size(v0))
     end function ellipse_fcn
  end interface

  integer, parameter :: SCREEN_WIDTH = 900, SCREEN_HEIGHT = 900

  ! Data
  integer :: nstep = 1000
  real(wp) :: ttime = 10, f1(2) = [ 2, 2 ], f2(2) = [ -1, 1 ], &
       f0(2) = [ 1.5, 1.5 ], xsize = 12
  logical :: axes_func = .true.

  ! Auxiliary data
  integer :: ival
  real(WP) :: tstep = 0, x_min = 0, x_max = 0, y_min = 0, y_max = 0, &
       rval, radius

  ! Pointers
  procedure(ellipse_fcn), pointer :: func => null()

  public :: app_menu

contains

  ! Given F1 and F2, two independent vectors, P1 = F0+F2 and P2 = F0+F1 are
  ! two points of the ellipse. Indeed would be
  !
  !   F1 = P2-F0
  !   F2 = P1-F0
  !
  ! and we would have fpoints() function being started with faxes()
  ! function.
  !
  function faxes(f0,f1,f2,t) result(r)
    real(WP), intent(in) :: f0(:), f1(size(f0)), f2(size(f0)), t
    real(WP) :: r(size(f0))

    r = f0+cos(t)*f1+sin(t)*f2
  end function faxes

  ! Here A and B are 2 points on the ellipse, C being the center (see
  ! ref. n. 4, node #1520207)
  function fpoints(c,a,b,t) result(r)
    real(WP), intent(in) :: c(:), a(size(c)), b(size(c)), t
    real(WP) :: r(size(c))

    r = c+cos(t)*(a-c)+sin(t)*(b-c)
  end function fpoints

  subroutine setup_params()
    tstep = ttime/nstep

    x_max = xsize/2
    x_min = -x_max
    y_min = x_min
    y_max = x_max

    ! 5 / 1000
    radius = 0.005_WP*xsize

    ! The pointers to the ellipse function
    if (axes_func) then
       func => faxes
       write(*,*) 'Ellipse contains the points:'
       write(*,*) 'A = ', f0+f1
       write(*,*) 'B = ', f0+f1
    else
       func => fpoints
       write(*,*) 'Independent vectors generating the ellipse:'
       write(*,*) 'F1 = ', f2-f0
       write(*,*) 'F2 = ', f1-f0
       write(*,*) 'they could be semiaxes'
    end if
  end subroutine setup_params

  subroutine set_ttime()
    rval= ttime
    call get('TTIME (s) =',rval)
    if (rval > 0) then
       ttime = rval
    else
       write(*,*) 'TTIME <= 0! UNCHANGED...'
    end if
  end subroutine set_ttime

  subroutine set_nstep()
    ival= nstep
    call get('NSTEP =',ival)
    if (ival > 0) then
       nstep = ival
    else
       write(*,*) 'NSTEP <= 0! UNCHANGED...'
    end if
  end subroutine set_nstep

  subroutine set_f0()
    call get('F0(1) =',f0(1))
    call get('F0(2) =',f0(2))
  end subroutine set_f0

  subroutine set_f1()
    call get('F1(1) =',f1(1))
    call get('F1(2) =',f1(2))
  end subroutine set_f1

  subroutine set_f2()
    call get('F2(1) =',f2(1))
    call get('F2(2) =',f2(2))
  end subroutine set_f2

  subroutine set_xsize()
    rval= xsize
    call get('XSIZE (m) =',rval)
    if (rval > 0) then
       xsize = rval
    else
       write(*,*) 'TTIME <= 0! UNCHANGED...'
    end if
  end subroutine set_xsize

  subroutine set_func()
    if (axes_func) then
       write(*,*) 'Setting points based function ...'
    else
       write(*,*) 'Setting semiaxes based function ...'
    end if
    axes_func = .not. axes_func
  end subroutine set_func

  subroutine run()
    use SDL2_app, only: init_graphics, close_graphics, QUIT_EVENT, &
         get_event, set_rgba_color, draw_point, draw_circle, draw_ellipse, &
         quit, refresh, clear_screen, fill_circle

    integer, parameter :: RED = int(z'FF0000FF')
    integer, parameter :: GREEN = int(z'FF00FF00')
    integer, parameter :: BLUE = int(z'FFFF0000')
    integer, parameter :: YELLOW = ior(RED,GREEN)
    integer, parameter :: MAGENTA = ior(RED,BLUE)
    integer, parameter :: CYAN = ior(GREEN,BLUE)
    integer, parameter :: WHITE = ior(YELLOW,BLUE)

    integer :: ievent = -1000

    ! We need to reset IEVENT if we want to restart the run
    ievent = -1000

    ! First the PARAMS ...
    call setup_params()

    call init_graphics('Bouncing Balls...', &
         WIDTH=screen_width,HEIGHT=screen_height, &
         X1=x_min,X2=x_max,Y1=y_min,Y2=y_max)

    call set_color(RED)
    call draw_ellipse(SCREEN_WIDTH/2,SCREEN_HEIGHT/2,250,100)
    call set_color(GREEN)
    call draw_ellipse(SCREEN_WIDTH/2,SCREEN_HEIGHT/2,100,250)
    call set_color(BLUE)
    call draw_circle(SCREEN_WIDTH/2,SCREEN_HEIGHT/2,250)
    call set_color(WHITE)
    call draw_circle(SCREEN_WIDTH/2,SCREEN_HEIGHT/2,100)
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

    ! Clean the pointer, if needed
    if (associated(func)) nullify(func)

  contains

    subroutine set_color(c)
      integer, intent(in) :: c

      call set_rgba_color(ibits(c,0,8),ibits(c,8,8),ibits(c,16,8))
    end subroutine set_color

    subroutine paint_screen()
      integer :: i
      real(WP) :: t, p(2)

      call clear_screen()
      call set_color(YELLOW)

      i = 0
      do while ((i < nstep) .and. (.not. quit()))
         t = i*tstep
         p = func(f0,f1,f2,t)
         call draw_point(p(1),p(2))
         call refresh()

         i = i+1
      end do

      call set_color(WHITE)
      call fill_circle(ZERO,ZERO,radius)

      call set_color(RED)
      call fill_circle(f0(1),f0(2),radius)

      call set_color(GREEN)
      call fill_circle(f1(1),f1(2),radius)

      call set_color(BLUE)
      call fill_circle(f2(1),f2(2),radius)

      call set_color(CYAN)

      ! P(0)
      p = f0+f1
      call fill_circle(p(1),p(2),radius)

      ! P(PI)
      p = f0-f1
      call fill_circle(p(1),p(2),radius)

      call set_color(MAGENTA)

      ! P(PI/2)
      p = f0+f2
      call fill_circle(p(1),p(2),radius)

      ! P(-PI/)
      p = f0-f2
      call fill_circle(p(1),p(2),radius)
      call refresh()

    end subroutine paint_screen
  end subroutine run

  subroutine app_menu()
    character :: key = 'R'
    integer :: ikey = ichar('R')

    do while (ikey /= ichar('Q'))

       ! SHOW Menu
       write(*,*) 'Choose item:'
       write(*,*) '  T : TTime'
       write(*,*) '  N : NSTEP'
       write(*,*) '  0 : F0'
       write(*,*) '  1 : F1'
       write(*,*) '  2 : F2'
       write(*,*) '  X : XSIZE'
       if (axes_func) then
          write(*,*) '  F : POINTS FUNC'
       else
          write(*,*) '  F : AXES FUNC'
       end if

       write(*,*) '  R : RUN'
       write(*,*) '  Q : QUIT'

       call get('Choice :',key)

       ! Convert in upcase if not
       if ('a' <= key .and. key <= 'z') then
          ikey = ichar(key)-32
       else
          ikey = ichar(key)
       end if

       write(*,*)

       select case (ikey)
       case (ichar('T'))
          call set_ttime()
       case (ichar('N'))
          call set_nstep()
       case (ichar('0'))
          call set_f0()
       case (ichar('1'))
          call set_f1()
       case (ichar('2'))
          call set_f2()
       case (ichar('X'))
          call set_xsize()
       case (ichar('F'))
          call set_func()
       case (ichar('R'))
          call run()
       end select

       write(*,*)

    end do
  end subroutine app_menu
end module app_lib

program plot_ellipse
  use :: app_lib

  implicit none

  call app_menu()

end program plot_ellipse
