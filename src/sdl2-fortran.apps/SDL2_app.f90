!
! Author: ANGELO GRAZIOSI
!
!   created   : Sep 08, 2018
!   last edit : Aug 24, 2023
!
!   Module to create SDL2 Fortran applications in DC or WC
!

module SDL2_app
  use, intrinsic :: iso_c_binding, only: C_NULL_CHAR, C_PTR, &
       c_associated
  use :: kind_consts, only: WP
  use :: math_consts, only: ZERO => Z0, ONE => Z1
  use :: sdl2, only: SDL_INIT_VIDEO, SDL_RENDERER_SOFTWARE, &
       SDL_RENDERER_ACCELERATED, SDL_RENDERER_TARGETTEXTURE, &
       SDL_WINDOW_SHOWN, SDL_WINDOWPOS_UNDEFINED, &
       SDL_QUITEVENT, SDL_KEYDOWN, SDLK_ESCAPE, SDLK_q, &
       SDL_KEYDOWN, SDL_MOUSEBUTTONDOWN, SDL_MOUSEWHEEL, &
       SDL_WINDOWEVENT, SDL_WINDOWEVENT_CLOSE, SDL_WINDOWEVENT_EXPOSED, &
       SDL_WINDOWEVENT_SHOWN, SDL_ALPHA_OPAQUE, &
       sdl_event, sdl_point, sdl_rect, &
       sdl_create_renderer, sdl_create_window, &
       sdl_destroy_renderer, sdl_destroy_window, &
       sdl_get_error, sdl_get_mouse_state, &
       sdl_init, sdl_poll_event, sdl_quit, &
       sdl_render_clear, sdl_render_draw_line, sdl_render_draw_lines, &
       sdl_render_draw_point, sdl_render_draw_points, sdl_render_draw_rect, &
       sdl_render_draw_rects, sdl_render_fill_rect, sdl_render_fill_rects, &
       sdl_render_get_viewport, sdl_render_present, &
       sdl_render_set_viewport, sdl_set_render_draw_color, &
       uint8

  implicit none
  private

  !
  ! Data for WS <==> DC map transformations
  !

  integer, parameter :: MAX_MAPS = 10
  integer, parameter :: WIDTH0 = 600, HEIGHT0 = 600

  ! WIDTH and HEIGHT refer to default map (0)
  integer :: screen_width = WIDTH0, screen_height = HEIGHT0
  !integer ::  clipping = 1
  integer :: current_map = 0
  logical :: graphics_on = .false.

  ! The viewports in SDL2 notations
  type(sdl_rect), dimension(0:MAX_MAPS) :: vp

  ! (x,y) are in WC, (u,v) are in DC. Remember that, usually, DC Y axis
  ! increases toward the bottom (v_min > v_max)...
  !
  real(WP), dimension(0:MAX_MAPS) :: x_min = ZERO, x_max = ONE, &
       y_min = ZERO, y_max = ONE, u_min = ZERO, u_max = WIDTH0-1, &
       v_min = HEIGHT0-1, v_max = ZERO

  real(WP), target :: maps(4,0:MAX_MAPS)
  real(WP), pointer :: m(:) => null()

  ! -----------------------------------

  ! Define our POINT in WC
  type, public :: sdlapp_point
     real(WP) :: x
     real(WP) :: y
  end type sdlapp_point

  integer, parameter, public :: QUIT_EVENT = -1
  integer, parameter, public :: WHEELUP_EVENT = -2
  integer, parameter, public :: WHEELDOWN_EVENT = -3

  type(C_PTR) :: window
  type(C_PTR) :: renderer
  integer :: rc, mouse_x = -1, mouse_y = -1

  interface draw_arc
     module procedure draw_arc_DC, draw_arc_WC
  end interface draw_arc

  interface draw_circle
     module procedure draw_circle_DC, draw_circle_WC
  end interface draw_circle

  interface draw_elliptic_arc
     module procedure draw_elliptic_arc_DC, draw_elliptic_arc_WC
  end interface draw_elliptic_arc

  interface draw_ellipse
     module procedure draw_ellipse_DC, draw_ellipse_WC, &
          draw_ellipse_DC_1, draw_ellipse_WC_1
  end interface draw_ellipse

  interface fill_circle
     module procedure fill_circle_DC, fill_circle_WC
  end interface fill_circle

  interface fill_ellipse
     module procedure fill_ellipse_DC, fill_ellipse_WC, &
          fill_ellipse_DC_1, fill_ellipse_WC_1
  end interface fill_ellipse

  interface draw_line
     module procedure draw_line_DC, draw_line_WC
  end interface draw_line

  interface draw_lines
     module procedure draw_lines_DC, draw_lines_WC_1, draw_lines_WC_2
  end interface draw_lines

  interface draw_point
     module procedure draw_point_DC, draw_point_WC
  end interface draw_point

  interface draw_points
     module procedure draw_points_DC, draw_points_WC
  end interface draw_points

  interface draw_rect
     module procedure draw_rect_DC, draw_rect_WC
  end interface draw_rect

  ! For map transformations
  public :: select_map, set_map_viewport, set_map_window

  public :: clear_screen, clear_viewport, close_graphics, draw_arc, &
       draw_circle, draw_elliptic_arc, draw_ellipse, draw_line, draw_lines, &
       draw_point, draw_points, draw_rect, draw_rects, &
       fill_circle, fill_ellipse, fill_rect, fill_rects, &
       get_event, get_mouse_x, get_mouse_y, &
       init_graphics, quit, refresh, set_rgba_color, set_viewport, &
       get_viewport, draw_axis_x, draw_axis_y, draw_axes, set_color

contains

  !
  ! Routines for WS <==> DC map transformations
  !

  subroutine setup_map(id_map)
    integer, intent(in) :: id_map

    real(WP) :: a, b, c, d

    ! A = IMAX/(X2-X1), IMAX = U_MAX-U_MIN
    a = (u_max(id_map)-u_min(id_map))/(x_max(id_map)-x_min(id_map))
    b = -a*x_min(id_map)

    ! C = -JMAX/(X2-X1), JMAX = V_MIN-V_MAX => C = (V_MAX-V_MIN)/(X2-X1)
    c = (v_max(id_map)-v_min(id_map))/(y_max(id_map)-y_min(id_map))
    d = -c*y_max(id_map)

    ! Now set the viewport in SDL2 notations:
    associate (vpl => vp(id_map)%x, vpt => vp(id_map)%y, &
         vpr => vp(id_map)%w, vpb => vp(id_map)%h, &
         ul => u_min(id_map), ur => u_max(id_map), &
         vt => v_max(id_map), vb => v_min(id_map))

      ! the upper left corner...
      vpl = nint(ul)
      vpt = nint(vt)

      ! ...the bottom right corner...
      vpr = nint(ur)
      vpb = nint(vb)

      ! ...the dimensions
      vp(id_map)%w = vpr-vpl+1
      vp(id_map)%h = vpb-vpt+1
    end associate

    ! The map
    maps(:,id_map) = [ a, b, c, d]
  end subroutine setup_map

  ! CANNOT BE CALLED before Graphics starts !!!  As you can see,
  ! INIT_GRAPHICS() initialize the defaul map (0) and the others to
  ! default
  subroutine set_map_window(id_map,x1,x2,y1,y2)
    integer, intent(in) :: id_map
    real(WP), intent(in) :: x1, x2, y1, y2

    ! Cannot set map n. 0, the default...
    if ((id_map < 1) .or. (MAX_MAPS < id_map)) then
       if (graphics_on) call close_graphics()
       write(*,*) 'ID_MAP = ', id_map
       write(*,*) 'ERROR : ID_MAP out of range parameter (SET_MAP_WINDOW).'
       stop
    end if

    x_min(id_map) = x1
    x_max(id_map) = x2
    y_min(id_map) = y1
    y_max(id_map) = y2

    call setup_map(id_map)
  end subroutine set_map_window

  ! CANNOT BE CALLED before Graphics starts !!!
  ! INIT_GRAPHICS() initialize the defaul map (0) and the others to
  ! default
  subroutine set_map_viewport(id_map,u1,u2,v1,v2)
    integer, intent(in) :: id_map
    real(WP), intent(in) :: u1, u2, v1, v2

    ! Cannot set map n. 0, the default...
    if ((id_map < 1) .or. (MAX_MAPS < id_map)) then
       if (graphics_on) call close_graphics()
       write(*,*) 'ID_MAP = ', id_map
       write(*,*) 'ERROR : ID_MAP out of range parameter (SET_MAP_VIEWPORT).'
       stop
    end if

    ! MUST BE u1 < u2
    u_min(id_map) = u1
    u_max(id_map) = u2

    !
    ! V1 and V2 are referred to a V axis pointing UP, V_MIN and V_MAX to a
    ! V' axis pointing DOWN. V_MIN is at the same level of V1 and V_MAX at
    ! the same level of V2. V' has the origin, 0, where V has (HEIGHT-1).
    ! V has the origin, 0, where V' has (HEIGHT-1). The transformation is:
    !
    !   V' = (HEIGHT-1) - V   (For V = (HEIGHT-1), V' = 0 and viceversa)
    !
    ! Now, V_MIN(0) stores (HEIGHT-1)...
    !

    ! MUST BE v1 < v2
    v_min(id_map) = v_min(0)-v1
    v_max(id_map) = v_min(0)-v2

    call setup_map(id_map)
  end subroutine set_map_viewport

  subroutine select_map(id_map)
    integer, intent(in) :: id_map

    if ((id_map < 0) .or. (MAX_MAPS < id_map)) then
       if (graphics_on) call close_graphics()
       write(*,*) 'ID_MAP = ', id_map
       write(*,*) 'ERROR : ID_MAP out of range parameter (SELECT_MAP).'
       stop
    end if

    current_map = id_map

    call set_viewport(vp(id_map))

    m => maps(:,id_map)
  end subroutine select_map

  ! ! WC to DC transformations... and viceversa
  ! ! XS = A*X+B == m(1)*X+m(2)
  ! function xs(x) result(r)
  !   real(WP), intent(in) :: x
  !   integer :: r

  !   r = nint(m(1)*x+m(2))
  ! end function xs

  ! !   YS = C*Y+D == m(3)*Y+m(4)
  ! function ys(y) result(r)
  !   real(WP), intent(in) :: y
  !   integer :: r

  !   r = nint(m(3)*y+m(4))
  ! end function ys

  ! ! Inverse of XS()
  ! function sx(ix) result(r)
  !   integer, intent(in) :: ix
  !   real(WP) :: r

  !   r = (ix-m(2))/m(1)
  ! end function sx

  ! ! Inverse of YS()
  ! function sy(iy) result(r)
  !   integer, intent(in) :: iy
  !   real(WP) :: r

  !   r = (iy-m(4))/m(3)
  ! end function sy

  ! -----------------------------------

  subroutine init_graphics(title,xpos,ypos,width,height,x1,x2,y1,y2,flags)
    character(len=*), intent(in) :: title
    integer, intent(in), optional :: xpos, ypos, width, height, flags
    real(WP), intent(in), optional :: x1, x2, y1, y2

    integer :: x, y, w, h, f

    ! Initialize SDL.
    rc = sdl_init(SDL_INIT_VIDEO)

    if (rc < 0) then
       write(*,*) 'SDL Error (SDL_INIT_VIDEO): ', sdl_get_error()
       stop
    end if

    if (present(xpos)) then
       x = xpos
    else
       x = SDL_WINDOWPOS_UNDEFINED
    end if

    if (present(ypos)) then
       y = ypos
    else
       y = SDL_WINDOWPOS_UNDEFINED
    end if

    if (present(width)) then
       w = width
    else
       w = screen_width
    end if

    if (present(height)) then
       h = height
    else
       h = screen_height
    end if

    ! Vieport
    u_min(0) = ZERO
    u_max(0) = (w-1)

    v_min(0) = (h-1)
    v_max(0) = ZERO

    ! Window
    x_min(0) = u_min(0)
    x_max(0) = u_max(0)

    y_min(0) = v_max(0)
    y_max(0) = v_min(0)

    if (present(x1)) x_min(0) = x1
    if (present(x2)) x_max(0) = x2
    if (present(y1)) y_min(0) = y1
    if (present(y2)) y_max(0) = y2

    ! Initialization to default all views
    ! Notice, F is in the right range and...
    do f = 0, MAX_MAPS
       call setup_map(f)
    end do

    ! ...reusable
    if (present(flags)) then
       f = flags
    else
       f = SDL_WINDOW_SHOWN
    end if

    ! Create the SDL window.
    window = sdl_create_window(title//C_NULL_CHAR,x,y,w,h,f)

    if (.not. c_associated(window)) then
       write(*,*) 'SDL Error (WINDOW): ', sdl_get_error()
       stop
    end if

    ! Create renderer. See https://wiki.libsdl.org/SDL2/SDL_RendererFlags
    !renderer = sdl_create_renderer(window,-1,0)
    !

    ! On GNU/Linux Mint the apps "flicker" as if things outside the
    ! "do while (ievent /= QUIT_EVENT)" loop were esecuted in parallel
    ! to the things onside the loop.
    !
    ! renderer = sdl_create_renderer(window,-1, &
    !      ior(SDL_RENDERER_ACCELERATED, SDL_RENDERER_TARGETTEXTURE))
    !

    renderer = sdl_create_renderer(window,-1,SDL_RENDERER_SOFTWARE)

    if (.not. c_associated(renderer)) then
       write(*,*) 'SDL Error (RENDERER): ', sdl_get_error()
       stop
    end if

    graphics_on = .true.
    call select_map(0)
  end subroutine init_graphics

  subroutine close_graphics()
    !call select_map(0)
    ! some graphics message...
    ! wait... and close graphics:

    ! Quit gracefully.
    call sdl_destroy_renderer(renderer)
    call sdl_destroy_window(window)
    call sdl_quit()

    graphics_on = .false.
    print *, 'ALL DONE'
  end subroutine close_graphics

  subroutine set_rgba_color(red,green,blue,alpha)
    integer, intent(in) :: red, green, blue
    integer, intent(in), optional :: alpha

    if (present(alpha)) then
       rc = sdl_set_render_draw_color(renderer,uint8(red),uint8(green), &
            uint8(blue),uint8(alpha))
    else
       ! SDL_ALPHA_OPAQUE should be 255
       rc = sdl_set_render_draw_color(renderer,uint8(red),uint8(green), &
            uint8(blue),uint8(SDL_ALPHA_OPAQUE))
    end if
  end subroutine set_rgba_color

  subroutine set_color(c)
    integer, intent(in) :: c

    call set_rgba_color(ibits(c,0,8),ibits(c,8,8),ibits(c,16,8),ibits(c,24,8))
  end subroutine set_color

  subroutine get_viewport(rect)
    type(sdl_rect), intent(inout) :: rect

    call sdl_render_get_viewport(renderer,rect)
  end subroutine get_viewport

  subroutine set_viewport(rect)
    type(sdl_rect), intent(in) :: rect

    rc = sdl_render_set_viewport(renderer,rect)
  end subroutine set_viewport

  subroutine clear_screen()
    !
    ! DO NOT use the HIGH LEVEL routine
    !
    !   call set_rgba_color(0,0,0)   ! BLACK
    !
    ! Just use the LOW LEVEL routine:
    !
    rc = sdl_set_render_draw_color(renderer,uint8(0),uint8(0),uint8(0), &
         uint8(255))
    rc = sdl_render_clear(renderer)
  end subroutine clear_screen

  subroutine clear_viewport()

    type(sdl_rect) :: rect

    call sdl_render_get_viewport(renderer,rect)

    rect%x = 0
    rect%y = 0

    rc = sdl_set_render_draw_color(renderer,uint8(0),uint8(0),uint8(0), &
         uint8(255))
    rc = sdl_render_fill_rect(renderer,rect)
  end subroutine clear_viewport

  subroutine refresh()
    call sdl_render_present(renderer)
  end subroutine refresh

  function quit() result(r)
    logical :: r

    type(sdl_event) :: event

    r = .false.

    if (sdl_poll_event(event) /= 0) then

       select case (event%type)
       case (SDL_QUITEVENT)
          r = .true.
       case (SDL_KEYDOWN)
          !write(*,'(A,i0)') 'KEY = ', event%key%key_sym%sym
          select case (event%key%key_sym%sym)
          case (SDLK_ESCAPE,SDLK_q)
             r = .true.
          case default
             r = .false.
          end select
          !if (event%key%key_sym%sym == SDLK_ESCAPE) r = .true.
          !if (event%key%key_sym%sym == SDLK_q) r = .true.
       end select
    end if
  end function quit

  function get_event() result(r)
    integer :: r

    type(sdl_event) :: event

    do while (.true.)
       do while (sdl_poll_event(event) /= 0)
          select case (event%type)
          case (SDL_WINDOWEVENT)
             select case (event%window%event)
             case (SDL_WINDOWEVENT_SHOWN, SDL_WINDOWEVENT_EXPOSED)
                call sdl_render_present(renderer)

             case (SDL_WINDOWEVENT_CLOSE)
                r = QUIT_EVENT
                return

             end select

          case (SDL_QUITEVENT)
             r = QUIT_EVENT
             return

          case (SDL_KEYDOWN)
             mouse_x = -1
             mouse_y = -1
             r = event%key%key_sym%sym
             if ((r == SDLK_ESCAPE) .or. (r == SDLK_q)) r = QUIT_EVENT
             return

          case (SDL_MOUSEBUTTONDOWN)
             mouse_x = event%button%x
             mouse_y = event%button%y
             r = event%button%button
             return

          case (SDL_MOUSEWHEEL)
             r = sdl_get_mouse_state(mouse_x,mouse_y)
             if (event%wheel%y > 0) then
                r = WHEELUP_EVENT
             else if (event%wheel%y < 0) then
                r= WHEELDOWN_EVENT
             else
                r = 0
             end if
             return

          end select
       end do
    end do
  end function get_event

  function get_mouse_x() result (r)
    integer r

    r = mouse_x
  end function get_mouse_x

  function get_mouse_y() result (r)
    integer r

    r = mouse_y
  end function get_mouse_y

  !
  ! PRIMITIVES
  !

  !
  ! BRESENHAM MIDPOINT CIRCLE ALGORITHM
  !
  ! Adapting the code from:
  !
  !   http://rosettacode.org/wiki/Bitmap/Midpoint_circle_algorithm#Fortran
  !
  ! See also: https://en.wikipedia.org/wiki/Midpoint_circle_algorithm
  !
  ! MUST BE start_angle < end_angle and both in [0,360] otherwise the
  ! result cannot be predicted...
  !
  subroutine draw_arc_DC(x0,y0,radius,start_angle,end_angle)
    ! START_ANGLE and END_ANGLE in degrees
    integer, intent(in) :: x0, y0, radius, start_angle, end_angle

    integer, parameter :: NQ = 4                   ! Number of quadrants
    real, parameter :: DEG2RAD = 1.74532925199E-2  ! Conversion factor

    real :: phi_r
    integer :: f, ddf_x, ddf_y, x, y, n, u(NQ+1), v(NQ+1)

    ! Computing the end points of the arc using X, Y as auxiliary
    ! variables
    phi_r = DEG2RAD*start_angle        ! Start point
    x = nint(x0+radius*cos(phi_r))
    y = nint(y0-radius*sin(phi_r))

    n = 1
    u(n) = x
    v(n) = y

    ! Cardinal point
    if (start_angle < 90 .and. 90 < end_angle) then
       n = n+1
       u(n) = x0
       v(n) = y0-radius
    end if

    ! Cardinal point
    if (start_angle < 180 .and. 180 < end_angle) then
       n = n+1
       u(n) = x0-radius
       v(n) = y0
    end if

    ! Cardinal point
    if (start_angle < 270 .and. 270 < end_angle) then
       n = n+1
       u(n) = x0
       v(n) = y0+radius
    end if

    phi_r = DEG2RAD*end_angle          ! End point
    x = nint(x0+radius*cos(phi_r))
    y = nint(y0-radius*sin(phi_r))

    n = n+1
    u(n) = x
    v(n) = y

    !
    ! Start the algorithm
    !

    f = 1-radius
    ddf_x = 0
    ddf_y = -2*radius
    x = 0
    y = radius

    ! The number of intervals between start-end
    n = n-1

    ! First the cardinal points...
    if (is_on_arc(x0,y0+radius)) &
         rc = sdl_render_draw_point(renderer,x0,y0+radius)

    if (is_on_arc(x0,y0-radius)) &
         rc = sdl_render_draw_point(renderer,x0,y0-radius)

    if (is_on_arc(x0+radius,y0)) &
         rc = sdl_render_draw_point(renderer,x0+radius,y0)

    if (is_on_arc(x0-radius,y0)) &
         rc = sdl_render_draw_point(renderer,x0-radius,y0)

    ! ...then the octant points; P(I) is the point in the octant I.
    ! In the DC Reference System (X-axis from L to R, Y-axis from T to
    ! B), the octant 1 covers the angle from 0 to 45 degrees, the
    ! octant 2 from 45 to 90 and so on...
    do while (x < y)
       if (f >= 0) then
          y = y-1
          ddf_y = ddf_y+2
          f = f+ddf_y
       end if
       x = x+1
       ddf_x = ddf_x+2
       f = f+ddf_x+1

       if (is_on_arc(x0+x,y0+y)) &
            rc = sdl_render_draw_point(renderer,x0+x,y0+y)  ! P(2)

       if (is_on_arc(x0-x,y0+y)) &
            rc = sdl_render_draw_point(renderer,x0-x,y0+y)  ! P(3)

       if (is_on_arc(x0+x,y0-y)) &
            rc = sdl_render_draw_point(renderer,x0+x,y0-y)  ! P(7)

       if (is_on_arc(x0-x,y0-y)) &
            rc = sdl_render_draw_point(renderer,x0-x,y0-y)  ! P(6)

       if (is_on_arc(x0+y,y0+x)) &
            rc = sdl_render_draw_point(renderer,x0+y,y0+x)  ! P(1)

       if (is_on_arc(x0-y,y0+x)) &
            rc = sdl_render_draw_point(renderer,x0-y,y0+x)  ! P(4)

       if (is_on_arc(x0+y,y0-x)) &
            rc = sdl_render_draw_point(renderer,x0+y,y0-x)  ! P(8)

       if (is_on_arc(x0-y,y0-x)) &
            rc = sdl_render_draw_point(renderer,x0-y,y0-x)  ! P(5)
    end do

  contains

    ! This function return TRUE if the point (X,Y) is in the region
    !
    !   [U(1),U(2)] x [V(1),V(2)] + ... + [U(n),U(n+1)] x [V(n),V(n+1)]
    !
    ! N is the number of intervals, i.e. the arrays U and V must be
    ! dimensioned N+1
    !
    ! IS_ON_ARC() being an internal function to DRAW_ARC(), we do not
    ! need to pass N, U(:), V(:):
    !
    !   function is_on_arc(x,y,n,u,v) result(r)
    !     integer, intent(in) :: x, y, n, u(:), v(:)
    !
    function is_on_arc(x,y) result(r)
      integer, intent(in) :: x, y

      logical :: r

      integer :: i, x1, x2, y1, y2, temp

      r = .false.

      ! N, U, V from the caller
      do i = 1, n
         x1 = u(i)
         x2 = u(i+1)

         if (x1 > x2) then
            temp = x1
            x1 = x2
            x2 = temp
         end if

         y1 = v(i)
         y2 = v(i+1)

         if (y1 > y2) then
            temp = y1
            y1 = y2
            y2 = temp
         end if

         r = r .or. ((x1 <= x .and. x <= x2) .and. (y1 <= y .and. y <= y2))
      end do
    end function is_on_arc
  end subroutine draw_arc_DC

  subroutine draw_arc_WC(x0,y0,radius,start_angle,end_angle)
    ! START_ANGLE and END_ANGLE in degrees
    real(WP), intent(in) :: x0, y0, radius, start_angle, end_angle

    call draw_elliptic_arc_DC(nint(m(1)*x0+m(2)),nint(m(3)*y0+m(4)), &
         nint(m(1)*radius),nint((-m(3))*radius), &
         nint(start_angle),nint(end_angle))
  end subroutine draw_arc_WC

  !
  ! BRESENHAM MIDPOINT CIRCLE ALGORITHM
  !
  ! Adapting the code from:
  !
  !   http://rosettacode.org/wiki/Bitmap/Midpoint_circle_algorithm#Fortran
  !
  ! See also: https://en.wikipedia.org/wiki/Midpoint_circle_algorithm
  !
  subroutine draw_circle_DC(x0,y0,radius)
    integer, intent(in) :: x0, y0, radius

    integer :: f, ddf_x, ddf_y, x, y

    f = 1-radius
    ddf_x = 0
    ddf_y = -2*radius
    x = 0
    y = radius

    ! First the cardinal points...
    rc = sdl_render_draw_point(renderer,x0,y0+radius)
    rc = sdl_render_draw_point(renderer,x0,y0-radius)
    rc = sdl_render_draw_point(renderer,x0+radius,y0)
    rc = sdl_render_draw_point(renderer,x0-radius,y0)

    ! ...then the octant points; P(I) is the point in the octant I.
    ! In the DC Reference System (X-axis from L to R, Y-axis from T to
    ! B), the octant 1 covers the angle from 0 to 45 degrees, the
    ! octant 2 from 45 to 90 and so on...
    do while (x < y)
       if (f >= 0) then
          y = y-1
          ddf_y = ddf_y+2
          f = f+ddf_y
       end if
       x = x+1
       ddf_x = ddf_x+2
       f = f+ddf_x+1
       rc = sdl_render_draw_point(renderer,x0+x,y0+y)  ! P(2)
       rc = sdl_render_draw_point(renderer,x0-x,y0+y)  ! P(3)
       rc = sdl_render_draw_point(renderer,x0+x,y0-y)  ! P(7)
       rc = sdl_render_draw_point(renderer,x0-x,y0-y)  ! P(6)
       rc = sdl_render_draw_point(renderer,x0+y,y0+x)  ! P(1)
       rc = sdl_render_draw_point(renderer,x0-y,y0+x)  ! P(4)
       rc = sdl_render_draw_point(renderer,x0+y,y0-x)  ! P(8)
       rc = sdl_render_draw_point(renderer,x0-y,y0-x)  ! P(5)
    end do
  end subroutine draw_circle_DC

  subroutine draw_circle_WC(x0,y0,radius)
    real(WP), intent(in) :: x0, y0, radius

    !
    ! radius_X__in_pixel = XS(radius)-XS(0) =
    ! m(1)*radius+m(2)-(m(1)*0+m(2)) = m(1)*radius+m(2)-m(2) =
    ! m(1)*radius
    !
    ! radius_Y__in_pixel = (-m(3)*radius)
    !
    ! BUT M(3) is NEGATIVE, given that YS is toward the bottom! So we
    ! have to use (-M(3)) (or ABS(M(3))) when converting UNITS in
    ! PIXELS otherwise we would have a negative number of pixels.
    !
    call draw_ellipse_DC(nint(m(1)*x0+m(2)),nint(m(3)*y0+m(4)), &
         nint(m(1)*radius),nint((-m(3))*radius))
  end subroutine draw_circle_WC

  ! Adapted from DRAW_ARC(): Could we could have a single overloaded
  ! DRAW_ARC?
  subroutine draw_elliptic_arc_DC(x0,y0,a,b,start_angle,end_angle)
    ! START_ANGLE and END_ANGLE in degrees
    integer, intent(in) :: x0, y0, a, b, start_angle, end_angle

    integer, parameter :: NQ = 4                   ! Number of quadrants
    real, parameter :: DEG2RAD = 1.74532925199E-2  ! Conversion factor

    integer :: n, u(NQ+1), v(NQ+1)
    integer(8) :: x, y, e2, dx, dy, err, a8, b8
    real :: phi_r

    ! Computing the end points of the arc using X, Y as auxiliary
    ! variables
    phi_r = DEG2RAD*start_angle        ! Start point
    x = nint(x0+a*cos(phi_r))
    y = nint(y0-b*sin(phi_r))

    n = 1
    u(n) = int(x)
    v(n) = int(y)

    ! Cardinal point
    if (start_angle < 90 .and. 90 < end_angle) then
       n = n+1
       u(n) = x0
       v(n) = y0-b
    end if

    ! Cardinal point
    if (start_angle < 180 .and. 180 < end_angle) then
       n = n+1
       u(n) = x0-a
       v(n) = y0
    end if

    ! Cardinal point
    if (start_angle < 270 .and. 270 < end_angle) then
       n = n+1
       u(n) = x0
       v(n) = y0+b
    end if

    phi_r = DEG2RAD*end_angle          ! End point
    x = nint(x0+a*cos(phi_r))
    y = nint(y0-b*sin(phi_r))

    n = n+1
    u(n) = int(x)
    v(n) = int(y)

    !
    ! Start the algorithm
    !

    a8 = a
    b8 = b

    ! II. quadrant from bottom left to top right
    x = -a8
    y = 0

    ! error increment
    e2 = b8
    dx = (1+2*x)*e2*e2

    ! error of 1.step
    dy = x*x
    err = dx+dy

    ! The number of intervals between start-end
    n = n-1

    ! First the cardinal points...
    if (is_on_arc(x0,y0+b)) &
         rc = sdl_render_draw_point(renderer,x0,y0+b)

    if (is_on_arc(x0,y0-b)) &
         rc = sdl_render_draw_point(renderer,x0,y0-b)

    if (is_on_arc(x0+a,y0)) &
         rc = sdl_render_draw_point(renderer,x0+a,y0)

    if (is_on_arc(x0-a,y0)) &
         rc = sdl_render_draw_point(renderer,x0-a,y0)

    do
       if (is_on_arc(int(x0-x),int(y0+y))) &
            rc = sdl_render_draw_point(renderer,int(x0-x),int(y0+y))

       if (is_on_arc(int(x0+x),int(y0+y))) &
            rc = sdl_render_draw_point(renderer,int(x0+x),int(y0+y))

       if (is_on_arc(int(x0+x),int(y0-y))) &
            rc = sdl_render_draw_point(renderer,int(x0+x),int(y0-y))

       if (is_on_arc(int(x0-x),int(y0-y))) &
            rc = sdl_render_draw_point(renderer,int(x0-x),int(y0-y))

       e2 = 2*err

       if (e2 >= dx) then
          x = x+1
          dx = dx+2*b8*b8
          err = err+dx
       end if

       if (e2 <= dy) then
          y = y+1
          dy = dy+2*a8*a8
          err = err+dy
       end if

       if (x > 0) exit
    end do

    ! The following DO should be EXACTLY equivalent to the C code:
    ! while(y++ < b).
    !
    ! Too early stop for flat ellipses with a=1, -> finish tip of
    ! ellipse
    do
       if (y < b) then
          y = y+1
          rc = sdl_render_draw_point(renderer,x0,int(y0+y))
          rc = sdl_render_draw_point(renderer,x0,int(y0-y))
       else
          y = y+1
          exit
       end if
    end do

    ! Too early stop for flat ellipses with A = 1, -> finish tip of ellipse
    do
       if (y < b) then
          y = y+1

          if (is_on_arc(x0,int(y0+y))) &
               rc = sdl_render_draw_point(renderer,x0,int(y0+y))

          if (is_on_arc(x0,int(y0-y))) &
               rc = sdl_render_draw_point(renderer,x0,int(y0-y))
       else
          y = y+1
          exit
       end if
    end do

  contains

    ! This function return TRUE if the point (X,Y) is in the region
    !
    !   [U(1),U(2)] x [V(1),V(2)] + ... + [U(n),U(n+1)] x [V(n),V(n+1)]
    !
    ! N is the number of intervals, i.e. the arrays U and V must be
    ! dimensioned N+1
    !
    ! IS_ON_ARC() being an internal function to DRAW_ARC(), we do not
    ! need to pass N, U(:), V(:):
    !
    !   function is_on_arc(x,y,n,u,v) result(r)
    !     integer, intent(in) :: x, y, n, u(:), v(:)
    !
    function is_on_arc(x,y) result(r)
      integer, intent(in) :: x, y

      logical :: r

      integer :: i, x1, x2, y1, y2, temp

      r = .false.

      ! N, U, V from the caller
      do i = 1, n
         x1 = u(i)
         x2 = u(i+1)

         if (x1 > x2) then
            temp = x1
            x1 = x2
            x2 = temp
         end if

         y1 = v(i)
         y2 = v(i+1)

         if (y1 > y2) then
            temp = y1
            y1 = y2
            y2 = temp
         end if

         r = r .or. ((x1 <= x .and. x <= x2) .and. (y1 <= y .and. y <= y2))
      end do
    end function is_on_arc

  end subroutine draw_elliptic_arc_DC

  subroutine draw_elliptic_arc_WC(x0,y0,a,b,start_angle,end_angle)
    ! START_ANGLE and END_ANGLE in degrees
    real(WP), intent(in) :: x0, y0, a, b, start_angle, end_angle

    call draw_elliptic_arc_DC(nint(m(1)*x0+m(2)),nint(m(3)*y0+m(4)), &
         nint(m(1)*a),nint((-m(3))*b),nint(start_angle),nint(end_angle))
  end subroutine draw_elliptic_arc_WC

  ! http://members.chello.at/~easyfilter/bresenham.html
  ! http://members.chello.at/~easyfilter/bresenham.c
  subroutine draw_ellipse_DC(x0,y0,a,b)
    integer, intent(in) :: x0, y0, a, b

    integer(8) :: x, y, e2, dx, dy, err, a8, b8

    a8 = a
    b8 = b

    ! II. quadrant from bottom left to top right
    x = -a8
    y = 0

    ! error increment
    e2 = b8
    dx = (1+2*x)*e2*e2

    ! error of 1.step
    dy = x*x
    err = dx+dy

    do
       rc = sdl_render_draw_point(renderer,int(x0-x),int(y0+y))
       rc = sdl_render_draw_point(renderer,int(x0+x),int(y0+y))
       rc = sdl_render_draw_point(renderer,int(x0+x),int(y0-y))
       rc = sdl_render_draw_point(renderer,int(x0-x),int(y0-y))

       e2 = 2*err

       if (e2 >= dx) then
          x = x+1
          dx = dx+2*b8*b8
          err = err+dx
       end if

       if (e2 <= dy) then
          y = y+1
          dy = dy+2*a8*a8
          err = err+dy
       end if

       if (x > 0) exit
    end do

    ! The following DO should be EXACTLY equivalent to the C code:
    ! while(y++ < b).
    !
    ! Too early stop for flat ellipses with a=1, -> finish tip of
    ! ellipse
    do
       if (y < b) then
          y = y+1
          rc = sdl_render_draw_point(renderer,x0,int(y0+y))
          rc = sdl_render_draw_point(renderer,x0,int(y0-y))
       else
          y = y+1
          exit
       end if
    end do

    ! This code, in essence, is equivalent to the C code: while(y++ < b)
    ! do while (y < b)
    !    y = y+1
    !    rc = sdl_render_draw_point(renderer,x0,int(y0+y))
    !    rc = sdl_render_draw_point(renderer,x0,int(y0-y))
    ! end do
  end subroutine draw_ellipse_DC

  subroutine draw_ellipse_WC(x0,y0,a,b)
    real(WP), intent(in) :: x0, y0, a, b

    ! m(1) is in pixel/unit, and so m(3). Indeed
    !
    ! a_in_pixel = XS(a)-XS(0) =
    ! m(1)*a+m(2)-(m(1)*0+m(2)) = m(1)*a+m(2)-m(2) =
    ! m(1)*a
    !
    ! BUT M(3) is NEGATIVE, given that YS is toward the bottom! So we
    ! have to use (-M(3)) (or ABS(M(3))) when converting UNITS in
    ! PIXELS otherwise we would have a negative number of pixels.
    !
    call draw_ellipse_DC(nint(m(1)*x0+m(2)),nint(m(3)*y0+m(4)), &
         nint(m(1)*a),nint((-m(3))*b))
  end subroutine draw_ellipse_WC

  subroutine draw_ellipse_DC_1(x0,y0,a,b,color)
    integer, intent(in) :: x0, y0, a, b, color

    call set_rgba_color(ibits(color,0,8),ibits(color,8,8),ibits(color,16,8), &
         ibits(color,24,8))
    call draw_ellipse_DC(x0,y0,a,b)
  end subroutine draw_ellipse_DC_1

  subroutine draw_ellipse_WC_1(x0,y0,a,b,color)
    real(WP), intent(in) :: x0, y0, a, b
    integer, intent(in) :: color

    ! m(1) is in pixel/unit, and so m(3). Indeed
    !
    ! a_in_pixel = XS(a)-XS(0) =
    ! m(1)*a+m(2)-(m(1)*0+m(2)) = m(1)*a+m(2)-m(2) =
    ! m(1)*a
    !
    ! BUT M(3) is NEGATIVE, given that YS is toward the bottom! So we
    ! have to use (-M(3)) (or ABS(M(3)))
    !
    call draw_ellipse_DC_1(nint(m(1)*x0+m(2)),nint(m(3)*y0+m(4)), &
         nint(m(1)*a),nint((-m(3))*b),color)
  end subroutine draw_ellipse_WC_1

  !
  ! BRESENHAM MIDPOINT CIRCLE ALGORITHM
  !
  ! Adapting the code from:
  !
  !   http://rosettacode.org/wiki/Bitmap/Midpoint_circle_algorithm#Fortran
  !
  ! See also: https://en.wikipedia.org/wiki/Midpoint_circle_algorithm
  ! and the discussion "filling a circle" in
  ! https://stackoverflow.com/questions/41524497/c-sdl2-rendering-a-circle
  !
  subroutine fill_circle_DC(x0,y0,radius)
    integer, intent(in) :: x0, y0, radius

    integer :: f, ddf_x, ddf_y, x, y

    f = 1-radius
    ddf_x = 0
    ddf_y = -2*radius
    x = 0
    y = radius

    ! first the horizontal diameter
    rc = sdl_render_draw_line(renderer,x0-radius,y0,x0+radius,y0)

    ! ...then the octant points; P(I) is the point in the octant I.
    ! In the DC Reference System (X-axis from L to R, Y-axis from T to
    ! B), the octant 1 covers the angle from 0 to 45 degrees, the
    ! octant 2 from 45 to 90 and so on...
    do while (x < y)
       if (f >= 0) then
          y = y-1
          ddf_y = ddf_y+2
          f = f+ddf_y
       end if
       x = x+1
       ddf_x = ddf_x+2
       f = f+ddf_x+1
       rc = sdl_render_draw_line(renderer,x0-x,y0+y,x0+x,y0+y) ! P(3)--P(2)
       rc = sdl_render_draw_line(renderer,x0-x,y0-y,x0+x,y0-y) ! P(6)--P(7)
       rc = sdl_render_draw_line(renderer,x0-y,y0+x,x0+y,y0+x) ! P(4)--P(1)
       rc = sdl_render_draw_line(renderer,x0-y,y0-x,x0+y,y0-x) ! P(5)--P(8)
    end do
  end subroutine fill_circle_DC

  subroutine fill_circle_WC(x0,y0,radius)
    real(WP), intent(in) :: x0, y0, radius

    ! We do not consider the aspect ratio, i.e. we suppose the same
    ! scale on both the axes
    !
    ! radius_in_pixel = XS(radius)-XS(0) =
    ! m(1)*radius+m(2)-(m(1)*0+m(2)) = m(1)*radius+m(2)-m(2) =
    ! m(1)*radius
    !
    call fill_circle_DC(nint(m(1)*x0+m(2)),nint(m(3)*y0+m(4)), &
         nint(m(1)*radius))
  end subroutine fill_circle_WC

  ! http://members.chello.at/~easyfilter/bresenham.html
  ! http://members.chello.at/~easyfilter/bresenham.c
  subroutine fill_ellipse_DC(x0,y0,a,b)
    integer, intent(in) :: x0, y0, a, b

    integer(8) :: x, y, e2, dx, dy, err, a8, b8

    a8 = a
    b8 = b

    ! II. quadrant from bottom left to top right
    x = -a8
    y = 0

    ! error increment
    e2 = b8
    dx = (1+2*x)*e2*e2

    ! error of 1.step
    dy = x*x
    err = dx+dy

    do
       rc = sdl_render_draw_line(renderer,int(x0-x),int(y0+y), &
            int(x0+x),int(y0+y))
       rc = sdl_render_draw_line(renderer,int(x0-x),int(y0-y), &
            int(x0+x),int(y0-y))

       e2 = 2*err

       if (e2 >= dx) then
          x = x+1
          dx = dx+2*b8*b8
          err = err+dx
       end if

       if (e2 <= dy) then
          y = y+1
          dy = dy+2*a8*a8
          err = err+dy
       end if

       if (x > 0) exit
    end do
  end subroutine fill_ellipse_DC

  subroutine fill_ellipse_WC(x0,y0,a,b)
    real(WP), intent(in) :: x0, y0, a, b

    !
    ! radius_X__in_pixel = XS(radius)-XS(0) =
    ! m(1)*radius+m(2)-(m(1)*0+m(2)) = m(1)*radius+m(2)-m(2) =
    ! m(1)*radius
    !
    ! radius_Y__in_pixel = (-m(3)*radius)
    !
    ! BUT M(3) is NEGATIVE, given that YS is toward the bottom! So we
    ! have to use (-M(3)) (or ABS(M(3))) when converting UNITS in
    ! PIXELS otherwise we would have a negative number of pixels.
    !
    call fill_ellipse_DC(nint(m(1)*x0+m(2)),nint(m(3)*y0+m(4)), &
         nint(m(1)*a),nint((-m(3))*b))
  end subroutine fill_ellipse_WC

  subroutine fill_ellipse_DC_1(x0,y0,a,b,color)
    integer, intent(in) :: x0, y0, a, b, color

    call set_rgba_color(ibits(color,0,8),ibits(color,8,8),ibits(color,16,8), &
         ibits(color,24,8))
    call fill_ellipse_DC(x0,y0,a,b)
  end subroutine fill_ellipse_DC_1

  subroutine fill_ellipse_WC_1(x0,y0,a,b,color)
    real(WP), intent(in) :: x0, y0, a, b
    integer, intent(in) :: color

    ! m(1) is in pixel/unit, and so m(3). Indeed
    !
    ! a_in_pixel = XS(a)-XS(0) =
    ! m(1)*a+m(2)-(m(1)*0+m(2)) = m(1)*a+m(2)-m(2) =
    ! m(1)*a
    !
    ! BUT M(3) is NEGATIVE, given that YS is toward the bottom! So we
    ! have to use (-M(3)) (or ABS(M(3)))
    !
    call fill_ellipse_DC_1(nint(m(1)*x0+m(2)),nint(m(3)*y0+m(4)), &
         nint(m(1)*a),nint((-m(3))*b),color)
  end subroutine fill_ellipse_WC_1

  subroutine draw_line_DC(x1,y1,x2,y2)
    integer, intent(in) :: x1, y1, x2, y2

    rc = sdl_render_draw_line(renderer,x1,y1,x2,y2)
  end subroutine draw_line_DC

  subroutine draw_line_WC(x1,y1,x2,y2)
    real(WP), intent(in) :: x1, y1, x2, y2

    rc = sdl_render_draw_line(renderer, &
         nint(m(1)*x1+m(2)),nint(m(3)*y1+m(4)), &
         nint(m(1)*x2+m(2)),nint(m(3)*y2+m(4)))
  end subroutine draw_line_WC

  subroutine draw_lines_DC(points,count)
    type(sdl_point), intent(in) :: points(:)
    integer, intent(in) :: count

    rc = sdl_render_draw_lines(renderer,points,count)
  end subroutine draw_lines_DC

  subroutine draw_lines_WC_1(points,count)
    type(sdlapp_point), intent(in) :: points(:)
    integer, intent(in) :: count

    type(sdl_point) :: p(count)

    ! BE CAREFUL: copy in p(:) ONLY THE NEEDED values
    p(1:count)%x = nint(m(1)*points(1:count)%x+m(2))
    p(1:count)%y = nint(m(3)*points(1:count)%y+m(4))

    rc = sdl_render_draw_lines(renderer,p,count)
  end subroutine draw_lines_WC_1

  subroutine draw_lines_WC_2(x,y,count)
    real(WP), intent(in) :: x(:), y(:)
    integer, intent(in) :: count

    type(sdl_point) :: p(count)

    ! BE CAREFUL: copy in p(:) ONLY THE NEEDED values
    p(1:count)%x = nint(m(1)*x(1:count)+m(2))
    p(1:count)%y = nint(m(3)*y(1:count)+m(4))

    rc = sdl_render_draw_lines(renderer,p,count)
  end subroutine draw_lines_WC_2

  subroutine draw_point_DC(x,y)
    integer, intent(in) :: x, y

    rc = sdl_render_draw_point(renderer,x,y)
  end subroutine draw_point_DC

  subroutine draw_point_WC(x,y)
    real(WP), intent(in) :: x, y

    rc = sdl_render_draw_point(renderer,nint(m(1)*x+m(2)),nint(m(3)*y+m(4)))
  end subroutine draw_point_WC

  subroutine draw_points_DC(points,count)
    type(sdl_point), intent(in) :: points(:)
    integer, intent(in) :: count

    rc = sdl_render_draw_points(renderer,points,count)
  end subroutine draw_points_DC

  subroutine draw_points_WC(points,count)
    type(sdlapp_point), intent(in) :: points(:)
    integer, intent(in) :: count

    type(sdl_point) :: p(count)

    ! BE CAREFUL: copy in p(:) ONLY THE NEEDED values
    p(1:count)%x = nint(m(1)*points(1:count)%x+m(2))
    p(1:count)%y = nint(m(3)*points(1:count)%y+m(4))

    rc = sdl_render_draw_points(renderer,p,count)
  end subroutine draw_points_WC

  subroutine draw_rect_DC(rect)
    type(sdl_rect), intent(in) :: rect

    rc = sdl_render_draw_rect(renderer,rect)
  end subroutine draw_rect_DC

  subroutine draw_rect_WC(x1,x2,y1,y2)
    real(WP), intent(in) :: x1,x2,y1,y2

    type(sdl_rect) :: r

    !
    ! In WC, (X1,Y2) should be the UPPER-LEFT corner and (X2,Y1)
    ! should be the BOTTOM-RIGHT corner
    !

    associate (xl => r%x, yt => r%y, xr => r%w, yb => r%h, temp => rc)
      ! Assuming UPPER-LEFT corner
      xl = nint(m(1)*x1+m(2))
      yt = nint(m(3)*y2+m(4))

      ! Assuming BOTTOM-RIGHT corner
      xr = nint(m(1)*x2+m(2))
      yb = nint(m(3)*y1+m(4))

      !
      ! Check if the hypothesis is correct
      !

      if (xr < xl) then
         temp = xr
         xr = xl
         xl = temp
      end if

      if (yb < yt) then
         temp = yb
         yb = yt
         yt = temp
      end if

      ! Now compute the true width and height of the rectangle
      r%w = xr-xl+1
      r%h = yb-yt+1
    end associate

    rc = sdl_render_draw_rect(renderer,r)
  end subroutine draw_rect_WC

  subroutine draw_rects(rects,count)
    type(sdl_rect), intent(in) :: rects(:)
    integer, intent(in) :: count

    rc = sdl_render_draw_rects(renderer,rects,count)
  end subroutine draw_rects

  subroutine fill_rect(rect)
    type(sdl_rect), intent(in) :: rect

    rc = sdl_render_fill_rect(renderer,rect)
  end subroutine fill_rect

  subroutine fill_rects(rects,count)
    type(sdl_rect), intent(in) :: rects(:)
    integer, intent(in) :: count

    rc = sdl_render_fill_rects(renderer,rects,count)
  end subroutine fill_rects

  !
  ! Very very basic routines to plot (play with) AXES
  !

  subroutine draw_axis_x(x1,x2,y,label)
    use nicelabels, only: loose_label

    real(WP), intent(in), optional :: x1, x2, y
    character(len=*), intent(in), optional :: label

    ! With a screen of about 2000 pixels, one can put, at most,
    ! 40+1 ticks with a space of 50 pixels between them..
    integer, parameter :: MAX_NTICK = 41, TICK_PIX = 3

    integer :: ix1, ix2, ix, iy, ntick, i
    character(len = 12) :: tick_str(MAX_NTICK)
    real(WP) :: x, dx, dy, xx1, xx2, yy

    i = current_map

    dx = 0.05_WP*(x_max(i)-x_min(i))
    dy = 0.05_WP*(y_max(i)-y_min(i))

    if (present(x1)) then
       xx1 = x1
    else
       xx1 = x_min(i)+dx
    end if

    if (present(x2)) then
       xx2 = x2
    else
       xx2 = x_max(i)-dx
    end if

    if (present(y)) then
       yy = y
    else
       yy = y_min(i)+dy
    end if

    ! Y position in pixel
    iy = nint(m(3)*yy+m(4))

    ! X positions in pixel
    ix1 = nint(m(1)*xx1+m(2))
    ix2 = nint(m(1)*xx2+m(2))

    ! If ix1 > ix2 swap..
    if (ix1 > ix2) then
       ix = ix2
       ix2 = ix1
       ix1 = ix
    end if

    ! Assuming a tick every 50 pixels, we should have about (tacking care of
    ! integer division truncation)...
    ntick = (1+(ix2-ix1)/50)+1

    if (ntick > MAX_NTICK) then
       if (graphics_on) call close_graphics()
       write(*,*) 'NTICK : ', ntick, ' > MAX_NTICK = ', MAX_NTICK
       write(*,*) 'ERROR : NTICK too large parameter (DRAW_AXIS_X).'
       stop
    end if

    call loose_label(xx1,xx2,ntick,tick_str)
    !
    ! Now we have the true number of ticks...
    !

    ! The axis...
    rc = sdl_render_draw_line(renderer,ix1,iy,ix2,iy)

    ! ..and the ticks
    do i = 1, ntick
       ! Get the positions of the tick #i, in pixels too..
       read(tick_str(i),*) x
       ix = nint(m(1)*x+m(2))

       if ((ix1 <= ix) .and. (ix <= ix2)) then
          rc = sdl_render_draw_line(renderer,ix,iy-TICK_PIX,ix,iy+TICK_PIX)
          write(*,*) ix,iy+TICK_PIX+1,trim(adjustl(tick_str(i)))
       end if
    end do

    if (present(label)) write(*,*) xx2-dx,yy+dy,trim(adjustl(label))
  end subroutine draw_axis_x

  subroutine draw_axis_y(y1,y2,x,label)
    use nicelabels, only: loose_label

    real(WP), intent(in), optional :: y1, y2, x
    character(len=*), intent(in), optional :: label

    ! With a screen of about 2000 pixels, one can put, at most,
    ! 40+1 ticks with a space of 50 pixels between them..
    integer, parameter :: MAX_NTICK = 41, TICK_PIX = 3

    integer :: iy1, iy2, ix, iy, ntick, i
    character(len = 12) :: tick_str(MAX_NTICK)
    real(WP) :: y, dx, dy, yy1, yy2, xx

    i = current_map

    dx = 0.05_WP*(x_max(i)-x_min(i))
    dy = 0.05_WP*(y_max(i)-y_min(i))

    if (present(y1)) then
       yy1 = y1
    else
       yy1 = y_min(i)+dy
    end if

    if (present(y2)) then
       yy2 = y2
    else
       yy2 = y_max(i)-dy
    end if

    if (present(x)) then
       xx = x
    else
       xx = x_min(i)+dx
    end if

    ! X position in pixel
    ix = nint(m(1)*xx+m(2))

    ! Y positions in pixel
    iy1 = nint(m(3)*yy1+m(4))
    iy2 = nint(m(3)*yy2+m(4))

    ! If iy1 > iy2 swap..
    if (iy1 > iy2) then
       iy = iy2
       iy2 = iy1
       iy1 = iy
    end if

    ! Assuming a tick every 50 pixels, we should have about (tacking care of
    ! integer division truncation)...
    ntick = (1+(iy2-iy1)/50)+1

    if (ntick > MAX_NTICK) then
       if (graphics_on) call close_graphics()
       write(*,*) 'NTICK : ', ntick, ' > MAX_NTICK = ', MAX_NTICK
       write(*,*) 'ERROR : NTICK too large parameter (DRAW_AXIS_Y).'
       stop
    end if

    call loose_label(yy1,yy2,ntick,tick_str)
    !
    ! Now we have the true number of ticks...
    !

    ! The axis...
    rc = sdl_render_draw_line(renderer,ix,iy1,ix,iy2)

    ! ..and the ticks
    do i = 1, ntick
       ! Get the positions of the tick #i, in pixels too..
       read(tick_str(i),*) y
       iy = nint(m(3)*y+m(4))

       if ((iy1 <= iy) .and. (iy <= iy2)) then
          rc = sdl_render_draw_line(renderer,ix-TICK_PIX,iy,ix+TICK_PIX,iy)
          write(*,*) ix-TICK_PIX-1,iy,trim(adjustl(tick_str(i)))
       end if
    end do

    if (present(label)) write(*,*) xx+dx/2,yy2,trim(adjustl(label))
  end subroutine draw_axis_y

  subroutine draw_axes(xlabel,ylabel)
    character(len=*), intent(in), optional :: xlabel, ylabel

    if (present(xlabel)) then
       call draw_axis_x(label=trim(adjustl(xlabel)))
    else
       call draw_axis_x()
    end if

    if (present(ylabel)) then
       call draw_axis_y(label=trim(adjustl(ylabel)))
    else
       call draw_axis_y()
    end if
  end subroutine draw_axes
end module SDL2_app
