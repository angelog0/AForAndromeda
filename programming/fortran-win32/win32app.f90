!
! Author: Angelo Graziosi
!
!   created   : Feb 10, 2014
!   last edit : Aug 18, 2016
!
!   (Partial) Fortran Interface to the Windows API Library
!   The main module
!
!
! DESCRIPTION
!
!   This is win32app module
!   Just to start with Windows applications in World Coordinete System...
!
! NOTE
!
!   int(0,UINT_T)    -->  0_UINT_T
!   int(0,WPARAM_T)  -->  0_WPARAM_T
!   int(0,LPARAM_T)  -->  0_LPARAM_T
!   ...
!

module win32app
  use kind_consts, only: WP
  use math_consts, only: ZERO => Z0, ONE => Z1, TWO => Z2, FOUR => Z4, &
       HF => Q1_2
  use win32, only: BOOL_T, COLORREF_T, DWORD_T, HBITMAP_T, HBRUSH_T, HDC_T, &
       HWND_T, INT_T, LPARAM_T, NUL, SRCCOPY, WORD_T, RECT_T, &
       BitBlt, CreateCompatibleBitmap, Ellipse, FillRect, hi_word, lo_word, &
       Rectangle, SetPixel, TextOut
  use xy_box_m, only: xy_box_t

  implicit none
  private

  integer  :: client_width = 0, client_height = 0

  type, public :: box_type
     real(WP) :: x1, x2
     real(WP) :: y1, y2
  end type box_type

  ! Output view region in WC
  real(WP) :: x_min = -ONE, x_max = ONE, &
       y_min = -ONE, y_max = ONE, &
       dx = ONE, dy = ONE

  public :: win32app_BitBlt, win32app_clearDC, &
       win32app_CreateCompatibleBitmap, win32app_dot, win32app_ellipse, &
       win32app_fillbox, win32app_setup, win32app_textout, win32app_xbounds, &
       win32app_ybounds, win32app_xmin, win32app_xmax, win32app_ymin, &
       win32app_ymax, win32app_height, win32app_width

contains

  subroutine win32app_setup(lParam,x1,x2,y1,y2)
    integer(LPARAM_T), intent(in) :: lParam
    real(WP), intent(in), optional :: x1, x2, y1, y2
    real(WP) :: cx, cy

    ! Initializing with defaults values...
    if (present(x1)) x_min = x1
    if (present(x2)) x_max = x2
    if (present(y1)) y_min = y1
    if (present(y2)) y_max = y2

    ! The true width and height of client area
    client_width = lo_word(int(lParam,DWORD_T)) + 1
    client_height = hi_word(int(lParam,DWORD_T)) + 1

    dx = x_max-x_min
    dy = y_max-y_min

    cx = x_min+HF*dx
    cy = y_min+HF*dy

    ! First, adjusts WC region...
    if (client_width > client_height) then
       dy = (dx*client_height)/client_width
       y_min = cy-HF*dy
       y_max = y_min+dy
    else
       dx = (dy*client_width)/client_height
       x_min = cx-HF*dx
       x_max = x_min+dx
    end if

    ! ...then, calculates the size of the mesh that represents each pixel
    dx = (x_max-x_min)/client_width
    dy = (y_max-y_min)/client_height

    ! Many Windows routines expect a "virtual" width and height,
    ! more precisely the client area bottom-right point coordinates
    client_width = client_width - 1
    client_height = client_height - 1
  end subroutine win32app_setup

  function xs(x)
    integer :: xs
    real(WP), intent(in) :: x

    xs = 0+int((x-x_min)/dx)
  end function xs

  function ys(y)
    integer :: ys
    real(WP), intent(in) :: y

    ys = 0+int((y_max-y)/dy)
  end function ys

  function win32app_xmin() result(r)
    real(WP) :: r

    r = x_min
  end function win32app_xmin

  function win32app_xmax() result(r)
    real(WP) :: r

    r = x_max
  end function win32app_xmax

  function win32app_ymin() result(r)
    real(WP) :: r

    r = y_min
  end function win32app_ymin

  function win32app_ymax() result(r)
    real(WP) :: r

    r = y_max
  end function win32app_ymax

  function win32app_width() result(r)
    integer :: r

    r = client_width !+1 ?
  end function win32app_width

  function win32app_height() result(r)
    integer :: r

    r = client_height !+1 ?
  end function win32app_height

  subroutine win32app_xbounds(hWnd,idd_data_xlimits,idc_xmin,idc_xmax)
    integer(HWND_T), intent(in) :: hWnd
    integer(WORD_T), intent(in) :: idd_data_xlimits
    integer(INT_T), intent(in) :: idc_xmin, idc_xmax

    type(xy_box_t) :: xyb
    real(WP) :: u_min, u_max, du, c_params

    ! The current Y view center
    c_params = HF*(y_max+y_min)

    call xyb%initialize(hWnd,idd_data_xlimits,idc_xmin,idc_xmax,x_min,x_max)

    if (xyb%run() > 0) then
       u_min = xyb%get_x()
       u_max = xyb%get_y()

       ! The assumed new intervall size
       du = u_max-u_min

       ! If it is too small
       if (abs(du) <= ZERO) then
          u_min = -TWO
          u_max = TWO
          du = FOUR
       else
          ! if u_max < u_min
          if (du < ZERO) then
             ! swap u_min/max using du as temp
             du = u_max
             u_max = u_min
             u_min = du
             du = u_max-u_min
          end if
       end if

       ! The new Y height (with the same aspect ratio)
       du = du*(y_max-y_min)/(x_max-x_min)

       ! The X limits just inserted
       x_min = u_min
       x_max = u_max

       ! Adjusting the Y limits accordingly
       y_min = c_params-HF*du
       y_max = y_min+du

       ! We need to recompute the size of the mesh that represents each pixel
       dx = (x_max-x_min)/(client_width+1)
       dy = (y_max-y_min)/(client_height+1)
    end if
  end subroutine win32app_xbounds

  subroutine win32app_ybounds(hWnd,idd_data_ylimits,idc_ymin,idc_ymax)
    integer(HWND_T), intent(in) :: hWnd
    integer(WORD_T), intent(in) :: idd_data_ylimits
    integer(INT_T), intent(in) :: idc_ymin, idc_ymax

    type(xy_box_t) :: xyb
    real(WP) :: u_min, u_max, du, c_params

    ! The current X view center
    c_params = HF*(x_max+x_min)

    call xyb%initialize(hWnd,idd_data_ylimits,idc_ymin,idc_ymax,y_min,y_max)

    if (xyb%run() > 0) then
       u_min = xyb%get_x()
       u_max = xyb%get_y()

       ! The assumed new intervall size
       du = u_max-u_min

       ! If it is too small
       if (abs(du) <= ZERO) then
          u_min = -TWO
          u_max = TWO
          du = FOUR
       else
          ! if u_max < u_min
          if (du < ZERO) then
             ! swap u_min/max using du as temp
             du = u_max
             u_max = u_min
             u_min = du
             du = u_max-u_min
          end if
       end if

       ! The new X width (with the same aspect ratio)
       du = du*(x_max-x_min)/(y_max-y_min)

       ! The Y limits just inserted
       y_min = u_min
       y_max = u_max

       ! Adjusting the X limits accordingly
       x_min = c_params-HF*du
       x_max = x_min+du

       ! We need to recompute the size of the mesh that represents each pixel
       dx = (x_max-x_min)/(client_width+1)
       dy = (y_max-y_min)/(client_height+1)
    end if
  end subroutine win32app_ybounds

  function win32app_BitBlt(hdc,hdcMem) result(r)
    integer(BOOL_T) :: r
    integer(HDC_T), intent(in) :: hdc, hdcMem

    r = BitBlt(hdc,0,0,client_width,client_height,hdcMem,0,0,SRCCOPY)
  end function win32app_BitBlt

  function win32app_clearDC(hdc,dwRop) result(r)
    integer(BOOL_T) :: r
    integer(HDC_T), intent(in) :: hdc
    integer(DWORD_T), intent(in) :: dwRop

    ! dwRop = BLACKNESS or WHITENESS?
    r = BitBlt(hdc,0,0,client_width,client_height,0_HDC_T,0,0,dwRop)
    !r = Rectangle(hdc,-1,-1,client_width+1,client_height+1)
  end function win32app_clearDC

  function win32app_CreateCompatibleBitmap(hdc) result(r)
    integer(HBITMAP_T) :: r
    integer(HDC_T), intent(in) :: hdc

    r = CreateCompatibleBitmap(hdc,client_width,client_height)
  end function win32app_CreateCompatibleBitmap

  function win32app_dot(hdc,x,y,color) result(r)
    integer(COLORREF_T) :: r
    integer(HDC_T), intent(in) :: hdc
    real(WP), intent(in) :: x, y
    integer(COLORREF_T), intent(in) :: color

    r = SetPixel(hdc,xs(x),ys(y),color)
  end function win32app_dot

  function win32app_ellipse(hdc,left,top,right,bottom) result(r)
    integer(BOOL_T) :: r
    integer(HDC_T), intent(in) :: hdc
    real(WP), intent(in) :: left, top, right, bottom

    r = Ellipse(hdc,xs(left),ys(top),xs(right),ys(bottom))
  end function win32app_ellipse

  function win32app_fillbox(hdc,box,hBrush) result(r)
    integer(INT_T) :: r
    integer(HDC_T), intent(in) :: hdc
    type(box_type), intent(in) :: box
    integer(HBRUSH_T), intent(in) :: hBrush

    type(RECT_T), save :: rect

    rect%left = xs(box%x1)
    rect%right = xs(box%x2)
    rect%bottom = ys(box%y1)
    rect%top = ys(box%y2)

    r = FillRect(hdc,rect,hBrush)
  end function win32app_fillbox

  function win32app_textout(hdc,x,y,text) result(r)
    integer(INT_T) :: r
    integer(HDC_T), intent(in) :: hdc
    real(WP), intent(in) :: x, y
    character(len=*), intent(in) :: text

    r = index(text,NUL)
    if (r /= 0) then
       ! The len(), i.e. strlen(), of "text" string
       r = r-1
    else
       ! The len()...
       r = len(text)

       ! ...or the len() without trailing blank characters, but in this case
       ! you CANNOT display strings with trailing spaces. They can be useful
       ! to clean some garbage. The 'trimming' should be done from the caller
       ! which can add useful trailing spaces
       !r = len_trim(text)
    end if
    ! As described here:
    !   https://msdn.microsoft.com/en-us/library/windows/desktop/\
    !   dd145133(v=vs.85).aspx,
    ! the string passed to TextOut "does not need to be zero-terminated"
    r = TextOut(hdc,xs(x),ys(y),text(1:r),r)
  end function win32app_textout
end module win32app
