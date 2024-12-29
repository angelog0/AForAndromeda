!
! Author: Angelo Graziosi
!
!   created   : Feb 10, 2014
!   last edit : Aug 18, 2016
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
! REFERENCES
!
!   Press W.H., Numerical Recipes, C.U.P
!   Karlen D., Computational Physics, Carleton University
!   Koonin S.E., Computational Physics, Addison-Wesley
!
!
! HOW TO BUILD THE APP (MSYS2/UCRT64 shell)
!
!   cd win32-fortran.apps/poisson2D.app
!
!   rm -rf {*.mod,*.res} && \
!   windres poisson2D.rc -O coff -o poisson2D.res && \
!   gfortran -std=f2018 -O3 -Wall \
!     -Wno-unused-dummy-argument -Wno-maybe-uninitialized \
!     [-static] -mwindows ../../basic-modules/{kind,math}_consts.f90 \
!     ../{win32,{basic,about,x,xy,radio}_box_m,win32app}.f90 \
!     poisson2D.f90 poisson2D.res -o poisson2D && \
!     rm -rf {*.mod,*.res}
!
!   ./poisson2D
!

! A new implementation of shade.kumac PAW macro. See
!   http://paw.web.cern.ch/paw/allfaqs.html
module color_map
  use win32, only: COLORREF_T, RGB
  implicit none
  private

  integer, parameter :: NMXPT = 20

  integer, parameter, public :: MAXCOLOURINDEX = 255
  integer, parameter, public :: MAXCOLOURS = MAXCOLOURINDEX+1

  integer(COLORREF_T), public :: crColors(0:MAXCOLOURINDEX)

  integer :: npt, idx(NMXPT)
  integer :: r(NMXPT), g(NMXPT), b(NMXPT)

  public :: set_color_map

contains

  subroutine set_shade(idxi,ri,gi,bi)
    integer, intent(in) :: idxi, ri, gi, bi

    if (idxi < 0) then
       npt = 0
       return
    end if

    npt = npt+1

    if (npt > NMXPT) error stop ': Too many colours.'

    idx(npt) = idxi
    r(npt)   = ri
    g(npt)   = gi
    b(npt)   = bi
  end subroutine set_shade

  subroutine shade()
    integer :: i, ii, i1, i2, j, n, rs, gs, bs, r1, g1, b1, r2, g2, b2
    real :: scale

    if (npt < 2) error stop ': At least two colours are needed.'

    do i = 2, npt
       j  = i-1
       i1 = idx(j)
       i2 = idx(i)
       r1 = r(j)
       g1 = g(j)
       b1 = b(j)
       r2 = r(i)
       g2 = g(i)
       b2 = b(i)
       n  = i2-i1+1
       do ii = i1, i2
          scale = (ii-i1)/(n-1.0)

          rs = int((r2 - r1)*scale + r1)
          gs = int((g2 - g1)*scale + g1)
          bs = int((b2 - b1)*scale + b1)

          crColors(ii) = RGB(rs,gs,bs)
       enddo
    enddo
  end subroutine shade

  subroutine set_color_map()
    !crColors(0) = RGB(0,0,0)           ! BLACK
    !crColors(255) = RGB(255,255,255)   ! WHITE

    ! The first call to set_shade() MUST be this INITIALIZATION
    call set_shade( -1,0,0,0)

    call set_shade(  0,  0,  0,128)
    call set_shade( 40,  0,  0,255)
    call set_shade(100,  0,255,255)
    call set_shade(120,  0,255,128)
    call set_shade(160,255,255,  0)
    call set_shade(255,128,  0,  0)
    call shade()
  end subroutine set_color_map
end module color_map

module the_app
  use kind_consts, only: WP
  use math_consts, only: ZERO => Z0, ONE => Z1, TWO => Z2, PI
  use color_map, only: MAXCOLOURINDEX, MAXCOLOURS, crColors, set_color_map
  use about_box_m, only: about_box_t
  use x_box_m, only: x_box_t
  use xy_box_m, only: xy_box_t
  use radio_box_m, only: radio_box_t
  use win32, only: BLACK_COLOR, BLACKNESS, COLORREF_T, BOOL_T, DWORD_T, &
       FALSE_T, HBITMAP_T, HBRUSH_T, HDC_T, HINSTANCE_T, HWND_T, IDYES, &
       INT_T, LPARAM_T, LRESULT_T, MAX_LEN, NL, NULL_T, &
       TA_CENTER, TA_LEFT, TRUE_T, UINT_T, VK_ESCAPE, WHITE_COLOR, WM_CHAR, &
       WM_CLOSE, WM_COMMAND, WM_CREATE, WM_DESTROY, WM_SIZE, WORD_T, &
       WPARAM_T, &
       ask_confirmation, CreateCompatibleDC, CreateSolidBrush, &
       DefWindowProc, DeleteDC, DeleteObject, DestroyWindow, dummy, &
       error_msg, GetDC, GetStockObject, lo_word, MessageBeep, PostMessage, &
       PostQuitMessage, ReleaseDC, SelectObject, SetBkColor, SetTextAlign, &
       SetTextColor
  use win32app, only: box_type, win32app_BitBlt, win32app_clearDC, &
       win32app_CreateCompatibleBitmap, win32app_fillbox, win32app_setup, &
       win32app_textout, win32app_xmin, win32app_xmax, win32app_ymax
  implicit none
  private

  integer(WORD_T), parameter, public :: IDI_POISSON2D = 1
  integer(WORD_T), parameter, public :: IDM_MAINMENU  = 9000

  integer(WORD_T), parameter :: IDM_FILE_EXIT       = 9010
  integer(WORD_T), parameter :: IDM_DATA_NDIV       = 9020
  integer(WORD_T), parameter :: IDM_DATA_MAXI       = 9021
  integer(WORD_T), parameter :: IDM_DATA_EPS        = 9022
  integer(WORD_T), parameter :: IDM_DATA_OMEGA      = 9023
  integer(WORD_T), parameter :: IDM_DATA_XBOUNDS    = 9024
  integer(WORD_T), parameter :: IDM_DATA_YBOUNDS    = 9025
  integer(WORD_T), parameter :: IDM_DATA_NSOUT      = 9026
  integer(WORD_T), parameter :: IDM_DATA_PHILIMITS  = 9027
  integer(WORD_T), parameter :: IDM_OPTIONS_CFGTYPE = 9030
  integer(WORD_T), parameter :: IDM_OPTIONS_FLDTYPE = 9031
  integer(WORD_T), parameter :: IDM_RUNAPP          = 9040
  integer(WORD_T), parameter :: IDM_HELP_DISCLAIMER = 9998
  integer(WORD_T), parameter :: IDM_HELP_ABOUT      = 9999

  !integer(WORD_T), parameter, public :: IDC_STATIC = -1

  integer(WORD_T), parameter :: IDD_DATA_NDIV = 100
  integer(INT_T), parameter :: IDC_NDIV       = 101

  integer(WORD_T), parameter :: IDD_DATA_MAXI = 200
  integer(INT_T), parameter :: IDC_MAXI       = 201

  integer(WORD_T), parameter :: IDD_DATA_EPS = 300
  integer(INT_T), parameter :: IDC_EPS       = 301

  integer(WORD_T), parameter :: IDD_DATA_OMEGA = 400
  integer(INT_T), parameter :: IDC_OMEGA       = 401

  integer(WORD_T), parameter :: IDD_DATA_XBOUNDS = 500
  integer(INT_T), parameter :: IDC_ULEFT         = 501
  integer(INT_T), parameter :: IDC_URIGHT        = 502

  integer(WORD_T), parameter :: IDD_DATA_YBOUNDS = 600
  integer(INT_T), parameter :: IDC_UBOTTOM       = 601
  integer(INT_T), parameter :: IDC_UTOP          = 602

  integer(WORD_T), parameter :: IDD_DATA_NSOUT = 700
  integer(INT_T), parameter :: IDC_NSOUT       = 701

  integer(WORD_T), parameter :: IDD_DATA_PHILIMITS = 800
  integer(INT_T), parameter :: IDC_PHIMIN          = 801
  integer(INT_T), parameter :: IDC_PHIMAX          = 802

  integer(WORD_T), parameter :: IDD_OPTIONS_CFGTYPE = 900
  integer(INT_T), parameter :: IDC_ONEBOX           = 901
  integer(INT_T), parameter :: IDC_TWOBOX           = 902
  integer(INT_T), parameter :: IDC_CONDENSER        = 903
  integer(INT_T), parameter :: IDC_THREECHARGES     = 904
  integer(INT_T), parameter :: IDC_CHARGEDLINE      = 905
  integer(INT_T), parameter :: ONE_BOX       = 1
  integer(INT_T), parameter :: TWO_BOX       = 2
  integer(INT_T), parameter :: CONDENSER     = 3
  integer(INT_T), parameter :: THREE_CHARGES = 4
  integer(INT_T), parameter :: CHARGED_LINE  = 5

  integer(WORD_T), parameter :: IDD_OPTIONS_FLDTYPE = 950
  integer(INT_T), parameter :: IDC_POTENTIAL        = 951
  integer(INT_T), parameter :: IDC_GRADIENT         = 952
  integer(INT_T), parameter :: POTENTIAL_FLD = 1
  integer(INT_T), parameter :: GRADIENT_FLD  = 2

  integer(WORD_T), parameter :: IDD_DISCLAIMER = 998
  integer(WORD_T), parameter :: IDD_ABOUT = 999

  ! COMMON data
  integer(HBITMAP_T) :: hBitmap = NULL_T
  logical :: run_flag = .true.
  real(WP) :: box_xmin, box_xmax, box_ymax

  ! Application data, strictly speaking...
  logical :: converg = .false., not_converg = .true.
  integer :: ndiv = 100, max_iter = 100, count_iter = 0, nsout = 10, &
       cfg_type = ONE_BOX, fld_type = POTENTIAL_FLD
  real(WP) :: eps = 0.0001_WP, omega = 1.8_WP
  real(WP) :: u_left = ONE, u_right = ZERO, &
       u_bottom = ZERO, u_top = ZERO, &
       phi_min = ZERO, phi_max = ONE
  real(WP) :: dphi = ZERO, h = ZERO, hq = ZERO, h2 = ZERO, &
       omega1 = ZERO, omega4 = ZERO, &
       energy = ZERO, energy_old = ZERO

  ! Dynamic memory...
  real(WP), target, allocatable :: u(:,:),f(:,:)
  real(WP), allocatable :: s(:,:)
  real(WP), pointer :: phi(:,:) => null()
  logical, allocatable :: b(:,:)

  public :: paint_screen, WndProc

contains

  subroutine grid_on()
    integer :: ierr

    allocate(u(0:ndiv,0:ndiv),stat=ierr)
    if (ierr /= 0) error stop ': Allocation failure for U(:,:).'

    allocate(s(0:ndiv,0:ndiv),stat=ierr)
    if (ierr /= 0) error stop ': Allocation failure for S(:,:).'

    allocate(f(0:ndiv,0:ndiv),stat=ierr)
    if (ierr /= 0) error stop ': Allocation failure for F(:,:).'

    allocate(b(0:ndiv,0:ndiv),stat=ierr)
    if (ierr /= 0) error stop ': Allocation failure for B(:,:).'

    ! Now that all is allocated, we can associate the pointer
    if (fld_type == POTENTIAL_FLD) then
       phi => u
    else
       phi => f      ! ...the GRADIENT_FLD
    end if
  end subroutine grid_on

  subroutine grid_off()
    integer :: ierr

    if (allocated(b)) deallocate(b,stat=ierr)
    if (ierr /= 0) error stop ': Deallocation failure for B(:,:).'

    if (allocated(f)) deallocate(f,stat=ierr)
    if (ierr /= 0) error stop ': Deallocation failure for F(:,:).'

    if (allocated(s)) deallocate(s,stat=ierr)
    if (ierr /= 0) error stop ': Deallocation failure for S(:,:).'

    if (allocated(u)) deallocate(u,stat=ierr)
    if (ierr /= 0) error stop ': Deallocation failure for U(:,:).'

    ! Now that all is deallocated, we can deassociate the pointer
    nullify(phi)
  end subroutine grid_off

  subroutine fcn_one_box(x,y,u,s,b)
    real(WP), intent(in) :: x, y
    real(WP), intent(out) :: u, s
    logical, intent(out) :: b

    u = ZERO
    s = ZERO
    b = .false.
  end subroutine fcn_one_box

  subroutine fcn_two_box(x,y,u,s,b)
    real(WP), intent(in) :: x, y
    real(WP), intent(out) :: u, s
    logical, intent(out) :: b
    !
    ! A box inside a box without charge. The inner box is
    ! (0.25,0.25) - (0.75,0.75), but the boundary conditions are assigned
    ! ONLY on its perimeter, NOT on its inner points!!!
    ! (We have a kind of square ring...)
    !
    integer :: i, i1, i2, j, j1, j2

    i1 = int(0.25_WP/h)
    i2 = int(0.75_WP/h)

    j1 = i1
    j2 = i2

    i = int(x/h)
    j = int(y/h)

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

  subroutine fcn_condenser(x,y,u,s,b)
    real(WP), intent(in) :: x, y
    real(WP), intent(out) :: u, s
    logical, intent(out) :: b
    !
    ! A Condenser inside a box without charge
    ! The condenser plates are at 0.25 and 0.75
    !
    integer :: i, i1, i2, j, j1, j2

    i1 = int(0.25_WP/h)
    i2 = int(0.75_WP/h)

    j1 = i1
    j2 = i2

    i = int(x/h)
    j = int(y/h)

    s = ZERO

    if ((i == i1) .and. (j1 <= j .and. j <= j2)) then
       u = ONE
       b = .true.
    else if ((i == i2) .and. (j1 <= j .and. j <= j2)) then
       u = -ONE
       b = .true.
    else
       u = ZERO
       b = .false.
    end if
  end subroutine fcn_condenser

  function delta(x,hwhm)
    real(WP) :: delta
    real(WP), intent(in) :: x, hwhm

    delta = hwhm/(PI*(hwhm*hwhm+x*x))
  end function delta

  function theta(x)
    real(WP) :: theta
    real(WP), intent(in) :: x

    if (x > ZERO) then
       theta = ONE
    else
       theta = ZERO
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
         -delta(x-0.5_WP,HWHM)*delta(y-0.75_WP,HWHM))
  end subroutine fcn_three_charges

  subroutine fcn_charged_line(x,y,u,s,b)
    real(WP), intent(in) :: x, y
    real(WP), intent(out) :: u, s
    logical, intent(out) :: b

    real(WP), parameter :: HWHM = 0.0005_WP

    u = ZERO
    b = .false.

    s = delta(x-0.5_WP,HWHM)*(theta(x-0.25_WP)-theta(x-0.75_WP))
  end subroutine fcn_charged_line

  function get_grid_energy()
    real(WP) :: get_grid_energy
    !
    ! Koonin's formula (6.7):
    !
    ! E = 0.5*Sum(i=1,N)Sum(j=1,N)[(u(i,j)-u(i-1,j))**2+(u(i,j)-u(i,j-1))**2]
    !   -h*h*Sum(i=1,N-1)Sum(j=1,N-1)[S(i,j)*u(i,j)]
    !
    integer, save :: i, j
    real(WP), save :: sum1, sum2, ff

    sum1 = ZERO
    sum2 = ZERO

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
    get_grid_energy = 0.5_WP*sum1-sum2
  end function get_grid_energy

  subroutine setup_grid()
    real(WP) :: x, y
    integer :: i, j

    ! Iterations initialization
    count_iter = 0

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
          select case(cfg_type)
          case(ONE_BOX)
             call fcn_one_box(x,y,u(i,j),s(i,j),b(i,j))
          case(TWO_BOX)
             call fcn_two_box(x,y,u(i,j),s(i,j),b(i,j))
          case(CONDENSER)
             call fcn_condenser(x,y,u(i,j),s(i,j),b(i,j))
          case(THREE_CHARGES)
             call fcn_three_charges(x,y,u(i,j),s(i,j),b(i,j))
          case(CHARGED_LINE)
             call fcn_charged_line(x,y,u(i,j),s(i,j),b(i,j))

          case default
             call fcn_one_box(x,y,u(i,j),s(i,j),b(i,j))
          end select

          s(i,j) = s(i,j)*hq
          f(i,j) = ZERO
       end do
    end do

    ! Initialization of energy and convergence flags
    energy = get_grid_energy()
    converg = .false.
    not_converg = .true.
  end subroutine setup_grid

  subroutine draw_colorbar(hdc)
    integer(HDC_T), intent(in) :: hdc
    !
    ! A simple colour bar scale
    !
    character(len=*), parameter :: NAME_FLD(2) = [ &
         'Potential', &
         'Gradient ' ]
    integer, parameter :: SCALE_PTS = 5
    integer(COLORREF_T), save :: old_bk_color, old_text_color
    integer(UINT_T), save :: old_align
    integer(HBRUSH_T), save :: hBrush
    character(len=MAX_LEN), save :: buffer = ''
    integer, save :: i
    real(WP), save :: bar_width, delta_y, phi, delta_phi
    type(box_type), save :: bar_box

    ! The space on the right of the grid is in [1,box_xmax], the bar width is
    ! 1/15
    bar_width = (box_xmax-ONE)/15

    ! In X, the bar is in [1+7*bar_width,1+8*bar_width], i.e at position 8
    ! (7+1+7 = 15)
    bar_box%x1 = ONE+7.0_WP*bar_width
    bar_box%x2 = ONE+8.0_WP*bar_width

    ! In Y the bar is in [0,1] and composed of 0, 1,... MAXCOLOURINDEX
    ! filled slices
    delta_y = (ONE-ZERO) / MAXCOLOURS

    bar_box%y2 = ZERO
    do i = 0, MAXCOLOURINDEX
       bar_box%y1 = bar_box%y2
       bar_box%y2 = bar_box%y2+delta_y

       hBrush = CreateSolidBrush(crColors(i))
       dummy = int(SelectObject(hdc,hBrush),INT_T)

       dummy = win32app_fillbox(hdc,bar_box,hBrush)

       dummy = DeleteObject(hBrush)
    end do

    delta_phi = (phi_max-phi_min)/(SCALE_PTS-1)
    delta_y = (ONE-ZERO)/(SCALE_PTS-1)

    old_bk_color = SetBkColor(hdc,BLACK_COLOR)
    old_text_color = SetTextColor(hdc,WHITE_COLOR)
    old_align = SetTextAlign(hdc,TA_LEFT)

    phi = phi_min  ! U
    bar_box%x2 = bar_box%x2+0.2_WP*bar_width
    bar_box%y2 = ZERO+0.2_WP*bar_width
    do i = 1, SCALE_PTS
       buffer = ''
       write(buffer,'(f10.4)') phi
       dummy = win32app_textout(hdc,bar_box%x2,bar_box%y2, &
            trim(adjustl(buffer)))

       phi = phi+delta_phi
       bar_box%y2 = bar_box%y2+delta_y
    end do

    bar_box%x2 = bar_box%x2+0.8_WP*bar_width
    bar_box%y2 = bar_box%y2-3*delta_y/2

    buffer = ''
    write(buffer,*) NAME_FLD(fld_type)
    dummy = win32app_textout(hdc,bar_box%x2,bar_box%y2,trim(adjustl(buffer)))

    ! Restore previous text colors...
    dummy = SetTextAlign(hdc,old_align)
    dummy = SetTextColor(hdc,old_text_color)
    dummy = SetBkColor(hdc,old_bk_color)
  end subroutine draw_colorbar

  subroutine draw_grid(hdc)
    integer(HDC_T), intent(in) :: hdc

    character(len=*), parameter :: FMT = '(a,i6,2(a,1pg14.7),a,i6,a,1pg14.7)'

    ! We use SAVE just to save something at each call
    ! (draw_time() is called intensively, at each iteration)
    integer(COLORREF_T), save :: old_bk_color, old_text_color
    integer(UINT_T), save :: old_align
    integer(HBRUSH_T), save :: hBrush
    character(len=MAX_LEN), save :: buffer = ''
    type(box_type), save :: box
    integer, save :: i, j, i_col
    real(WP), save :: x, y

    old_bk_color = SetBkColor(hdc,BLACK_COLOR)
    old_text_color = SetTextColor(hdc,WHITE_COLOR)
    old_align = SetTextAlign(hdc,TA_CENTER)

    x = 0.5_WP
    y = 0.5_WP*(ONE+box_ymax)
    buffer = ''
    write(buffer,FMT) 'NDIV = ',ndiv,'   OMEGA = ',omega,'   EPS = ',eps, &
         '   COUNT = ',count_iter,'   E = ',energy
    dummy = win32app_textout(hdc,x,y,trim(adjustl(buffer)))

    ! Restore previous text colors...
    dummy = SetTextAlign(hdc,old_align)
    dummy = SetTextColor(hdc,old_text_color)
    dummy = SetBkColor(hdc,old_bk_color)

    x = -h2
    box%x2 = x
    do i = 0, ndiv
       box%x1 = box%x2

       x = x+h
       box%x2 = x

       y = -h2
       box%y2 = y
       do j = 0, ndiv
          box%y1 = box%y2

          y = y+h
          box%y2 = y

          i_col = int((phi(i,j)-phi_min)/dphi)

          if (i_col < 0) then
             i_col = 0
          else if (i_col > MAXCOLOURINDEX) then
             i_col = MAXCOLOURINDEX
          end if

          hBrush = CreateSolidBrush(crColors(i_col))
          dummy = int(SelectObject(hdc,hBrush),INT_T)

          dummy = win32app_fillbox(hdc,box,hBrush)

          dummy = DeleteObject(hBrush)
       end do
    end do
  end subroutine draw_grid

  subroutine painting_setup(hWnd)
    integer(HWND_T), intent(in) :: hWnd

    logical, save :: first = .true.
    integer(HDC_T) :: hdc, hdcMem

    if (first) then
       call set_color_map()
       dphi = (phi_max-phi_min)/MAXCOLOURS
       h = ONE/ndiv
       hq = h*h
       h2 = 0.5_WP*h
       omega1 = ONE-omega
       omega4 = 0.25_WP*omega
       call setup_grid()
       first = .false.
    end if

    if (hBitmap /= NULL_T) then
       dummy = DeleteObject(hBitmap)
    end if

    hdc = GetDC(hWnd)
    hdcMem = CreateCompatibleDC(hdc)
    hBitmap = win32app_CreateCompatibleBitmap(hdc)
    dummy = ReleaseDC(hWnd,hdc)

    dummy = int(SelectObject(hdcMem,hBitmap),INT_T)

    ! Clear the off-screen DC (hdcMem) for the next drawing
    dummy = win32app_clearDC(hdcMem,BLACKNESS)

    ! Draw (on the off-screen DC) the color scale
    call draw_colorbar(hdcMem)

    ! Draw (on the off-screen DC) grid at current iteration
    call draw_grid(hdcMem)

    dummy = DeleteDC(hdcMem)
  end subroutine painting_setup

  function process_char(hWnd,wParam)
    integer(BOOL_T) :: process_char
    integer(HWND_T), intent(in) :: hWnd
    integer(WPARAM_T), intent(in) :: wParam

    select case(wParam)
    case(VK_ESCAPE)
       dummy = DestroyWindow(hWnd)

       process_char = TRUE_T
       return

    case default
       process_char = FALSE_T
       return
    end select
  end function process_char

  subroutine set_ndiv(hWnd)
    integer(HWND_T), intent(in) :: hWnd

    type(x_box_t) :: xb
    real(WP) :: x

    x = real(ndiv,WP)

    call xb%initialize(hWnd,IDD_DATA_NDIV,IDC_NDIV,x,iflag_x=.true.)

    if (xb%run() > 0) then

       x = xb%get()

       if (x > ZERO) then
          ! Destroy the current grid...
          call grid_off()

          ! Get the new value
          ndiv = int(x)

          ! Readjust some params
          h = ONE/ndiv
          hq = h*h
          h2 = 0.5_WP*h

          ! Create the new grid...
          call grid_on()

          ! If you prefer to see something, uncomment the following...
          !call setup_grid()
       else
          call error_msg('NDIV <= 0 !!!'//NL &
               //'Must be NDIV > 0...   ')
       end if
    end if
  end subroutine set_ndiv

  subroutine set_maxi(hWnd)
    integer(HWND_T), intent(in) :: hWnd

    type(x_box_t) :: xb
    real(WP) :: x

    x = real(max_iter,WP)

    call xb%initialize(hWnd,IDD_DATA_MAXI,IDC_MAXI,x,iflag_x=.true.)

    if (xb%run() > 0) then

       x = xb%get()

       if (x > ZERO) then
          max_iter = int(x)
       else
          call error_msg('MAX_ITER <= 0 !!!'//NL &
               //'Must be MAX_ITER > 0...   ')
       end if
    end if
  end subroutine set_maxi

  subroutine set_eps(hWnd)
    integer(HWND_T), intent(in) :: hWnd

    type(x_box_t) :: xb
    real(WP) :: x

    x = eps

    call xb%initialize(hWnd,IDD_DATA_EPS,IDC_EPS,x)

    if (xb%run() > 0) then

       x = xb%get()

       if (ZERO < x .and. x < ONE) then
          eps = x
       else
          call error_msg('EPS not in (0,1)!!!'//NL &
               //'Must be 0 < EPS < 1...   ')
       end if
    end if
  end subroutine set_eps

  subroutine set_omega(hWnd)
    integer(HWND_T), intent(in) :: hWnd

    type(x_box_t) :: xb
    real(WP) :: x

    x = omega

    call xb%initialize(hWnd,IDD_DATA_OMEGA,IDC_OMEGA,x)

    if (xb%run() > 0) then

       x = xb%get()

       if (ZERO < x .and. x < TWO) then
          omega = x

          ! Readjust some params
          omega1 = ONE-omega
          omega4 = 0.25_WP*omega
       else
          call error_msg('OMEGA not in (0,2)!!!'//NL &
               //'Must be 0 < OMEGA < 2...   ')
       end if
    end if
  end subroutine set_omega

  subroutine set_xbounds(hWnd)
    integer(HWND_T), intent(in) :: hWnd

    type(xy_box_t) :: xyb

    call xyb%initialize(hWnd,IDD_DATA_XBOUNDS,IDC_ULEFT,IDC_URIGHT, &
         u_left,u_right)

    if (xyb%run() > 0) then
       u_left = xyb%get_x()
       u_right = xyb%get_y()
    end if
  end subroutine set_xbounds

  subroutine set_ybounds(hWnd)
    integer(HWND_T), intent(in) :: hWnd

    type(xy_box_t) :: xyb

    call xyb%initialize(hWnd,IDD_DATA_YBOUNDS,IDC_UBOTTOM,IDC_UTOP, &
         u_bottom,u_top)

    if (xyb%run() > 0) then
       u_bottom = xyb%get_x()
       u_top = xyb%get_y()
    end if
  end subroutine set_ybounds

  subroutine set_nsout(hWnd)
    integer(HWND_T), intent(in) :: hWnd

    type(x_box_t) :: xb
    real(WP) :: x

    x = real(nsout,WP)

    call xb%initialize(hWnd,IDD_DATA_NSOUT,IDC_NSOUT,x,iflag_x=.true.)

    if (xb%run() > 0) then

       x = xb%get()

       if (x > ZERO) then
          nsout = int(x)
       else
          call error_msg('NSOUT <= 0 !!!'//NL &
               //'Must be NSOUT > 0...   ')
       end if
    end if
  end subroutine set_nsout

  subroutine set_phi_limits(hWnd)
    integer(HWND_T), intent(in) :: hWnd

    type(xy_box_t) :: xyb
    real(WP) :: x, y

    x = phi_min
    y = phi_max

    call xyb%initialize(hWnd,IDD_DATA_PHILIMITS,IDC_PHIMIN,IDC_PHIMAX,x,y)

    if (xyb%run() > 0) then

       x = xyb%get_x()
       y = xyb%get_y()

       if (x < y) then
          phi_min = x
          phi_max = y

          ! Readjust some params
          dphi = (phi_max-phi_min)/MAXCOLOURS
       else
          call error_msg('PHI_MIN >= PHI_MAX !!!'//NL &
               //'Must be PHI_MIN < PHI_MAX...   ')
       end if
    end if
  end subroutine set_phi_limits

  subroutine set_cfgtype(hWnd)
    integer(HWND_T), intent(in) :: hWnd

    integer, parameter :: NUM_BUTTONS = 5
    character(len=*), parameter :: BUTTON_NAMES(NUM_BUTTONS) = [ &
         '&One Box      ', &
         '&Two Box      ', &
         '&Condenser    ', &
         'T&hree Charges', &
         'Charged &Line ' ]
    type(radio_box_t) :: rb

    call rb %initialize(hWnd,IDD_OPTIONS_CFGTYPE,IDC_ONEBOX,BUTTON_NAMES, &
         NUM_BUTTONS,cfg_type)

    if (rb%run() > 0) cfg_type = rb%get()
  end subroutine set_cfgtype

  subroutine set_fldtype(hWnd)
    integer(HWND_T), intent(in) :: hWnd

    integer, parameter :: NUM_BUTTONS = 2
    character(len=*), parameter :: BUTTON_NAMES(NUM_BUTTONS) = [ &
         '&Potential', &
         '&Gradient ' ]
    type(radio_box_t) :: rb

    call rb%initialize(hWnd,IDD_OPTIONS_FLDTYPE,IDC_POTENTIAL,BUTTON_NAMES, &
         NUM_BUTTONS,fld_type)

    if (rb%run() > 0) then
       fld_type = rb%get()

       ! Now we can re-associate the pointer
       if (fld_type == POTENTIAL_FLD) then
          phi => u
       else
          phi => f   ! ...the GRADIENT_FLD
       end if
    end if
  end subroutine set_fldtype

  subroutine disclaimer_dlg(hWnd)
    integer(HWND_T), intent(in) :: hWnd
    type(about_box_t) :: about

    call about%initialize(hWnd,IDD_DISCLAIMER)
    dummy = about%run()
  end subroutine disclaimer_dlg

  subroutine about_dlg(hWnd)
    integer(HWND_T), intent(in) :: hWnd
    type(about_box_t) :: about

    call about%initialize(hWnd,IDD_ABOUT)
    dummy = about%run()
  end subroutine about_dlg

  function process_command(hWnd,wParam)
    integer(BOOL_T) :: process_command
    integer(HWND_T), intent(in) :: hWnd
    integer(WPARAM_T), intent(in) :: wParam

    run_flag = .false.

    select case(lo_word(int(wParam,DWORD_T)))
    case(IDM_FILE_EXIT)
       dummy = MessageBeep(64)
       if (ask_confirmation(hWnd,'Sure you want to exit?   ', &
            'Exit?') == IDYES) then
          dummy = PostMessage(hWnd,WM_CLOSE,0_WPARAM_T,0_LPARAM_T)
       end if
       process_command = TRUE_T
       return

    case(IDM_DATA_NDIV)
       call set_ndiv(hWnd)
       process_command = TRUE_T
       return

    case(IDM_DATA_MAXI)
       call set_maxi(hWnd)
       process_command = TRUE_T
       return

    case(IDM_DATA_EPS)
       call set_eps(hWnd)
       process_command = TRUE_T
       return

    case(IDM_DATA_OMEGA)
       call set_omega(hWnd)
       process_command = TRUE_T
       return

    case(IDM_DATA_XBOUNDS)
       call set_xbounds(hWnd)
       process_command = TRUE_T
       return

    case(IDM_DATA_YBOUNDS)
       call set_ybounds(hWnd)
       process_command = TRUE_T
       return

    case(IDM_DATA_NSOUT)
       call set_nsout(hWnd)
       process_command = TRUE_T
       return

    case(IDM_DATA_PHILIMITS)
       call set_phi_limits(hWnd)
       process_command = TRUE_T
       return

    case(IDM_OPTIONS_CFGTYPE)
       call set_cfgtype(hWnd)
       process_command = TRUE_T
       return

    case(IDM_OPTIONS_FLDTYPE)
       call set_fldtype(hWnd)
       process_command = TRUE_T
       return

    case(IDM_RUNAPP)
       run_flag = .true.
       call setup_grid()
       process_command = TRUE_T
       return

    case(IDM_HELP_DISCLAIMER)
       call disclaimer_dlg(hWnd)
       process_command = TRUE_T
       return

    case(IDM_HELP_ABOUT)
       call about_dlg(hWnd)
       process_command = TRUE_T
       return

    case default
       process_command = FALSE_T
       return
    end select
  end function process_command

  function WndProc(hWnd,iMsg,wParam,lParam) bind(C)
    !GCC$ ATTRIBUTES STDCALL :: WndProc
    integer(LRESULT_T) :: WndProc
    integer(HWND_T), value :: hWnd
    integer(UINT_T), value :: iMsg
    integer(WPARAM_T), value :: wParam
    integer(LPARAM_T), value :: lParam

    logical, save :: first = .true.

    select case(iMsg)
    case(WM_CREATE)
       ! Creating the grid...
       call grid_on()

       WndProc = 0
       return

    case(WM_SIZE)
       if (first) then
          call win32app_setup(lParam,-0.7_WP,1.7_WP,-0.5_WP,1.5_WP)
          first = .false.
       else
          call win32app_setup(lParam)
       end if

       ! Getting the box boundaries... each time, maybe, the mapping changed...
       box_xmin = win32app_xmin()
       box_xmax = win32app_xmax()
       box_ymax = win32app_ymax()

       ! ...and initialize the painting
       call painting_setup(hWnd)

       WndProc = 0
       return

    case(WM_CHAR)
       if (process_char(hWnd,wParam) == TRUE_T) then
          WndProc = 0
          return
       end if
       ! ...else it continues with DefWindowProc

    case(WM_COMMAND)
       if (process_command(hWnd,wParam) == TRUE_T) then
          WndProc = 0
          return
       end if
       ! ...else it continues with DefWindowProc

    case(WM_CLOSE)
       dummy = DestroyWindow(hWnd)
       WndProc = 0
       return

    case(WM_DESTROY)
       if (hBitmap /= NULL_T) then
          dummy = DeleteObject(hBitmap)
       end if

       ! Destroying the grid...
       call grid_off()

       call PostQuitMessage(0)
       ! Commenting out the next two statements, it continues
       ! with DefWindowProc()
       WndProc = 0
       return
    end select

    WndProc = DefWindowProc(hWnd,iMsg,wParam,lParam)
  end function WndProc

  subroutine update_grid()
    ! We use SAVE just to save something at each call
    ! (update_ball_position() is called intensively, at each iteration)
    integer, save :: i, j

    do i = 1, ndiv-1
       do j = 1, ndiv-1
          if (b(i,j)) cycle
          u(i,j) = omega1*u(i,j) &
               +omega4*(u(i+1,j)+u(i-1,j)+u(i,j+1)+u(i,j-1)+s(i,j))
       end do
    end do
  end subroutine update_grid

  subroutine paint_screen(hWnd)
    integer(HWND_T), intent(in) :: hWnd
    ! We use SAVE just to save something at each call
    ! (paint_screen() is called intensively, at each iteration)
    integer(HDC_T), save :: hdc, hdcMem

    if (hBitmap /= NULL_T) then
       hdc = GetDC(hWnd)
       hdcMem = CreateCompatibleDC(hdc)
       dummy = int(SelectObject(hdcMem,hBitmap),INT_T)

       if ((mod(count_iter,nsout) == 0) .or. converg) then
          ! Transfer the off-screen DC to the screen
          dummy = win32app_BitBlt(hdc,hdcMem)
       end if

       dummy = ReleaseDC(hWnd,hdc)

       if (count_iter < max_iter .and. not_converg .and. run_flag) then
          energy_old = energy        ! Save current energy
          count_iter = count_iter+1  ! Update iteration counter...

          call update_grid()         ! ...then UPDATE the grid
          energy = get_grid_energy() ! Get the energy for the updated grid

          ! Set the convergence flags for the updated grid
          not_converg = abs(energy-energy_old) > eps
          converg = .not. not_converg
       end if

       if ((mod(count_iter,nsout) == 0) .or. converg) then
          ! Clear the off-screen DC (hdcMem) for the next drawing
          dummy = win32app_clearDC(hdcMem,BLACKNESS)

          ! Draw (on the off-screen DC) the color scale
          call draw_colorbar(hdcMem)

          ! Draw (on the off-screen DC) grid at current iteration
          call draw_grid(hdcMem)
       end if

       dummy = DeleteDC(hdcMem)
    end if
  end subroutine paint_screen
end module the_app

function WinMain(hInstance,hPrevInstance,lpCmdLine,nCmdShow) &
     bind(C, name='WinMain')
  use, intrinsic :: iso_c_binding, only: C_PTR, C_CHAR, c_sizeof, c_funloc, &
       C_FUNPTR, c_loc
  use win32, only: BLACK_BRUSH, CS_HREDRAW, CS_VREDRAW, CW_USEDEFAULT, &
       DWORD_T, HINSTANCE_T, HWND_T, INT_T, NUL, NULL_PTR_T, NULL_T, &
       PM_REMOVE, WM_QUIT, WS_OVERLAPPEDWINDOW, UINT_T, &
       MSG_T, WNDCLASSEX_T, &
       arrow_cursor, CreateWindowEx, DispatchMessage, dummy, error_msg, &
       ExitProcess, GetStockObject, LoadCursor, LoadIcon, make_int_resource, &
       make_int_resource_C_PTR, PeekMessage, RegisterClassEx, ShowWindow, &
       TranslateMessage, UpdateWindow
  use the_app, only: IDI_POISSON2D, IDM_MAINMENU, &
       paint_screen, WndProc

  implicit none

  !GCC$ ATTRIBUTES STDCALL :: WinMain
  integer(INT_T) :: WinMain
  integer(HINSTANCE_T),  value :: hInstance
  integer(HINSTANCE_T),  value :: hPrevInstance
  type(C_PTR), value :: lpCmdLine ! LPSTR
  integer(INT_T), value :: nCmdShow

  character(kind=C_CHAR,len=128), target :: app_name = &
       'Poisson2D'//NUL
  character(kind=C_CHAR,len=*), parameter :: WINDOW_CAPTION = &
       'Poisson Equation in 2D'//NUL
  type(WNDCLASSEX_T) :: WndClass
  integer(HWND_T) :: hWnd
  type(MSG_T) :: msg

  WndClass%cbSize = int(c_sizeof(Wndclass),UINT_T)
  WndClass%style = ior(CS_HREDRAW,CS_VREDRAW)
  WndClass%lpfnWndProc = c_funloc(WndProc)
  WndClass%cbClsExtra = 0
  WndClass%cbWndExtra = 0
  WndClass%hInstance = hInstance
  WndClass%hIcon = LoadIcon(hInstance,make_int_resource(IDI_POISSON2D))
  WndClass%hCursor = LoadCursor(NULL_T,arrow_cursor())
  WndClass%hbrBackground = GetStockObject(BLACK_BRUSH)
  WndClass%lpszMenuName = make_int_resource_C_PTR(IDM_MAINMENU)
  WndClass%lpszClassName = c_loc(app_name(1:1))
  WndClass%hIconSm = LoadIcon(hInstance,make_int_resource(IDI_POISSON2D))

  if (RegisterClassEx(WndClass) == 0) then
     call error_msg('Window Registration Failure!   ')
     call ExitProcess(0_UINT_T)
     WinMain = 0
     !return
  end if

  hWnd = CreateWindowEx(0_DWORD_T, &
       app_name, &
       WINDOW_CAPTION, &
       WS_OVERLAPPEDWINDOW, &
       CW_USEDEFAULT,CW_USEDEFAULT,CW_USEDEFAULT,CW_USEDEFAULT, &
       NULL_T,NULL_T,hInstance,NULL_PTR_T)

  if (hWnd == NULL_T) then
     call error_msg('Window Creation Failure!   ')
     call ExitProcess(0_UINT_T)
     WinMain = 0
     !return
  end if

  dummy = ShowWindow(hWnd,nCmdShow)
  dummy = UpdateWindow(hWnd)

  ! See: Charles Petzold "Programming Windows", 5th ed., pag. 162
  ! 'Random Rectangles'
  do
     if (PeekMessage(msg,NULL_T,0,0,PM_REMOVE) /= 0) then
        if (msg%message == WM_QUIT) exit
        dummy = TranslateMessage(msg)
        dummy = int(DispatchMessage(msg),INT_T)
     else
        call paint_screen(hWnd)
     end if
  end do

  call ExitProcess(int(msg%wParam,UINT_T))
  WinMain = 0
end function WinMain
