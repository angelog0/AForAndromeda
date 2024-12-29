!
! Author: ANGELO GRAZIOSI
!
!   created   : Feb 10, 2014
!   last edit : Jan 05, 2024
!
!   A new implementation of shade.kumac PAW macro.  See
!   http://paw.web.cern.ch/paw/allfaqs.html
!
!
! LICENSE
!
!     Copyright (c) 2014-2024, ANGELO GRAZIOSI.  ALL RIGHTS RESERVED.
!
! ANGELO GRAZIOSI retains all intellectual property and proprietary rights
! in and to this software, related documentation and any modifications
! thereto.  Any use, reproduction, disclosure or distribution of this
! software and related documentation without an express license agreement
! from ANGELO GRAZIOSI is strictly prohibited.
!
!          THIS CODE AND INFORMATION ARE PROVIDED "AS IS" WITHOUT
!   WARRANTY OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING BUT
!   NOT LIMITED TO THE IMPLIED WARRANTIES OF MERCHANTABILITY AND/OR
!   FITNESS FOR A PARTICULAR PURPOSE.
!

module shading_colors

  implicit none
  private

  type :: color_rgb_t
     integer :: r, g, b
  end type color_rgb_t

  integer, parameter :: NMXPT = 20

  ! Borland alike (opaque) colors
  integer, parameter, public :: BLACK = int(z'FF000000')
  integer, parameter, public :: BLUE = int(z'FF800000')
  integer, parameter, public :: GREEN = int(z'FF008000')
  integer, parameter, public :: CYAN = ior(GREEN,BLUE)
  integer, parameter, public :: RED = int(z'FF000080')
  integer, parameter, public :: MAGENTA = ior(RED,BLUE)
  integer, parameter, public :: BROWN = ior(RED,GREEN)
  integer, parameter, public :: GRAY = ior(BROWN,BLUE)
  integer, parameter, public :: LGRAY = int(z'FFC0C0C0')
  integer, parameter, public :: LBLUE = int(z'FFFF0000')
  integer, parameter, public :: LGREEN = int(z'FF00FF00')
  integer, parameter, public :: LCYAN = ior(LGREEN,LBLUE)
  integer, parameter, public :: LRED = int(z'FF0000FF')
  integer, parameter, public :: LMAGENTA = ior(LRED,LBLUE)
  integer, parameter, public :: YELLOW = ior(LRED,LGREEN)
  integer, parameter, public :: WHITE = ior(YELLOW,LBLUE)

  integer, parameter, public :: BORLAND_PALETTE(0:15) = [ &
       BLACK, &
       BLUE, &
       GREEN, &
       CYAN, &
       RED, &
       MAGENTA, &
       BROWN, &
       GRAY, &
       LGRAY, &
       LBLUE, &
       LGREEN, &
       LCYAN, &
       LRED, &
       LMAGENTA, &
       YELLOW, &
       WHITE &
       ]

  integer, parameter, public :: MAX_COLOUR_INDEX = 255
  integer, parameter, public :: MAX_COLOURS = MAX_COLOUR_INDEX+1

  type(color_rgb_t) :: cr_colors(0:MAX_COLOUR_INDEX)

  integer :: npt, idx(NMXPT)
  integer :: r(NMXPT), g(NMXPT), b(NMXPT)

  public :: color_rgb_t
  public :: RGBA, shading_setup, get_shading_color

contains

  ! Adapted from RGB() function in WIN32 module Return a RGBA color in
  ! a SDL2_gfx format (AABBGGRR), which is similar to COLORREF (WIN32)
  function RGBA(red,green,blue,alpha) result(color)
    integer, intent(in) :: red, green, blue
    integer, intent(in), optional :: alpha
    integer :: color ! AABBGGRR

    ! See sdl2_pixels.f90
    integer, parameter :: ALPHA_OPAQUE = 255

    if (present(alpha)) then
       color = ior(ior(ior((red),ishft((green),8)),ishft((blue),16)),&
            ishft(alpha,24))
    else
       color = ior(ior(ior((red),ishft((green),8)),ishft((blue),16)),&
            ishft(ALPHA_OPAQUE,24))
    end if
  end function RGBA

  subroutine set_shade(idxi,ri,gi,bi)
    integer, intent(in) :: idxi, ri, gi, bi

    if (idxi < 0) then
       npt = 0
       return
    end if

    npt = npt+1

    if (npt > NMXPT) stop ': Too many colours. (SET_SHADE)'

    idx(npt) = idxi
    r(npt)   = ri
    g(npt)   = gi
    b(npt)   = bi
  end subroutine set_shade

  subroutine shade()
    integer :: i, ii, i1, i2, j, n, rs, gs, bs, r1, g1, b1, r2, g2, b2
    real :: scale

    if (npt < 2) stop ': At least two colours are needed. (SHADE)'

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

          cr_colors(ii) = color_rgb_t(rs,gs,bs)
       enddo
    enddo
  end subroutine shade

  subroutine shading_setup()
    !cr_colors(0) = color_rgb_t(0,0,0)           ! BLACK
    !cr_colors(255) = color_rgb_t(255,255,255)   ! WHITE

    ! The first call to set_shade() MUST be this INITIALIZATION
    call set_shade( -1,0,0,0)

    call set_shade(  0,  0,  0,128)
    call set_shade( 40,  0,  0,255)
    call set_shade(100,  0,255,255)
    call set_shade(120,  0,255,128)
    call set_shade(160,255,255,  0)
    call set_shade(255,128,  0,  0)
    call shade()
  end subroutine shading_setup

  function get_shading_color(i) result(c)
    integer, intent(in) :: i
    type(color_rgb_t) :: c

    c = cr_colors(i)
  end function get_shading_color
end module shading_colors
