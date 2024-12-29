!
! Author: Angelo Graziosi
!
!   created   : Jul 25, 2018
!   last edit : Jul 29, 2018
!
!   Contour Plots module
!
! REFERENCES
!
!   http://paulbourke.net/papers/conrec
!
! NOTES
!
!   Adapted from:
!
!     http://paulbourke.net/papers/conrec/conrec_for.txt
!     http://paulbourke.net/papers/conrec/conrec.c
!

module contour_plots
  use kind_consts, only: WP

  implicit none
  private

  abstract interface
     subroutine vecout(x1,y1,x2,y2,z)
       import :: WP
       real(WP), intent(in) :: x1, y1, x2, y2
       real(WP), intent(in) :: z
     end subroutine vecout
  end interface

  public :: conrec

contains

  !
  ! CONREC is a contouring subroutine for rectangularily spaced data.
  !
  ! It emits calls to a line drawing subroutine (CONREC_LINE) supplied
  ! by the user which draws a contour map corresponding to data,
  ! d(:,:), on a randomly spaced rectangular grid. The coordinates
  ! emitted are in the same units given in the x(:) and y(:) arrays.
  !
  ! Any number of contour levels may be specified but they must be
  ! in order of increasing value.
  !
  subroutine conrec(d,x,y,z,conrec_line)
    real(WP), intent(in) :: d(:,:), x(:), y(:), z(:)
    procedure(vecout) :: conrec_line

    integer, parameter :: IM(1:4) = [ 0, 1, 1, 0 ], JM(1:4) = [ 0, 0, 1, 1 ]
    integer, parameter :: CASTAB(-1:1,-1:1,-1:1) = reshape([ &
         0,0,9,  0,1,5,  7,4,8, &
         0,3,6,  2,3,2,  6,3,0, &
         8,4,7,  5,1,0,  9,0,0],&
         shape(CASTAB))

    integer :: i, j, k, m, m1, m2, m3, ilb, iub, jlb, jub, nc
    real(WP) :: h(0:4), xh(0:4),yh(0:4), dmin, dmax, x1, x2, y1, y2
    integer :: sh(0:4), case_val

    !integer :: p1,p2
    !real(WP) :: xsect, ysect

    ! Use statement functions for the line intersections: OBSOLETE for
    ! recent standards (>= f95), so we use internal functions in the same
    ! manner...
    !xsect(p1,p2) = (h(p2)*xh(p1)-h(p1)*xh(p2))/(h(p2)-h(p1))
    !ysect(p1,p2) = (h(p2)*yh(p1)-h(p1)*yh(p2))/(h(p2)-h(p1))

    ilb = 1
    iub = size(d,1)

    jlb = 1
    jub = size(d,2)

    nc = size(z)

    ! Scan the arrays, top down, left to right within rows
    do j = jub-1, jlb, -1
       do i = ilb, iub-1
          dmin = min(d(i,j),d(i,j+1),d(i+1,j),d(i+1,j+1))
          dmax = max(d(i,j),d(i,j+1),d(i+1,j),d(i+1,j+1))
          if (dmax >= z(1) .and. dmin <= z(nc)) then
             do k = 1, nc
                if (z(k) >= dmin .and. z(k) <= dmax) then
                   do m = 4, 0, -1
                      if (m > 0) then
                         h(m) = d(i+IM(m),j+JM(m))-z(k)
                         xh(m) = x(i+IM(m))
                         yh(m) = y(j+JM(m))
                      else
                         h(0) = 0.25_WP*(h(1)+h(2)+h(3)+h(4))
                         xh(0) = 0.5_WP*(x(i)+x(i+1))
                         yh(0) = 0.5_WP*(y(j)+y(j+1))
                      end if
                      if (h(m) > 0.0_WP) then
                         sh(m) = +1
                      else if (h(m) < 0.0_WP) then
                         sh(m) = -1
                      else
                         sh(m) = 0
                      end if
                   end do

                   !
                   ! Note: at this stage the relative heights of the
                   ! corners and the centre are in the h array, and
                   ! the corresponding coordinates are in the xh and
                   ! yh arrays. The centre of the box is indexed by 0
                   ! and the 4 corners by 1 to 4 as shown below. Each
                   ! triangle is then indexed by the parameter m, and
                   ! the 3 vertices of each triangle are indexed by
                   ! parameters m1,m2,and m3.  It is assumed that the
                   ! centre of the box is always vertex 2 though this
                   ! is important only when all 3 vertices lie exactly
                   ! on the same contour level, in which case only the
                   ! side of the box is drawn.
                   !
                   !   vertex 4 +-------------------+ vertex 3
                   !            | \               / |
                   !            |   \    m=3    /   |
                   !            |     \       /     |
                   !            |       \   /       |
                   !            |  m=4    X   m=2   |  the centre is vertex 0
                   !            |       /   \       |
                   !            |     /       \     |
                   !            |   /    m=1    \   |
                   !            | /               \ |
                   !   vertex 1 +-------------------+ vertex 2
                   !

                   ! Scan each triangle in the box
                   do m = 1, 4
                      m1 = m
                      m2 = 0
                      if (m /= 4) then
                         m3 = m+1
                      else
                         m3 = 1
                      end if
                      case_val = CASTAB(sh(m1),sh(m2),sh(m3))
                      if (case_val /= 0) then
                         select case(case_val)
                         case (1)
                            ! Line between vertices 1 and 2
                            x1 = xh(m1)
                            y1 = yh(m1)
                            x2 = xh(m2)
                            y2 = yh(m2)
                         case (2)
                            ! Line between vertices 2 and 3
                            x1 = xh(m2)
                            y1 = yh(m2)
                            x2 = xh(m3)
                            y2 = yh(m3)
                         case (3)
                            ! Line between vertices 3 and 1
                            x1 = xh(m3)
                            y1 = yh(m3)
                            x2 = xh(m1)
                            y2 = yh(m1)
                         case (4)
                            ! Line between vertex 1 and side 2-3
                            x1 = xh(m1)
                            y1 = yh(m1)
                            x2 = xsect(m2,m3)
                            y2 = ysect(m2,m3)
                         case (5)
                            ! Line between vertex 2 and side 3-1
                            x1 = xh(m2)
                            y1 = yh(m2)
                            x2 = xsect(m3,m1)
                            y2 = ysect(m3,m1)
                         case (6)
                            ! Line between vertex 3 and side 1-2
                            x1 = xh(m3)
                            y1 = yh(m3)
                            x2 = xsect(m1,m2)
                            y2 = ysect(m1,m2)
                         case (7)
                            ! Line between sides 1-2 and 2-3
                            x1 = xsect(m1,m2)
                            y1 = ysect(m1,m2)
                            x2 = xsect(m2,m3)
                            y2 = ysect(m2,m3)
                         case (8)
                            ! Line between sides 2-3 and 3-1
                            x1 = xsect(m2,m3)
                            y1 = ysect(m2,m3)
                            x2 = xsect(m3,m1)
                            y2 = ysect(m3,m1)
                         case (9)
                            ! Line between sides 3-1 and 1-2
                            x1 = xsect(m3,m1)
                            y1 = ysect(m3,m1)
                            x2 = xsect(m1,m2)
                            y2 = ysect(m1,m2)
                         end select
                         call conrec_line(x1,y1,x2,y2,z(k))
                      end if
                   end do
                end if
             end do
          end if
       end do
    end do

  contains

    function xsect(p1,p2) result(r)
      integer, intent(in) :: p1,p2
      real(WP) :: r

      r = (h(p2)*xh(p1)-h(p1)*xh(p2))/(h(p2)-h(p1))
    end function xsect

    function ysect(p1,p2) result(r)
      integer, intent(in) :: p1,p2
      real(WP) :: r

      r = (h(p2)*yh(p1)-h(p1)*yh(p2))/(h(p2)-h(p1))
    end function ysect

  end subroutine conrec
end module contour_plots
