!
! EGF: Ellipsoid Gravitational Field
!
! First implementation: May 31, 2025
!
! From a suggestion of ChatGPT
!

module ellipsoid_gravitation
  use :: kind_consts, only: WP
  use :: math_consts, only: ZERO => Z0, ONE => Z1, TWO => Z2, HF => Q1_2

  implicit none
  private

  ! Gravitational constant (m**3 kg**(-1) s**(-2))
  real(WP), parameter :: G = 6.67430E-11_WP

  ! Grid divisions
  integer :: nx, ny, nz

  ! Semiaxes of the ellipsoid and "lower" end of the integration domain
  real(WP) :: a, b, c, x1, y1, z1

  ! Grid steps and grid volume by G
  real(WP) :: dx, dy, dz, dV

  abstract interface
     function density_fcn(p) result(fp)
       import :: WP
       real(WP), intent(in) :: p(:)
       real(WP) :: fp
     end function density_fcn
  end interface

  procedure(density_fcn), pointer :: f => null()

  public :: density_fcn, ellipsoid_setup, ellipsoid_gfield

contains

  subroutine ellipsoid_setup(mx, my, mz, aa, bb, cc, ff)
    integer, intent(in) :: mx, my, mz
    real(WP), intent(in) :: aa, bb, cc
    procedure(density_fcn) :: ff

    nx = mx
    ny = my
    nz = mz
    a = aa
    b = bb
    c = cc

    ! Grid setup
    dx = TWO * a / nx
    dy = TWO * b / ny
    dz = TWO * c / nz
    dV = G * dx * dy * dz

    ! "Lower" end of the integration domain
    x1 = -(a + HF * dx)
    y1 = -(b + HF * dy)
    z1 = -(c + HF * dz)

    f => ff
  end subroutine ellipsoid_setup

  subroutine ellipsoid_gfield(q,gfield)
    real(WP), intent(in) :: q(:)        ! The point where the field is computed
    real(WP), intent(out) :: gfield(:)  ! The FIELD

    integer, save :: i, j, k

    ! Vector position and its modulus, ellipsoid point and density
    ! at that point, mass of the grid cell by G: dMu = G*dm = G*rho*dV
    real(WP), save :: d(3), r, p(3), fp

    associate (x => p(1), y => p(2), z => p(3))

      gfield = ZERO

      do i = 1, nx
         x = x1 + i * dx         ! -a+dx*(i-HF)
         do j = 1, ny
            y = y1 + j * dy      ! -b+dy*(j-HF)
            do k = 1, nz
               z = z1 + k * dz   ! -c+dz*(k-HF)

               ! Checks whether the point is inside the ellipsoid
               if ((x/a)**2 + (y/b)**2 + (z/c)**2 <= ONE) then

                  ! Computing p(:)-q(:) = -(q-p): : this avoids g =
                  ! g-G*dm*(q-p)/r**3...  See STAR_WALK implementation
                  d = p-q

                  r = norm2(d)

                  if (r /= ZERO) then
                     ! Density
                     fp = f(p)

                     ! Computing the mass by G (ie MU) of the grid cell reusing the
                     ! RHO variable
                     fp = fp*dV

                     ! Computing the contribute of the current
                     ! ellipsoid element to GFIELD
                     d = fp*d/r**3

                     gfield = gfield+d  ! g = g - G * (rho*dV)*(q-p)/r**3
                  end if
               end if
            end do
         end do
      end do

    end associate

  end subroutine ellipsoid_gfield

end module ellipsoid_gravitation
