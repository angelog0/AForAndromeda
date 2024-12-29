!
! Author: Angelo Graziosi
!
!   created   : May 28, 2016
!   last edit : Sep 27, 2016
!
!   The Camera View transformation module
!
! REFERENCES
!
!   1. General Graphics Resources in
!        https://www.willamette.edu/~gorr/classes/GeneralGraphics
!

module camera_view_m
  use kind_consts, only: WP
  use math_consts, only: DEG2RAD, ZERO => Z0, ONE => Z1

  implicit none
  private

  type, public :: camera_view_t
     private
     real(WP) :: k_view = ZERO
     real(WP) :: phi = ZERO
     real(WP) :: theta = ZERO
     real(WP) :: alpha = ZERO

     real(WP) :: rot_m(3,3) = ZERO
   contains
     private
     procedure, pass(self), public :: setup
     procedure, pass(self), public :: do_projection
     procedure, pass(self), public :: get_radius
  end type camera_view_t

contains

  subroutine setup(self,k_view,phi,theta,alpha)
    class(camera_view_t), intent(inout) :: self
    real(WP), intent(in) :: k_view      ! Distance along Z-axis
    real(WP), intent(in) :: phi, theta  ! In degrees...
    real(WP), intent(in), optional :: alpha  ! In degrees...
    !
    ! Set up the camera_view_t object
    !
    self%k_view = abs(k_view)

    ! Conversion from degrees to radians
    self%phi = phi*DEG2RAD
    self%theta = theta*DEG2RAD

    if (present(alpha)) then
       self%alpha = alpha*DEG2RAD
    else
       self%alpha = 45*DEG2RAD ! Default: 45_deg
    end if

    ! With PHI and THETA we can compute ROT_M
    self%rot_m(1,1) = -sin(self%phi)
    self%rot_m(1,2) = cos(self%phi)
    self%rot_m(1,3) = ZERO

    self%rot_m(2,3) = sin(self%theta)
    self%rot_m(3,3) = cos(self%theta)

    ! -cos(theta)*cos(phi), -cos(theta)*sin(phi)
    self%rot_m(2,1) = -self%rot_m(3,3)*self%rot_m(1,2)
    self%rot_m(2,2) = self%rot_m(3,3)*self%rot_m(1,1)

    ! sin(theta)*cos(phi), sin(theta)*sin(phi)
    self%rot_m(3,1) = self%rot_m(2,3)*self%rot_m(1,2)
    self%rot_m(3,2) = -self%rot_m(2,3)*self%rot_m(1,1)
  end subroutine setup

  subroutine do_projection(self,p,u,v)
    class(camera_view_t), intent(in) :: self
    real(WP), intent(in) :: p(3)
    real(WP), intent(out) :: u, v

    real(WP) :: pv(3)

    pv = matmul(self%rot_m,p)

    v = (pv(3)/self%k_view)-ONE

    u = -pv(1)/v
    v = -pv(2)/v
  end subroutine do_projection

  ! The image circle radius..
  function get_radius(self) result(r)
    class(camera_view_t), intent(in) :: self
    real(WP) :: r

    r = self%k_view*atan(self%alpha/2)
  end function get_radius
end module camera_view_m
