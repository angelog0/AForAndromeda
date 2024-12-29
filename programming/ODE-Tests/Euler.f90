!
! Author: ANGELO GRAZIOSI
!
!   created   : Aug 13, 2019
!   last edit : Sep 11, 2019
!
!   Solving Euler's equation of motion for a rigid body without
!   external forces.
!
! DESCRIPTION
!
!   The exact solution is Y(1:3) = (sn(x;k),cn(x;k),dn(x;k))
!
! REFERENCES
!
!   R. Bulirsch and J. Stoer, Numerical treatment of ordinary
!   differential equations by extrapolationmethods, Numer. Math. 8
!   (1966) 1-13.
!
!   https://keisan.casio.com/exec/system/1180573437
!
! Build with:
!
!   cd ODE-Tests
!
!   rm -rf *.mod; \
!     gfortran -std=f2008 -O3 -Wall -Wno-unused-dummy-argument \
!       ../basic-modules/{{kind,math}_consts,getdata}.f90 \
!       ../ode-modules/{everhart,ode}_integrator.f90 Euler.f90 -o Euler.out; \
!   rm -rf *.mod
!

module euler_lib
  use kind_consts, only: WP
  use ode_integrator, only: ode_on, ode_integrate, ode_off

  implicit none
  private

  integer, parameter :: NEQ = 3

  integer :: ll = 12

  ! KQ = k**2; try also k = 0.8, i.e. KQ = 0.64 with step H0 = 0.1
  ! from T0 = 0 to T1 = 2
  real(WP) :: kq = 0.51_WP

  real(WP) :: t0 = 0.0_WP, t1 = 4.0_WP, h0 = 7.8125E-3_WP, &
       y0(NEQ) = [ 0.0_WP, 1.0_WP, 1.0_WP ]

  public:: run_app

contains

  subroutine sub(t,y,f)
    real(WP), intent(in) :: t, y(:)
    real(WP), intent(out) :: f(:)

    f(1) = y(2)*y(3)
    f(2) = -y(1)*y(3)
    f(3) = -kq*y(1)*y(2)
  end subroutine sub

  subroutine display_data(t,y)
    real(WP), intent(in) :: t, y(:)

    write(*,'(4f16.12)') t, y(:)
  end subroutine display_data

  subroutine run_app()
    use getdata, only: get

    character(len=*), parameter :: METHOD(4) = [ 'RK4', 'GBS', 'RKM', 'R15' ]

    integer :: id_method = 2

    write(*,*) 'Choose the method:'
    write(*,*) '  1 : RK4'
    write(*,*) '  2 : GBS'
    write(*,*) '  3 : RKM'
    write(*,*) '  4 : R15'
    call get('ID_METHOD =',id_method)
    write(*,*)

    ! Correction so that ID_METHOD is always in [1,4]
    id_method = mod(id_method-1,4)+1

    ! For GBS, RKM or R15 step, the default initial H0 step can be greather..
    if (id_method /= 1) h0 = 0.0156250_WP

    call get('T0 = ',t0)
    call get('T1 = ',t1)
    call get('H0 = ',h0)
    write(*,*)

    if (id_method /= 1) then
       call get('LL = ',ll)
       write(*,*)
       if ((id_method /= 4 .and. ll > 13) .or. &
            (id_method == 4 .and. ll > 20)) &
            stop ': LL too big parameter (RUN_APP).'
    end if

    call get('KQ = ',kq)
    write(*,*)

    write(*,'(a/)') '    USING '//METHOD(id_method)//' METHOD'

    call ode_on(NEQ,sub,display_data,id_method)
    call ode_integrate(t0,t1,y0,h0)
    call ode_off()

  end subroutine run_app
end module euler_lib

program euler
  use euler_lib

  implicit none

  call run_app()
end program euler
