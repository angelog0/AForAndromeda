!
! Author: ANGELO GRAZIOSI
!
!   created   : May 02, 2015
!   last edit : Aug 24, 2019
!
!   Solving Thomas-Fermi Equation
!
! DESCRIPTION
!
!   BEST SLOPE for decreasing function u(x) (>= 0) satisying the
!   Thomas-Fermi equation and u(0) = 1 condition.
!
!   With RA15 method and MU_ERR = 5.0E-9 (H = 1.953125E-3, LL = 12), we find
!
!
!     MU = 1.5880710214376457   DELTA_MU = 2.9802322387695339E-009
!      X = 70.005859375000000       y(X) = 7.9253250134681805E-007
!
!     Computing MU with Majorana method ... done!
!     MU(MAJO) = 1.5880710214220619 with N = 101 coefficients...
!
!     Completed in 1.484 seconds!
!
!   With RA15 method and MU_ERR = 1.0E-14 (H = 1.953125E-3, LL = 20), we find
!
!
!     MU = 1.5880710226113661   DELTA_MU = 5.6843418860808065E-015
!      X = 449.25195312500000       y(X) =    2.5403426114828267E-010
!
!     Computing MU with Majorana method ... done!
!     MU(MAJO) = 1.5880710226113872 with N = 1001 coefficients...
!
!     Completed in   11.296 seconds!
!
!   Other interesting results are obtained with MU_ERR = 1.0E-15, 1.0E-16...
!
!   With GBS method and MU_ERR = 5.0E-9 (H = 5.0E-3, EPS = 1.0E-12), we find
!
!
!     MU = 1.5880710214376457   DELTA_MU = 2.9802322387695339E-009
!      X = 69.490000000009289       y(X) = 8.1768135084719740E-007
!
!     Computing MU with Majorana method...
!     MU(MAJO) = 1.5880710214220619  with N = 101 coefficients...
!
!     Completed in 0.125 seconds!
!
!   These results could be compared with
!
!     u'(0) = -MU = -1.588071022611375312718684508
!
!   as reported in
!
!     P.Amore et al.,
!     Accurate calculation of the solutions to the Thomas-Fermi equations,
!     http://arxiv.org/pdf/1205.1704v2.pdf (http://arxiv.org/abs/1205.1704)
!
!
! HOW TO BUILD THE APP
!
!   cd Thomas-Fermi
!
!   rm -rf *.mod; \
!   gfortran[-mp-X] -std=f2008 -O3 [-march=native -funroll-loops] -Wall \
!     -Wno-unused-dummy-argument \
!     ../basic-modules/{{kind,math}_consts,ft_timer_m,getdata}.f90 \
!     ../ode-modules/everhart_integrator.f90 \
!     thomas_fermi-ra15.f90 -o thomas_fermi-ra15$EXE; \
!   rm -rf *.mod
!
!   thomas_fermi-ra15$EXE
!
!   where, for the build on GNU/Linux [OSX+MacPorts X server], is:
!
!     EXE = .out
!
!   while for the build on MINGW{32,64} is:
!
!     EXE = -$MSYSTEM (or EMPTY)
!

module thomas_fermi_lib
  use kind_consts, only: WP
  use math_consts, only: ZERO => Z0, ONE => Z1, THREE => Z3, FOUR => Z4, &
       Z73, Q1_2, Q1_3, Q4_3, Q3_16

  implicit none
  private

  integer, parameter :: NC_MAX = 1000

  ! ONE/512 = 0.001953125
  integer :: nc_majo = 100, ll = 12
  real(WP) :: h0 = ONE/512, mu_err = 5.0E-9_WP

  !
  ! To avoid the singularity at the origin, we use this transformation
  !
  !   u(x) = (1+(4/3) * x**(3/2))*y(x)
  !
  ! and the Thomas-Fermi eq. for u(x),
  !
  !   u''(x) = u(x) ** (3/2) / sqrt(x)
  !
  ! becomes, for y(x),
  !
  !   (1+(4/3) * x**(3/2))*y''(x) + 4*sqrt(x)*y'(x)
  !
  !     + (y(x)/sqrt(x))*(1-sqrt(y(x))*(1+(4/3) * x**(3/2))**(3/2)) = 0
  !
  ! Notice, the boundary conditions for neutral atoms satisfied by u(x),
  !
  !   u(0) = 1,  u(+inf) = 0
  !
  ! are the same for y(x), y(0) = 1,  y(+inf) = 0
  !
  ! Notice also, that u'(0) = y'(0).
  !

  public :: run_app

contains

  subroutine input_data()
    use getdata, only: get

    call get('H0 = ',h0)
    write(*,*)

    call get('MU_ERR = ',mu_err)
    write(*,*)

    call get('LL = ',ll)
    write(*,*)
    if (ll > 20) error stop ': LL too large parameter (INPUT_DATA).'

    call get('NC_MAJO =',nc_majo)
    write(*,*)

    if (nc_majo < 3) then
       write(*,*) 'NC_MAJO TOO LOW. DEFAULTING TO 10'
       write(*,*)
       nc_majo = 10
    end if
    if (nc_majo > NC_MAX) then
       write(*,*) 'NC_MAJO TOO HIGH. DEFAULTING TO ', NC_MAX
       write(*,*)
       nc_majo = NC_MAX
    end if
  end subroutine input_data

  subroutine run_app()
    use ft_timer_m, only: ft_timer_t
    use everhart_integrator, only: ra15_on, ra15_run, ra15_off

    integer, parameter :: NV = 1, NCLASS = 2

    ! We assume mu in (1.5,1.6) and an initial guess mu = 1.6
    real(WP) :: x, x1, y1_old, y(NV), yp(NV), h, hp, mu, delta_mu
    type(ft_timer_t) :: run_timer

    call input_data()

    call ra15_on(NV,ll,NCLASS,force)

    mu = 1.6_WP
    delta_mu = 1.6_WP-1.5_WP

    ! Just to start from a new line...
    write(*,*) 'Please wait, we are working ...'
    write(*,*)
    call run_timer%start()

    ! Start 'run_app()'...

    do
       h = h0
       x = ZERO
       y = ONE
       yp = -mu

       ! Just a bit bigger, so that the following loop is executed
       ! at least once...
       y1_old = y(1)+0.1_WP

       do while (y(1) >= ZERO .and. y(1) < y1_old)
          !print *, x,y(1)

          y1_old = y(1)

          ! We take a RA15 integrator step
          hp = h
          x1 = x+hp
          call ra15_run(x,x1,y,yp,hp)
          x = x1
          !print *, x,y(1)
       end do

       write(*,*) 'MU = ', mu, 'X = ', x

       if (abs(delta_mu) < mu_err) exit

       delta_mu = sign(abs(Q1_2*delta_mu),y(1))
       mu = mu+delta_mu
    end do

    write(*,*)
    write(*,*) 'MU = ', mu, 'DELTA_MU = ', delta_mu
    write(*,*) ' X = ', x,  '    y(X) = ', y(1)

    call majo_result()

    ! Stop 'run_app()'...

    call run_timer%stop()

    write(*,*)
    write(*,'(A,F8.3,A)') 'Completed in ',run_timer%elapsed_time(), &
         ' seconds!'

    call ra15_off()

  contains

    subroutine force(x,y,yp,f)
      real(WP), intent(in) :: x, y(:), yp(:)
      real(WP), intent(out) :: f(:)

      real(WP), save :: a, b

      if (x == ZERO) then
         f(1) = ZERO
      else
         a = sqrt(x)
         b = ONE+Q4_3*x*a

         ! We use abs(y(1)) as argument of sqrt() to avoid troubles when
         ! y(1) < 0
         f(1) = (y(1)*(b*sqrt(abs(y(1))*b)-ONE)-FOUR*x*yp(1))/(a*b)
      end if
    end subroutine force

    subroutine majo_result()
      real(WP), parameter :: R73 = sqrt(Z73), &
           A1 = 9-R73, A2 = (6497-755*R73)/152, &
           R3_16 = Q3_16 ** Q1_3
      !
      ! A simple implementation of the Majorana method as described in
      !
      !   S. Esposito, Majorana solution of the Thomas-Fermi equation,
      !   Am. J. Phys. 70, 852 (2002).
      !
      integer, save :: m, n, &
           mm2, mm1, nm1, &
           m1, m3_2, m6, m7, m8_2, &
           n1, n4_2, n7
      real(WP), save :: sum_val, a(0:NC_MAX), tt(NC_MAX-2)

      write(*,*)
      write(*,'(A)',advance='NO') 'Computing MU with Majorana method ...'

      a(0:2) = [ONE, A1, A2]
      ! print *
      ! print *, a(0)
      ! print *, a(1)
      ! print *, a(2)

      do m = 3, nc_majo
         mm1 = m-1
         mm2 = m-2
         m1 = m+1
         m3_2 = 2*(m+3)
         m6 = m+6
         m7 = m+7
         m8_2 = 2*(m+8)

         n = mm2
         nm1 = n-1
         n1 = n+1
         n4_2 = 2*(n+4)
         n7 = n+7
         tt(n) = n1*a(n1)-n4_2*a(n)+n7*a(nm1)

         sum_val = ZERO
         do n = 1, mm2
            sum_val = sum_val+a(m-n)*tt(n)
         end do

         ! Partial value
         a(m) = sum_val+a(mm1)*(m7-m3_2*A1)+a(mm2)*m6*A1

         ! Final value
         a(m) = a(m)/(m8_2-m1*A1)
         !print *, a(m)
      end do

      ! The MU value as computed with Majorana method
      sum_val = R3_16*sum(a(:nc_majo))

      write(*,*) 'done!'

      write(*,*)
      write(*,*) 'MU(MAJO) = ', sum_val, &
           'with N = ', nc_majo+1, 'coefficients...'
    end subroutine majo_result
  end subroutine run_app
end module thomas_fermi_lib

program thomas_fermi
  use thomas_fermi_lib

  implicit none

  call run_app()
end program thomas_fermi
