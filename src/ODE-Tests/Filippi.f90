!
! Author: ANGELO GRAZIOSI
!
!   created   : Aug 15, 2019
!   last edit : Nov 09, 2022
!
!   Restricted Problem of Three Bodies (Earth-Moon-Spaceship).
!
! DESCRIPTION
!
!   This problem is often used to test different methods of numerical
!   integration of differential equations. The same problem is
!   described by E. Everhart at section 3.1.
!
! REFERENCES
!
!   1. R. Bulirsch and J. Stoer, Numerical treatment of ordinary
!      differential equations by extrapolationmethods, Numer. Math. 8
!      (1966) 113.
!
!   2. See ref. 6 in [1] above.
!
!   3. E. Everhart, An efficient integrator that uses Gauss-Radau
!      spacings, International Astronomical Union Colloquium, Volume
!      83 (Dynamics of Comets: Their Origin and Evolution)
!      https://www.cambridge.org/core/services/aop-cambridge-core/content/view/F942BC9121C74CC2FA296050FC18D824/S0252921100083913a.pdf/an_efficient_integrator_that_uses_gaussradau_spacings.pdf
!
!   4. T. Levi Civita, Traiettorie singolari ed urti nel problema
!      ristretto dei tre corpi, Ann. di Mat. s. 3a, t. IX (1903),
!      pp. 1-32. Tale memoria si trova publicata a pag. 279 de
!      https://matematicaitaliana.sns.it/media/volumi/433/LeviCivita_2.pdf
!
! Build with:
!
!   cd ODE-Tests
!
!   rm -rf *.mod; \
!   gfortran -std=f2008 -O3 -Wall -Wno-unused-dummy-argument \
!     ../basic-modules/{{kind,math}_consts,getdata}.f90 \
!     ../ode-modules/{everhart,ode}_integrator.f90 \
!     Filippi.f90 -o Filippi.out; \
!   rm -rf *.mod
!

module Filippi_lib
  use kind_consts, only: WP
  use math_consts, only: ZERO => Z0, ONE => Z1, TWO => Z2

  implicit none
  private

  integer, parameter :: NEQ = 4

  ! Moon and Earth masses. We use Levi Civita notation NU=1-MU (ref. 4)
  real(WP), parameter :: MU = ONE/82.45_WP, NU = ONE-MU

  integer :: ll = 12

  ! Y0(:) = [ X0, Y0, VX0, VY0]; T1 = 6.192169331396, H0 = 0.2 (in
  ! ref. 3 we find T1 = 6.1921693313196... therein is cited a Fortran
  ! version, DIFSYS, of Bulirsch-Stoer _diffsys_ ALGOL routine)
  real(WP) :: t0 = ZERO, t1 = 10*ONE, h0 = ONE/128, &
       y0(NEQ) = [ 1.2_WP, ZERO, ZERO, -1.04935750983_WP ], &
       y(NEQ), h, t

  public :: run_app

contains

  !
  ! Notice that the primary body has mass M1 = 1-MU = NU and is
  ! located at (-MU,0) in the rotating frame, while the secondary body
  ! has mass M2 = MU and is located at (1-MU,0) = (NU,0) in the
  ! rotating frame.
  ! See http://brain2.math.fau.edu/~jmirelesjames/hw4Notes.pdf
  !
  subroutine sub(t,y,f)
    real(WP), intent(in) :: t, y(:)
    real(WP), intent(out) :: f(:)

    real(WP), save :: c1, c2, d(2)

    ! Computing the first coeff. for field G(:) and its first part
    d = y(1:2)+[MU, ZERO]     ! [x+MU,y]
    c1 = norm2(d)**3          ! d**(3/2)
    c1 = NU/(c1)              ! NU/[d**(3/2)] = NU/[(x+MU)**2 + y**2]**(3/2)
    f(3:4) = -c1*d            ! g = -c1*[x+MU,y]

    ! Computing the 2nd coeff. for field G(:) and its 2nd part
    d = d-[ONE, ZERO]         ! [x-1+MU,y] = [x-NU,y]
    c2 = norm2(d)**3          ! d**(3/2)
    c2 = MU/(c2)              ! MU/[d**(3/2)] = MU/[(x-NU)**2 + y**2]**(3/2)
    f(3:4) = f(3:4)-c2*d      ! g = -c1*[x+MU,y]-c2*[x-NU,y]

    f(1) = y(3)
    f(2) = y(4)
    f(3) = TWO*y(4)+y(1)+f(3)
    f(4) = -TWO*y(3)+y(2)+f(4)
  end subroutine sub

  subroutine display_data(t,y)
    real(WP), intent(in) :: t, y(:)

    write(*,'(5f16.12)') t, y(:)
  end subroutine display_data

  subroutine run_app()
    use getdata, only: get
    use ode_integrator, only: ID_GBS, ID_R15, &
         ode_on, ode_integrate, ode_off, ode_calc => calc

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

    write(*,'(a/)') '    USING '//METHOD(id_method)//' METHOD'

    call ode_on(NEQ,sub,display_data,id_method)
    call ode_integrate(t0,t1,y0,h0)
    call ode_off()

    write(*,*)
    write(*,*) '    (RE)USING GBS METHOD'
    write(*,*)

    h = 0.2_WP
    y = y0
    call ode_on(NEQ,sub,METHOD=ID_GBS,DATA_FILE='GBS.data')
    call ode_calc(t0,t1,y,h)
    write(*,*)
    write(*,'(6f16.12)') h, t1, y(:)
    write(*,*)

    call ode_off()

    call read_GBS('GBS.data')

    write(*,*)
    write(*,*) '    (RE)USING R15 METHOD'
    write(*,*)

    h = 0.2_WP
    y = y0
    call ode_on(NEQ,sub,METHOD=ID_R15,DATA_FILE='R15.data')
    call ode_calc(t0,t1,y,h)
    write(*,*)
    write(*,'(6f16.12)') h, t1, y(:)
    write(*,*)

    call ode_off()

    call read_R15('R15.data')

  contains

    subroutine read_GBS(gbs_file)
      use, intrinsic :: iso_fortran_env, only: IOSTAT_END
      character(len=*), intent(in) :: gbs_file

      integer :: data_unit = 0, io_status = 0, n, m

      open(newunit=data_unit,file=gbs_file,access='STREAM', &
           form='UNFORMATTED',status='OLD',iostat=io_status)

      if (io_status /= 0) then
         write(*,*)
         write(*,*)
         write(*,*) 'Error reading file: '//trim(gbs_file)
         stop ': Invalid file name (READ_GBS).'
      end if

      read(data_unit) n, m

      if (n /= NEQ) stop ': Mismatch for NEQ parameter (READ_GBS).'
      if (m /= ID_GBS) stop ': Mismatch for METHOD parameter (READ_GBS).'

      ! Reading the initial conditions. We do not test EOF
      ! because we assume at least a few values
      read(data_unit) h, t, y(1:n)
      write(*,'(6f16.12)') h, t, y(:)

      do
         read(data_unit,iostat=io_status) h, t, y(1:n)

         if (io_status == IOSTAT_END) exit
         if (io_status > 0) &
              stop ': Error occurred while reading file (READ_GBS).'

         write(*,'(6f16.12)') h, t, y(:)
      end do

      close(data_unit)
    end subroutine read_GBS

    subroutine read_R15(r15_file)
      use, intrinsic :: iso_fortran_env, only: IOSTAT_END
      character(len=*), intent(in) :: r15_file

      integer :: data_unit = 0, io_status = 0, nv0, ll0, nclass0, ns
      real(WP) :: yp(NEQ)

      open(newunit=data_unit,file=r15_file,access='STREAM', &
           form='UNFORMATTED',status='OLD',iostat=io_status)

      if (io_status /= 0) then
         write(*,*)
         write(*,*)
         write(*,*) 'Error reading file: '//trim(r15_file)
         stop ': Invalid file name (READ_R15).'
      end if

      read(data_unit) nv0, ll0, nclass0

      if (nv0 /= NEQ ) stop ': Mismatch for NV parameter (READ_R15).'

      write(*,*) '    LL: ', ll0
      write(*,*) 'NCLASS: ', nclass0
      write(*,*)

      ! Reading sequence n. 0, i.e. initial conditions. We do not test EOF
      ! because we assume at least a few sequences (NS > 1)
      read(data_unit) ns, h, t, y(1:nv0), yp(1:nv0)
      write(*,'(6f16.12)') h, t, y(:)

      do
         read(data_unit,iostat=io_status) ns, h, t, y(1:nv0), yp(1:nv0)

         if (io_status == IOSTAT_END) exit
         if (io_status > 0) &
              stop ': Error occurred while reading file (READ_R15).'

         write(*,'(6f16.12)') h, t, y(:)
      end do

      close(data_unit)
    end subroutine read_R15
  end subroutine run_app
end module Filippi_lib

program Filippi
  use Filippi_lib

  implicit none

  call run_app()
end program Filippi
