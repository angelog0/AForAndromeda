!
! Author: ANGELO GRAZIOSI
!
!   created   : Sep 08, 2014
!   last edit : Oct 02, 2017
!
!   Computing orbits with the Everhart method
!
! APOPHIS TEST
!
!   On MSYS2/MINGW64 (GFortran 4.9.2), with T1 = 2477476.5 D, H = 0, LL = 16,
!   JPL-1_Ceres-13_bodies.cards, the result is
!
!     CPA at time (D):      2462240.40688  (  2029   4  13.90688)
!     CPA distance |P1-P2| (AU):    2.5991485382768258E-004
!
!   Instead, on MSYS2/MINGW32 (GFortran 4.9.2), the result is
!
!     CPA at time (D):      2462240.40643  (  2029   4  13.90643)
!     CPA distance |P1-P2| (AU):    2.5990024453900653E-004
!
!   In MSYS2 shell (GFortran 5.3.0):
!
!     CPA at time (D):      2462240.40833  (  2029   4  13.90833)
!     CPA distance |P1-P2| (AU):    2.6004083257238377E-004
!
!   Assuming AU ~ 150E6 km, |P1-P2| ~ 2.6E-4 * 150E6 = 39000 km
!   Wikipedia (http://it.wikipedia.org/wiki/99942_Apophis) says
!   |P1-P2| ~ 36350 km on April 13, 2029...
!
!   On GNU/Linux (Mint 17.1+Mate, GFortran 4.8.4), the result is
!
!     CPA at time (D):      2462240.40684  (  2029   4  13.90684)
!     CPA distance |P1-P2| (AU):    2.5991332899197143E-004
!
!   On OSX (Yosemit 10.10.5, MacPorts GFortran 5.2), the result is
!
!     CPA at time (D):      2462240.40771  (  2029   4  13.90771)
!     CPA distance |P1-P2| (AU):    2.5997209783692072E-004
!
! NICE WEB PAGES
!
!   https://phet.colorado.edu/sims/my-solar-system/my-solar-system_en.html
!   http://en.wikipedia.org/wiki/Numerical_model_of_the_Solar_System
!
!
! HOW TO BUILD THE APP
!
!   cd N-Body
!
!   rm -rf *.mod && \
!   gfortran[-mp-X] -O3 [-march=native -funroll-loops] -Wall \
!     -Wno-unused-dummy-argument $BLD_OPTS \
!     ../basic-modules/{{kind,math}_consts,ft_timer_m,getdata}.f90 \
!     ../ode-modules/everhart_integrator.f90 \
!     calc_orbits.f90 -o calc_orbits$EXE && \
!     rm -rf *.mod
!
!   ./calc_orbits$EXE
!
!   where, for the build on GNU/Linux [OSX+MacPorts X server], is:
!
!     BLD_OPTS =
!     PLATFORM =
!     EXE = .out
!
!   while for the build on MSYS2/MINGW{32,64} is:
!
!     BLD_OPTS = [-static]
!     PLATFORM = MSYS/MINGW{32,64}
!     EXE = -$PLATFORM
!
!   (EXE could be EMPTY or .out for MSYS2).
!

module calc_orbits_lib
  use kind_consts, only: WP
  use math_consts, only: ZERO => Z0

  implicit none
  private
  !
  ! GAUSS Units : AU (A) for lengths, Day (D) for times,
  ! Solar Mass (MS) for masses.
  !
  ! With these units,
  !
  !   G*MSUN = MU = KQ = K*K = 2.959122082855910 x 10**(-4)
  !
  ! being K == 0.01720209895 A**(3/2) MS**(-1/2) D**(-1) the
  ! Gauss's Gravitational Constant
  !
  integer, parameter :: NDIM = 3, NDIM1 = NDIM-1, &
       MAX_NBODY = 15, MAX_NV = NDIM*MAX_NBODY, NCLASS = -2

  integer :: p1(NDIM) = 0, p2(NDIM) = 0, ll = 16

  ! NV is the number of 2nd order equations.
  integer :: nb, nb1, nv
  real(WP) :: t0 = ZERO, t1 = 2477476.5_WP, h = ZERO, &
       m(MAX_NBODY) = ZERO, mm(MAX_NBODY) = ZERO

  ! We adopt the variables with this meaning (for example with 4 bodies in 3D)
  !
  !
  !   x(1:3)   = q1(1:3)
  !   x(4:6)   = q2(1:3)
  !   x(7:9)   = q3(1:3)
  !   x(10:12) = q4(1:3)
  !
  !   v(1:3)   = v1(1:3)
  !   v(4:6)   = v2(1:3)
  !   v(7:9)   = v3(1:3)
  !   v(10:12) = v4(1:3)
  !
  ! Notice that the first index sequences,
  !
  !   1   4   7   10
  !
  ! can be produced with
  !
  !   3*i-2,           i - 1,2,3,...,NBODY
  !
  ! i.e.
  !
  !   NDIM*i-(NDIM-1) = 1 + (i-1)*NDIM
  !
  real(WP) :: x(MAX_NV) = ZERO, v(MAX_NV) = ZERO

  public :: input_data, run_app

contains

  subroutine input_data()
    use getdata, only: get
    !
    ! The coordinates system is rectangular, heliocentric and ecliptic,
    ! which means a NOT-inertial reference system, where the SUN is ALWAYS
    ! at rest. See the note
    !
    !   M. Carpino, Introduzione ai metodi di calcolo delle effemeridi e
    !   determinazione orbitale.
    !
    !   (http://www.brera.mi.astro.it/~carpino/didattica/detorb.pdf)
    !
    ! and
    !
    !   G. Matarazzo, Moto perturbato degli N-corpi (Metodo di Cowell) risolto
    !   con l'integratore di Everhart al 15-esimo ordine.
    !
    !   (http://astrodinamica.altervista.org/PDF/MotoPert.pdf)
    !
    ! Just a clarification about the table on page 7 of the last cited work.
    ! The table does not report the close(st) encounters AS computed by
    ! COW.FOR program. This would mean to compute not only the distance
    ! of the close encounter but also the time at which this occurs.
    ! Instead the table shows only the positions at the times 'tf' of
    ! first column. The times 'tf' are the times of close(st) encounters
    ! AS computed by the astronomer E. Goffin.
    !
    ! This program tries to compute both times and distances of close(st)
    ! encounters. Obviously, we can verify the results of Goffin and
    ! COW.FOR ONLY approximately, in the limit of time step H and
    ! "precision" LL.
    !
    ! Another clarification. Often the data refer to the ecliptic plane
    ! with which most planetary orbits are almost co-planar. So an interesting
    ! point of view is on the equatorial plane. This forms an angle of about
    ! 23 degrees with the ecliptic plane. Put, then, the observer on the
    ! equatorial plane choosing a THETA angle of 90-23 = 67 degrees.
    !
    write(*,'(A)',advance='NO') 'Reading data...'
    call read_cards()
    write(*,'(A)') 'done!'

    write(*,*)
    write(*,*) 'Integration starts at time T0 (JD): ', t0
    write(*,*) 'Number of interacting bodies: ', nb
    write(*,*)

    ! The starting integration time, t0, is read from the cards file.
    ! The final time, t1, and the integration step (guess) is read here,
    ! interactively.
    call get('T1 (JD) = ',t1)
    call get('H (D) = ',h)
    write(*,*)

    call get('LL = ',ll)
    if (abs(ll) > 20) stop ': LL too large parameter (INPUT_DATA).'
    write(*,*)

  contains

    subroutine read_cards()

      character(len=*), parameter :: CARDSFNAME = 'close_encounters.cards'

      integer :: i, ip1, ip2, cards_unit, io_status
      real(WP) :: mu = ZERO

      open(newunit=cards_unit,file=CARDSFNAME,status='OLD',iostat=io_status)

      if (io_status /= 0) then
         write(*,*)
         write(*,*)
         write(*,*) 'Error reading file: '//trim(CARDSFNAME)
         stop ': Invalid file name (READ_CARDS).'
      end if

      ! Epoch of the data to be read (starting time of integration interval)
      read(cards_unit,*) t0

      ! Number of bodies
      read(cards_unit,*) nb

      if (nb > MAX_NBODY) then
         write(*,*) 'NB = ', nb, ' .GT. ', MAX_NBODY
         stop ': NB too large parameter (READ_CARDS).'
      end if
      nb1 = nb-1
      nv = NDIM*nb

      ! Gravitational parameter (G*M) for Sun (in AU**3/D**2)
      read(cards_unit,*) mu

      ! Planets data: gravitational parameter (in AU**3/D**2),
      ! positions (in AU) and velocities (in AU/D) at time t0
      ! Notice: m(1:nb) is the gravitational parameter (G*mass) NOT the mass..
      do i = 1, nb
         ip1 = 1+NDIM*(i-1)
         ip2 = ip1+NDIM1

         read(cards_unit,*) m(i)
         read(cards_unit,*) x(ip1:ip2)
         read(cards_unit,*) v(ip1:ip2)
      end do

      ! Computing the constants : -(mu+m(i))
      mm(1:nb) = ZERO-(mu+m(1:nb))

      ! The bodies for which we want the Closest Point Approach (CPA) data
      ! (should be in the range 1..nb and ip1 /= ip2)
      !
      ! BUT NOT USED IN THIS APP
      !
      read(cards_unit,*) ip1, ip2

      if (ip1 == ip2 .or. (ip1 < 1 .or. nb < ip1) &
           .or. (ip2 < 1 .or. nb < ip2)) then
         write(*,*) 'P1 = ', ip1, ' P2 = ', ip2
         stop ': Wrong request for CPA parameters (READ_CARDS).'
      end if

      ! Now IP1 and IP2 point to the X coordinate of body P1 and P2,
      ! respectively...
      !
      ! STRICLY SPEAKING, WE DO NOT NEED THIS IN THIS APP
      !
      ip1 = 1+NDIM*(ip1-1)
      p1 = [ ip1, ip1+1, ip1+2 ]

      ip2 = 1+NDIM*(ip2-1)
      p2 = [ ip2, ip2+1, ip2+2 ]

      close(cards_unit)
    end subroutine read_cards
  end subroutine input_data

  subroutine run_app()
    use everhart_integrator, only: ra15_on, ra15_run, ra15_off

    write(*,*)
    write(*,'(A)',advance='NO') 'Computing the orbits...'
    call ra15_on(nv,ll,NCLASS,force,data_file='ra15.data')
    call ra15_run(t0,t1,x(1:nv),v(1:nv),h)
    call ra15_off()
    write(*,'(A)') 'done!'

    ! Just to test/debug...
    print *
    print '(A,F15.10,F15.4)', 'H,T =', h, t1
    print '(A,3F15.10)', 'Position (P2) =', x(p2)
    print '(A,3F15.10)', 'Position (P1) =', x(p1)
    print '(A,F15.8)', 'D =', norm2(x(p1)-x(p2))
    print *

  contains

    subroutine force(t,x,v,f)
      real(WP), intent(in) :: t, x(:), v(:)
      real(WP), intent(out) :: f(:)

      integer, save :: i, j, ip1, ip2, jp1, jp2
      real(WP), save :: a(NDIM*MAX_NBODY), d(NDIM)

      ! Initialization of a(:) and field f(:).
      ! In a(:) we store
      !
      !   (r(p)/|r(p)|**3)
      !
      ! where r(p) is the radius vector of planet p from the Sun.
      !
      do i = 1, nb
         ip1 = 1+NDIM*(i-1)
         ip2 = ip1+NDIM1

         ! d = qi/|qi|**3
         d = x(ip1:ip2)
         d = d/norm2(d)**3
         a(ip1:ip2) = d
         f(ip1:ip2) = mm(i)*a(ip1:ip2)
      end do

      ! Filling with forces/accelerations the field f(:)
      do i = 1, nb1
         ip1 = 1+NDIM*(i-1)
         ip2 = ip1+NDIM1

         do j = i+1, nb
            jp1 = 1+NDIM*(j-1)
            jp2 = jp1+NDIM1

            ! d = (qi-qj)/|qi-qj|**3
            d = x(ip1:ip2)-x(jp1:jp2)
            d = d/norm2(d)**3

            f(ip1:ip2) = f(ip1:ip2)-m(j)*(d+a(jp1:jp2))
            f(jp1:jp2) = f(jp1:jp2)+m(i)*(d-a(ip1:ip2))
         end do
      end do

      ! To avoid the 'Unused dummy argument' annoying Warning if not using
      ! the '-Wno-unused-dummy-argument' option..
      !return
      !if (.false.) print *, t, v
    end subroutine force
  end subroutine run_app
end module calc_orbits_lib

program calc_orbits
  use ft_timer_m, only: ft_timer_t
  use calc_orbits_lib

  implicit none

  character(len=*), parameter :: TIME_FMT = '(A,F0.3,A)'

  type(ft_timer_t) :: orbit_timer

  call input_data()

  write(*,*)
  write(*,'(A)',advance='NO') 'Please wait, we are working...'

  call orbit_timer%start()
  call run_app()
  call orbit_timer%stop()

  write(*,*)
  write(*,TIME_FMT) 'Completed in ', orbit_timer%elapsed_time(), ' seconds!'
end program calc_orbits
