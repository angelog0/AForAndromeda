!
! Author: ANGELO GRAZIOSI
!
!   created   : Sep 08, 2014
!   last edit : Nov 13, 2022
!
!   Process orbits (computed with calc_orbits.f90) and prints close encounters.
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
!     [-Wno-unused-dummy-argument] $BLD_OPTS \
!     ../basic-modules/{{kind,math}_consts,ft_timer_m,getdata,
!       julian_dates}.f90 process_orbits.f90 \
!     -o process_orbits$EXE && \
!     rm -rf *.mod
!
!   ./process_orbits$EXE
!
!   where, for the build on GNU/Linux [OSX+MacPorts X server], is:
!
!     BLD_OPTS =
!     EXE = .out
!
!   while for the build on MSYS2/MINGW{32,64} is:
!
!     BLD_OPTS = [-static]
!     EXE = -$PLATFORM
!
!   (EXE could be EMPTY or .out for MSYS2).
!

module process_orbit_lib
  use kind_consts, only: WP
  use math_consts, only: ZERO => Z0, ONE => Z1

  implicit none
  private

  character(len=80) :: filename = 'ra15.data'

  ! By default close encounters Earth(3)-Apophis(12)
  integer :: ip1 = 3, ip2 = 12

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

    call get('FILENAME = ',filename)
    write(*,*)

    call get('IP1 = ',ip1)
    call get('IP2 = ',ip2)
    write(*,*)
  end subroutine input_data

  subroutine run_app()
    use, intrinsic :: iso_fortran_env, only: IOSTAT_END
    use julian_dates, only: jd2cal

    integer, parameter :: NDIM = 3, MAX_NBODY = 15, MAX_NV = NDIM*MAX_NBODY
    real(WP), parameter :: DQ_THRESHOLD = (0.1_WP)**2

    integer  :: nv, ll, nclass, ns, k, data_unit, io_status, &
         year, month, &
         nb = 0, p1(NDIM) = 0, p2(NDIM) = 0
    real(WP) :: x(MAX_NV) = ZERO, v(MAX_NV) = ZERO, &
         h, t, dq, dq_min, d(NDIM), &
         t_cpa, p1_cpa(NDIM), p2_cpa(NDIM), &
         dq_ce, t_ce, p1_ce(NDIM), p2_ce(NDIM), &
         day
    logical :: find_ce

    ! Start 'run_app()'...

    ! Opening DATA file
    open(newunit=data_unit,file=filename,access='STREAM',form='UNFORMATTED', &
         status='OLD',iostat=io_status)

    if (io_status /= 0) then
       write(*,*)
       write(*,*)
       write(*,*) 'Error reading file: '//trim(filename)
       stop ': Invalid file name (RUN_APP).'
    end if

    read(data_unit) nv, ll, nclass

    nb = nv/NDIM

    if (nb > MAX_NBODY) then
       write(*,*) 'NB = ', nb, ' > ', MAX_NBODY
       stop ': NB out of range parameter (RUN_APP).'
    end if

    ! Verify if IP1 and IP2 are in the right range
    if (ip1 == ip2 .or. (ip1 < 1 .or. nb < ip1) &
         .or. (ip2 < 1 .or. nb < ip2)) then
       write(*,*) 'P1 = ', ip1, ' P2 = ', ip2
       stop ': Wrong request for CPA parameters (RUN_APP).'
    end if

    ! Now K points to the X coordinate of body P1...
    k = 1+NDIM*(ip1-1)
    p1 = [ k, k+1, k+2 ]

    ! Now K points to the X coordinate of body P2...
    k = 1+NDIM*(ip2-1)
    p2 = [ k, k+1, k+2 ]

    ! Reading sequence n. 0, i.e. initial conditions. We do not test EOF
    ! because we assume at least a few sequences (NS > 1)
    read(data_unit) ns, h, t, x(1:nv), v(1:nv)

    ! Just to test/debug...
    !print *, 'NS,H,T,X,V =', ns, h, t, x(1:nv), v(1:nv)

    !
    ! Initialization for Close-Encounter (CE) and Closest Point Approach (CPA)
    !
    ! d      is the distance vector between P1 and P2
    ! t_ce   is the time at CE
    ! p1_ce  is the position of body P1 at CE
    ! p2_ce  is the position of body P2 at CE
    !
    ! t_cpa   is the time at CPA
    ! p1_cpa  is the position of body P1 at CPA
    ! p2_cpa  is the position of body P2 at CPA
    !
    ! We try to find CEs which are below DQ_THRESHOLD (distance squared
    ! threshold), i.e. when the flag FIND_CE is set. This occurs the first
    ! time that DQ < DQ_THRESHOLD, for current search).
    !
    ! We can lose CEs in certain situations. For example, if bodies are at CE,
    ! i.e. below DQ_THRESHOLD, when we start the integration.
    !
    ! If we start above DQ_THRESHOLD, we should be able to find all the
    ! CE < DQ_THRESHOLD.
    !
    find_ce = .false.
    d = x(p1)-x(p2)
    dq = dot_product(d,d)
    dq_ce = dq
    t_ce = t
    p1_ce = x(p1)
    p2_ce = x(p2)

    ! Being the CPA the minimum of all CE, dq_min is the minimum of all dq_ce.
    dq_min = dq_ce
    t_cpa = t_ce
    p1_cpa = p1_ce
    p2_cpa = p2_ce

    do

       ! We take another step...
       read(data_unit,iostat=io_status) ns, h, t, x(1:nv), v(1:nv)

       if (io_status == IOSTAT_END) exit
       if (io_status > 0) &
            stop ': Error occurred while reading file (RUN_APP).'

       d = x(p1)-x(p2)
       dq = dot_product(d,d)

       ! We are entering the "region" DQ < DQ_THRESHOLD. Hunting can begin...
       if (.not. find_ce .and. dq < DQ_THRESHOLD) find_ce = .true.

       ! We are leaving the "region" DQ < DQ_THRESHOLD. Hunting stops...
       ! ...and we emptied its pouch, i.e. we output the result and
       ! reset essential variables.. DQ_CE is reset to DQ which
       ! is >= DQ_THRESHOLD!
       !
       if (find_ce .and. dq >= DQ_THRESHOLD) then
          call jd2cal(t_ce,1,year,month,day)
          write(*,*)
          write(*,*)
          write(*,'(A,F18.5,A,I6,I4,F10.5,A)') 'CE at time (D): ', t_ce, &
               '  (', year, month, day, ')'
          write(*,*) 'CE P1 position (AU): ', p1_ce
          write(*,*) 'CE P2 position (AU): ', p2_ce
          write(*,*) 'CE distance |P1-P2| (AU): ', sqrt(dq_ce)

          ! We have found a CE.. but is this also the CPA?
          if (dq_ce < dq_min) then
             dq_min = dq_ce
             t_cpa = t_ce
             p1_cpa = p1_ce
             p2_cpa = p2_ce
          end if

          ! Reset of the relevant variables for the next search...
          find_ce = .false.
          dq_ce = dq
       end if

       ! If we are hunting, let's see if we are close the prey..
       if (find_ce .and. (dq < dq_ce)) then
          dq_ce = dq
          t_ce = t
          p1_ce = x(p1)
          p2_ce = x(p2)
       end if
    end do
    call jd2cal(t_cpa,1,year,month,day)
    write(*,*)
    write(*,*)
    write(*,'(A,F18.5,A,I6,I4,F10.5,A)') 'CPA at time (D): ', t_cpa, &
         '  (', year, month, day, ')'
    write(*,*) 'CPA P1 position (AU): ', p1_cpa
    write(*,*) 'CPA P2 position (AU): ', p2_cpa
    write(*,*) 'CPA distance |P1-P2| (AU): ', sqrt(dq_min)

    close(data_unit)

    ! Stop 'run_app()'...
  end subroutine run_app

end module process_orbit_lib

program process_orbit
  use ft_timer_m, only: ft_timer_t
  use process_orbit_lib

  implicit none

  character(len=*), parameter :: TIME_FMT = '(A,F0.3,A)'
  type(ft_timer_t) :: process_timer

  call input_data()

  write(*,*)
  write(*,'(A)',advance='NO') 'Please wait, we are working...'

  call process_timer%start()
  call run_app()
  call process_timer%stop()

  write(*,*)
  write(*,TIME_FMT) 'Processting completed in ',process_timer%elapsed_time(), &
       ' seconds!'
end program process_orbit
