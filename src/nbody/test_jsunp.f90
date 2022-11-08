!
! Author: ANGELO GRAZIOSI
!
!   created   : Jun 20, 2015
!   last edit : Sep 06, 2017
!
!   Test for JSUNP (J-upiter, S-aturn, U-ranus, N-eptune, P-luto) problem
!   with data found in the file JSUNP.JSU sent by A. JACKSON on Jul 01, 2016.
!
! NOTES
!
! The date range used in the work
!
!   Eckert, Brouwer, Clemence (1951),
!     Coordinates of the Five Outer Planets 1653-2060,
!     Astronom. Papers American Ephem. XII
!
!
! is : [ JD = 2325000.5 = 1653.07.13.5, JD = 2473800.5 = 2060.12.07.0 ]
!
! On the WEB, that work is at the URL
!
!   http://hdl.handle.net/2027/mdp.39015017142152
!
! Notice that before 1925 Jan. 01, the astronomical dates began at the
! Greenwich Mean Noon (i.e. at the 12.0 hours): this explain why the day of
! the Gregorian date which corresponds to the JD 2325000.5 is 13.5 and not
! 14.0 as one, normally, would expect. For the same reason, the date
! 1653.05.02.0 has as JD, 2324928.0 and not 2324927.5.
!
! RESULTS
!
!   With this program we find
!
!       At t =   2325000.50 the result is (JUPITER):
!
!     RA15   :   X =     3.548739356   -3.280988352   -1.495613025
!     RA15   :   V =     0.005304295    0.005246342    0.002121567
!     Run time    0.016 seconds!
!
!     DEQGBS :   X =     3.548739356   -3.280988352   -1.495613025
!     DEQGBS :   V =     0.005304295    0.005246342    0.002121567
!     Run time    0.062 seconds!
!
!       RA15-DEQGBS, |RA15-DEQGBS| :
!
!     DX =   2.86E-11  4.06E-11  1.67E-11
!     DV =  -3.26E-14  4.93E-14  2.21E-14
!
!     ABS(DX) =   5.24E-11
!     ABS(DV) =   6.31E-14
!
! The position for JUPITER is exactly what reported in the file VALUE.SET.
!
! HOW TO BUILD THE APP
!
!   cd nbody
!
!   rm -rf *.mod && \
!   gfortran[-mp-X] [-Warray-temporaries] -O3 -Wall \
!     -Wno-unused-dummy-argument $BLD_OPTS \
!     ../basic-modules/{kind,math}_consts.f90 \
!     ../ode-modules/{everhart_integrator,\
!       cernlib_integrators}.f90 test_jsunp.f90 -o test_jsunp$EXE && \
!     rm -rf *.mod
!
!   ./test_jsunp$EXE
!
!   where, for the build on GNU/Linux [OSX+MacPorts X server], is:
!
!     BLD_OPTS =
!     EXE = .out
!
!   while for the build on MSYS2/MINGW{32,64} is:
!
!     BLD_OPTS = [-static]
!     EXE = -msys2/-mingw{32,64}
!
!   (EXE could be EMPTY or .out for MSYS2).
!

program test_jsunp
  use kind_consts, only: WP
  use math_consts, only: ZERO => Z0
  use cernlib_integrators, only: deqgbs
  use everhart_integrator, only: ra15_on, ra15_run, ra15_off

  implicit none

  integer, parameter :: NDIM = 3, NB = 5, NV = NDIM*NB, NEQ = 2*NV
  integer, parameter :: NCLASS = -2, LL = -14
  real(WP), parameter :: T_FACT = 800, V_FACT = 20

  real(WP), parameter :: X0(NV) = &
       [ .34294741518908E1_WP,.33538695971068E1_WP,.13549490171466E1_WP, &
       .66414554254920E1_WP, .59715695787840E1_WP, .21823149972760E1_WP, &
       .11263043720737E2_WP, .14695257679351E2_WP, .62796052506730E1_WP, &
       -.30155226875935E2_WP, .16569996640440E1_WP, .14378575272050E1_WP, &
       -.21123835337949E2_WP, .28446509814206E2_WP, .15388265967925E2_WP ], &
       V0(NV) = &
       [ -2.2286422817945E-1_WP,2.0227871331527E-1_WP,9.223141756018E-2_WP, &
       -1.6622831053739E-1_WP, 1.4627308912448E-1_WP, 6.765728531701E-2_WP, &
       -1.3013026766296E-1_WP, 7.588240878556E-2_WP, 3.509061291124E-2_WP, &
       -9.619050167000E-3_WP, -1.1506381304315E-1_WP, -4.688781726993E-2_WP, &
       -7.074430124870E-2_WP, -8.655738121042E-2_WP, -5.94591572372E-3_WP ]

  real(WP) :: tf, h, &
       ta = 2430000.5_WP, &   ! 1941.01.06.0
       tz = 2325000.5_WP, &   ! 1653.07.13.5
       !tz = 2469600.5_WP, &   ! 2049.06.08.0
       h0 = 320   ! The time step used in Everhart paper, sec. 3.3

  real(WP) :: x(NV), v(NV), y(NEQ), w(NEQ,36), eps = 1.0E-12_WP, &
       dx(3), dv(3)
  real(WP) :: t0, t1

  write(*,*) 'Testing CLASS IIS differential equations:'
  write(*,*) '       The Outer Planets Problem'
  write(*,*) '      (sec. 3.3 of Everhart paper)'
  write(*,*)

  ! Conversion to use time unit 800 days. Being the velocity given in
  ! AU/40D-unit, the conversion factor is 800/40 = 20
  x = x0
  v = v0*V_FACT
  tf = (tz-ta)/T_FACT
  h = h0/T_FACT

  !call ra15_on(NV,LL,NCLASS,force,'ra15.log','ra15.data')
  !call ra15_on(NV,LL,NCLASS,force,data_file='ra15.data')
  call ra15_on(NV,LL,NCLASS,force,'ra15.log')
  call cpu_time(t0)
  call ra15_run(ZERO,tf,x,v,h)
  call cpu_time(t1)
  call ra15_off()

  ! Conversion to AU/D
  v(1:3) = v(1:3)/T_FACT

  write(*,*)
  write(*,'(A,F12.2,A)') 'At t = ', tz, ' the result is (JUPITER):'
  write(*,'(A,3F15.9)') 'RA15   :   X = ', x(1:3)
  write(*,'(A,3F15.9)') 'RA15   :   V = ', v(1:3)
  write(*,'(A,F8.3,A)') 'Run time ',t1-t0,' seconds!'

  y(1:NV) = x0
  y(NV+1:NEQ) = v0*V_FACT
  tf = (tz-ta)/T_FACT
  h = h0/T_FACT

  call cpu_time(t0)
  call deqgbs(NEQ,ZERO,tf,y,h,eps,w,sub)
  call cpu_time(t1)

  ! Conversion to AU/D
  y(NV+1:NV+3) = y(NV+1:NV+3)/T_FACT

  ! The difference between the two methods
  dx = x(1:3)-y(1:3)
  dv = v(1:3)-y(NV+1:NV+3)

  write(*,*)
  write(*,'(A,F12.2,A)') 'At t = ', tz, ' the result is (JUPITER):'
  write(*,'(A,3F15.9)') 'DEQGBS :   X = ', y(1:3)
  write(*,'(A,3F15.9)') 'DEQGBS :   V = ', y(NV+1:NV+3)
  write(*,'(A,F8.3,A)') 'Run time ',t1-t0,' seconds!'
  write(*,*)
  write(*,*) 'RA15-DEQGBS, |RA15-DEQGBS| :'
  write(*,*)
  write(*,'(A,3ES10.2)') 'DX = ', dx
  write(*,'(A,3ES10.2)') 'DV = ', dv
  write(*,*)
  write(*,'(A,ES10.2)') 'ABS(DX) = ', norm2(dx)
  write(*,'(A,ES10.2)') 'ABS(DV) = ', norm2(dv)

contains

  subroutine force(t,x,v,f)
    !
    ! The FORCE subroutine for the 5 outer planet integration.
    !
    real(WP), intent(in) :: t, x(:), v(:)
    real(WP), intent(out) :: f(:)
    !
    ! The above statement assumes an 8-byte doubel word (64 bits).
    ! X, V, and F are dimensioned assumed-shape because they appear
    ! in the call.
    !
    ! SCZ is the Gaussian constant for an 800-day time unit, and SC is the
    ! same except the mass of sun is augmented by masses of inner
    ! planets, Mercury through Mars.
    ! X, V, and F are dimensioned for 15 in the calling programs. Indices 1,2,3
    ! are for x,y,z for Jupiter, 4,5,6 are for x,y,z Saturn, 7,8,9 are for
    ! x,y,z Uranus, 10,11,12 for x,y,z Neptune, and 13,14,15 for Pluto.
    !
    real(WP), parameter :: SCZ = -((1.720209895E-2_WP)**2)*((800._WP)**2), &
         SC = -1.8938494521574133E2_WP, ONE = 1

    ! The reciprocal masses of the 5 planets, units of reciprocal sun.
    real(WP), parameter :: RM(NB) = [ 1047.355_WP, 3501.6_WP, 22869._WP, &
         19314._WP, 360000._WP]

    real(WP), save :: pm(NB), r(NB), rh(NB,NB), scm
    integer, save :: j, k, l, n, na
    logical, save :: first = .true.

    if (first) then
       first = .false.
       pm(:) = -SCZ/RM(:)
    end if

    do  n = 1, NB
       j = (n-1)*3+1
       r(n) = ONE/norm2(x(j:j+2))**3
       if (n == NB) cycle
       na = n+1
       do  l = na, NB
          k = (l-1)*3+1
          rh(n,l) = ONE/norm2(x(j:j+2)-x(k:k+2))**3
          rh(l,n) = rh(n,l)
       end do
       ! Indices K and J run 1-15, indices N and L for the planets run 1-5.
       ! The mass factors are in PM, the distance from the sun of each planet
       ! contribute to R, and the planet-to-planet distances contribute to RH.
    end do

    do n = 1, NB
       j = (n-1)*3+1
       scm = (SC-pm(n))*r(n)
       f(j:j+2) = scm*x(j:j+2)
       ! The F-values above are for the sun-planet forces/unit mass.
       do l = 1, NB
          if (l == n) cycle
          k = (l-1)*3+1
          f(j:j+2) = f(j:j+2)+pm(l)*((x(k:k+2)-x(j:j+2))*rh(n,l)-x(k:k+2)*r(l))
          ! The mutual planetary perturbation forces/unit mass are added on. The
          ! first part of the second term is due to the planet-to-planet force,
          ! and the second part is the indirect term because the sun at the
          ! origin is not at the center of mass of the system.
       end do
    end do
  end subroutine force

  subroutine sub(t,y,f)
    real(WP), intent(in) :: t, y(:)
    real(WP), intent(out) :: f(:)
    f(1:NV) = y(NV+1:NEQ)
    call force(t,y(1:NV),y(NV+1:NEQ),f(NV+1:NEQ))
  end subroutine sub

end program test_jsunp
