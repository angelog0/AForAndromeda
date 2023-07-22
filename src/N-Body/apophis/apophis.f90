!
! Author: ANGELO GRAZIOSI
!
!   created   : Sep 08, 2014
!   last edit : Jul 22, 2023
!
!   Apophis
!
! DESCRIPTION
!
!   Close Encounters in a N-Body system with the Everhart integrator.
!   We integrate the equations of the motion for a number of bodies in
!   the solar system and will try to find the closest encounters. The
!   default data are for Earth-Apophis99942 encounters.
!
! UNITS
!
!   GAUSS Units : AU (A) for lengths, Day (D) for times, Solar Mass
!   (MS) for masses.
!
!   With these units,
!
!     G*MSUN = MU = KQ = K*K = 2.959122082855910 x 10**(-4)
!
!   being K == 0.01720209895 A**(3/2) MS**(-1/2) D**(-1) the Gauss's
!   Gravitational Constant
!
! 99942 APOPHIS  TEST
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
! HOW TO BUILD THE APP  (MSYS2/MINGW64, GNU/Linux, macOS)
!
!   cd N-Body
!
!   git clone https://github.com/interkosmos/fortran-sdl2.git
!
!   cd apophis
!
!   rm -rf *.mod; \
!   gfortran[-mp-X] -std=f2018 -O3 [-march=native -funroll-loops] -Wall \
!     -Wno-unused-dummy-argument [`sdl2-config --cflags`] \
!     $B/basic-modules/{{kind,math}_consts,getdata,julian_dates,\
!       nicelabels,camera_view_m}.f90 $B/ode-modules/everhart_integrator.f90 \
!     $SDL2F90 $C/SDL2_{app,shading}.f90 apophis.f90 $LIBS \
!     -o apophis$EXE; \
!   rm -rf *.mod
!
!   ./apophis$EXE
!
!   where, for the build on GNU/Linux [OSX+MacPorts X server], is:
!
!     EXE = .out
!
!   while for the build on MINGW{32,64} is:
!
!     EXE = -$MSYSTEM (or EMPTY)
!
!   and (all platforms):
!
!     B = ../..
!     C = $B/sdl2-fortran.apps
!     S = ..
!
!     SDL2F90 = $S/fortran-sdl2/src/{c_util,sdl2/{sdl2_stdinc,sdl2_audio,\
!       sdl2_blendmode,sdl2_cpuinfo,sdl2_gamecontroller,sdl2_error,\
!       sdl2_events,sdl2_filesystem,sdl2_hints,sdl2_joystick,sdl2_keyboard,\
!       sdl2_log,sdl2_messagebox,sdl2_rect,sdl2_pixels,sdl2_platform,\
!       sdl2_scancode,sdl2_surface,sdl2_render,sdl2_keycode,sdl2_mouse,\
!       sdl2_rwops,sdl2_thread,sdl2_timer,sdl2_version,sdl2_video,\
!       sdl2_opengl},sdl2}.f90
!
!
!     LIBS = `sdl2-config --libs`
!
!   Notice that the above definition for LIBS produces a pure Windows
!   app on MSYS2/MINGW64. This means that will not show up a
!   console/terminal to input data. On these systems, the LIBS
!   definition should be:
!
!     LIBS = [-lSDL2main] -lSDL2 -lgdi32 -lcomdlg32 -luuid -loleaut32 -lole32
!
!   For a static build (run from Explorer), I have found usefull
!
!     LIBS = -static -lmingw32 -lSDL2main -lSDL2 -lws2_32 -ldinput8 \
!            -ldxguid -ldxerr8 -luser32 -lgdi32 -lwinmm -limm32 -lole32 \
!            -loleaut32 -lshell32 -lversion -luuid -lcomdlg32 -lhid -lsetupapi
!
!   See as references:
!
!     1. https://stackoverflow.com/questions/53885736/issues-when-statically-compiling-sdl2-program
!     2. https://groups.google.com/g/comp.lang.fortran/c/Usgys7Gww6o/m/CYEfzQfbhckJ
!
!
! NOTE FOR WINDOWS
!
!   On Windows the application _hangs_ (NOT RESPONDING) when its
!   window has focus (i.e. is selected) so the best way to launch it
!   is from CMD or Explorer. From the MSYS2/MINGW64 shell one should
!   use:
!
!     open PROGNAME
!
!   being:
!
!     alias open='start'
!
!   Maybe the same considerations hold for GNU/Linux and macOS.
!
!
! LICENSE
!
!     Copyright (c) 2014-2023, ANGELO GRAZIOSI.  ALL RIGHTS RESERVED.
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

module apophis_lib
  use kind_consts, only: WP
  use camera_view_m, only: camera_view_t
  use SDL2_shading, only: color_rgb_t

  implicit none
  private

  integer, parameter :: NDIM = 3, NDIM1 = NDIM-1, &
       MAX_NBODY = 15, MAX_NV = NDIM*MAX_NBODY, NCLASS = -2

  character(len=*), parameter :: FILENAME = 'ra15.data'

  type(color_rgb_t), parameter :: BODY_COLOR(0:MAX_NBODY) = [ &
       color_rgb_t(0,0,0),       &   ! BLACK
       color_rgb_t(0,0,128),     &   ! BLUE
       color_rgb_t(0,128,0),     &   ! GREEN
       color_rgb_t(0,128,128),   &   ! CYAN
       color_rgb_t(128,0,0),     &   ! RED
       color_rgb_t(128,0,128),   &   ! MAGENTA
       color_rgb_t(128,128,0),   &   ! BROWN
       color_rgb_t(192,192,192), &   ! LIGHTGRAY
       color_rgb_t(128,128,128), &   ! DARKGRAY
       color_rgb_t(0,0,255),     &   ! LIGHTBLUE
       color_rgb_t(0,255,0),     &   ! LIGHTGREEN
       color_rgb_t(0,255,255),   &   ! LIGHTCYAN
       color_rgb_t(255,0,0),     &   ! LIGHTRED
       color_rgb_t(255,0,255),   &   ! LIGHTMAGENTA
       color_rgb_t(255,255,0),   &   ! YELLOW
       color_rgb_t(255,255,255) ]    ! WHITE

  ! NV is the number of 2nd order equations.
  integer :: nb, nb1, nv, ll = 16, p1(NDIM) = 0, p2(NDIM) = 0
  real(WP) :: t0 = 0, t1 = 2477476.5_WP, h0 = 0, &
       m(MAX_NBODY) = 0, mm(MAX_NBODY) = 0

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
  real(WP) :: x0(MAX_NV) = 0, v0(MAX_NV) = 0

  integer :: screen_width = 900, screen_height = 900
  logical :: recalc = .true.

  real(WP) :: k_view = 4, phi = 270, theta = 67, alpha = 45, &
       u_min = 0, u_max = 0, v_min = 0, v_max = 0, kq_clip = 4**2, &
       dt_rate = 0, len_axis = 0
  type(camera_view_t) :: my_view

  public :: app_menu

contains

  subroutine read_cards()

    ! Special treatment for GitHub
    character(len=*), parameter :: CARDSFNAME = '../close_encounters.cards'

    integer :: i, ip1, ip2, cards_unit, io_status
    real(WP) :: mu = 0

    open(NEWUNIT=cards_unit,FILE=CARDSFNAME,STATUS='OLD',IOSTAT=io_status)

    if (io_status /= 0) then
       write(*,*)
       write(*,*)
       write(*,*) 'Error reading file: '//trim(CARDSFNAME)
       stop ': Invalid file name (READ_CARDS).'
    end if

    write(*,'(A)',advance='NO') 'Reading data ... '

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
       read(cards_unit,*) x0(ip1:ip2)
       read(cards_unit,*) v0(ip1:ip2)
    end do

    ! Computing the constants : -(mu+m(i))
    mm(1:nb) = 0-(mu+m(1:nb))

    ! The bodies for which we want the Closest Point Approach (CPA) data
    ! (should be in the range 1..nb and ip1 /= ip2)
    read(cards_unit,*) ip1, ip2

    if (ip1 == ip2 .or. (ip1 < 1 .or. nb < ip1) &
         .or. (ip2 < 1 .or. nb < ip2)) then
       write(*,*) 'P1 = ', ip1, ' P2 = ', ip2
       stop ': Wrong request for CPA parameters (READ_CARDS).'
    end if

    ! Now IP1 and IP2 point to the X coordinate of body P1 and P2,
    ! respectively...
    ip1 = 1+NDIM*(ip1-1)
    p1 = [ ip1, ip1+1, ip1+2 ]

    ip2 = 1+NDIM*(ip2-1)
    p2 = [ ip2, ip2+1, ip2+2 ]

    close(cards_unit)
    write(*,*) 'done!'
    write(*,*)
  end subroutine read_cards

  subroutine calc_orbit()
    use everhart_integrator, only: ra15_on, ra15_run, ra15_off

    real(WP) :: x(MAX_NV) = 0, v(MAX_NV) = 0, h

    h = h0
    x(1:nv) = x0(1:nv)
    v(1:nv) = v0(1:nv)

    write(*,*)
    write(*,'(A)',advance='NO') 'Computing the orbits ... '

    call ra15_on(nv,ll,NCLASS,force,data_file=FILENAME)
    call ra15_run(t0,t1,x(1:nv),v(1:nv),h)
    call ra15_off()

    write(*,*) 'done!'

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
  end subroutine calc_orbit

  subroutine process_orbit
    use, intrinsic :: iso_fortran_env, only: IOSTAT_END
    use julian_dates, only: jd2cal

    real(WP), parameter :: DQ_THRESHOLD = (0.1_WP)**2

    integer  :: nv0, ll0, nclass0, ns, data_unit, io_status, &
         year, month
    real(WP) :: h, t, x(MAX_NV) = 0, v(MAX_NV) = 0, &
         dq, dq_min, d(NDIM), t_cpa, p1_cpa(NDIM), p2_cpa(NDIM), &
         dq_ce, t_ce, p1_ce(NDIM), p2_ce(NDIM), day
    logical :: find_ce

    write(*,*)
    write(*,'(A)',advance='NO') 'Please wait, we are working ...'

    ! Start 'process_orbit()'...

    ! Opening DATA file
    open(NEWUNIT=data_unit,FILE=FILENAME,ACCESS='STREAM', &
         FORM='UNFORMATTED',STATUS='OLD',IOSTAT=io_status)

    if (io_status /= 0) then
       write(*,*)
       write(*,*)
       write(*,*) 'Error reading file: '//trim(FILENAME)
       stop ': Invalid file name (PROCESS_ORBIT).'
    end if

    read(data_unit) nv0, ll0, nclass0

    if (nv0 /= nv ) stop ': Mismatch for NV parameter (PROCESS_ORBIT).'
    if (ll0 /= ll ) stop ': Mismatch for LL parameter (PROCESS_ORBIT).'

    if (nclass0 /= NCLASS ) &
         stop ': Mismatch for NCLASS parameter (PROCESS_ORBIT).'

    ! Reading sequence n. 0, i.e. initial conditions. We do not test EOF
    ! because we assume at least a few sequences (NS > 1)
    read(data_unit) ns, h, t, x(1:nv), v(1:nv)

    ! Just to test/debug...
    !print *, 'NS,H,T,X,V =', ns, h, t, x(1:nv), v(1:nv)
    !

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
            stop ': Error occurred while reading file (PROCESS_ORBIT).'

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

  end subroutine process_orbit

  subroutine setup_params()
    logical, save :: first = .true.

    if (first) then
       call read_cards()
       first = .false.
    end if

    if (recalc) then
       call calc_orbit()
       call process_orbit()
       recalc = .false.
    end if

    len_axis = 10*k_view
  end subroutine setup_params

  subroutine setup_graphics()
    use SDL2_app, only: init_graphics
    real(WP) :: r

    call my_view%setup(k_view,phi,theta,alpha)

    r = my_view%get_radius()

    u_min = -r
    u_max = r
    v_min = -r
    v_max = r

    ! We clip everything that is outside the sphere of radius K_VIEW
    ! (for convenience we use its square)
    kq_clip = k_view**2

    call init_graphics('Apophis', &
           WIDTH=screen_width,HEIGHT=screen_height, &
           X1=u_min,X2=u_max,Y1=v_min,Y2=v_max)
  end subroutine setup_graphics

  subroutine plot()
    use, intrinsic :: iso_fortran_env, only: IOSTAT_END
    use SDL2_app, only: refresh, quit

    integer :: nv0, ll0, nclass0, ns, data_unit, io_status
    real(WP) :: h, t, t_old, x(MAX_NV) = 0, v(MAX_NV) = 0

    ! Opening DATA file
    open(NEWUNIT=data_unit,FILE=FILENAME,ACCESS='STREAM', &
         FORM='UNFORMATTED',STATUS='OLD',IOSTAT=io_status)

    if (io_status /= 0) then
       write(*,*)
       write(*,*)
       write(*,*) 'Error reading file: '//trim(FILENAME)
       stop ': Invalid file name (PLOT).'
    end if

    read(data_unit) nv0, ll0, nclass0

    if (nv0 /= nv ) stop ': Mismatch for NV parameter (PLOT).'
    if (ll0 /= ll ) stop ': Mismatch for LL parameter (PLOT).'

    if (nclass0 /= NCLASS ) &
         stop ': Mismatch for NCLASS parameter (PLOT).'

    ! Reading sequence n. 0, i.e. initial conditions. We do not test EOF
    ! because we assume at least a few sequences (NS > 1)
    read(data_unit) ns, h, t, x(1:nv), v(1:nv)

    call display_sun()

    t_old = t

    do
       if (abs(t-t_old) >= dt_rate) then
          t_old = t_old+dt_rate

          call display_bodies()
          call refresh()
       end if

       if (quit()) then
          write(*,*)
          write(*,*) 'Stopped at T : ', t
          write(*,*)
          exit
       end if

       ! We take another step...
       read(data_unit,iostat=io_status) ns, h, t, x(1:nv), v(1:nv)

       if (io_status == IOSTAT_END) exit
       if (io_status > 0) &
            stop ': Error occurred while reading file (PLOT).'
    end do

    close(data_unit)

  contains

    subroutine clip_in(up,vp)
      real(WP), intent(inout) :: up, vp

      if (up < u_min) then
         up = u_min
      else if (up > u_max) then
         up = u_max
      end if

      if (vp < v_min) then
         vp = v_min
      else if (vp > v_max) then
         vp = v_max
      end if
    end subroutine clip_in

    subroutine display_sun()
      use SDL2_app, only: draw_line, draw_point, set_rgba_color

      real(WP) :: u0, v0, u1, v1

      !
      ! Plotting the SUN and the axes
      !

      ! Project the origin of reference system (SUN)
      call my_view%do_projection([ 0.0_WP, 0.0_WP, 0.0_WP ],u0,v0)

      ! Plot X axis
      call set_rgba_color(255,0,0)       ! LIGHTRED
      call my_view%do_projection([ len_axis, 0.0_WP, 0.0_WP ],u1,v1)

      ! Clip the result
      call clip_in(u1,v1)
      call draw_line(u0,v0,u1,v1)

      ! Plot Y axis
      call set_rgba_color(0,255,0)       ! LIGHTGREEN
      call my_view%do_projection([ 0.0_WP, len_axis, 0.0_WP ],u1,v1)

      ! Clip the result
      call clip_in(u1,v1)
      call draw_line(u0,v0,u1,v1)

      ! Plot Z axis
      call set_rgba_color(0,0,255)       ! LIGHTBLUE
      call my_view%do_projection([ 0.0_WP, 0.0_WP, len_axis ],u1,v1)

      ! Clip the result
      call clip_in(u1,v1)
      call draw_line(u0,v0,u1,v1)

      ! Plot the SUN
      call set_rgba_color(255,255,0)     ! YELLOW
      call draw_point(u0,v0)
    end subroutine display_sun

    subroutine display_bodies()
      use SDL2_app, only: draw_point, set_rgba_color

      integer  :: k, kp
      real(WP) :: dq, d(NDIM), u, v

      ! Plotting planets at current position
      do k = 1, nb
         kp = 1+NDIM*(k-1)

         ! We could associate d => x(kp:kp+2), and then dq =>
         ! dot_product(d,d) instead of using d, dq variables
         d = x(kp:kp+2)
         dq = dot_product(d,d)

         ! We draw only points inside the clipping sphere...
         if (dq < kq_clip) then
            call my_view%do_projection(d,u,v)

            ! Clipping with respect to the frame in U-V space
            if ((u_min < u .and. u < u_max) .and. &
                 (v_min < v .and. v < v_max)) then
               call set_rgba_color(BODY_COLOR(k)%r,BODY_COLOR(k)%g, &
                    BODY_COLOR(k)%b)
               call draw_point(u,v)
            end if
         end if
      end do
    end subroutine display_bodies
  end subroutine plot

  subroutine run()
    use SDL2_app, only: clear_screen, close_graphics, QUIT_EVENT, get_event

    integer :: ievent = -1000

    call setup_params()
    call setup_graphics()

    ! We need to reset IEVENT if we want to restart the run
    ievent = -1000
    do while (ievent /= QUIT_EVENT)

       call clear_screen()

       call plot()

       write(*,*)
       write(*,'(A)',advance='NO') &
            'Press a Q/ESC or click X to quit graphics when done ... '

       ievent = get_event()
    end do

    write(*,*)
    call close_graphics()
  end subroutine run

  subroutine show_params()

    character(len=*), parameter :: METHOD = 'RADAU-15'

    write(*,*) 'Current parameters:'
    write(*,*)
    write(*,*) 'USING '//METHOD//' METHOD'
    write(*,*)
    write(*,*) 'NB      = ', nb
    write(*,*)
    write(*,*) 'T0 (JD) = ', t0
    write(*,*) 'T1 (JD) = ', t1
    write(*,*) 'H0  (D) = ', h0
    write(*,*)
    write(*,*) 'LL = ', ll, '(', 10.0_WP ** (-ll), ')'
    write(*,*)
    write(*,*) 'K_VIEW (AU) = ', k_view
    write(*,*) 'PHI   (DEG) = ', phi
    write(*,*) 'THETA (DEG) = ', theta
    write(*,*) 'ALPHA (DEG) = ', alpha
    write(*,*)
    write(*,*) 'DT_RATE (D) = ', dt_rate
    write(*,*)
    write(*,*) 'SCREEN WIDTH  (pixels) = ', screen_width
    write(*,*) 'SCREEN HEIGHT (pixels) = ', screen_height
    write(*,*)
  end subroutine show_params

  subroutine show_menu()
    write(*,*) 'Choose item:'
    write(*,*) '  T : T1 Time'
    write(*,*) '  H : Step'
    write(*,*) '  L : ACCURACY (LL)'
    write(*,*) '  K : KView'
    write(*,*) '  P : Polar angles'
    write(*,*) '  A : Field angle'
    write(*,*) '  D : DT Rate'
    write(*,*) '  S : Screen Size'
    write(*,*) '  R : RUN'
    write(*,*) '  Q : QUIT'
  end subroutine show_menu

  subroutine process_menu(ikey)
    use getdata, only: get

    integer, intent(in) :: ikey

    select case (ikey)
    case (ichar('T'))
       call get('T1 (JD) = ',t1)
       recalc = .true.
    case (ichar('H'))
       call get('H0 (D) = ',h0)
       recalc = .true.
    case (ichar('L'))
       call get('LL = ',ll)

       if (ll > 20) stop ': LL too big parameter (PROCESS_MENU).'

       recalc = .true.
    case (ichar('K'))
       call get('K_VIEW (AU) = ',k_view)
    case (ichar('P'))
       call get('PHI   (DEG) = ',phi)
       call get('THETA (DEG) = ',theta)
    case (ichar('A'))
       call get('ALPHA (DEG) = ',alpha)
    case (ichar('D'))
       call get('DT RATE (D) = ',dt_rate)
    case (ichar('S'))
       call get('SCREEN WIDTH  (pixels) = ',screen_width)
       call get('SCREEN HEIGHT (pixels) = ',screen_height)

    case (ichar('R'))
       call run()
    end select

    write(*,*)
  end subroutine process_menu

  subroutine app_menu()
    use getdata, only: get

    character :: key = 'R'
    integer :: ikey = ichar('R') ! Default

    do while (ikey /= ichar('Q'))
       call show_params()
       call show_menu()

       call get('Choice :',key)

       ! Convert in upcase if not
       if ('a' <= key .and. key <= 'z') then
          ikey = ichar(key)-32
       else
          ikey = ichar(key)
       end if

       write(*,*)

       call process_menu(ikey)
    end do
    write(*,*) 'All done.'
  end subroutine app_menu
end module apophis_lib

program apophis
  use apophis_lib

  call app_menu()
end program apophis
