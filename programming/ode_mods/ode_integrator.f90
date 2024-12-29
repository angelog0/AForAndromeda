!
! Author: ANGELO GRAZIOSI
!
!   created   : Jul 18, 2019
!   last edit : Jul 17, 2023
!
!   Simple module for ODE integration, being DY/DX = F(X,Y)
!   the ODE system.
!
!   The CALC() routines, for variable step algorithms, could not
!   return if the H step is too small (<~ 0.0075). This could cause
!   that the program hangs or takes a long long time to start
!   plotting. Indeed the STEP() routing could try to decrease the step
!   to reach a give precision, and this could never be reached...
!   CERNLIB manual says (D201-2):
!
!     For well-conditioned systems of equations any reasonable value
!     of the initial step length H0 may be chosen. For ill-conditioned
!     systems, the initial value of H0 maybe important, and tests with
!     different values are advised. An inappropriate choice may lead
!     to wrong results in such cases.
!

module ode_integrator
  use kind_consts, only: WP

  !
  ! Inspired by
  !
  !   http://degenerateconic.com
  !
  !   https://www.fortran90.org/src/best-practices.html#iii-private-module-variables
  !
  ! but somehow, we already have this programming pattern! See
  ! everhart_integrator.f90
  !

  implicit none
  private

  abstract interface
     subroutine field_fcn(x,y,f)
       import :: WP
       real(WP), intent(in) :: x, y(:)
       real(WP), intent(out) :: f(:)
     end subroutine field_fcn

     subroutine display_fcn(x,y)
       import :: WP
       real(WP), intent(in) :: x, y(:)
     end subroutine display_fcn

     subroutine step_fcn(h,x,y)
       import :: WP
       real(WP), intent(in) :: h
       real(WP), intent(inout) :: x, y(:)
     end subroutine step_fcn

     subroutine calc_fcn(x0,x1,y,h0)
       import :: WP
       real(WP), intent(in) :: x0, x1
       real(WP), intent(inout) :: y(:), h0
     end subroutine calc_fcn

     function quit_fcn() result(r)
       logical :: r
     end function quit_fcn
  end interface

  integer, parameter, public :: ID_RK4 = 1, ID_GBS = 2, ID_RKM = 3, ID_R15 = 4
  integer, parameter :: LL0 = 12

  integer :: n = 0, id_method = ID_RK4, ll = LL0
  real(WP), save, allocatable :: w(:,:)

  procedure(field_fcn), pointer :: field => null()
  procedure(display_fcn), pointer :: display => null()
  procedure(quit_fcn), pointer :: quit => null()
  procedure(step_fcn), pointer :: step => null()
  procedure(calc_fcn), pointer :: calc => null()

  ! For GBS and RKM methods, EPS should not be smaller than
  ! approximately 1000 times the machine precision
  real(WP) :: eps = 0

  logical :: save_data = .false.
  integer :: data_unit = 0

  ! Routines
  public :: step, calc, &
       ode_on, ode_integrate, ode_off

contains

  subroutine ode_on(neq,sub,display_data,method,data_file,accuracy,stop_run)
    use everhart_integrator, only:  ra15_on
    integer, intent(in) :: neq
    procedure(field_fcn) :: sub
    procedure(display_fcn), optional :: display_data
    integer, intent(in), optional :: method
    character(len=*), intent(in), optional :: data_file
    integer, intent(in), optional :: accuracy
    procedure(quit_fcn), optional :: stop_run

    real(WP), parameter :: MACHEPS = epsilon(1.0_WP)
    real(WP), parameter :: EPS_MIN = 1000*MACHEPS

    ! NV, The number of working vectors of length NEQ. If default is RK4,
    ! then default NV = 3. For RADAU15 the default precision is
    ! SS = 10**(-LL); notice that in this case it is the absolute NOT
    ! the relative precision: "Thus SS = 10.**(-LL) controls the size of
    ! the last term in a series". As stated else where, in this module
    ! we ONLY use NCLASS==1 problems, i.e. y'(x) = F(x,y)
    integer, parameter :: NCLASS = 1
    integer :: nv = 3, ierr = 0

    n = neq
    field => sub

    ! Set ID_METHOD and friends
    if (present(method)) then
       select case(method)
       case (ID_RK4)
          id_method = ID_RK4
          step => rk4_step
          calc => rk4_calc
          nv = 3
       case (ID_GBS)
          id_method = ID_GBS
          step => gbs_step
          calc => gbs_calc
          nv = 36
       case (ID_RKM)
          id_method = ID_RKM
          step => rkm_step
          calc => rkm_calc
          nv = 6
       case (ID_R15)
          id_method = ID_R15
          step => r15_step
          calc => r15_calc
          ! For RADAU15 NV is NEQ, as always..
          nv = neq
       case default
          ! Just for a wrong ID value
          id_method = ID_RK4
          step => rk4_step
          calc => rk4_calc
          nv = 3
       end select
    else
       ! Our default step
       id_method = ID_RK4
       step => rk4_step
       calc => rk4_calc
       nv = 3
    end if

    !
    ! Now ID_METHOD is in the interval [ID_RK4,ID_R15 ] even if METHOD
    ! has a wrong value. This is the reason to use ID_METHOD from here on
    !

    if (present(display_data)) then
       display => display_data
    else
       display => display_default
    end if

    if (present(stop_run)) then
       quit => stop_run
    else
       quit => quit_default
    end if

    if (present(accuracy)) ll = abs(accuracy)

    if (id_method /= ID_R15) then
       if (id_method /= ID_RK4) then
          eps = 10.0_WP**(-ll)
          if (eps < EPS_MIN) then
             write(*,*) 'EPS = ', eps
             write(*,*) 'WARNING: EPS < 1000*MACHEPS =', EPS_MIN, ' (ODE_ON).'
          end if
       end if

       ! WORK SPACE ALLOCATION
       allocate(w(neq,nv),stat=ierr)
       if (ierr /= 0) stop ': Allocation failure for W(:,:) (ODE_ON).'

       if (present(data_file)) then
          save_data = .true.
       else
          save_data = .false.
       end if

       ! Opening DATA file: 'data_file' MUST BE a valid file name
       if (save_data) then
          open(newunit=data_unit,file=data_file,access='STREAM', &
               form='UNFORMATTED',status='REPLACE')
          write(data_unit) neq, method
       end if
    else
       if (ll > 20) then
          write(*,*) 'LL = ', ll
          stop ': LL > 20 TOO LARGE (ODE_ON).'
       end if

       !write(*,*) 'RA15 initialized with LL=', ll
       !write(*,*)
       if (present(data_file)) then
          call ra15_on(nv,ll,NCLASS,force,DATA_FILE=data_file)
       else
          call ra15_on(nv,ll,NCLASS,force)
       end if
    end if

    !print *, ' ******* ==> ', eps, id_method, ll

  contains

    subroutine force(x,y,yp,f)
      real(WP), intent(in) :: x, y(:), yp(:)
      real(WP), intent(out) :: f(:)
      call field(x,y,f)
    end subroutine force
  end subroutine ode_on

  subroutine ode_off()
    use everhart_integrator, only:  ra15_off
    integer :: ierr = 0

    if (id_method /= ID_R15) then
       ! Closing DATA file
       if (save_data) close(data_unit)
       save_data = .false.
       data_unit = 0

       ! WORK SPACE DEALLOCATION
       if (allocated(w)) deallocate(w,stat=ierr)
       if (ierr /= 0) stop ': Deallocation failure for W(:,:) (ODE_OFF).'
    else
       call ra15_off()
    end if

    if (associated(step)) nullify(calc)
    if (associated(step)) nullify(step)
    if (associated(quit)) nullify(quit)
    if (associated(display)) nullify(display)
    if (associated(field)) nullify(field)

    ! Reset
    if (n /= 0) n = 0
    if (id_method /= ID_RK4) id_method = ID_RK4
    if (ll /= LL0) ll = LL0
    if (eps /= 0) eps = 0

  end subroutine ode_off

  ! Integrate the ODE from X0 to X1 with constant step H0
  subroutine ode_integrate(x0,x1,y0,h0)
    real(WP), intent(in) :: x0, x1, y0(:), h0

    real(WP) :: x, xp, y(n), h
    logical :: last

    last = .false.
    x = x0
    y = y0
    h = sign(h0,x1-x0)
    do
       call display(x,y)

       if (last) exit

       if (quit()) then
          write(*,*)
          write(*,*) 'Stopped at X : ', x
          write(*,*)
          exit
       end if

       xp = x+h

       ! Adjust last time step
       last = ((h >= 0.0_WP .and. xp >= x1) .or. (h < 0.0_WP .and. xp <= x1))

       if (last) h = x1-x

       ! We take an integration step
       call step(h,x,y)
    end do
  end subroutine ode_integrate

  subroutine display_default(x,y)
    real(WP), intent(in) :: x, y(:)

    write(*,*) x, y(:)
  end subroutine display_default

  ! DEFAULT: not quit
  function quit_default() result(r)
    logical :: r
    r = .false.
  end function quit_default

  subroutine rk4_step(h,x,y)
    real(WP), intent(in) :: h
    real(WP), intent(inout) :: x, y(:)
    !
    !  THIS SUBROUTINE REPLACES X BY X+H AND ADVANCES THE SOLUTION OF THE
    !  SYSTEM OF DIFFERENTIAL EQUATIONS DY/DX=F(X,Y) FROM Y(X) TO Y(X+H)
    !  USING A FIFTH-ORDER RUNGE-KUTTA METHOD.
    !
    !  FIELD IS THE NAME OF A SUBROUTINE FIELD(X,Y,F) WHICH SETS THE VECTOR F
    !  TO THE DERIVATIVE AT X OF THE VECTOR Y.
    !
    !  W IS A WORKING-SPACE ARRAY, TREATED AS CONSISTING OF THREE CONSEC-
    !  UTIVE WORKING VECTORS OF LENGTH NEQ.
    !
    !  Adapted from CERNLIB drkstp.F:
    !
    !    http://cernlib.sourcearchive.com/documentation/2005.05.09.dfsg/
    !    drkstp_8F_source.html
    !
    real(WP), parameter :: Z1 = 1, HF = Z1/2, HS = Z1/6
    real(WP), save :: h2, h6, xh, xh2

    h2 = HF*h
    h6 = HS*h
    xh = x+h
    xh2 = x+h2

    ! Computing w(1:n,1) = K1
    call field(x,y,w(1:n,1))

    ! Computing w(1:n,2) = y+H*K1/2
    w(1:n,2) = y(1:n)+h2*w(1:n,1)

    ! Computing w(1:n,3) = K2
    call field(xh2,w(1:n,2),w(1:n,3))

    ! Computing w(1:n,1) = K1+2*K2
    w(1:n,1) = w(1:n,1)+2*w(1:n,3)

    ! Computing w(1:n,2) = y+H*K2/2
    w(1:n,2) = y(1:n)+h2*w(1:n,3)

    ! Computing w(1:n,3) = K3
    call field(xh2,w(1:n,2),w(1:n,3))

    ! Computing w(1:n,1) = (K1+2*K2)+2*K3
    w(1:n,1) = w(1:n,1)+2*w(1:n,3)

    ! Computing w(1:n,2) = y+H*K3
    w(1:n,2) = y(1:n)+h*w(1:n,3)

    ! Computing w(1:n,3) = K4
    call field(xh,w(1:n,2),w(1:n,3))

    ! Advance the solution Y(t+h) = Y(t) + H*[(K1+2*K2+2*K3)+K4]/6
    y(1:n)=y(1:n)+h6*(w(1:n,1)+w(1:n,3))

    x = xh
  end subroutine rk4_step

  subroutine rk4_calc(x0,x1,y,h0)
    real(WP), intent(in) :: x0, x1
    real(WP), intent(inout) :: y(:), h0

    real(WP) :: x, xp, h
    logical :: last

    last = .false.
    x = x0
    h = sign(h0,x1-x0)
    do
       if (save_data) write(data_unit) h, x, y(1:n)

       if (last) exit

       xp = x+h

       ! Adjust last time step
       last = ((h >= 0.0_WP .and. xp >= x1) .or. (h < 0.0_WP .and. xp <= x1))

       if (last) h = x1-x

       ! We take an integration step
       call rk4_step(h,x,y)
    end do
    h0 = h
  end subroutine rk4_calc

  subroutine gbs_calc(x0,x1,y,h0)
    real(WP), intent(in) :: x0, x1
    real(WP), intent(inout) :: y(:), h0
    !
    !  FIRST-ORDER DIFFERENTIAL EQUATIONS (GRAGG-BULIRSCH-STOER)
    !
    !  THIS SUBROUTINE ADVANCES THE SOLUTION, Y(X), OF THE
    !  SYSTEM OF DIFFERENTIAL EQUATIONS DY/DX=F(X,Y) FROM A SPECIFIED VALUE
    !  X0 TO A SPECIFIED VALUE  X1 OF THE INDEPENDENT VARIABLE X.
    !  THE INTEGRATION STEP-LENGTH, H0, IS CHOSEN TO BE THE SMALLEST OF THE
    !  NUMBERS H0, H0/2, H0/4... FOR WHICH NOT MORE THAN 9 STAGES OF INTERNAL
    !  EXTRAPOLATION YIELD AN ESTIMATED ERROR LESS THAN EPS.
    !  EPS SHOULD NOT BE SMALLER THAN 1000 TIMES MACHINE PRECISION.
    !
    !  FIELD IS THE NAME OF A SUBROUTINE FIELD(X,Y,F) WHICH SETS THE VECTOR F
    !  TO THE DERIVATIVE AT X OF THE VECTOR Y.
    !
    !  W IS A WORKING-SPACE ARRAY, TREATED AS CONSISTING OF 36 CONSECUTIVE
    !  WORKING VECTORS OF LENGTH NEQ.
    !
    !  This subroutine is based on the Algol procedure  diffsys as
    !  presented in R. Bulirsch and J. Stoer, Numerical Treatment of
    !  Ordinary Differential Equations by Extrapolation Methods,
    !  Numer. Math. 8 (1966) 1-13.  The adaption for integration over
    !  a given interval (not only over one step) is due to G. Janin.
    !
    !  Adapted from CERNLIB deqbs64.F:
    !
    !   https://github.com/apc-llc/cernlib/blob/master/2006/src/mathlib/gen/d/deqbs64.F
    !
    !   (http://cernlib.sourcearchive.com/documentation/2005.05.09.dfsg/deqbs64_8F_source.html)
    !
    real(WP), parameter :: DELTA = 1.0E-14_WP, &
         Z1 = 1, HF = Z1/2, C1 = 3*Z1/2, &
         C9 = 9, C6 = C9/15, &
         C2 = 16/C9, C3 = 64/C9, C4 = 256/C9, C5 = C9/4
    integer, save :: i, m, ir, is, jj, j, l, kk, k
    logical, save :: lfn, lbh, lbo, lcv
    real(WP), save :: deltax, x, h1, sgh, hh, a, fc, g, b, u, v, b1, c, ta
    !
    ! The statement:
    !   n = size(y)
    ! is wrong here because the size of Y is, generally, greater than the
    ! number of differential equations.
    !
    if (n < 1 .or. x0 == x1 .or. h0 == 0) &
         return

    deltax = DELTA*abs(x1-x0)
    x = x0
    h1 = sign(abs(h0),x1-x0)
    sgh = sign(Z1,h1)

    if (save_data) write(data_unit) h0, x, y(1:n)

    main_loop: do
       w(1:n,28) = 0
       w(1:n,36) = 0
       w(1:n,23) = y(1:n)
       w(1:n,1:6) = 0
       if (sgh*(x+h1-x1) < 0) then
          hh = h1
          lfn = .false.
       else
          hh = x1-x
          if (abs(hh) < deltax) return
          lfn = .true.
       end if

       ! We should extract GBS_STEP() from here... It prints
       ! X0, X0+H, X0+2H, etc.
       !print *, 'X = ', x

       call field(x,y,w(1:n,27))
       lbh = .false.

       do
          if (abs(hh) < deltax) then
             write(*,*)
             write(*,*) 'GBS_CALC : TOO HIGH ACCURACY REQUIRED NEAR  x = ', x
             write(*,*)
             return ! or STOP
          end if
          a = x+hh
          fc = C1
          lbo = .false.
          m = 1
          ir = 2
          is = 3
          jj = 6

          do j = 0, 9
             if (lbo) then
                w(1,30) = C2
                w(1,32) = C3
                w(1,34) = C4
             else
                w(1,30) = C5
                w(1,32) = C9
                w(1,34) = 36
             end if
             lcv = j > 2
             if (j > 6) then
                l = 6
                w(1,35) = 64
                fc = C6*fc
             else
                l = j
                w(1,l+29) = m*m
             end if
             m = m+m
             g = hh/m
             b = g+g
             if (lbh .and. j < 8) then
                w(1:n,25) = w(1:n,j+15)
                w(1:n,24) = w(1:n,j+7)
             else
                kk = (m-2)/2
                m = m-1
                w(1:n,24) = w(1:n,23)
                w(1:n,25) = w(1:n,23)+g*w(1:n,27)
                do k = 1, m
                   call field(x+k*g,w(1:n,25),w(1:n,26))
                   do  i = 1, n
                      u = w(i,24)+b*w(i,26)
                      w(i,24) = w(i,25)
                      w(i,25) = u
                      w(i,28) = max(w(i,28),abs(u))
                   end do
                   if (k == kk .and. k /= 2) then
                      jj = jj+1
                      w(1:n,jj+8) = w(1:n,25)
                      w(1:n,jj) = w(1:n,24)
                   end if
                end do
             end if

             call field(a,w(1:n,25),w(1:n,26))
             do i = 1, n
                v = w(i,36)
                w(i,36) = HF*(w(i,25)+w(i,24)+g*w(i,26))
                c = w(i,36)
                ta = c
                do k = 1, l
                   b1 = w(1,k+29)*v
                   b = b1-c
                   u = v
                   if (b /= 0) then
                      b = (c-v)/b
                      u = c*b
                      c = b1*b
                   end if
                   v = w(i,k)
                   w(i,k) = u
                   ta = u+ta
                end do
                if (abs(y(i)-ta) > eps*w(i,28)) lcv = .false.
                y(i) = ta
             end do
             if (lcv) then
                x = a
                h0 = h1

                if (save_data) write(data_unit) h0, x, y(1:n)

                if (lfn .or. abs(x-x1) < deltax) return
                h1 = fc*h1
                cycle main_loop
             end if
             w(1,31) = 4
             w(1,33) = 16
             lbo = .not.lbo
             m = ir
             ir = is
             is = m+m
          end do

          lbh = .not.lbh
          hh = HF*hh
          h1 = hh
          lfn = .false.
       end do
    end do main_loop
  end subroutine gbs_calc

  subroutine gbs_step(h,x,y)
    real(WP), intent(in) :: h
    real(WP), intent(inout) :: x, y(:)
    !
    !  THIS SUBROUTINE (WRAPPER) REPLACES X BY X+H AND ADVANCES THE
    !  SOLUTION OF THE SYSTEM OF DIFFERENTIAL EQUATIONS DY/DX=F(X,Y)
    !  FROM Y(X) TO Y(X+H) USING THE GRAGG-BULIRSCH-STOER METHOD.
    !
    real(WP), save :: x1, hp

    hp = h
    x1 = x+hp

    call gbs_calc(x,x1,y,hp)
    x = x1
  end subroutine gbs_step

  subroutine rkm_calc(xa,xz,y,h0)
    real(WP), intent(in) :: xa, xz
    real(WP), intent(inout) :: y(:), h0
    !
    !  FIRST-ORDER DIFFERENTIAL EQUATIONS (RUNGE-KUTTA-MERSON)
    !
    !  THIS SUBROUTINE ADVANCES THE SOLUTION, Y(X), OF THE
    !  SYSTEM OF DIFFERENTIAL EQUATIONS DY/DX=F(X,Y) FROM A SPECIFIED VALUE
    !  XA TO A SPECIFIED VALUE  XZ OF THE INDEPENDENT VARIABLE X.
    !  THE INTEGRATION STEP-LENGTH, H0, IS AUTOMATICALLY ADJUSTED TO KEEP THE
    !  ESTIMATED ERROR PER STEP LESS THAN THE SPECIFIED VALUE, EPS.
    !  EPS SHOULD NOT BE SMALLER THAN 1000 TIMES MACHINE PRECISION.
    !
    !  FIELD IS THE NAME OF A SUBROUTINE FIELD(X,Y,F) WHICH SETS THE VECTOR F
    !  TO THE DERIVATIVE AT X OF THE VECTOR Y.
    !
    !  W IS A WORKING-SPACE ARRAY, TREATED AS CONSISTING OF SIX CONSEC-
    !  UTIVE WORKING VECTORS OF LENGTH NEQ.
    !
    !  Based on a modification of the Runge-Kutta method suggested
    !  by Merson. See G.N. Lance, Numerical Methods for High speed
    !  Computers, Iliffe & Sons, London 1960, pp. 56-57
    !
    !  Adapted from CERNLIB deqmr64.F:
    !
    !    http://cernlib.sourcearchive.com/documentation/2005.05.09.dfsg/
    !    deqmr64_8F_source.html
    !
    ! See also:
    !
    !   http://www.encyclopediaofmath.org/index.php/Kutta-Merson_method
    !
    real(WP), parameter :: DELTA = 1.0E-14_WP, &
         Z1 = 1, R2 = Z1/2, R3 = Z1/3, &
         R4 = 3*Z1/8, R5 = 3*Z1/2, R6 = 9*Z1/2, &
         R7 = 4*Z1/3, R0 = Z1/32
    integer, save :: i
    logical, save :: ler, lfn
    real(WP), save :: deltax, eps5, eps0, x, h1, sgh, hh, s2, s3, s7, &
         x1, x2, x3
    !
    ! The statement:
    !   n = size(y)
    ! is wrong here because the size of Y is, generally, greater than the
    ! number of differential equations.
    !
    if (n < 1 .or. xa == xz .or. h0 == 0) &
         return

    deltax = DELTA*abs(xz-xa)
    eps5 = 5*abs(eps)
    eps0 = R0*eps5
    x = xa
    h1 = sign(abs(h0),xz-xa)
    sgh = sign(Z1,h1)

    if (save_data) write(data_unit) h0, x, y(1:n)

    main_loop: do
       if (sgh*(x+h1-xz) < 0) then
          hh = h1
          h0 = h1
          lfn = .false.
       else
          hh = xz-x
          if (abs(hh) < deltax) then
             y(1:n) = w(1:n,6)
             return
          end if
          lfn = .true.
       end if

       s2 = R2*hh
       s3 = R3*hh
       s7 = R7*hh
       x1 = x+hh
       x2 = x+s2
       x3 = x+s3

       call field(x,y,w(1:n,1))
       w(1:n,1) = s3*w(1:n,1)
       w(1:n,6) = y(1:n)+w(1:n,1)

       call field(x3,w(1:n,6),w(1:n,2))
       w(1:n,2) = s3*w(1:n,2)
       w(1:n,6) = y(1:n)+R2*(w(1:n,1)+w(1:n,2))

       call field(x3,w(1:n,6),w(1:n,3))
       w(1:n,3) = s3*w(1:n,3)
       w(1:n,2) = 3*w(1:n,3)
       w(1:n,6) = y(1:n)+R4*(w(1:n,1)+w(1:n,2))

       call field(x2,w(1:n,6),w(1:n,4))
       w(1:n,4) = s7*w(1:n,4)
       w(1:n,6) = y(1:n)+R5*(w(1:n,1)-w(1:n,2)+w(1:n,4))

       call field(x1,w(1:n,6),w(1:n,5))
       w(1:n,5) = s3*w(1:n,5)
       w(1:n,6) = y(1:n)+R2*(w(1:n,1)+w(1:n,4)+w(1:n,5))

       do i = 1, n
          w(i,2) = abs(w(i,1)-R6*w(i,3)+w(i,4)-R2*w(i,5))
          w(i,1) = abs(w(i,6))
          if (w(i,2) > eps5*w(i,1)) then
             h1 = R2*hh
             if (abs(h1) < deltax) then
                write(*,*)
                write(*,*) &
                     'RKM_CALC : TOO HIGH ACCURACY REQUIRED NEAR  x = ', x
                write(*,*)
                return ! or STOP
             end if
             cycle main_loop
          end if
       end do

       ler = .true.
       do i = 1, n
          ler = ler .and. w(i,2) < eps0*w(i,1)
       end do

       y(1:n) = w(1:n,6)

       if (ler) then
          h0 = h1+h1
          h1 = hh+hh
       end if

       if (save_data) write(data_unit) h0, x1, y(1:n)

       if (lfn) return
       x = x1
    end do main_loop
  end subroutine rkm_calc

  subroutine rkm_step(h,x,y)
    real(WP), intent(in) :: h
    real(WP), intent(inout) :: x, y(:)
    !
    !  THIS SUBROUTINE (WRAPPER) REPLACES X BY X+H AND ADVANCES THE
    !  SOLUTION OF THE SYSTEM OF DIFFERENTIAL EQUATIONS DY/DX=F(X,Y)
    !  FROM Y(X) TO Y(X+H) USING THE RUNGE-KUTTA-MERSON METHOD.
    !
    real(WP), save :: x1, hp

    hp = h
    x1 = x+hp

    call rkm_calc(x,x1,y,hp)
    x = x1
  end subroutine rkm_step

  ! Just a wrapper
  subroutine r15_calc(x0,x1,y,h0)
    use everhart_integrator, only:  ra15_run
    real(WP), intent(in) :: x0, x1
    real(WP), intent(inout) :: y(:), h0
    !  THIS SUBROUTINE (WRAPPER) ADVANCES THE SOLUTION, Y(X), OF THE
    !  SYSTEM OF DIFFERENTIAL EQUATIONS DY/DX=F(X,Y) FROM A SPECIFIED VALUE
    !  X0 TO A SPECIFIED VALUE  X1 OF THE INDEPENDENT VARIABLE X.
    !  THE INTEGRATION STEP-LENGTH, H0, IS AUTOMATICALLY ADJUSTED TO KEEP THE
    !  ESTIMATED ERROR PER STEP LESS THAN THE SPECIFIED VALUE.
    !
    ! In this module we use ONLY NCLASS==1 Radau problems
    real(WP) :: yp(n)

    call ra15_run(x0,x1,y,yp,h0)
  end subroutine r15_calc

  subroutine r15_step(h,x,y)
    real(WP), intent(in) :: h
    real(WP), intent(inout) :: x, y(:)
    !
    !  THIS SUBROUTINE (WRAPPER) REPLACES X BY X+H AND ADVANCES THE
    !  SOLUTION OF THE SYSTEM OF DIFFERENTIAL EQUATIONS DY/DX=F(X,Y)
    !  FROM Y(X) TO Y(X+H) USING THE RADAU15 METHOD.
    !
    real(WP), save :: x1, hp

    hp = h
    x1 = x+hp

    call r15_calc(x,x1,y,hp)
    x = x1
  end subroutine r15_step
end module ode_integrator
