!
! gfortran -O2 -Wall -Wno-unused-function solve_equation.f90 \
!   -o solve_equation.out
!
! RUN
!
!   ./solve_equation 0.5E-12
!   ./solve_equation
!
! References
!
!   Demidovich B. P. - Maron I. A. - COMPUTATIONAL MATHEMATHICS - MIR
!   (https://archive.org/details/computational-mathematics/mode/2up)
!
!   Demidovich et al. - PROBLEMS IN MATEMATICAL ANALYSIS - MIR
!   (https://archive.org/details/DemidovichEtAlProblemsInMathematicalAnalysisMir1970/mode/2up)
!

module types

  implicit none
  private

  integer, parameter, public :: DP = selected_real_kind(15,307)
  integer, parameter, public :: EP = selected_real_kind(18,90)
  integer, parameter, public :: QP = selected_real_kind(30,300)

  ! Working (FP) Precision
  integer, parameter, public :: WP = QP

end module types

module konsts
  use types, only: WP

  implicit none
  private

  real(WP), parameter, public :: MACHEPS = epsilon(1.0_WP)
  real(WP), parameter, public :: EPS_MIN = 1000*MACHEPS

end module konsts

module the_solver
  use types, only: WP

  implicit none
  private

  abstract interface
     function equation_function(x) result(func)
       import :: WP
       real(WP), intent(in) :: x
       real(WP) :: func
     end function equation_function
  end interface

  public :: fixed_point, zeroin

contains

  ! Fixed-point iteration.
  ! See https://onlinelibrary.wiley.com/doi/pdf/10.1002/9781118673515.app8
  !
  ! Solve X = PHI(X), given X0 and EPS in no more than MAXIT
  !
  !   X0        (REAL(WP)) On entry the starting (guess) value of the root.
  !             Unchanged on exit
  !
  !   EPS       (REAL(WP)) On entry the absolute accuracy. (EPS should not
  !             be smaller than approximately 10**3 times the machine
  !             precision). On exit the last |X(n)-X(n-1)| intervall.
  !
  !   MAXIT     (INTEGER) On entry the maximum number of iterations
  !             permitted. On exit the actual number of iterations
  !             needed to achieve convergence.
  !
  !   PHI       The user supplied FUNCTION to compute PHI(X) in X = PHI(X).
  !
  !   X         The value of the root, when it converges.
  !
  function fixed_point(x0,eps,maxit,phi) result (x)
    real(WP), intent(in) :: x0
    real(WP), intent(inout) :: eps
    integer, intent(inout) :: maxit
    procedure(equation_function) :: phi
    real(WP) :: x

    integer :: i
    real(WP) :: delta, delta_p, xm, xp
    save

    xm = x0

    x = phi(xm)
    delta = abs(x-xm)
    do i = 1, maxit
       xp = phi(x)
       delta_p = abs(xp-x)
       if (delta < eps .and. delta_p >= delta) exit
       xm = x
       x = xp
       delta = delta_p
    end do

    if (i > maxit) then
       write(*,*)
       write(*,*) 'TOO MANY ITERATIONS'
       write(*,*) 'I = ', i
       write(*,*)
    end if

    maxit = i
    eps = delta
  end function fixed_point

  !
  ! This is a remake of ZEROX CERNLIB routine:
  !
  !   https://github.com/apc-llc/cernlib/blob/master/2006/src/mathlib/gen/c/zerox64.F
  !
  ! Based on
  !
  !   J.C.P. Bus and T.J. Dekker, Two Efficient Algorithms with
  !   Guaranteed Convergence for Finding a Zero of a Function,
  !   ACM Trans. Math. Software 1 (1975) 330-345.
  !
  !   (MODE = 1: Algorithm M;    MODE = 2: Algorithm R)
  !
  !  http://www.dsc.ufcg.edu.br/~rangel/msn/downloads/p330-bus.pdf
  !
  function zeroin(a0,b0,eps,maxf,f,mode) result (c)
    real(WP), intent(in) :: a0, b0
    real(WP), intent(in) :: eps
    integer, intent(in) :: maxf
    procedure(equation_function) :: f
    integer, intent(in) :: mode
    real(WP) :: c

    integer, parameter :: IM1(2) = [ 2, 3 ], IM2(2) = [ -1, 3 ]
    real(WP), parameter :: Z1 = 1, HALF = Z1/2

    logical :: lmt(2)

    real(WP) :: a, b, d, fa, fb, fc, fd, fda, fdb, atl, tol, h, hb, &
         w, p, q
    integer :: mf, ie
    save

    if (mode /= 1 .and. mode /= 2) then
       c = 0
       write(*,*)
       write(*,'(a,i0,a)') 'MODE = ', mode, ' ILLEGAL'
       write(*,*)
       return
    end if

    fa = f(b0)
    fb = f(a0)

    if (fa*fb > 0) then
       c = 0
       write(*,*)
       write(*,'(2(a,g0))') 'F(A) AND F(B) HAVE THE SAME SIGN, A = ', &
            A0, ', B = ', B0
       write(*,*)
       return
    end if

    atl = abs(eps)
    b = a0
    a = b0
    lmt(2) = .true.
    mf = 2

    ! Initialization for the DO-WHILE loop: it could be simplified
    c = a
    fc = fa
    ie = 0

    if (abs(fc) < abs(fb)) then
       a = b
       b = c
       c = a
       fa = fb
       fb = fc
       fc = fa
    end if

    tol = atl*(1+abs(c))
    h = HALF*(c+b)
    hb = h-b
    ! END initialization for the DO-WHILE loop

    do while (abs(hb) > tol)

       if (ie > IM1(mode)) then
          w = hb
       else
          tol = tol*sign(Z1,hb)
          p = (b-a)*fb
          lmt(1) = ie <= 1

          if (lmt(mode)) then
             q = fa-fb
             lmt(2)=.false.
          else
             fdb = (fd-fb)/(d-b)
             fda = (fd-fa)/(d-a)
             p = fda*p
             q = fdb*fa-fda*fb
          end if

          if (p < 0) then
             p = -p
             q = -q
          end if

          if (ie == IM2(mode)) p = p+p

          if (p == 0 .or. p <= q*tol) then
             w = tol
          else if (p < hb*q) then
             w = p/q
          else
             w = hb
          end if

       end if

       d = a
       a = b
       fd = fa
       fa = fb
       b = b+w
       mf = mf+1

       if (mf > maxf) then
          write(*,*)
          write(*,*) 'TOO MANY FUNCTION CALLS'
          write(*,*)
          return
       end if

       fb = f(b)
       if (fb == 0 .or. sign(Z1,fc) == sign(Z1,fb)) then
          c = a
          fc = fa
       else
          if (w == hb) then
             ie = 0
          else
             ie = ie+1
          end if
       end if

       ! Re-computing the parameters for the next iteration
       if (abs(fc) < abs(fb)) then
          if (c /= a) then
             d = a
             fd = fa
          end if
          a = b
          b = c
          c = a
          fa = fb
          fb = fc
          fc = fa
       end if

       tol = atl*(1+abs(c))
       h = HALF*(c+b)
       hb = h-b
       ! END Re-computing the parameters for the next iteration

    end do

  end function zeroin

  function zeroin0(a0,b0,eps,maxf,f,mode) result (c)
    real(WP), intent(in) :: a0, b0
    real(WP), intent(in) :: eps
    integer, intent(in) :: maxf
    procedure(equation_function) :: f
    integer, intent(in) :: mode
    real(WP) :: c

    integer, parameter :: IM1(2) = [ 2, 3 ], IM2(2) = [ -1, 3 ]
    real(WP), parameter :: Z1 = 1, HALF = Z1/2

    logical :: lmt(2)

    real(WP) :: a, b, d, fa, fb, fc, fd, fda, fdb, atl, tol, h, hb, &
         w, p, q
    integer :: mf, ie

    if (mode /= 1 .and. mode /= 2) then
       c = 0
       write(*,*)
       write(*,'(a,i0,a)') 'MODE = ', mode, ' ILLEGAL'
       write(*,*)
       return
    end if

    fa = f(b0)
    fb = f(a0)

    if (fa*fb > 0) then
       c = 0
       write(*,*)
       write(*,'(2(a,g0))') 'F(A) AND F(B) HAVE THE SAME SIGN, A = ', &
            A0, ', B = ', B0
       write(*,*)
       return
    end if

    atl = abs(eps)
    b = a0
    a = b0
    lmt(2) = .true.
    mf = 2

    interpolate: do
       c = a
       fc = fa
       ie = 0

       extrapolate: do
          if (abs(fc) < abs(fb)) then
             if (c /= a) then
                d = a
                fd = fa
             end if
             a = b
             b = c
             c = a
             fa = fb
             fb = fc
             fc = fa
          end if

          tol = atl*(1+abs(c))
          h = HALF*(c+b)
          hb = h-b

          if (abs(hb) > tol) then

             if (ie > IM1(mode)) then
                w = hb
             else
                tol = tol*sign(Z1,hb)
                p = (b-a)*fb
                lmt(1) = ie <= 1

                if (lmt(mode)) then
                   q = fa-fb
                   lmt(2)=.false.
                else
                   fdb = (fd-fb)/(d-b)
                   fda = (fd-fa)/(d-a)
                   p = fda*p
                   q = fdb*fa-fda*fb
                end if

                if (p < 0) then
                   p = -p
                   q = -q
                end if

                if (ie == IM2(mode)) p = p+p

                if (p == 0 .or. p <= q*tol) then
                   w = tol
                else if (p < hb*q) then
                   w = p/q
                else
                   w = hb
                end if

             end if

             d = a
             a = b
             fd = fa
             fa = fb
             b = b+w
             mf = mf+1

             if (mf > maxf) then
                write(*,*)
                write(*,*) 'TOO MANY FUNCTION CALLS'
                write(*,*)
                return
             end if

             fb = f(b)
             if (fb == 0 .or. sign(Z1,fc) == sign(Z1,fb)) then
                cycle interpolate
             else
                if (w == hb) then
                   ie = 0
                else
                   ie = ie+1
                   cycle extrapolate
                end if
             end if
          end if
          exit interpolate
       end do extrapolate
    end do interpolate

  end function zeroin0

end module the_solver

program solve_equation
  use types, only: WP
  use konsts, only: EPS_MIN
  use the_solver, only: fixed_point, zeroin

  implicit none

  integer :: i, ndigits, maxiter = 500, niter
  real(WP) :: x0 = -2.0_WP, eps0 = 0.5E-12_WP, eps, x
  character (len=:), allocatable :: format_str
  character(len = 32) :: args

  i = 0
  do
     call get_command_argument(i,args)
     if (len_trim(args) == 0) exit

     if (i == 1) read(args,*) maxiter
     if (i == 2) read(args,*) x0
     if (i == 3) read(args,*) eps0

     i = i+1
  end do

  ! The numer of arguments that have been read is stored in 'i'
  !
  ! In this command line,
  !
  !   ./solve_nonlinear.out 0.5E-12
  !
  ! we have 2 arguments: the program name and the number '0.5E-12'
  !
  if (i > 4) stop ': USAGE: ./solve_nonlinear.out [MAXITER [X0 [EPS]]]'

  ! write(*,'(a)',advance='no') 'EPS: '
  ! read(*,*) eps

  if (eps0 < EPS_MIN) then
     write(*,*) 'EPS0 = ', eps0
     write(*,*) 'WARNING: EPS0 < 1000*MACHEPS =', EPS_MIN, ' (MAIN).'
  end if

  ndigits = abs(int(log10(eps0)))+1

  associate (digits => args)
    write(digits,'(i0)') ndigits

    format_str = '(a,i0,2(a,g0.'//trim(digits)//'))'

    write(*,*) WP, ndigits, ' ', trim(digits), ' ', format_str
  end associate

  eps = eps0
  niter = maxiter
  x = fixed_point(x0,eps,niter,phi)
  call print_result(format_str)

  eps = eps0
  niter = maxiter
  x = fixed_point(x0,eps,niter,phi1)
  call print_result(format_str)

  eps = eps0
  niter = maxiter
  x = fixed_point(x0,eps,niter,phi2)
  call print_result(format_str)

  x0 = 1.0_WP
  eps = eps0
  niter = maxiter
  x = fixed_point(x0,eps,niter,phi3)
  call print_result(format_str)

  x0 = 2.0_WP
  eps = eps0
  niter = maxiter
  x = fixed_point(x0,eps,niter,phi4)
  call print_result(format_str)

  x0 = 3.5_WP
  eps = eps0
  niter = maxiter
  x = fixed_point(x0,eps,niter,phi5)
  call print_result(format_str)

  x0 = 4.0_WP
  eps = eps0
  niter = maxiter
  x = fixed_point(x0,eps,niter,phi6)
  call print_result(format_str)

  eps = eps0
  niter = maxiter

  associate (maxf => niter, a0 => x0, b0 => x)
    a0 = 3.1_WP
    b0 = 3.2_WP

    write(*,*)
    write(*,format_str) 'MAXF = ', maxf,' A0 = ', a0, ' B0  = ', B0

    x = zeroin(a0,b0,eps,maxf,f,2)

    write(*,format_str) 'MAXF = ', maxf,' X  = ', x, ' TOL = ', eps
    write(*,*)

    a0 = 4.0_WP
    b0 = 5.0_WP

    write(*,*)
    write(*,format_str) 'MAXF = ', maxf,' A0 = ', a0, ' B0  = ', B0

    x = zeroin(a0,b0,eps,maxf,f1,2)

    write(*,format_str) 'MAXF = ', maxf,' X  = ', x, ' TOL = ', eps
    write(*,*)
  end associate

contains

  subroutine print_result(fmt)
    character(len=*), intent(in) :: fmt
    write(*,*)
    write(*,fmt) 'MAXITER = ', maxiter,' X0 = ', x0, ' EPS0 = ', eps0
    write(*,fmt) 'NITER = ', niter,' X = ', x, ' DELTA = ', eps
    write(*,*)
  end subroutine print_result

  function phi(x) result (r)
    real(WP), intent(in) :: x
    real(WP) :: r

    r = x-1.0_WP+2.0_WP*x*log(1.0_WP+x/(2.0_WP*x*x+1.0_WP))
  end function phi

  ! Does not converge (does not respect conditions for convergence)
  function phi1(x) result (r)
    real(WP), intent(in) :: x
    real(WP) :: r

    r = x*x

    r = 2.0_WP*r*log(1.0_WP+x/(2.0_WP*r+1.0_WP))
  end function phi1

  ! Converge slowly
  function phi2(x) result (r)
    real(WP), intent(in) :: x
    real(WP) :: r

    r = x-0.5_WP+x*log(1.0_WP+x/(2.0_WP*x*x+1.0_WP))
  end function phi2

  ! See: https://apod.nasa.gov/htmltest/gifcity/sqrt2.1mil
  !
  ! sqrt(2) up to 'ndigits'
  !
  function phi3(x) result (r)
    real(WP), intent(in) :: x
    real(WP) :: r

    r = -0.5_WP*x*x+x+1.0_WP
  end function phi3

  ! Golden section
  function phi4(x) result (r)
    real(WP), intent(in) :: x
    real(WP) :: r

    r = 1.0_WP+(1.0_WP/x)
  end function phi4

  ! PI
  function phi5(x) result (r)
    real(WP), intent(in) :: x
    real(WP) :: r

    r = x+sin(x)
  end function phi5

  ! Wien x0 = 5*(1-exp(-x0))
  ! X0 = 4.965114231744
  !
  ! lambda*T = b = (h*c)/(KB*x0)
  !
  ! http://personalpages.to.infn.it/~botta/AEM/AEMcapitolo1.pdf
  !
  function phi6(x) result (r)
    real(WP), intent(in) :: x
    real(WP) :: r

    r = 5.0_WP*(1.0_WP-exp(-x))
  end function phi6

  function f(x) result (r)
    real(WP), intent(in) :: x
    real(WP) :: r

    r = sin(x)
  end function f

  function f1(x) result (r)
    real(WP), intent(in) :: x
    real(WP) :: r

    r = x+5.0_WP*(exp(-x)-1.0_WP)
  end function f1

end program solve_equation
