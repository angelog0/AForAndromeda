!
! Author: Angelo Graziosi
!
!   created   : Oct 07, 2014
!   last edit : Jul 22, 2016
!
!   A few ODE integrators from CERNLIB
!

module cernlib_integrators
  use kind_consts, only: WP

  implicit none
  private

  abstract interface
     subroutine ode_field(x,y,f)
       use kind_consts, only: WP
       real(WP), intent(in) :: x, y(:)
       real(WP), intent(out) :: f(:)
     end subroutine ode_field
  end interface

  public :: drkstep, deqgbs, deqrkm

contains

  subroutine drkstep(n,h,x,y,w,sub)
    integer, intent(in) :: n
    real(WP), intent(in) :: h
    real(WP), intent(inout) :: x, y(:), w(:,:)
    procedure(ode_field) :: sub
    !
    !  FIRST-ORDER DIFFERENTIAL EQUATIONS (RUNGE-KUTTA)
    !
    !  THIS SUBROUTINE REPLACES X BY X+H AND ADVANCES THE SOLUTION OF THE
    !  SYSTEM OF DIFFERENTIAL EQUATIONS DY/DX=F(X,Y) FROM Y(X) TO Y(X+H)
    !  USING A FIFTH-ORDER RUNGE-KUTTA METHOD.
    !
    !  SUB IS THE NAME OF A SUBROUTINE SUB(X,Y,F) WHICH SETS THE VECTOR F
    !  TO THE DERIVATIVE AT X OF THE VECTOR Y.
    !
    !  W IS A WORKING-SPACE ARRAY, TREATED AS CONSISTING OF THREE
    !  CONSECUTIVE WORKING VECTORS OF LENGTH NEQ.
    !
    !  Adapted from CERNLIB drkstp.F:
    !
    !    http://cernlib.sourcearchive.com/documentation/2005.05.09.dfsg/
    !    drkstp_8F_source.html
    !
    real(WP), save :: h2, h6, xh, xh2

    h2 = 0.5_WP*h
    h6 = h/6.0_WP
    xh = x+h
    xh2 = x+h2

    ! Computing w(1:n,1) = K1
    call sub(x,y,w(1:n,1))

    ! Computing w(1:n,2) = y+H*K1/2
    w(1:n,2) = y(1:n)+h2*w(1:n,1)

    ! Computing w(1:n,3) = K2
    call sub(xh2,w(1:n,2),w(1:n,3))

    ! Computing w(1:n,1) = K1+2*K2
    w(1:n,1) = w(1:n,1)+2.0_WP*w(1:n,3)

    ! Computing w(1:n,2) = y+H*K2/2
    w(1:n,2) = y(1:n)+h2*w(1:n,3)

    ! Computing w(1:n,3) = K3
    call sub(xh2,w(1:n,2),w(1:n,3))

    ! Computing w(1:n,1) = (K1+2*K2)+2*K3
    w(1:n,1) = w(1:n,1)+2.0_WP*w(1:n,3)

    ! Computing w(1:n,2) = y+H*K3
    w(1:n,2) = y(1:n)+h*w(1:n,3)

    ! Computing w(1:n,3) = K4
    call sub(xh,w(1:n,2),w(1:n,3))

    ! Advance the solution Y(t+h) = Y(t) + H*[(K1+2*K2+2*K3)+K4]/6
    y(1:n) = y(1:n)+h6*(w(1:n,1)+w(1:n,3))

    x = xh
  end subroutine drkstep

  subroutine deqgbs(n,xa,xz,y,h0,eps,w,sub)
    integer, intent(in) :: n
    real(WP), intent(in) :: xa, xz, eps
    real(WP), intent(inout) :: y(:), h0, w(:,:)
    procedure(ode_field) :: sub
    !
    !  FIRST-ORDER DIFFERENTIAL EQUATIONS (GRAGG-BULIRSCH-STOER)
    !
    !  THIS SUBROUTINE ADVANCES THE SOLUTION, Y(X), OF THE
    !  SYSTEM OF DIFFERENTIAL EQUATIONS DY/DX=F(X,Y) FROM A SPECIFIED VALUE
    !  XA TO A SPECIFIED VALUE  XZ OF THE INDEPENDENT VARIABLE X.
    !  THE INTEGRATION STEP-LENGTH, H0, IS CHOSEN TO BE THE SMALLEST OF THE
    !  NUMBERS H0, H0/2, H0/4... FOR WHICH NOT MORE THAN 9 STAGES OF INTERNAL
    !  EXTRAPOLATION YIELD AN ESTIMATED ERROR LESS THAN EPS.
    !  EPS SHOULD NOT BE SMALLER THAN 1000 TIMES MACHINE PRECISION.
    !
    !  SUB IS THE NAME OF A SUBROUTINE SUB(X,Y,F) WHICH SETS THE VECTOR F
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
    !    http://cernlib.sourcearchive.com/documentation/2005.05.09.dfsg/
    !    deqbs64_8F_source.html
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
    if (n < 1 .or. xa == xz .or. h0 == 0) &
         return

    deltax = DELTA*abs(xz-xa)
    x = xa
    h1 = sign(abs(h0),xz-xa)
    sgh = sign(Z1,h1)

    main_loop: do
       w(1:n,28) = 0
       w(1:n,36) = 0
       w(1:n,23) = y(1:n)
       w(1:n,1:6) = 0
       if (sgh*(x+h1-xz) < 0) then
          hh = h1
          lfn = .false.
       else
          hh = xz-x
          if (abs(hh) < deltax) return
          lfn = .true.
       end if
       call sub(x,y,w(1:n,27))
       lbh = .false.

       do
          if (abs(hh) < deltax) then
             write(*,*)
             write(*,*) 'DEQGBS : TOO HIGH ACCURACY REQUIRED NEAR  x = ', x
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
                fc =C6*fc
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
                   call sub(x+k*g,w(1:n,25),w(1:n,26))
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

             call sub(a,w(1:n,25),w(1:n,26))
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
                if (lfn .or. abs(x-xz) < deltax) return
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
  end subroutine deqgbs

  subroutine deqrkm(n,xa,xz,y,h0,eps,w,sub)
    integer, intent(in) :: n
    real(WP), intent(in) :: xa, xz, eps
    real(WP), intent(inout) :: y(:), h0, w(:,:)
    procedure(ode_field) :: sub
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
    !  SUB IS THE NAME OF A SUBROUTINE SUB(X,Y,F) WHICH SETS THE VECTOR F
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

       call sub(x,y,w(1:n,1))
       w(1:n,1) = s3*w(1:n,1)
       w(1:n,6) = y(1:n)+w(1:n,1)

       call sub(x3,w(1:n,6),w(1:n,2))
       w(1:n,2) = s3*w(1:n,2)
       w(1:n,6) = y(1:n)+R2*(w(1:n,1)+w(1:n,2))

       call sub(x3,w(1:n,6),w(1:n,3))
       w(1:n,3) = s3*w(1:n,3)
       w(1:n,2) = 3*w(1:n,3)
       w(1:n,6) = y(1:n)+R4*(w(1:n,1)+w(1:n,2))

       call sub(x2,w(1:n,6),w(1:n,4))
       w(1:n,4) = s7*w(1:n,4)
       w(1:n,6) = y(1:n)+R5*(w(1:n,1)-w(1:n,2)+w(1:n,4))

       call sub(x1,w(1:n,6),w(1:n,5))
       w(1:n,5) = s3*w(1:n,5)
       w(1:n,6) = y(1:n)+R2*(w(1:n,1)+w(1:n,4)+w(1:n,5))

       do i = 1, n
          w(i,2) = abs(w(i,1)-R6*w(i,3)+w(i,4)-R2*w(i,5))
          w(i,1) = abs(w(i,6))
          if (w(i,2) > eps5*w(i,1)) then
             h1 = R2*hh
             if (abs(h1) < deltax) then
                write(*,*)
                write(*,*) 'DEQRKM : TOO HIGH ACCURACY REQUIRED NEAR  x = ', x
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
       if (lfn) return
       x = x1
    end do main_loop
  end subroutine deqrkm
end module cernlib_integrators
