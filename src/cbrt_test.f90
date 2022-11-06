!
! gfortran -std=f2018 -O2 -Wall cbrt_test.f90 -o cbrt_test.out
!
!
! For a discussion of CBRT function implementation see
!
!   https://github.com/symengine/symengine/pull/1644
!
! and this comment
!
!   https://github.com/symengine/symengine/pull/1644#issuecomment-597239761
!

module types

  implicit none
  private

  integer, parameter, public :: SP = selected_real_kind(6,30)
  integer, parameter, public :: DP = selected_real_kind(15,307)
  integer, parameter, public :: EP = selected_real_kind(18,90)
  integer, parameter, public :: QP = selected_real_kind(30,300)

  ! Working (FP) Precision
  integer, parameter, public :: WP = QP

end module types

! module konsts
!   use types, only: WP

!   implicit none
!   private

!   real(WP), parameter, public :: MACHEPS = epsilon(1.0_WP)
!   real(WP), parameter, public :: EPS_MIN = 1000*MACHEPS

! end module konsts

module cbrt_lib
  use types, only: WP, SP, DP

  implicit none
  private

  ! Overloading
  interface cbrt
     module procedure cbrt_fike, cbrt_complex
  end interface

  public :: cbrt_apple, cbrt

contains

  ! function cbrt(x) result (y)
  !   real(WP), intent(in) :: x
  !   real(WP) :: y

  !   real(WP) ::  a(2),r(2),d1,d2
  !   integer :: i

  !   a(1) = x
  !   a(2) = 1-x

  !   do i = 1,2
  !      r(i) = a(i)**(1.0_WP/3)
  !   enddo

  !   d1 = r(1)+1-x*0.12614422239872725_WP-r(2)
  !   d1 = d1*1.6765391932197435_WP
  !   d2 = (a(2)*a(2)+x*x)*4+1
  !   d2 = d2*0.005_WP*a(2)*x*(1-x*2)

  !   y = d1-d2
  ! end function cbrt

  ! recursive function cbrt(x) result (y)
  !   real(WP), intent(in) :: x
  !   real(WP) :: y

  !   real(WP), parameter :: C1 = 0.12614422239872725_WP, &
  !        C2 = 1.6765391932197435_WP

  !   real(WP) :: e, f

  !   e = x
  !   f = 1.0 - x

  !   y = (cbrt(e)+1-e*C1-cbrt(f))*C2-((f*f+e*e)*4+1)*0.005_WP*f*e*(1-e*2)
  ! end function cbrt

  ! Adapted from:
  ! https://people.freebsd.org/~lstewart/references/apple_tr_kt32_cuberoot.pdf
  function cbrt_apple(v) result (r)
    real(WP), intent(in) :: v
    real(WP) :: r

    real(WP), parameter :: C13 = 1.0_WP/3, C23 = 2.0_WP/3

    real(WP) :: fr, x
    integer :: ex, shx

    x = abs(v)

    ! Argument reduction
    !
    ! Separate into mantissa and exponent
    fr = fraction(x)
    ex = exponent(x)

    shx = mod(ex,3)

    ! Compute SHX such that (EX-SHX) is divisible by 3
    if (shx > 0) shx = shx-3

    ! Exponent of cube root
    ex = (ex-shx)/3

    ! 0.125 <= fr < 1.0
    ! SCALE(X,I) = X * RADIX(X)**I, the equivalent of C LDEXP(X,I)
    fr = scale(fr,shx)

    ! Compute seed with a quadratic approximation
    !
    ! 0.5 <= fr < 1
    fr = (-0.46946116_WP*fr+1.072302_WP)*fr+0.3812513_WP

    ! 6 bits of precision
    r = scale(fr,ex)

    ! Newton-Raphson iterations
    !
    ! 12 bits
    r = C23*r+C13*x/(r*r)

    ! 24 bits
    r = C23*r+C13*x/(r*r)

    if (WP == SP) then
       r = sign(r,v)
       return
    end if

    ! 48 bits
    r = C23*r+C13*x/(r*r)

    ! 96 bits
    r = C23*r+C13*x/(r*r)

    if (WP == DP) then
       r = sign(r,v)
       return
    end if

    ! 192 bits QP
    r = C23*r+C13*x/(r*r)

    r = sign(r,v)

  end function cbrt_apple

  ! Implemented from https://epdf.tips (Computer Evaluation of
  ! Mathematical Functions, C. T. FIKE) and suggestions extracted from
  ! above (apple_tr_kt32_cuberoot.pdf)
  !
  function cbrt_fike(v) result (y)
    real(WP), intent(in) :: v
    real(WP) :: y

    ! C16 = 1/(16)**(1/3), C256 = 1/(256)**(1/3)
    real(WP), parameter :: A = 0.391807_WP, B = 0.706799_WP, &
         C16 = 0.396850262992049869_WP, &
         C256 = 0.157490131236859146_WP

    real(WP) :: m, x
    integer :: q, r

    x = abs(v)

    ! Argument reduction
    !
    ! Separate into mantissa and exponent
    m = fraction(x)
    q = exponent(x)

    r = mod(q,4)

    ! Compute R such that (Q-R) is divisible by 4
    if (r > 0) r = r-4

    ! Exponent of cube root
    q = (q-r)/4

    ! 0.0625 <= m < 1.0  (1/16 == 0.625)
    ! SCALE(X,I) = X * RADIX(X)**I, the equivalent of C LDEXP(X,I)
    m = scale(m,r)

    ! Compute seed with a linear approximation (r(mp)). See FIKE book
    m = A+B*m

    r = mod(q,3)

    select case (r)
    case (0)
       ! 16 ** (q/3)r(mp)
       y = scale(m,4*(q/3))
    case (2)
       ! 16 ** ((q+1)/3)[r(mp)/16**(1/3)]
       y = scale(m*C16,4*((q+1)/3))
    case (1)
       ! 16 ** ((q+2)/3)[r(mp)/256**(1/3)]
       y = scale(m*C256,4*((q+2)/3))
    case default
       ! 16 ** (q/3)r(mp)
       y = scale(m,4*(q/3))
    end select

    ! Newton-Raphson iterations
    !
    ! RHO(0) = 2 ** (-3.34)
    ! RHO(k+1) = (2/3)*RHO(k) ** 3,  k = 0, 1, 2, ...:
    !
    !   RHO(1) = 2 ** (-10.605)
    !   RHO(2) = 2 ** (-32.400)
    !   RHO(3) = 2 ** (-97.785)
    !   RHO(4) = 2 ** (-293.940)
    !
    ! RHO is, say, the tolerance (DELTA X/ X)
    !

    ! First iteration (say 10 bit)
    m = y*y
    y = y-(m-(x/y))/(2*y+x/m)

    ! Second iteration (say 32 bit)
    m = y*y
    y = y-(m-(x/y))/(2*y+x/m)

    ! Third iteration (say 97 bit)
    m = y*y
    y = y-(m-(x/y))/(2*y+x/m)

    ! Fourth iteration (say 293 bit)
    m = y*y
    y = y-(m-(x/y))/(2*y+x/m)

    y = sign(y,v)

  end function cbrt_fike

  ! Return ALL the three complex roots
  function cbrt_complex(z) result(w)
    complex(WP), intent(in) :: z
    complex(WP) :: w(3)

    real(WP), parameter :: S3 = sqrt(3.0_WP)

    real(WP) :: phi, a, b, r, a3, b3

    a = real(z)
    b = aimag(z)

    ! arg(z)/3
    phi = atan2(b,a)/3

    ! CBRT(R)
    r = abs(z)
    r = cbrt_fike(r)

    a = r*cos(phi)
    b = r*sin(phi)

    w(1) = cmplx(a,b,kind=WP)

    a = -0.5_WP*a
    b = -0.5_WP*b

    a3 = S3*a
    b3 = S3*b

    w(2) = cmplx(a+b3,b-a3,kind=WP)
    w(3) = cmplx(a-b3,b+a3,kind=WP)

  end function cbrt_complex
end module cbrt_lib

program cbrt_test
  use types, only: WP
  use cbrt_lib, only: cbrt_apple, cbrt
  implicit none

  integer :: i
  real(WP) :: x, x3, fx3

  print *, cbrt_apple(1.0_WP), cbrt_apple(-1.0_WP)
  print *, cbrt_apple(8.0_WP), cbrt_apple(-8.0_WP)
  print *, cbrt_apple(27.0_WP), cbrt_apple(-27.0_WP)
  print *, cbrt_apple(64.0_WP), cbrt_apple(-64.0_WP)

  ! This app:
  !   4.32674871092222514696491493234032892 (QP)
  !
  ! Online (https://keisan.casio.com/calculator):
  !   4.3267487109222251469649149323403287652 (38 digits)
  print *, cbrt_apple(81.0_WP), cbrt_apple(-81.0_WP)
  print *, cbrt_apple(125.0_WP), cbrt_apple(-125.0_WP)
  print *, cbrt_apple(1000.0_WP), cbrt_apple(-1000.0_WP)

  ! print *, exponent(1.0_WP), fraction(1.0_WP)
  ! print *, exponent(sqrt(2.0_WP)), fraction(sqrt(2.0_WP))
  ! print *, exponent(25.0_WP), fraction(25.0_WP)
  ! print *, exponent(1024.0_WP), fraction(1024.0_WP)

  ! x = 178.1387e-4_WP
  ! print *, fraction(x), exponent(x), radix(x), &
  !      x * radix(x)**(-exponent(x)), &
  !      radix(x), exponent(x), &
  !      radix(x)**(exponent(x)), 2**(-5), &
  !      fraction(x) * radix(x)**(exponent(x))

  print *
  print *, cbrt(1.0_WP), cbrt(-1.0_WP)
  print *, cbrt(8.0_WP), cbrt(-8.0_WP)
  print *, cbrt(27.0_WP), cbrt(-27.0_WP)
  print *, cbrt(64.0_WP), cbrt(-64.0_WP)
  print *, cbrt(81.0_WP), cbrt(-81.0_WP)
  print *, cbrt(125.0_WP), cbrt(-125.0_WP)
  print *, cbrt(1000.0_WP), cbrt(-1000.0_WP)

  ! Adapted from:
  !
  ! https://github.com/fortran-lang/stdlib/issues/214#issuecomment-656625976
  !
  do i = 1, 100
     call random_number(x)
     x = x*10.0_WP**i
     x3 = cbrt_apple(x)
     fx3 = cbrt(x)
     write(*,*) i, x, abs(x-x3**3)/x, abs(x-fx3**3)/x
  end do

  print *

  ! https://github.com/symengine/symengine/pull/1644
  print *, cbrt((-8.0_WP,0))

  ! CBRT() example 2 from IMSL, https://help.imsl.com/fortran/current/pdf/Fortran_Special_Functions_Library.pdf, page 19
  print *, cbrt((-3.0_WP,0.0076_WP))

  print *, cbrt((1.0_WP,0.0_WP))
  print *, cbrt((-1.0_WP,0.0_WP))
end program cbrt_test
