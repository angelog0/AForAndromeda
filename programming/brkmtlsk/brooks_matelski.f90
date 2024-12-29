!
!   rm -rf *.mod; \
!     gfortran[-mp-X] [-g3 -fbacktrace -fcheck=all] [-march=native] \
!       -Wall -std=f2018 [-fmax-errors=1] -O3 \
!       brooks_matelski.f90 -o brooks_matelski$EXE $LIBS; \
!   rm -rf *.mod
!
! ./brooks_matelski
!
! For the build on GNU/Linux [OSX+MacPorts X server], is:
!
!   EXE = .out
!
! while for the build on MSYS2 is:
!
!   EXE = -$MSYSTEM (or EMPTY)
!
!   For a static build (run from Explorer), I have found usefull
!
!     LIBS = -static -lmingw32 -lws2_32 -ldinput8 \
!            -ldxguid -ldxerr8 -luser32 -lgdi32 -lwinmm -limm32 -lole32 \
!            -loleaut32 -lshell32 -lversion -luuid -lcomdlg32 -lhid -lsetupapi
!
!   In this case one should avoid to use '-march=native' flag because
!   it makes the binaries not portable: on another machine they crash
!   (abort).
!
!   See as references:
!
!     1. https://stackoverflow.com/questions/53885736/issues-when-statically-compiling-sdl2-program
!     2. https://groups.google.com/g/comp.lang.fortran/c/Usgys7Gww6o/m/CYEfzQfbhckJ
!
!
! Just another rewrite of the code present here:
!
!   https://scruss.com/blog/2020/07/07/the-mandelbrot-set-before-mandelbrot
!
! See also: https://fortran-lang.discourse.group/t/first-mandelbrot-set/2198
!
!
! Brooks and Matelski paper:
!
! https://abel.math.harvard.edu/archive/118r_spring_05/docs/brooksmatelski.pdf
!

program brooks_matelski
  implicit none

  ! GENERATE FIGURE FROM BROOKS-MATELSKI PAPER C.1978
  ! THAT EVENTUALLY BECAME KNOWN AS THE MANDELBROT SET
  ! - SCRUSS, 2022-05
  ! REF: BROOKS, ROBERT, AND J. PETER MATELSKI.
  ! "THE DYNAMICS OF 2-GENERATOR SUBGROUPS OF PSL (2, C)."
  ! RIEMANN SURFACES AND RELATED TOPICS: PROCEEDINGS OF THE
  ! 1978 STONY BROOK CONFERENCE,
  ! ANN. OF MATH. STUD. VOL. 97. 1981: FIG. 2, P. 81

  integer, parameter :: ROWS = 31, COLS = 71, MAXIT = 200
  real, parameter :: X1S = 1.0, X2S = real(COLS), &
       Y1S = 1.0, Y2S = real(ROWS), &
       XMIN = -1.975, XMAX = 0.475, &
       YMIN = -0.8715, YMAX = 0.8715
  real, parameter :: SX = (XMAX-XMIN)/(X2S-X1S), &
       SY = (YMAX-YMIN)/(Y2S-Y1S)

  integer :: i, j, k
  real :: cr, ci
  complex :: c, z
  character(len=80) :: out = ''
  character :: ch

  ! See
  ! https://math.stackexchange.com/questions/2358889/grid-spacing-iterations-used-in-the-1978-first-published-rendering-of-the-mande
  !
  print *, 'SX = ', SX
  print *, 'SY = ', SY
  print *, 'SY/SX = ', SY/SX

  do j = 1, ROWS
     ci = map_y(real(j))
     do i = 1, COLS
        cr = map_x(real(i))
        c = cmplx(cr,ci)
        z = cmplx(0.0,0.0)
        ch = '*'
        do k = 1, MAXIT
           z = z**2+c
           if (abs(z) > 2.0) then
              ch = ' '
              exit
           end if
        end do
        out(i:i) = ch
     end do
     write(*,*) out
  end do

contains

  function map_x(xs) result (x)
    real, intent(in) :: xs
    real :: x

    x = XMIN+SX*(xs-X1S)
  end function map_x

  function map_y(ys) result (y)
    real, intent(in) :: ys
    real :: y

    y = YMIN+SY*(ys-Y1S)
  end function map_y

end program brooks_matelski
