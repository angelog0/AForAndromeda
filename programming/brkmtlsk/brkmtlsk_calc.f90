!
! Computing the figure of the Mandelbrot Set and saving it in a binary file
!
! The figure can be displayed with DISPLAY_BRKMTLSK
!
!
!   rm -rf *.mod; \
!     gfortran[-mp-X] [-g3 -fbacktrace -fcheck=all] [-march=native] \
!       -Wall -std=f2018 [-fmax-errors=1] -O3 \
!       -I ../finclude \
!       brkmtlsk_calc.f90 -o brkmtlsk_calc$EXE \
!       -L ../lib -lbasic_mods $LIBS; \
!   rm -rf *.mod
!
! ./brkmtlsk_calc 200 -0.75 0.0 2.5
!
!
! The input parameters are:
!
!   - MAXIT  : The maximum number of iterations
!   - XC, YC : The coordinates of center of the region in complex plane
!   - SIZE   : The size of the region in complex plane (a square)
!
! See: https://fortran-lang.discourse.group/t/first-mandelbrot-set/2198
!
! For the build on GNU/Linux [OSX+MacPorts X server], is:
!
!   EXE = .out
!
! while for the build on MSYS2 is:
!
!   EXE = -$MSYSTEM (or EMPTY)
!
! For a static build (run from Explorer), I have found usefull
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

program brkmtlsk_calc
  implicit none

  ! GENERATE FIGURE FROM BROOKS-MATELSKI PAPER C.1978
  ! THAT EVENTUALLY BECAME KNOWN AS THE MANDELBROT SET
  ! - SCRUSS, 2022-05
  ! REF: BROOKS, ROBERT, AND J. PETER MATELSKI.
  ! "THE DYNAMICS OF 2-GENERATOR SUBGROUPS OF PSL (2, C)."
  ! RIEMANN SURFACES AND RELATED TOPICS: PROCEEDINGS OF THE
  ! 1978 STONY BROOK CONFERENCE,
  ! ANN. OF MATH. STUD. VOL. 97. 1981: FIG. 2, P. 81

  integer, parameter :: ROWS = 900, COLS = 900
  character(len=*), parameter :: FNAME = 'brkmtlsk.data'

  integer :: i, j, k, maxit, code, data_unit
  real :: x0, y0, dsize, &
       xmin, xmax, ymin, ymax, &
       x1s, x2s, y1s, y2s, &
       sx, sy
  integer :: fig(ROWS,COLS) = 0
  real :: cr, ci
  complex :: c, z

  call get_parms(maxit,x0,y0,dsize)

  x1s = 1.0
  x2s = real(COLS)
  y1s = 1.0
  y2s = real(ROWS)

  xmin = x0-dsize/2
  xmax = xmin+dsize

  ymin = y0-dsize/2
  ymax = ymin+dsize

  sx = (xmax-xmin)/(x2s-x1s)
  sy = (ymax-ymin)/(y2s-y1s)

  print *, 'SX = ', sx
  print *, 'SY = ', sy
  print *, 'SY/SX = ', sy/sx

  write(*,'(A)',advance='NO') 'Please wait, we are working ... '

  ! Opening DATA file
  open(newunit=data_unit,file=FNAME,access='STREAM',form='UNFORMATTED', &
       status='REPLACE')

  write(data_unit) ROWS, COLS

  do j = 1, ROWS
     ci = map_y(real(j))
     do i = 1, COLS
        cr = map_x(real(i))
        c = cmplx(cr,ci)
        z = cmplx(0.0,0.0)

        ! Mset
        code = 0

        do k = 1, maxit
           z = z**2+c
           if (abs(z) > 2.0) then
              ! Escape velocity?
              code = k
              exit
           end if
        end do
        fig(j,i) = code
     end do
  end do

  write(data_unit) fig

  close(data_unit)

  write(*,*) 'done!'

contains

  subroutine get_parms(it,xc,yc,sz)
    integer, intent(out) :: it
    real, intent(out) :: xc, yc, sz

    character(len=80) :: args
    integer :: i

    i = 0
    do
       call get_command_argument(i,args)
       if (len_trim(args) == 0) exit

       if (i == 1) read(args,*) it
       if (i == 2) read(args,*) xc
       if (i == 3) read(args,*) yc
       if (i == 4) read(args,*) sz

       i = i+1
    end do

    ! The numer of arguments that have been read is stored in 'i'
    !
    ! In this command line,
    !
    !   ./brkmtlsk_calc.out 100 0.0, 0.0, 4.0
    !
    ! we have 5 arguments: the program name, the number of iteration,
    ! the x-center, the y-center and the size of the region
    !
    if (i /= 5) stop ': USAGE: ./brkmtlsk_calc.out ITER XC YC SIZE'

    ! Just a correction if needed
    it = abs(it)
    sz = abs(sz)
  end subroutine get_parms

  function map_x(xs) result (x)
    real, intent(in) :: xs
    real :: x

    x = xmin+sx*(xs-x1s)
  end function map_x

  function map_y(ys) result (y)
    real, intent(in) :: ys
    real :: y

    y = ymin+sy*(ys-y1s)
  end function map_y

end program brkmtlsk_calc
