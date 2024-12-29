!
! Author: ANGELO GRAZIOSI
!
!   created   : Dec 15, 2022
!   last edit : Dec 28, 2024
!
!   Display Brooks Matelski figure.
!   (Just a SDL2-Fortran app in less than 160 lines of code!)
!
! DESCRIPTION
!
!   Displaying the content of the files saved with BRKMTLSK_CALC.
!
!
! HOW TO BUILD THE APP (MSYS2, GNU/Linux, macOS)
!
!   cd programming
!
!   git clone https://github.com/interkosmos/fortran-sdl2.git
!
!   cd fortran-sdl2
!
!   make FFLAGS='[-march=native] -Wall -std=f2018 -fmax-errors=1 $(SDL_CFLAGS) -O3' all examples
!   mv libfortran-sdl2.a ../lib/
!   mv c_util.mod glu.mod sdl2*.mod ../finclude/
!   make clean
!   cd ..
!
!   cd basic_mods
!
!   make FFLAGS='[-march=native] -Wall -std=f2018 -fmax-errors=1 -O3' all
!   mv *.a ../lib/
!   mv *.mod ../finclude/
!   make clean
!   cd ..
!
!   cd fortran-sdl2apps
!
!   make FFLAGS='[-march=native] -Wall -std=f2018 -fmax-errors=1 -O3' all
!   mv *.a ../lib/
!   mv *.mod ../finclude/
!   make clean
!   cd ..
!
!   cd brkmtlsk
!
!   rm -rf *.mod; \
!     gfortran[-mp-X] [-g3 -fbacktrace -fcheck=all] [-march=native] \
!       -Wall [-Wno-unused-dummy-argument] -std=f2018 [-fmax-errors=1] -O3 \
!       -I ../finclude [`sdl2-config --cflags`] \
!       display_brkmtlsk.f90 -o display_brkmtlsk$EXE \
!       -L ../lib -lbasic_mods -lfortran-sdl2apps -lfortran-sdl2 $LIBS; \
!   rm -rf *.mod
!
!   ./display_brkmtlsk$EXE
!
!   where, for the build on GNU/Linux [OSX+MacPorts X server], is:
!
!     EXE = .out
!
!   while for the build on MSYS2 is:
!
!     EXE = -$MSYSTEM (or EMPTY)
!
!   and
!
!     LIBS = `sdl2-config --libs`
!
!   Notice that the above definition for LIBS produces a pure Windows
!   app. This means that it will not show up a console/terminal for
!   input data. On these systems, the LIBS definition should be:
!
!     LIBS = [-lSDL2main] -lSDL2 -lgdi32 -lcomdlg32 -luuid -loleaut32 -lole32
!
!   For a static build (run from Explorer), I have found usefull
!
!     LIBS = -static -lmingw32 [-lSDL2main] -lSDL2 -lws2_32 -ldinput8 \
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

program display_brkmtlsk
  use :: sdl2app, only: init_graphics, close_graphics, set_rgba_color, &
       draw_point, get_event, QUIT_EVENT, clear_screen, set_viewport, &
       refresh

  implicit none

  character(len=*), parameter :: FNAME = 'brkmtlsk.data'

  integer :: data_unit, rows, cols, ierr, io_status, i, j, code, r, g, b
  integer :: ievent = -1000, i_max, j_max
  integer, allocatable :: fig(:,:)

  ! Opening DATA file
  open(newunit=data_unit,file=FNAME,access='STREAM', &
       form='UNFORMATTED',status='OLD',iostat=io_status)

  if (io_status /= 0) then
     write(*,*)
     write(*,*)
     write(*,*) 'Error reading file: '//trim(FNAME)
     stop ': Invalid file name.'
  end if

  ! ROWS and COLS are the dimensions of the FIG(:,:) matrix but also
  ! those of the window where the figure is drawn
  read(data_unit) rows, cols

  ! I_MAX is the id of the pixel at the right edge of the window
  ! J_MAX is the id of the pixel at the bottom edge of the window
  i_max = cols-1
  j_max = rows-1

  allocate(fig(0:j_max,0:i_max),stat=ierr)
  if (ierr /= 0) stop ': Allocation failure for FIG(:,:).'

  read(data_unit) fig

  call init_graphics('Display Brooks Matelski', &
       width=cols,height=rows)

  call clear_screen()

  do while (ievent /= QUIT_EVENT)

     do j = 0, j_max
        do i = 0, i_max
           !
           ! Remember that FIG(:,:), on the first row, stores the
           ! bottom of the complex plane while the last row stores the
           ! top...
           !
           code = fig(j_max-j,i)

           if (code == 0) then
              r = 0
              g = 0
              b = 0
           else
              ! As in https://www.pythonpool.com/mandelbrot-set-python
              ! 'Method 2'
              r = mod(code,4)*64
              g = mod(code,8)*32
              b = mod(code,16)*16
           end if

           call set_rgba_color(r,g,b)
           call draw_point(i,j)
        end do
     end do

     call refresh()
     ievent = get_event()
  end do

  call close_graphics()

  if (allocated(fig)) deallocate(fig,stat=ierr)
    if (ierr /= 0) stop ': Deallocation failure for FIG(:).'

  close(data_unit)

end program display_brkmtlsk
