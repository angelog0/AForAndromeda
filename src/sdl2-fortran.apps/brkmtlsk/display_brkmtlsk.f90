!
! Author: ANGELO GRAZIOSI
!
!   created   : Dec 15, 2022
!   last edit : Dec 16, 2022
!
!   Display Brooks Matelski figure.
!   (Just a SDL2-Fortran app in less than 160 lines of code!)
!
! DESCRIPTION
!
!   Displaying the content of the files saved with BRKMTLSK_CALC.
!
!
! HOW TO BUILD THE APP
!
!   cd sdl2-fortran.apps
!
!   git clone https://github.com/interkosmos/fortran-sdl2.git
!
!   cd brkmtlsk
!
!   rm -rf *.mod; \
!     gfortran[-mp-X] -std=f2008 -O3 -Wall [`sdl2-config --cflags`] \
!       ../../basic-modules/{{kind,math}_consts,nicelabels}.f90 \
!       $SDL2F90 ../SDL2_app.f90 display_brkmtlsk.f90 \
!       $LIBS -o display_brkmtlsk$EXE; \
!   rm -rf *.mod
!
!   ./display_brkmtlsk$EXE
!
!   where, for the build on GNU/Linux [OSX+MacPorts X server], is:
!
!     EXE = .out
!
!   while for the build on MSYS2/MINGW64 is:
!
!     EXE = -$MSYSTEM (or EMPTY)
!
!   and (all platform):
!
!     SDL2F90 = ../fortran-sdl2/src/{c_util,sdl2/{sdl2_stdinc,sdl2_audio,\
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
!   app on MSYS2/MINGW64. This means that it will not show up a
!   console/terminal for input data. On these systems, the LIBS
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

program display_brkmtlsk
  use sdl2, only: sdl_rect
  use SDL2_app, only: init_graphics, close_graphics, set_rgba_color, &
       draw_point, get_event, QUIT_EVENT, clear_screen, set_viewport, &
       refresh

  implicit none

  character(len=*), parameter :: FNAME = 'brkmtlsk.data'

  integer :: data_unit, rows, cols, ierr, io_status, i, j, code, r, g, b
  integer :: ievent = -1000, i_max, j_max
  integer, allocatable :: fig(:,:)
  type(sdl_rect)  :: rect

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

  rect = sdl_rect(0,0,cols,rows)
  call set_viewport(rect)

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
