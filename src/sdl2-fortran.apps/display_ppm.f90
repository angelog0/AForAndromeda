!
! Author: ANGELO GRAZIOSI
!
!   created   : Dec 12, 2022
!   last edit : Dec 13, 2022
!
!   Displaying PPM files
!
! DESCRIPTION
!
!   Displaying a PPM file using SDL2-FORTRAN.
!
!
! HOW TO BUILD THE APP
!
!   cd sdl2-fortran.apps
!
!   git clone https://github.com/interkosmos/fortran-sdl2.git
!
!   rm -rf *.mod \
!     gfortran[-mp-X] -std=f2008 -O3 -Wall [`sdl2-config --cflags`] \
!       ../basic-modules/{{kind,math}_consts,nicelabels}.f90 \
!       $SDL2F90 SDL2_app.f90 display_ppm.f90 \
!       $LIBS -o display_ppm$EXE; \
!   rm -rf *.mod
!
!   [./]display_ppm$EXE PPM_NAME
!
!   where, for the build on GNU/Linux [OSX+MacPorts X server], is:
!
!     EXE = .out
!
!   while for the build on MINGW64 is:
!
!     EXE = -$MSYSTEM (or EMPTY)
!
!   and (all platform):
!
!     SDL2F90 = fortran-sdl2/src/{c_util,sdl2/{sdl2_stdinc,sdl2_audio,\
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
!   app on MINGW64. This means that it will not show up a
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

module display_ppm_lib
  implicit none
  private

  character(len=80) :: filename
  integer :: i_max, j_max

  public :: app_on, app_run, app_off

contains

  subroutine get_commandline()
    character(len=80) :: args
    integer :: i

    i = 0
    do
       call get_command_argument(i,args)
       if (len_trim(args) == 0) exit

       if (i == 1) read(args,*) filename

       i = i+1
    end do

    ! The numer of arguments that have been read is stored in 'i'
    !
    ! In this command line,
    !
    !   ./display_ppm.out SDL2_mandelzoom.ppm
    !
    ! we have 2 arguments: the program name and the file name
    ! 'SDL2_mandelzoom.ppm'
    !
    if (i /= 2) stop ': USAGE: ./display_ppm.out PPM_NAME'
  end subroutine get_commandline

  subroutine get_screen()
    integer :: io_status, data_unit
    character(len=80) :: buf

    open(newunit=data_unit,file=filename,status='OLD',iostat=io_status)

    if (io_status /= 0) then
       write(*,*)
       write(*,*)
       write(*,*) 'Error reading file: '//trim(filename)
       stop ': Invalid file name.'
    end if

    buf = ''
    read(data_unit,*) buf
    !print *, buf
    read(data_unit,*) i_max, j_max

    i_max = i_max-1
    j_max = j_max-1

    close(data_unit)
  end subroutine get_screen

  subroutine app_on()
    use SDL2_app, only: init_graphics

    call get_commandline()
    call get_screen()

    ! GRAPHICS
    call init_graphics('Display PPM Files',WIDTH=i_max+1,HEIGHT=j_max+1)
  end subroutine app_on

  subroutine app_off()
    use SDL2_app, only: close_graphics

    ! GRAPHICS
    call close_graphics()
  end subroutine app_off

  subroutine app_run()
    use sdl2, only: sdl_rect
    use SDL2_app, only: QUIT_EVENT, &
         clear_screen, get_event, refresh, set_viewport

    integer :: ievent = -1000
    type(sdl_rect)  :: rect

    call clear_screen()

    rect = sdl_rect(0,0,i_max+1,j_max+1)
    call set_viewport(rect)

    do while (ievent /= QUIT_EVENT)

       call display()
       call refresh()

       ! Wait for a key or mouse click
       ievent = get_event()

       call refresh()
    end do

  contains
    subroutine display()
      !use kind_consts, only: WP
      use SDL2_app, only: set_rgba_color, draw_point

      ! 1 byte
      integer(1) :: r, g, b

      integer :: io_status, data_unit, i, j
      character(len=256) :: buf

      open(newunit=data_unit,file=filename,access='STREAM', &
           form='UNFORMATTED', status='OLD',iostat=io_status)

      if (io_status /= 0) then
         write(*,*)
         write(*,*)
         write(*,*) 'Error reading file: '//trim(filename)
         stop ': Invalid file name.'
      end if

      buf = ''
      read(data_unit) buf

      ! Where is the last newline (\n)
      i = index(buf,'255'//char(10))+3

      ! Positioning to read the next record
      read(data_unit,pos=i) buf(1:1)

      do j = 0, j_max
         do i = 0, i_max
            read(data_unit) r, g, b

            call set_rgba_color(int(r),int(g),int(b))
            call draw_point(i,j)
         end do
      end do

      buf = ''
      read(data_unit,iostat=io_status) buf

      write(*,*) trim(adjustl(buf))

      close(data_unit)
    end subroutine display
  end subroutine app_run
end module display_ppm_lib

program display_ppm
  use display_ppm_lib

  call app_on()
  call app_run()
  call app_off()
end program display_ppm
