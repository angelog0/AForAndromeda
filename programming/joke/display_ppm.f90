!
! Author: ANGELO GRAZIOSI
!
!   created   : Dec 12, 2022
!   last edit : Dec 29, 2024
!
!   Displaying PPM files
!
! DESCRIPTION
!
!   Displaying a PPM file using fortran-sdl2.
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
!   cd fortran-sdl2apps
!
!   make FFLAGS='[-march=native] -Wall -std=f2018 -fmax-errors=1 -O3' all
!   mv *.a ../lib/
!   mv *.mod ../finclude/
!   make clean
!   cd ..
!
!   cd joke
!
!   rm -rf *.mod; \
!     gfortran[-mp-X] [-g3 -fbacktrace -fcheck=all] [-march=native] \
!       -Wall -std=f2018 [-fmax-errors=1] -O3 \
!       -I ../finclude [`sdl2-config --cflags`] \
!       display_ppm.f90 -o display_ppm$EXE \
!       -L ../lib -lfortran-sdl2apps -lfortran-sdl2 \
!       $LIBS; \
!   rm -rf *.mod
!
!   ./display_ppm$EXE SDL2_mandelzoom.ppm
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
!   app. This means that will not show up a console/terminal to input
!   data. On these systems, the LIBS definition should be:
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
    !   ./display_ppm SDL2_mandelzoom.ppm
    !
    ! we have 2 arguments: the program name and the file name
    ! 'SDL2_mandelzoom.ppm'
    !
    if (i /= 2) stop ': USAGE: ./display_ppm PPM_NAME'
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
    use :: sdl2app, only: init_graphics

    call get_commandline()
    call get_screen()

    ! GRAPHICS
    call init_graphics('Display PPM Files',WIDTH=i_max+1,HEIGHT=j_max+1)
  end subroutine app_on

  subroutine app_off()
    use :: sdl2app, only: close_graphics

    ! GRAPHICS
    call close_graphics()
  end subroutine app_off

  subroutine app_run()
    use :: sdl2app, only: QUIT_EVENT, &
         clear_screen, get_event, refresh, set_viewport

    integer :: ievent = -1000

    call clear_screen()

    do while (ievent /= QUIT_EVENT)

       call display()
       call refresh()

       ! Wait for a key or mouse click
       ievent = get_event()

       call refresh()
    end do

  contains
    subroutine display()
      !use :: kind_consts, only: WP
      use :: sdl2app, only: set_rgba_color, draw_point

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
  use :: display_ppm_lib

  call app_on()
  call app_run()
  call app_off()
end program display_ppm
