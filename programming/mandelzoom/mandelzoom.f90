!
! Author: ANGELO GRAZIOSI
!
!   created   : Aug 12, 2018
!   last edit : Dec 28, 2024
!
!   Zooming in the Mandelbrot Set
!
! DESCRIPTION
!
!   Drawing the fractal working only in screen coordinates and using SDL2.
!
!                BASED ON THE GREAT WORK OF INTERKOSMOS
!             (https://github.com/interkosmos/fortran-sdl2)
!
!
! HISTORICAL NOTES
!
!   I wrote the first program to draw Mandelbrot set in Pascal (1990,
!   with Turbo Pascal and BGI graphics). It produced a 16-color figure
!   (VGA, 640x480) and took more than an hour: I would start the
!   program, go to put the car back more than 2 km and walk back
!   (uphill!) and the program still hadn't finished!!! All this on a
!   286 with no math coprocessor. After I bought the coprocessor, the
!   program finished in 13 minutes....
!
!
! REFERENCES
!
!   https://it.wikipedia.org/wiki/Insieme_di_Mandelbrot
!   https://people.sc.fsu.edu/~jburkardt/f_src/mandelbrot/mandelbrot.f90
!   https://rosettacode.org/wiki/Mandelbrot_set#Fortran
!
!   Alexander Dewdney, A computer microscope zooms in for a close look
!   at the most complicated object in mathematics, Scientific
!   American, Volume 257, Number 8, August 1985, pages 16-24.
!
!   Heinz-Otto Peitgen, Hartmut Juergens, Dietmar Saupe, Chaos and
!   Fractals - New Frontiers in Science, Springer, 1992
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
!   cd mandelzoom
!
!   rm -rf *.mod; \
!     gfortran[-mp-X] [-g3 -fbacktrace -fcheck=all] [-march=native] \
!       -Wall [-Wno-unused-dummy-argument] -std=f2018 [-fmax-errors=1] -O3 \
!       -I ../finclude [`sdl2-config --cflags`] \
!       mandelzoom.f90 -o mandelzoom$EXE \
!       -L ../lib -lbasic_mods -lfortran-sdl2apps -lfortran-sdl2 $LIBS; \
!   rm -rf *.mod
!
!   ./mandelzoom$EXE
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
!   For a static build (run in Explorer), I have found usefull
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
!
! HOW TO USE
!
!   Start the app from command line:
!
!     [./]mandelzoom
!
!   You should not need to change the defauls, so just press ENTER.
!
!   Then (Emacs jargon )
!
!     - mouse-1 to zoom in where you click
!     - mouse-3 to zoom out where you click
!     - mouse-2 to reset
!     - '+' (keypad) or WHEELDOWN to zoom in where you are
!     - '-' (keypad) or WHEELUP to zoom out where you are
!     - '1', '2' or '3' to change the color palette (purple, blue, amber)
!     - 'i' to increase the iterations (+50)
!     - 'd' to decrease the iterations (-50)
!     - 's' to save the fractal (PPM format, to be converted in other
!     - formats with GraphicsMagick or ImageMagick)
!
!     - ESC or Q/q to quit
!
!

module color_palette

  implicit none
  private

  abstract interface
     subroutine palette()
     end subroutine palette
  end interface

  type :: palette_proc
     procedure(palette), pointer, nopass :: build => null()
  end type palette_proc

  type :: color_rgb_t
     integer :: r, g, b
  end type color_rgb_t

  integer :: ncolors
  type(color_rgb_t), allocatable :: pal_colors(:)

  integer :: current_pal = 1
  type(palette_proc) :: pal(3)

  public :: color_rgb_t
  public :: init_pal, reset_pal, set_pal, delete_pal, get_color

contains

  subroutine create_pal(n)
    integer, intent(in) :: n

    integer :: ierr

    if (n <= 0) &
         stop ': NCOLORS NOT VALID PARAMETER IN OBJ%CREATE()!'

    allocate(pal_colors(0:n),stat=ierr)
    if (ierr /= 0) &
         stop ': ALLOCATION FAILURE FOR PAL_COLORS(:) IN CREATE_PAL()!'

    ncolors = n

    call pal(current_pal)%build()
  end subroutine create_pal

  subroutine delete_pal()
    integer :: ierr = 0 ! To silence the compiler ('may be used uninitialized')

    if (allocated(pal_colors)) deallocate(pal_colors,stat=ierr)
    if (ierr /= 0) &
         stop ': DEALLOCATION FAILURE FOR PAL_COLORS(:) IN DESTROY_PAL()!'

    ncolors = 0

  end subroutine delete_pal

  subroutine purple_pal()
    integer :: c

    c = ncolors

    ! The Mandelbrot set is black
    pal_colors(c) = color_rgb_t(0,0,0)

    do
       c = c-1
       if (c < 0) exit

       pal_colors(c) = color_rgb_t(50+2*c,c,ncolors-c)
    end do
  end subroutine purple_pal

  subroutine blue_pal()
    integer :: c

    c = ncolors

    ! The Mandelbrot set is black
    pal_colors(c) = color_rgb_t(0,0,0)

    do
       c = c-1
       if (c < 0) exit

       pal_colors(c) = color_rgb_t(0,c,50+2*c)
    end do
  end subroutine blue_pal

  subroutine amber_pal()
    integer :: c

    c = ncolors

    ! The Mandelbrot set is purple
    pal_colors(c) = color_rgb_t(int(Z'30'),0,int(Z'30'))

    do
       c = c-1
       if (c < 0) exit

       pal_colors(c) = color_rgb_t(ncolors-c,50+2*c,c)

    end do
  end subroutine amber_pal

  subroutine init_pal(n,id_pal)
    integer, intent(in) :: n
    integer, intent(in), optional :: id_pal

    ! Color palettes initialization
    pal(1)%build => purple_pal
    pal(2)%build => blue_pal
    pal(3)%build => amber_pal

    if (present(id_pal)) current_pal = id_pal

    call create_pal(n)
  end subroutine init_pal

  subroutine reset_pal(n)
    integer, intent(in) :: n

    if (n /= ncolors) then
       call delete_pal()

       ! ncolors is set here
       call create_pal(n)

       call pal(current_pal)%build()
    end if
  end subroutine reset_pal

  subroutine set_pal(id_pal)
    integer, intent(in) :: id_pal

    if (id_pal /= current_pal) then
       current_pal = id_pal
       call pal(current_pal)%build()
    end if
  end subroutine set_pal

  function get_color(c) result(f)
    integer, intent(in) :: c
    type(color_rgb_t) :: f

    f = pal_colors(c)
  end function get_color
end module color_palette

module mandelzoom_lib
  use :: kind_consts, only: WP

  implicit none
  private

  ! Number of iterations to generate the fractal
  integer :: max_iter = 100

  ! Output window/region layout (where the fractal lives)
  integer :: screen_width = 0, screen_height = 0, &
       d1 = 25, d2 = 1600, d3 = 900, i_max, j_max

  real(WP) :: x_min = -2.95_WP, x_max = 1.45_WP, &
       y_min = -1.2375_WP, y_max = 1.2375_WP

  ! The storage for the fractal figure
  integer, dimension(:,:), allocatable :: g0

  public :: app_on, app_run, app_off

contains

  subroutine input_data()
    use :: getdata, only: get
    use :: color_palette, only: init_pal

    integer :: ierr

    call get('MAX_ITER = ',max_iter)
    write(*,*)

    call init_pal(max_iter)

    ! Output window setup
    call get('D1 (pixel) = ',d1)
    call get('D2 (pixel) = ',d2)
    call get('D3 (pixel) = ',d3)
    write(*,*)

    ! 'screen' initialization: (0:screen_width-1) X (0:screen_height-1)
    screen_width = 2*d1+d2
    screen_height = 2*d1+d3

    ! The output windows is (d1:d1+d2-1) X (d1:d1+d3-1) in absolute
    ! coordinates. The size is
    !
    !   d1+d2-1-d1+1 = d2, i.e. in (0:i_max = d2-1) in relative coordinates
    !   d1+d3-1-d1+1 = d3, i.e. in (0:j_max = d3-1) in relative coordinates
    !
    i_max = d2-1
    j_max = d3-1

    allocate(g0(0:i_max,0:j_max),stat=ierr)
    if (ierr /= 0) stop ': ALLOCATION FAILURE FOR G0(:,:) IN INPUT_DATA()!'

    ! Output region layout
    call get('X_MIN =',x_min)
    call get('X_MAX =',x_max)
    write(*,*)

    call get('Y_MIN =',y_min)
    call get('Y_MAX =',y_max)
    write(*,*)

  end subroutine input_data

  subroutine app_on()
    use :: sdl2app, only: init_graphics

    !
    ! WE 'SWITCH ON' THINGS IN THIS ORDER: palette, G0, GRAPHICS
    !

    ! Palette, G0
    call input_data()

    ! GRAPHICS
    call init_graphics('Mandelbrot Set Zoom', &
         width=screen_width,height=screen_height)
  end subroutine app_on

  subroutine app_off()
    use :: sdl2app, only: close_graphics
    use :: color_palette, only: delete_pal

    integer :: ierr = 0 ! To silence the compiler ('may be used uninitialized')

    !
    ! WE 'SWITCH OFF' THINGS IN INVERSE ORDER: GRAPHICS, G0, PAL_COLORS
    !

    ! GRAPHICS
    call close_graphics()

    ! G0
    if (allocated(g0)) deallocate(g0,stat=ierr)
    if (ierr /= 0) stop ': DEALLOCATION FAILURE FOR G0(:,:) IN APP_OFF()!'

    ! Switch OFF the palette
    call delete_pal()
  end subroutine app_off

  subroutine app_run()
    use :: sdl2, only: sdl_rect
    use :: sdl2app, only: QUIT_EVENT, &
         clear_screen, get_event, draw_rect, refresh, set_rgba_color, &
         set_viewport

    integer :: ievent = -1000
    real(WP) :: xm, ym, xstep, ystep, x1, y1, x2, y2, dx, dy, &
         xm0, ym0, xstep0, ystep0
    logical :: initial_window, recalc
    type(sdl_rect)  :: rect

    ! Getting the working view region...
    x1 = x_min
    x2 = x_max
    y1 = y_min
    y2 = y_max

    ! Getting DEFAULT for initial window (on complex plane)
    xm0 = 0.5_WP*(x1+x2)
    ym0 = 0.5_WP*(y1+y2)
    xstep0 = (x2-x1)/2
    ystep0 = (y2-y1)/2

    ! Initial window (on complex plane)
    xm = xm0
    ym = ym0
    xstep = xstep0
    ystep = ystep0

    ! Default flags
    initial_window = .true.
    recalc = .true.

    call clear_screen()

    ! Setting up parameters for full frame
    !
    ! Width: right-left+1 = (d1+i_max+2)-(d1-2)+1 = i_max+5 = d2+4
    ! Height: bottom-topt+1 = (d1+j_max+2)-(d1-2)+1 = j_max+5 = d3+4
    !
    rect = sdl_rect(d1-2,d1-2,i_max+5,j_max+5)

    ! Drawing the full frame...
    call set_rgba_color(255,255,0)   ! Yellow
    call draw_rect(rect)

    rect = sdl_rect(d1,d1,d2,d3)
    call set_viewport(rect)

    do while (ievent /= QUIT_EVENT)
       ! Define the current region in which lives the fractal...
       x1 = xm-xstep
       y1 = ym-ystep
       x2 = xm+xstep
       y2 = ym+ystep

       ! The increments per pixel
       dx = (x2-x1)/i_max
       dy = (y2-y1)/j_max

       if (recalc) then
          call calc_fractal()
          recalc = .false.
       end if

       call display_fractal()
       call refresh()

       ! Wait for a key or mouse click
       ievent = get_event()

       call process_event()
       call refresh()
    end do

  contains

    subroutine calc_fractal()
      use :: ft_timer_m, only: ft_timer_t

      integer :: i, j, k
      real(WP) :: x, y, u, v, uq, vq
      type(ft_timer_t) :: run_timer

      write(*,'(A)',advance='NO') 'Please wait, we are working ... '
      call run_timer%start()

      ! Start 'run_app()'...

      do j = 0, j_max
         y = y2-j*dy
         do i = 0, i_max
            x = x1+i*dx

            u = 0.0_WP
            v = 0.0_WP
            k = 0

            !
            ! Iterate (https://rosettacode.org/wiki/Mandelbrot_set#Fortran)
            !
            !   z(n+1) = z(n)**2 + c,   with: z = u+i*v; c = x+i*y
            !
            ! in order to know if a certain point is in the set or not
            !
            do
               uq = u*u
               vq = v*v

               ! Only when k == max_iter the point belongs to the MSet
               if (uq+vq > 4.0_WP .or. k == max_iter) then
                  g0(i,j) = k
                  exit
               end if

               ! Update position
               v = 2.0_WP*u*v+y
               u = uq-vq+x
               k = k+1
            end do
         end do
      end do

      ! Stop 'run_app()'...

      call run_timer%stop()
      write(*,*) 'done!'

      write(*,*)
      write(*,'(A,F0.3,A)') 'Completed in ',run_timer%elapsed_time(), &
           ' seconds!'
    end subroutine calc_fractal

    subroutine display_fractal()
      use :: sdl2app, only: draw_point
      use :: color_palette, only: color_rgb_t, get_color

      integer :: i, j
      type(color_rgb_t) :: c

      do j = 0, j_max
         do i = 0, i_max
            c = get_color(g0(i,j))

            call set_rgba_color(c%r,c%g,c%b)

            ! We DO NEED the offset, P = D1 and Q = D1, for (i,j): we
            ! are NOT in the viewport where the fractal lives
            call draw_point(i,j)
         end do
      end do
    end subroutine display_fractal

    subroutine save_fractal()
      use :: color_palette, only: color_rgb_t, get_color

      character(len=*), parameter :: FNAME = 'mandelzoom.ppm'

      integer :: i, j, data_unit
      character(len=256) :: buf
      type(color_rgb_t) :: c

      ! Opening DATA file
      open(newunit=data_unit,file=FNAME,access='STREAM',form='UNFORMATTED', &
           status='REPLACE')

      ! Now PPM start
      ! Writing to a character buffer
      buf = ''
      write(buf,'(2(A,i0),A)') 'P6'//char(10), &
           i_max+1, ' ', j_max+1, &
           char(10)//'255'//char(10)

      ! These data MUST be written as characters string, not as binaries..
      ! If I_MAX+1 = J_MAX+1 = 800 (i.e. 3 chs), this string has dimension
      !
      !   2+1+3+1+3+1+3+1 = 15
      !
      write(data_unit) trim(adjustl(buf))

      do j = 0, j_max
         do i = 0, i_max
            c = get_color(g0(i,j))

            ! R, G, B are 1 byte (3 byte = 24 bit) If I_MAX+1 =
            ! J_MAX+1 = 800, this writes 800*800*3 byte at the end of
            ! loops
            write(data_unit) int(c%r,1), int(c%g,1), int(c%b,1)
         end do
      end do

      !
      ! Saving 'The Fractal' informations... The PPM format should ignore the
      ! lines starting with '#'
      !
      ! Start with the max. number of iterations, dimensions and then
      ! the region where the fractal lives..
      buf = ''
      write(buf,*) '# ', max_iter, i_max+1, j_max+1, xm, ym, xstep, ystep
      write(data_unit) trim(adjustl(buf))

      ! If I_MAX+1 = J_MAX+1 = 800, the total size is
      ! 800*800*3 + 15 = 1920015 byte
      close(data_unit)
    end subroutine save_fractal

    subroutine draw_cross()
      use :: sdl2app, only: draw_line

      integer, parameter :: LENGTH = 14, HLENGTH = LENGTH/2
      integer :: x0, y0

      ! It must be centered with respect to the viewport, so CANNOT
      !
      !   (screen_width/2,screen_height/2)   <== NO, NO
      !
      x0 = i_max/2
      y0 = j_max/2

      call set_rgba_color(255,255,255)

      ! Horizontal, vertical
      call draw_line(x0-HLENGTH,y0,x0+HLENGTH,y0)
      call draw_line(x0,y0-HLENGTH,x0,y0+HLENGTH)

    end subroutine draw_cross

    subroutine process_event()
      use :: sdl2, only: SDL_BUTTON_LEFT, SDL_BUTTON_MIDDLE, &
         SDL_BUTTON_RIGHT, &
         SDLK_1, SDLK_2, SDLK_3, &
         SDLK_d, SDLK_i, SDLK_s, &
         SDLK_LEFT, SDLK_RIGHT, &
         SDLK_UP, SDLK_DOWN, &
         SDLK_KP_PLUS, SDLK_KP_MINUS
      use :: sdl2app, only: WHEELUP_EVENT, WHEELDOWN_EVENT, &
         get_mouse_x, get_mouse_y
      use :: color_palette, only: reset_pal, set_pal

      !
      ! NOTICE that 'get_mouse_x()' and 'get_mouse_y()' return the
      ! clicked point in absolute coordinate. We need to map it in
      ! the output window and with right orientation (Y pixels axis
      ! pointing bottom)
      !
      !   i = (get_mouse_x()-i_left), j = (get_mouse_y()-j_top)
      !
      !   x = x1+i*dx, y = y2-j*dy
      !

      ! Process the event
      select case (ievent)
      case (SDL_BUTTON_LEFT)
         ! Set the center point and zoom in
         xm = x1+(get_mouse_x()-d1)*dx
         ym = y2-(get_mouse_y()-d1)*dy
         xstep = xstep/2
         ystep = ystep/2
         initial_window = .false.
         recalc = .true.
         call draw_cross()

      case (SDL_BUTTON_RIGHT)
         ! Set the center point and zoom out
         xm = x1+dx*(get_mouse_x()-d1)
         ym = y2-dy*(get_mouse_y()-d1)
         xstep = xstep*2
         ystep = ystep*2
         initial_window = .false.
         recalc = .true.
         call draw_cross()

      case (SDL_BUTTON_MIDDLE)
         ! Reset the default window (on complex plane)
         if (.not. initial_window) then
            initial_window = .true.
            xm = xm0
            ym = ym0
            xstep = xstep0
            ystep = ystep0
            recalc = .true.
         end if

      case (SDLK_KP_PLUS, WHEELDOWN_EVENT)
         ! Zoom in
         xstep = xstep/2
         ystep = ystep/2
         initial_window = .false.
         recalc = .true.
         call draw_cross()

      case (SDLK_KP_MINUS, WHEELUP_EVENT)
         ! Zoom out
         xstep = xstep*2
         ystep = ystep*2
         initial_window = .false.
         recalc = .true.
         call draw_cross()

      case (SDLK_1)
         call set_pal(1)

      case (SDLK_2)
         call set_pal(2)

      case (SDLK_3)
         call set_pal(3)

      case (SDLK_i)
         max_iter = max_iter+50
         call reset_pal(max_iter)
         write(*,'(A,I0)') 'Iter = ', max_iter
         recalc = .true.

      case (SDLK_d)
         max_iter = max_iter-50

         ! Lower bound for 'max_iter'
         if (max_iter < 50) max_iter = 50

         call reset_pal(max_iter)
         write(*,'(A,I0)') 'Iter = ', max_iter
         recalc = .true.

      case (SDLK_s, ichar('S'))
         call save_fractal()

      case default
         recalc = .false.

      end select  ! End processing the event
    end subroutine process_event

  end subroutine app_run
end module mandelzoom_lib

program mandelzoom
  use :: mandelzoom_lib

  call app_on()
  call app_run()
  call app_off()
end program mandelzoom
