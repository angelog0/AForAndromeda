!
! Author: ANGELO GRAZIOSI
!
!   created   : Jul 08, 2018
!   last edit : Dec 28, 2024
!
!   Domain coloring for complex functions (Conformal Map coloring)
!
! DESCRIPTION
!
!   Technique for visualizing complex functions, which assigns a color
!   to each point of the complex plane.
!
! REFERENCES
!
!   [1] https://it.wikipedia.org/wiki/Mappa_conforme
!   [2] https://it.wikipedia.org/wiki/Colorazione_del_dominio
!   [3] https://en.wikipedia.org/wiki/Domain_coloring
!   [4] https://commons.wikimedia.org/wiki/File:Color_complex_plot.jpg
!   [5] https://gist.github.com/mjackson/5311256
!
!   The core algorithm is adapted from the original (Ref. [4]).
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
!   wget -q http://warp.povusers.org/FunctionParser/fparser4.5.2.zip
!   aunpack -q fparser4.5.2.zip -X fparser-4.5.2/ > /dev/null
!   rm -rf fparser4.5.2.zip
!
!   cd fortran-fparser
!
!   make FFLAGS='[-march=native] -Wall -std=f2018 -fmax-errors=1 -O3' all
!   mv *.a ../lib/
!   mv *.mod ../finclude/
!   make clean
!   cd ..
!
!   cd complex-dynamics
!
!   rm -rf *.mod; \
!     gfortran[-mp-X] [-g3 -fbacktrace -fcheck=all] [-march=native] \
!       -Wall [-Wno-unused-dummy-argument] -std=f2018 [-fmax-errors=1] -O3 \
!       -I ../finclude [`sdl2-config --cflags`] \
!       complexplot.f90 -o complexplot$EXE \
!       -L ../lib -lfortran-fparser -lbasic_mods -lfortran-sdl2apps -lfortran-sdl2 \
!       -lfpc++ -lstdc++ $LIBS; \
!   rm -rf *.mod
!
!   ./complexplot$EXE
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

module complexplot_lib
  use :: kind_consts, only: WP
  use :: math_consts, only: ZERO => Z0, ONE => Z1, TWO => Z2, SIX => Z6, &
       PI, TWO_PI, E_NEPER, JJ
  use :: additional_functions, only: sqr => sqr_z, bar => conjg_z
  use :: fparser_cd, only: FunctionParser_cd_type, NewParser, Parse, &
       ErrorMsg, GetParseErrorType, DeleteParser, Eval, AddConstant, &
       AddFunction

  implicit none
  private

  ! Output window/region layout
  integer :: screen_width = 0, screen_height = 0, &
       d1 = 25, d2 = 900, d3 = 900
  real(WP) ::  x_min = -3, x_max = 3, y_min = -3, y_max = 3

  type(FunctionParser_cd_type) :: fp

  public :: app_on, app_run, app_off

contains

  subroutine input_data()
    use :: getdata, only: get, MAXLEN

    character(len=MAXLEN) :: fz_buffer = '((z*z-1)*sqr(z-2-i))/(z*z+2+2*i)'
    integer :: ierr

    write(*,*)
    write(*,*) "*** TYPE 's/S' IN THE PLOT TO SAVE IT AS PPM ***"
    write(*,*)

    call get('F(z) = ',fz_buffer)
    write(*,*)

    ! Create the fparser for F(z)
    call NewParser(fp)

    if (AddConstant(fp,'i',JJ) <= 0) then
       stop ': AddConstant(i) error...'
    end if

    if (AddConstant(fp,'pi',(PI,ZERO)) <= 0) then
       stop ': AddConstant(pi) error...'
    end if

    if (AddFunction(fp,'sqr',sqr,1) <= 0) then
       stop ': AddFunction(sqr) error (complex Fortran function)...'
    end if

    if (AddFunction(fp,'bar',bar,1) <= 0) then
       stop ': AddFunction(bar) error (complex Fortran function)...'
    end if

    ierr = Parse(fp,fz_buffer,'z')

    if (ierr >= 0) then
       write(*,*) 'Failure creating FP parser ...'
       write(*,*)
       ! Notice, 7 is the number of characters in 'F(z) = '...
       write(*,'(A)') 'F(z) = '//trim(fz_buffer)
       write(*,'(A)') repeat(' ',ierr+7)//'^'

       ! Remember : ErrorMsg() is an array of characters...
       write(*,*) ErrorMsg(fp)
       write(*,*) 'Error type: ',GetParseErrorType(fp)
       stop ': FParser creation failure (INPUT_DATA).'
    end if

    ! Output window setup
    call get('D1 (pixel) = ',d1)
    call get('D2 (pixel) = ',d2)
    call get('D3 (pixel) = ',d3)
    write(*,*)

    ! 'screen' initialization...
    screen_width = 2*d1+d2
    screen_height = 2*d1+d3

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

    call input_data()
    call init_graphics('Color Complex Plot', &
         WIDTH=screen_width,HEIGHT=screen_height)
  end subroutine app_on

  subroutine app_off()
    use :: sdl2app, only: close_graphics

    call close_graphics()
    call DeleteParser(fp)
  end subroutine app_off

  subroutine get_hsv(z,hsv_color)
    complex(WP), intent(in) :: z
    real(WP), intent(out) :: hsv_color(3)

    real(WP), save :: m, range_s, range_e, k

    !
    ! Convert a Z complex number in HSV, or HSB, (hue, saturation, and
    ! value or brightness) color
    !

    associate (a => hsv_color(1), sat => hsv_color(2), val => hsv_color(3))

      ! Result in [-PI,PI] ...
      a = atan2(aimag(z),real(z))

      ! .. convert it in [0,2*PI]
      do while (a < ZERO)
         a = a+TWO_PI
      end do

      ! .. and then in [0,1]
      a = a / TWO_PI

      ! The Z radius
      m = abs(z)

      range_s = ZERO
      range_e = ONE

      do while (m > range_e)
         range_s = range_e
         range_e = range_e*E_NEPER
      end do

      k = (m-range_s)/(range_e-range_s)

      ! Saturation (= g(abs(z))) initialization
      if (k < 0.5_WP) then
         sat = TWO*k
      else
         sat = ONE-(k-0.5_WP)*TWO
      end if

      ! The k-dependent initialization of SAT and VAL are the same
      val = sat

      ! Completing the value of SAT
      sat = ONE-(ONE-sat)**3
      sat = 0.4_WP+sat*0.6_WP

      ! Value (= h(abs(z))) initialization has been done. Now completing ...
      val = ONE-val
      val = ONE-(ONE-val)**3
      val = 0.6_WP+val*0.4_WP

      ! HUE correction before we return
      if ((sat /= ZERO) .and. (a == ONE)) a = ZERO

    end associate

    ! Now we can return

  end subroutine get_hsv

  subroutine hsv2rgb(hsv_color,rgb_color)
    real(WP), intent(in) :: hsv_color(3)
    integer, intent(out) :: rgb_color(3)

    integer, save :: i
    real(WP), save :: r, g, b, z, f, p, q, t

    ! Hue, saturation, value
    associate (h => hsv_color(1), s => hsv_color(2), v => hsv_color(3))

      if (s == ZERO) then
         r = v
         g = v
         b = v
      else
         !if (h == ONE) h = ZERO ! The HUE has already been corrected
         f = h*SIX     ! Now F is H*6
         z = floor(f)

         i = int(z)
         f = (f-z)     ! Now F is (H*6-Z) = (H*6-FLOOR(H*6))
         p = v*(ONE-s)
         q = v*(ONE-s*f)
         t = v*(ONE-s*(ONE-f))

         select case (i)
         case (0)
            r = v
            g = t
            b = p
         case (1)
            r = q
            g = v
            b = p
         case (2)
            r = p
            g = v
            b = t
         case (3)
            r = p
            g = q
            b = v
         case (4)
            r = t
            g = p
            b = v
         case (5)
            r = v
            g = p
            b = q
         end select
      end if

    end associate

    ! RGB values conversion from REAL to INTEGER
    associate (ir => rgb_color(1), ig => rgb_color(2), ib => rgb_color(3))

      ir = int(256.0_WP*r)
      if (ir > 255) ir = 255

      ig = int(256.0_WP*g)
      if (ig > 255) ig = 255

      ib = int(256.0_WP*b)
      if (ib > 255) ib = 255

    end associate

  end subroutine hsv2rgb

  subroutine get_RGB_color(z,rgb_color)
    complex(WP), intent(in) :: z
    integer, intent(out) :: rgb_color(3)

    real(WP), save :: hsv_color(3)

    !
    ! Convert a Z complex number into RGB (BGI) color
    !

    call get_hsv(z,hsv_color)
    call hsv2rgb(hsv_color,rgb_color)
  end subroutine get_RGB_color

  subroutine app_run()
    use :: sdl2, only: sdl_rect
    use :: sdl2app, only: QUIT_EVENT, clear_screen, draw_rect, get_event, &
         refresh, set_rgba_color, set_viewport

    ! The bounding rectangle and viewport
    type(sdl_rect) :: rect

    integer :: i_max, j_max, ievent = -1000
    real(WP) :: dx, dy
    logical :: save_fig = .false.

    i_max = d2-1
    j_max = d3-1

    dx = x_max-x_min
    dy = y_max-y_min

    call clear_screen()

    ! Bounding rectangle
    rect = sdl_rect(d1-2,d1-2,d2+4,d3+4)
    call set_rgba_color(255,255,0)
    call draw_rect(rect)

    ! Viewport
    rect = sdl_rect(d1,d1,d2,d3)
    call set_viewport(rect)

    do while (ievent /= QUIT_EVENT)
       call display_fun()
       call refresh()

       ievent = get_event()

       ! Process event
       if (ievent == ichar('s')) save_fig = .true.
    end do

  contains

    subroutine display_fun()
      use :: ft_timer_m, only: ft_timer_t
      use :: sdl2app, only: draw_point, set_rgba_color

      integer :: i, j, color(3), data_unit
      real(WP) :: x, y
      complex(WP) :: z, w
      character(len=256) :: buf

      type(ft_timer_t) :: run_timer

      if (save_fig) then
         ! Opening DATA file
         open(newunit=data_unit,file='complexplot.ppm',access='STREAM', &
              form='UNFORMATTED',status='REPLACE')

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
      end if

      write(*,'(A)',advance='NO') 'Please wait, we are working ... '
      call run_timer%start()

      ! Start 'run_app()'...

      associate (r => color(1), g => color(2), b => color(3))
        do j = 0, j_max
           y = y_max-(j*dy)/j_max

           do i = 0, i_max
              x = x_min+(i*dx)/i_max
              !x = x_max-(i*dx)/i_max ! As in the original code
              z = x+JJ*y ! Or: z = cmplx(x,y,WP)
              w = Eval(fp,[z])

              call get_RGB_color(w,color)

              call set_rgba_color(r,g,b)
              call draw_point(i,j)

              if (save_fig) then
                 ! R, G, B are 1 byte (3 byte = 24 bit) If I_MAX+1 =
                 ! J_MAX+1 = 800, this writes 800*800*3 byte at the end of
                 ! loops
                 write(data_unit) int(r,1), int(g,1), int(b,1)
              end if
           end do
        end do
      end associate

      ! Stop 'run_app()'...

      call run_timer%stop()
      write(*,*) 'done!'

      write(*,*)
      write(*,'(A,F0.3,A)') 'Completed in ',run_timer%elapsed_time(), &
           ' seconds!'

      if (save_fig) then
         ! If I_MAX+1 = J_MAX+1 = 800, the total size is
         ! 800*800*3 + 15 = 1920015 byte
         close(data_unit)
         save_fig = .false.
      end if
    end subroutine display_fun

  end subroutine app_run
end module complexplot_lib

program complexplot
  use :: complexplot_lib

  call app_on()
  call app_run()
  call app_off()
end program complexplot
