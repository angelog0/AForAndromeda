!
! Author: Angelo Graziosi
!
!   created   : Aug 09, 2016
!   last edit : Aug 18, 2016
!
!   The class for the dialog to enter a single data (X)
!
!
! DESCRIPTION
!
!   This is 'x_box_m' module implementing the class (x_box_t)
!   for our X dialog.
!
!   From: my C++ Windows Applications and Borland C++ 2.0 examples and the
!   style used from https://functionallyparanoid.com examples
!   (see https://github.com/bceverly/fortranString) and from
!   http://degenerateconic.com.
!
! NOTE
!
!   int(0,UINT_T)    -->  0_UINT_T
!   int(0,WPARAM_T)  -->  0_WPARAM_T
!   int(0,LPARAM_T)  -->  0_LPARAM_T
!   ...
!

module x_box_m
  use kind_consts, only: WP
  use math_consts, only: ZERO => Z0
  use win32, only: HWND_T, IDOK, INT_T, INT_PTR_T, MAX_LEN, &
       MB_ICONINFORMATION, MB_OK, NUL, WORD_T, &
       I_FMT, R_FMT, &
       dummy, EndDialog, GetDlgItemText, MessageBox, PostQuitMessage, &
       SetDlgItemText
  use basic_box_m, only : basic_box_t

  implicit none
  private

  ! The derived class..
  type, extends(basic_box_t), public :: x_box_t
     private

     ! We asume a real default X data
     logical :: iflag_x = .false.

     integer(INT_T) :: idc_x
     real(WP) :: x
   contains
     private
     generic, public :: initialize => initialize_x_box_t
     procedure, pass(self) :: initialize_x_box_t
     procedure, pass(self) :: init_dialog
     procedure, pass(self) :: ok_command
     procedure, pass(self), public :: get => get_x_box_t
  end type x_box_t

contains

  ! This is the "class" initialization routine: The Constructor!
  subroutine initialize_x_box_t(self,h_dlg,id_dlg,idc_x,x,iflag_x)
    class(x_box_t), intent(inout) :: self
    integer(HWND_T), intent(in) :: h_dlg
    integer(WORD_T), intent(in) :: id_dlg
    integer(INT_T), intent(in) :: idc_x
    real(WP), intent(in) :: x
    logical, intent(in), optional :: iflag_x

    ! Create the base class
    call self%initialize(h_dlg,id_dlg)

    if (present(iflag_x)) self%iflag_x = iflag_x

    self%idc_x = idc_x
    self%x = x
  end subroutine initialize_x_box_t

  subroutine init_dialog(self,h_dlg)
    class(x_box_t), intent(inout) :: self
    integer(HWND_T), intent(in) :: h_dlg

    character(len=MAX_LEN) :: buffer

    buffer = ''
    if (self%iflag_x) then
       write(buffer,I_FMT) int(self%x,INT_T)
    else
       write(buffer,R_FMT) self%x
    end if
    dummy = SetDlgItemText(h_dlg,self%idc_x,trim(adjustl(buffer))//NUL)
  end subroutine init_dialog

  subroutine ok_command(self,h_dlg)
    class(x_box_t), intent(inout) :: self
    integer(HWND_T), intent(in) :: h_dlg

    character(len=MAX_LEN) :: buffer
    integer :: ierr
    real(WP) :: x_try = ZERO

    buffer = ''
    dummy = GetDlgItemText(h_dlg,self%idc_x,buffer,MAX_LEN)
    if (dummy > 0) then
       dummy = index(buffer,NUL)
       read(buffer(1:dummy-1),*,iostat = ierr) x_try
       if (ierr /= 0) then
          write(*,*) 'IERR, X = ', ierr, x_try
       else
          self%x = x_try
       end if
    else
       dummy = MessageBox(h_dlg,'Failure reading X data!'//NUL, &
            'Fatal Error!!!'//NUL, &
            ior(MB_OK,MB_ICONINFORMATION))
       call PostQuitMessage(1) ! Exit code 1 to flag an error occured
    end if

    dummy = EndDialog(h_dlg,int(IDOK,INT_PTR_T))
  end subroutine ok_command

  function get_x_box_t(self) result(r)
    class(x_box_t), intent(in) :: self
    real(WP) :: r

    r = self%x
  end function get_x_box_t
end module x_box_m
