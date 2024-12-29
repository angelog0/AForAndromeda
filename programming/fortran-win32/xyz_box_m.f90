!
! Author: Angelo Graziosi
!
!   created   : Aug 10, 2016
!   last edit : Aug 18, 2016
!
!   The class for the dialog to enter three data (X,Y,Z)
!
!
! DESCRIPTION
!
!   This is 'xyz_box_m' module implementing the class (xyz_box_t)
!   for our XYZ dialog.
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

module xyz_box_m
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
  type, extends(basic_box_t), public :: xyz_box_t
     private

     ! We asume a real default X,Y,Z data
     logical :: iflag_x = .false., iflag_y = .false., iflag_z = .false.

     integer(INT_T) :: idc_x, idc_y, idc_z
     real(WP) :: x, y, z
   contains
     private
     generic, public :: initialize => initialize_xyz_box_t
     procedure, pass(self) :: initialize_xyz_box_t
     procedure, pass(self) :: init_dialog
     procedure, pass(self) :: ok_command
     procedure, pass(self), public :: get_x => get_x_xyz_box_t
     procedure, pass(self), public :: get_y => get_y_xyz_box_t
     procedure, pass(self), public :: get_z => get_z_xyz_box_t
  end type xyz_box_t

contains

  ! This is the "class" initialization routine: The Constructor!
  subroutine initialize_xyz_box_t(self,h_dlg,id_dlg,idc_x,idc_y,idc_z,x,y,z, &
       iflag_x,iflag_y,iflag_z)
    class(xyz_box_t), intent(inout) :: self
    integer(HWND_T), intent(in) :: h_dlg
    integer(WORD_T), intent(in) :: id_dlg
    integer(INT_T), intent(in) :: idc_x, idc_y, idc_z
    real(WP), intent(in) :: x, y, z
    logical, intent(in), optional :: iflag_x, iflag_y, iflag_z

    ! Create the base class
    call self%initialize(h_dlg,id_dlg)

    if (present(iflag_x)) self%iflag_x = iflag_x
    if (present(iflag_y)) self%iflag_y = iflag_y
    if (present(iflag_z)) self%iflag_z = iflag_z

    self%idc_x = idc_x
    self%idc_y = idc_y
    self%idc_z = idc_z
    self%x = x
    self%y = y
    self%y = z
  end subroutine initialize_xyz_box_t

  subroutine init_dialog(self,h_dlg)
    class(xyz_box_t), intent(inout) :: self
    integer(HWND_T), intent(in) :: h_dlg

    character(len=MAX_LEN) :: buffer

    buffer = ''
    if (self%iflag_x) then
       write(buffer,I_FMT) int(self%x,INT_T)
    else
       write(buffer,R_FMT) self%x
    end if
    dummy = SetDlgItemText(h_dlg,self%idc_x,trim(adjustl(buffer))//NUL)

    buffer = ''
    if (self%iflag_y) then
       write(buffer,I_FMT) int(self%y,INT_T)
    else
       write(buffer,R_FMT) self%y
    end if
    dummy = SetDlgItemText(h_dlg,self%idc_y,trim(adjustl(buffer))//NUL)

    buffer = ''
    if (self%iflag_z) then
       write(buffer,I_FMT) int(self%z,INT_T)
    else
       write(buffer,R_FMT) self%z
    end if
    dummy = SetDlgItemText(h_dlg,self%idc_z,trim(adjustl(buffer))//NUL)
  end subroutine init_dialog

  subroutine ok_command(self,h_dlg)
    class(xyz_box_t), intent(inout) :: self
    integer(HWND_T), intent(in) :: h_dlg

    character(len=MAX_LEN) :: buffer
    integer :: ierr
    real(WP) :: x_try = ZERO, y_try = ZERO, z_try = ZERO

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

    buffer = ''
    dummy = GetDlgItemText(h_dlg,self%idc_y,buffer,MAX_LEN)
    if (dummy > 0) then
       dummy = index(buffer,NUL)
       read(buffer(1:dummy-1),*,iostat = ierr) y_try
       if (ierr /= 0) then
          write(*,*) 'IERR, Y = ', ierr, y_try
       else
          self%y = y_try
       end if
    else
       dummy = MessageBox(h_dlg,'Failure reading Y data!'//NUL, &
            'Fatal Error!!!'//NUL, &
            ior(MB_OK,MB_ICONINFORMATION))
       call PostQuitMessage(1) ! Exit code 1 to flag an error occured
    end if

    buffer = ''
    dummy = GetDlgItemText(h_dlg,self%idc_z,buffer,MAX_LEN)
    if (dummy > 0) then
       dummy = index(buffer,NUL)
       read(buffer(1:dummy-1),*,iostat = ierr) z_try
       if (ierr /= 0) then
          write(*,*) 'IERR, Z = ', ierr, z_try
       else
          self%z = z_try
       end if
    else
       dummy = MessageBox(h_dlg,'Failure reading Z data!'//NUL, &
            'Fatal Error!!!'//NUL, &
            ior(MB_OK,MB_ICONINFORMATION))
       call PostQuitMessage(1) ! Exit code 1 to flag an error occured
    end if

    dummy = EndDialog(h_dlg,int(IDOK,INT_PTR_T))
  end subroutine ok_command

  function get_x_xyz_box_t(self) result(r)
    class(xyz_box_t), intent(in) :: self
    real(WP) :: r

    r = self%x
  end function get_x_xyz_box_t

  function get_y_xyz_box_t(self) result(r)
    class(xyz_box_t), intent(in) :: self
    real(WP) :: r

    r = self%y
  end function get_y_xyz_box_t

  function get_z_xyz_box_t(self) result(r)
    class(xyz_box_t), intent(in) :: self
    real(WP) :: r

    r = self%z
  end function get_z_xyz_box_t
end module xyz_box_m
