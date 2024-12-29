!
! Author: Angelo Graziosi
!
!   created   : Aug 09, 2016
!   last edit : Aug 18, 2016
!
!   The class for About dialog
!
! DESCRIPTION
!
!   This is 'about_box_m' module implementing the class (about_box_t)
!   for our About dialog.
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

module about_box_m
  use win32, only: HWND_T, IDOK, INT_PTR_T, WORD_T, dummy, EndDialog
  use basic_box_m, only : basic_box_t

  implicit none
  private

  ! The derived class..
  type, extends(basic_box_t), public :: about_box_t
     ! Private by default. No member data in this case (we use the data in
     ! the base), so no "constructor" (commented out)
     private
   contains
     private
     !generic, public :: initialize => initialize_about_box_t
     !procedure, pass(self) :: initialize_about_box_t
     procedure, pass(self) :: init_dialog
     procedure, pass(self) :: ok_command
  end type about_box_t

contains

  ! This is the "class" initialization routine: The Constructor!
  ! subroutine initialize_about_box_t(self,h_dlg,id_dlg)
  !   class(about_box_t), intent(inout) :: self
  !   integer(HWND_T), intent(in) :: h_dlg
  !   integer(WORD_T), intent(in) :: id_dlg

  !   ! Create the base class
  !   call self%initialize(h_dlg,id_dlg)
  ! end subroutine initialize_about_box_t

  subroutine init_dialog(self,h_dlg)
    class(about_box_t), intent(inout) :: self
    integer(HWND_T), intent(in) :: h_dlg
    !
    ! For how the About dialog is designed, this routine is empty
    ! ...
    !
  end subroutine init_dialog

  subroutine ok_command(self,h_dlg)
    class(about_box_t), intent(inout) :: self
    integer(HWND_T), intent(in) :: h_dlg
    !
    ! For how the About dialog is designed, this routine is 'almost' empty
    ! ...
    dummy = EndDialog(h_dlg,int(IDOK,INT_PTR_T))
  end subroutine ok_command
end module about_box_m
