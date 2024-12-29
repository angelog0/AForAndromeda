!
! Author: Angelo Graziosi
!
!   created   : Aug 09, 2016
!   last edit : Aug 18, 2016
!
!   The base (abstract) class for box dialogs
!
! DESCRIPTION
!
!   This is 'basic_box_m' module implementing the BASE class (basic_box_t)
!   for our dialogs.
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

module basic_box_m
  use, intrinsic :: iso_c_binding, only: c_funloc
  use win32, only: BOOL_T, DWORD_T, FALSE_T, HWND_T, IDCANCEL, IDOK, &
       INT_PTR_T, INT_T, LPARAM_T, NULL_LPSTR, TRUE_T, UINT_T, WM_COMMAND, &
       WM_INITDIALOG, WORD_T, WPARAM_T, &
       dialog_box, dummy, EndDialog, GetModuleHandle, lo_word, &
       make_int_resource

  implicit none
  private

  ! The abstract class. We cannot have objects of this type..
  type, abstract, public :: basic_box_t
     private
     integer(HWND_T):: h_dlg
     integer(WORD_T) :: id_dlg

     ! TRUE_T if OK button is pressed, otherwise it is FALSE_T
     integer(BOOL_T) :: dialog_result
   contains
     private
     generic, public :: initialize => initialize_basic_box_t
     procedure, pass(self) :: initialize_basic_box_t
     procedure(dialog_func), pass(self), deferred :: init_dialog
     procedure(dialog_func), pass(self), deferred :: ok_command
     procedure, pass(self) :: process_command
     procedure, pass(self), non_overridable, public :: run
  end type basic_box_t

  abstract interface
     subroutine dialog_func(self,h_dlg)
       import :: basic_box_t, HWND_T
       class(basic_box_t), intent(inout) :: self
       integer(HWND_T), intent(in) :: h_dlg
     end subroutine dialog_func
  end interface

  ! We cannot use 'type' if we want that the pointer points also to all its
  ! descendants
  class(basic_box_t), pointer :: p_self => null()

contains

  ! This is the "class" initialization routine: the "constructor"
  ! It is a type-bound procedure..
  subroutine initialize_basic_box_t(self,h_dlg,id_dlg)
    class(basic_box_t), intent(inout) :: self
    integer(HWND_T), intent(in) :: h_dlg
    integer(WORD_T), intent(in) :: id_dlg

    self%h_dlg = h_dlg
    self%id_dlg = id_dlg
    self%dialog_result = FALSE_T
  end subroutine initialize_basic_box_t

  ! Notice that 'process_command' is called by 'dialog_proc' which is a
  ! C-Binding and needs h_dlg argument. So we cannot use the h_dlg data
  ! member of basic_box_t class and we have to do the trick with p_self
  ! pointer.
  ! The same arguments are valid for 'ok_command' and 'init_dialog' routines
  !
  function process_command(self,h_dlg,wParam,dialog_result)
    class(basic_box_t), intent(inout) :: self
    integer(BOOL_T) :: process_command
    integer(HWND_T), intent(in) :: h_dlg
    integer(WPARAM_T), intent(in) :: wParam
    integer(BOOL_T), intent(inout) :: dialog_result

    select case (lo_word(int(wParam,DWORD_T)))
    case (IDOK)
       call self%ok_command(h_dlg)
       dialog_result = TRUE_T
       process_command = TRUE_T
       return

    case (IDCANCEL)
       dummy = EndDialog(h_dlg,int(IDCANCEL,INT_PTR_T))
       process_command = TRUE_T
       return

    case default
       process_command = FALSE_T
       return
    end select
  end function process_command

  ! This is a module (C-Binding) routine NOT a type-bound procedure
  function dialog_proc(h_dlg,iMsg,wParam,lParam) bind(C)
    !GCC$ ATTRIBUTES STDCALL :: dialog_proc
    integer(BOOL_T) :: dialog_proc
    integer(HWND_T), value :: h_dlg
    integer(UINT_T), value :: iMsg
    integer(WPARAM_T), value :: wParam
    integer(LPARAM_T), value :: lParam

    select case (iMsg)
    case (WM_INITDIALOG)
       call p_self%init_dialog(h_dlg)
       p_self%dialog_result = FALSE_T
       dialog_proc = TRUE_T
       return

    case (WM_COMMAND)
       if (p_self%process_command(h_dlg,wParam, &
            p_self%dialog_result) == TRUE_T) then
          dialog_proc = TRUE_T
          return
       end if
       ! ...else it continues returning FALSE
    end select

    dialog_proc = FALSE_T
    return

  end function dialog_proc

  function run(self) result(r)
    class(basic_box_t), intent(inout), target :: self
    integer(BOOL_T) :: r

    ! Input. We need this to "comunicate" with 'dialog_proc' and friends
    p_self => self

    dummy = int(dialog_box(GetModuleHandle(NULL_LPSTR), &
         make_int_resource(self%id_dlg),self%h_dlg, &
         c_funloc(dialog_proc)),INT_T)

    r = self%dialog_result
  end function run
end module basic_box_m
