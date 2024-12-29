!
! Author: Angelo Graziosi
!
!   created   : Aug 10, 2016
!   last edit : Aug 18, 2016
!
!   The class for Radio dialog
!
!
! DESCRIPTION
!
!   This is 'radio_box_m' module implementing the class (radio_box_t)
!   for our Radio dialog.
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

module radio_box_m
  use win32, only: BOOL_T, DWORD_T, FALSE_T, HWND_T, IDCANCEL, IDOK, INT_T, &
       INT_PTR_T, MAX_LEN, NUL, TRUE_T, WORD_T, WPARAM_T, &
       CheckRadioButton, dummy, EndDialog, lo_word, SetDlgItemText
  use basic_box_m, only : basic_box_t

  implicit none
  private

  integer, parameter :: MAX_RADIO_BUTTONS = 10

  ! The derived class..
  type, extends(basic_box_t), public :: radio_box_t
     private
     integer(INT_T) :: idc_first_button, idc_last_button, idc_current_button
     character(len=MAX_LEN) :: button_names(MAX_RADIO_BUTTONS)
     integer :: num_buttons
     integer :: current_button
   contains
     private
     generic, public :: initialize => initialize_radio_box_t
     procedure, pass(self) :: initialize_radio_box_t
     procedure, pass(self) :: init_dialog
     procedure, pass(self) :: ok_command
     procedure, pass(self) :: process_command
     procedure, pass(self), public :: get => get_radio_box_t
  end type radio_box_t

contains

  ! This is the "class" initialization routine: The Constructor!
  subroutine initialize_radio_box_t(self,h_dlg,id_dlg,idc_first_button, &
       button_names,num_buttons,current_button)
    class(radio_box_t), intent(inout) :: self
    integer(HWND_T), intent(in) :: h_dlg
    integer(WORD_T), intent(in) :: id_dlg
    integer(INT_T), intent(in) :: idc_first_button
    character(len=*), intent(in) :: button_names(:)
    integer, intent(in) :: num_buttons
    integer, intent(in) :: current_button

    integer :: i

    if (num_buttons > MAX_RADIO_BUTTONS) then
       write(*,*) 'NUM_BUTTONS > ', MAX_RADIO_BUTTONS, ' NOT ALLOWED!!!'
       error stop ': NUM_BUTTONS too large parameter.'
    end if

    ! Create the base class
    call self%initialize(h_dlg,id_dlg)

    self%idc_first_button = idc_first_button
    self%idc_last_button = idc_first_button+(num_buttons-1)

    do i = 1, num_buttons
       self%button_names(i) = trim(adjustl(button_names(i)))
    end do

    self%num_buttons = num_buttons
    self%current_button = current_button
  end subroutine initialize_radio_box_t

  subroutine init_dialog(self,h_dlg)
    class(radio_box_t), intent(inout) :: self
    integer(HWND_T), intent(in) :: h_dlg

    integer :: i

    do i = 1, self%num_buttons
       dummy = SetDlgItemText(h_dlg,self%idc_first_button+(i-1), &
            trim(adjustl(self%button_names(i)))//NUL)
    end do

    self%idc_current_button = self%idc_first_button+(self%current_button-1)
    dummy = CheckRadioButton(h_dlg, &
         self%idc_first_button,self%idc_last_button,self%idc_current_button)
  end subroutine init_dialog

  subroutine ok_command(self,h_dlg)
    class(radio_box_t), intent(inout) :: self
    integer(HWND_T), intent(in) :: h_dlg

    self%current_button = (self%idc_current_button-self%idc_first_button)+1
    dummy = EndDialog(h_dlg,int(IDOK,INT_PTR_T))
  end subroutine ok_command

  function process_command(self,h_dlg,wParam,dialog_result)
    class(radio_box_t), intent(inout) :: self
    integer(BOOL_T) :: process_command
    integer(HWND_T), intent(in) :: h_dlg
    integer(WPARAM_T), intent(in) :: wParam
    integer(BOOL_T), intent(inout) :: dialog_result

    ! Now we use dummy to store the current button if it is valid. Se below...
    dummy = lo_word(int(wParam,DWORD_T))

    ! We test if the current button is valid...
    if ((self%idc_first_button <= dummy) .and. &
         (dummy <= self%idc_last_button)) then

       ! ...being valid, we save it...
       self%idc_current_button = dummy

       ! Now dumy is "free" and can be reused... :-)
       dummy = CheckRadioButton(h_dlg,self%idc_first_button, &
            self%idc_last_button,self%idc_current_button)
       process_command = TRUE_T
       return
    end if

    ! ...if it is not valid, it could be something else...
    select case (dummy)
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

  ! Getting the current button
  function get_radio_box_t(self) result(r)
    class(radio_box_t), intent(in) :: self
    integer :: r

    r = self%current_button
  end function get_radio_box_t
end module radio_box_m
