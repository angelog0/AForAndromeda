! Author: Angelo Graziosi
!
!   created   : Mar 10, 2006
!   last edit : Jul 27, 2016
!
!   Some utilities module
!

module utilities
  use, intrinsic :: iso_c_binding, only: C_CHAR, C_PTR, C_SIZE_T, &
       c_associated, c_f_pointer

  implicit none
  private

  character(C_CHAR), dimension(1), save, target, private :: dummy_string='?'

  interface str_len
     !
     !  From Tobias Burnus,
     !  http://gcc.gnu.org/ml/fortran/2010-02/msg00029.html
     !
     !  Note: as both strlen and strlen2 have the same
     !  binding name, you can only use one of them at a
     !  time.
     !
     ! function strlen(str) bind(C)
     !   import :: C_CHAR, C_SIZE_T
     !   character(kind=C_CHAR) :: str(*)
     !   integer(C_SIZE_T) :: strlen
     ! end function strlen
     function strlen2(str) bind(C, name = 'strlen')
       import :: C_PTR, C_SIZE_T
       type (C_PTR), value :: str
       integer(C_SIZE_T)  :: strlen2
     end function strlen2
  end interface str_len

  ! interface strlen
  !    function strlen2(str) bind(C,name='strlen')
  !      ! We do not need the following "use" if we "import" only
  !      ! what we need!
  !      !use iso_c_binding, only: C_PTR, C_SIZE_T
  !      import :: C_PTR, C_SIZE_T
  !      type(C_PTR), value :: str
  !      integer(C_SIZE_T) :: strlen2
  !    end function strlen2
  ! end interface strlen

  public :: str_len, c_f_string, upcase, system_time

contains

  function c_f_string(cstr) result(fstr)
    type(C_PTR), intent(in) :: cstr ! The C address
    character(C_CHAR), dimension(:), pointer :: fstr
    ! Convert a null-terminated C string into a Fortran character array
    ! pointer. Adapted from
    ! http://cims.nyu.edu/~donev/Fortran/DLL/DLL.Forum.txt

    if (c_associated(cstr)) then
       call c_f_pointer(cstr,fstr,[str_len(cstr)])
    else
       ! To avoid segfaults, associate FSTR with a dummy target:
       fstr => dummy_string
    end if
  end function c_f_string

  function upcase(string) result(upper)
    character(len=*), intent(in) :: string
    character(len=len(string)) :: upper
    integer :: j

    do j = 1, len(string)
       if (string(j:j) >= 'a' .and. string(j:j) <= 'z') then
          upper(j:j) = achar(iachar(string(j:j)) - 32)
       else
          upper(j:j) = string(j:j)
       end if
    end do
  end function upcase

  ! =========================
  !  system time since 00:00
  ! =========================
  function system_time()
    use kind_consts, only: WP
    real(WP) :: system_time
    integer, dimension(8) :: dt
    call date_and_time(values=dt)
    system_time = dt(5)*3600.0_WP+dt(6)*60.0_WP+dt(7)+dt(8)*0.001_WP
  end function system_time
end module utilities
