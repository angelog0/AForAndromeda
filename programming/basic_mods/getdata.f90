!
! Author: Angelo Graziosi
!
!   created   : Mar 10, 2006
!   last edit : Jul 14, 2023
!
!   Useful GET data routines module
!

module getdata
  use kind_consts, only: WP
  use, intrinsic :: iso_fortran_env, only: ERROR_UNIT

  implicit none
  private

  integer, parameter, public :: MAXLEN = 256

  character(len=MAXLEN) :: buf

  interface get
     module procedure get_string, get_integer, get_real_wp, get_complex_wp, &
          get_logical
  end interface

  public :: get

contains

  subroutine get_string(msg,x)
    character(len=*), intent(in) :: msg
    character(len=*), intent(inout) :: x

    buf = ''
    write(ERROR_UNIT,*) trim(msg)//' ('//trim(x)//')'
    write(ERROR_UNIT,'(A)',advance='NO') 'New value: '
    read(*,'(A)') buf
    if (len(trim(buf)) > 0) then
       x = trim(buf)
    end if
  end subroutine get_string

  subroutine get_integer(msg,x)
    character(len=*), intent(in) :: msg
    integer, intent(inout) :: x

    buf = ''
    write(ERROR_UNIT,*) trim(msg)//' (',x,')'
    write(ERROR_UNIT,'(A)',advance='NO') 'New value: '
    read(*,'(A)') buf
    read(buf,*,err=10,end=10) x
10  continue
  end subroutine get_integer

  subroutine get_real_wp(msg,x)
    character(len=*), intent(in) :: msg
    real(WP), intent(inout) :: x

    buf = ''
    write(ERROR_UNIT,*) trim(msg)//' (',x,')'
    write(ERROR_UNIT,'(A)',advance='NO') 'New value: '
    read(*,'(A)') buf
    read(buf,*,err=10,end=10) x
10  continue
  end subroutine get_real_wp

  subroutine get_complex_wp(msg,x)
    character(len=*), intent(in) :: msg
    complex(WP), intent(inout) :: x

    buf = ''
    write(ERROR_UNIT,*) trim(msg),x
    write(ERROR_UNIT,'(A)',advance='NO') 'New value: '
    read(*,'(A)') buf
    read(buf,*,err=10,end=10) x
10  continue
  end subroutine get_complex_wp

  subroutine get_logical(msg,x)
    character(len=*), intent(in) :: msg
    logical, intent(inout) :: x

    buf = ''
    write(ERROR_UNIT,*) trim(msg),x
    write(ERROR_UNIT,'(A)',advance='NO') 'New value: '
    read(*,'(A)') buf
    read(buf,*,err=10,end=10) x
10  continue
  end subroutine get_logical
end module getdata
