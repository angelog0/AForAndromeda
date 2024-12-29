!
! Author: ANGELO GRAZIOSI
!
!   created   : Jul 18, 2019
!   last edit : Aug 26, 2019
!
!   Simple test app for ode_integrator.f90 module.
!
!
! Build with:
!
!   rm -rf *.mod; \
!     gfortran -std=f2008 -O3 -Wall -Wno-unused-dummy-argument \
!       ../basic-modules/kind_consts.f90 \
!       ../ode-modules/{everhart,ode}_integrator.f90 \
!       decay_field.f90 -o decay_field.out; \
!   rm -rf *.mod
!

module decay_field_lib
  use kind_consts, only: WP
  use ode_integrator, only: ID_GBS, ID_RKM, ID_R15, &
       ode_on, ode_integrate, ode_off, ode_calc => calc

  implicit none
  private

  integer, parameter :: NEQ = 1
  real(WP) :: k = 2.0_WP, t0 = 0.0_WP, t1 = 4.20_WP, h0 = 7.8125E-3_WP, &
       y0(NEQ) = 1.0_WP, y(NEQ), h, t

  public:: run_app

contains

  subroutine sub(t,y,f)
    real(WP), intent(in) :: t, y(:)
    real(WP), intent(out) :: f(:)

    f(1) = -k*y(1)
  end subroutine sub

  subroutine display_data(t,y)
    real(WP), intent(in) :: t, y(:)

    write(*,'(2f16.12)') t, y(:)
  end subroutine display_data

  subroutine run_app()
    use, intrinsic :: iso_fortran_env, only: IOSTAT_END

    write(*,*)
    write(*,*) '    USING RK4 METHOD'
    write(*,*)

    ! Classical RK4
    call ode_on(NEQ,sub,display_data)
    call ode_integrate(t0,t1,y0,h0)

    write(*,*)
    write(*,'(3f16.12)') h0, t0, y0(:)
    write(*,*)

    call ode_off()

    write(*,*)
    write(*,*) '    USING GBS METHOD'
    write(*,*)

    ! GBS
    h = 0.05_WP
    y = y0
    call ode_on(NEQ,sub,display_data,ID_GBS)
    call ode_calc(t0,t1,y,h)
    write(*,*)
    write(*,'(3f16.12)') h, t1, y(:)
    write(*,*)

    call ode_integrate(t0,t1,y0,h0)

    write(*,*)
    write(*,'(3f16.12)') h0, t0, y0(:)
    write(*,*)

    call ode_off()

    write(*,*)
    write(*,*) '    USING RKM METHOD'
    write(*,*)

    ! RKM
    h = 0.05_WP
    y = y0
    call ode_on(NEQ,sub,display_data,ID_RKM)
    call ode_calc(t0,t1,y,h)
    write(*,*)
    write(*,'(3f16.12)') h, t1, y(:)
    write(*,*)

    call ode_integrate(t0,t1,y0,h0)

    write(*,*)
    write(*,'(3f16.12)') h0, t0, y0(:)
    write(*,*)

    call ode_off()

    write(*,*)
    write(*,*) '    USING R15 METHOD'
    write(*,*)

    ! R15
    h = 0.05_WP
    y = y0
    call ode_on(NEQ,sub,display_data,ID_R15)
    call ode_calc(t0,t1,y,h)
    write(*,*)
    write(*,'(3f16.12)') h, t1, y(:)
    write(*,*)

    call ode_integrate(t0,t1,y0,h0)

    write(*,*)
    write(*,'(3f16.12)') h0, t0, y0(:)
    write(*,*)

    call ode_off()

    write(*,*)
    write(*,*) '    STILL USING GBS METHOD'
    write(*,*)

    ! GBS
    h = 0.05_WP
    y = y0
    call ode_on(NEQ,sub,METHOD=ID_GBS,DATA_FILE='GBS.data')
    call ode_calc(t0,t1,y,h)
    write(*,*)
    write(*,'(3f16.12)') h, t1, y(:)
    write(*,*)

    call ode_off()

    ! Now reading the GBS data file
    call read_GBS('GBS.data')

    write(*,*)
    write(*,*) '    STILL USING R15 METHOD'
    write(*,*)

    ! R15
    h = 0.05_WP
    y = y0
    call ode_on(NEQ,sub,METHOD=ID_R15,DATA_FILE='R15.data')
    call ode_calc(t0,t1,y,h)
    write(*,*)
    write(*,'(3f16.12)') h, t1, y(:)
    write(*,*)

    call ode_off()

    ! Now reading the R15 data file
    call read_R15('R15.data')

  contains

    subroutine read_GBS(gbs_file)
      use, intrinsic :: iso_fortran_env, only: IOSTAT_END
      character(len=*), intent(in) :: gbs_file

      integer :: data_unit = 0, io_status = 0, n, m

      open(newunit=data_unit,file=gbs_file,access='STREAM', &
           form='UNFORMATTED',status='OLD',iostat=io_status)

      if (io_status /= 0) then
         write(*,*)
         write(*,*)
         write(*,*) 'Error reading file: '//trim(gbs_file)
         stop ': Invalid file name (READ_GBS).'
      end if

      read(data_unit) n, m

      if (n /= NEQ) stop ': Mismatch for NEQ parameter (READ_GBS).'
      if (m /= ID_GBS) stop ': Mismatch for METHOD parameter (READ_GBS).'

      ! Reading the initial conditions. We do not test EOF
      ! because we assume at least a few values
      read(data_unit) h, t, y(1:n)
      write(*,'(3f16.12)') h, t, y(:)

      do
         read(data_unit,iostat=io_status) h, t, y(1:n)

         if (io_status == IOSTAT_END) exit
         if (io_status > 0) &
              stop ': Error occurred while reading file (READ_GBS).'

         write(*,'(3f16.12)') h, t, y(:)
      end do

      close(data_unit)
    end subroutine read_GBS

    subroutine read_R15(r15_file)
      use, intrinsic :: iso_fortran_env, only: IOSTAT_END
      character(len=*), intent(in) :: r15_file

      integer :: data_unit = 0, io_status = 0, nv0, ll0, nclass0, ns
      real(WP) :: yp(NEQ)

      open(newunit=data_unit,file=r15_file,access='STREAM', &
           form='UNFORMATTED',status='OLD',iostat=io_status)

      if (io_status /= 0) then
         write(*,*)
         write(*,*)
         write(*,*) 'Error reading file: '//trim(r15_file)
         stop ': Invalid file name (READ_R15).'
      end if

      read(data_unit) nv0, ll0, nclass0

      if (nv0 /= NEQ ) stop ': Mismatch for NV parameter (READ_R15).'

      write(*,*) '    LL: ', ll0
      write(*,*) 'NCLASS: ', nclass0
      write(*,*)

      ! Reading sequence n. 0, i.e. initial conditions. We do not test EOF
      ! because we assume at least a few sequences (NS > 1)
      read(data_unit) ns, h, t, y(1:nv0), yp(1:nv0)
      write(*,'(3f16.12)') h, t, y(:)

      do
         read(data_unit,iostat=io_status) ns, h, t, y(1:nv0), yp(1:nv0)

         if (io_status == IOSTAT_END) exit
         if (io_status > 0) &
              stop ': Error occurred while reading file (READ_R15).'

         write(*,'(3f16.12)') h, t, y(:)
      end do

      close(data_unit)
    end subroutine read_R15
  end subroutine run_app
end module decay_field_lib

program decay_field
  use decay_field_lib

  implicit none

  call run_app()
end program decay_field
