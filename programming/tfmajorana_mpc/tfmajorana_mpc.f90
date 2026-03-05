!
! THOMAS-FERMI EQUATION FOR NEUTRAL ATOM
! ======================================
!
!   u''(x) = x**(-1/2)*u(x)**(3/2), u(0) = 1, u(+inf) = 0
!
!   Slope at the origin,
!
!     u'(0) = -MU
!
!
! Computing the absolute value of the initial slope of the
! Thomas-Fermi equation with Majorana method using FMZM library,
! Multiple Precision Computation, https://dmsmith.lmu.build.
!
!
!   MU WITH MAJORANA METHOD, N = 321 COEFFICIENTS (28 digits)
!   MU = 1.588071022611375312718684509M+0
!
! To get the above result, NC_MAJO >= 320 (321 coefficients).
! This result should be compared with
!
!   u'(0) = -MU = -1.588071022611375312718684508
!
! as reported in
!
!   P.Amore et al.,
!   Accurate calculation of the solutions to the Thomas-Fermi equations,
!   http://arxiv.org/pdf/1205.1704v2.pdf (http://arxiv.org/abs/1205.1704)
!
!
!   MU WITH MAJORANA METHOD, N = 346 COEFFICIENTS (30 digits)
!   MU = 1.58807102261137531271868450942M+0
!
! To get the above result, NC_MAJO >= 345 (346 coefficients).
!
!
!   MU WITH MAJORANA METHOD N = 1001 COEFFICIENTS (50 digits)
!   MU = 1.5880710226113753127186845094239501094527466216748M+0
!
! To get the above result, NC_MAJO >= 592 (593 coefficients).
!
!
!   MU WITH MAJORANA METHOD N = 723 COEFFICIENTS (60 digits)
!   MU = 1.58807102261137531271868450942395010945274662167482561676568M+0
!
! To get the above result, NC_MAJO >= 722 (723 coefficients).
!
!
!   MU WITH MAJORANA METHOD N = 1214 COEFFICIENTS (100 digits)
!   MU = 1.588071022611375312718684509423950109452746621674825616765677418166551961154309262332033970138428665M+0
!
! To get the above result, NC_MAJO >= 1213 (1214 coefficients). It is also reported in
! https://en.wikipedia.org/wiki/Thomas%E2%80%93Fermi_equation#Solution_near_origin
!
!
! For a better comparison, consider
!
!   MU WITH MAJORANA METHOD, N = 4963 COEFFICIENTS (400 digits)
!   MU = 1.5880710226113753127186845094239501094527466216748256167656774181665519
!       6115430926233203397013842866526981402442246654652538163078118812938288132
!       7709036592147440868067789014756261697331421855147217728034321268939057847
!       5024448325043747243250416015320550636789524781158677290490115919823955029
!       0936872919960125747115454641997589356517736943217510752047768900396683865
!       9383125779338396907940522218829375862M+0
!
! To get the above result, NC_MAJO >= 4962 (4963 coefficients) and
! should be compared with what reported in
!
!   F.M.Fernandez et al.
!   On the Majorana solution to the Thomas-Fermi equation
!   https://arxiv.org/pdf/2105.02686v1 (https://arxiv.org/abs/2105.02686v1)
!
!
! Build with
!
!   gfortran [-O3 -static] tfmajorana_mpc.f90 -o tfmajorana_mpc -L . -lfm
!   (gfortran [-O3 -static] tfmajorana_mpc.f90 -o tfmajorana_mpc libfm.a)
!
! Run with
!
!   time ./tfmajorana_mpc 4962 400
!
! Build FMZM library with
!
!   cd FM_files
!
!   gfortran -O3 -I./ -o fmsave.o -c fmsave.f95
!   gfortran -O3 -I./ -o fm.o -c fm.f95
!   gfortran -O3 -I./ -o fmzm90.o -c fmzm90.f95
!
!   ar rcs libfm.a fmsave.o fmzm90.o fm.o
!
! Move libfm.a, fmzm.mod where you need or to the libraries or modules
! folders.
!

program tfmajorana_mpc
  use :: fmzm

  implicit none

  character(len=*), parameter :: FMT = '(*(g0))'

  character(len = 32) :: args
  integer :: nc_majo = 1000, ndigits = 50, i
  type(fm) :: mu

  i = 0
  do
     call get_command_argument(i,args)
     if (len_trim(args) == 0) exit

     if (i == 1) read(args,*) nc_majo
     if (i == 2) read(args,*) ndigits

     i = i+1
  end do

  if (i > 3) stop ': USAGE: ./tfmajorana_mpc.out [NC_MAJO [NDIGITS]]'

  if (ndigits /= 50) call fm_set(ndigits)

  mu = tfmajorana_slope0(nc_majo)

  write(*,FMT) 'MU WITH MAJORANA METHOD, N = ', nc_majo+1, &
       ' COEFFICIENTS (', ndigits, ' digits)'
  write(*,FMT,advance='no') 'MU ='
  call fm_print(mu)

contains

  function tfmajorana_slope0(nc) result(r)
    integer, intent(in) :: nc
    type(fm) :: r

    type(fm), save :: R73, A1, A2, R3_16
    logical, save :: first_call = .true.

    integer, save :: m, n, &
         mm2, mm1, nm1, &
         mp1, mp3_2, mp6, mp7, mp8_2, &
         np1, np4_2, np7, ierr = 0
    type(fm), save, allocatable :: a(:), tt(:)

    !
    ! A simple implementation of the Majorana method as described in
    !
    !   S. Esposito, Majorana solution of the Thomas-Fermi equation,
    !   Am. J. Phys. 70, 852 (2002).
    !   http://arxiv.org/pdf/physics/0111167.pdf
    !

    if (nc < 3) stop ': NC < 3 TOO LOW (TFMAJORANA_SLOPE0).'

    allocate(a(0:nc),STAT=ierr)
    if (ierr /= 0) stop ': Allocation failure for A(:) (TFMAJORANA_SLOPE0).'

    allocate(tt(nc-2),STAT=ierr)
    if (ierr /= 0) stop ': Allocation failure for TT(:) (TFMAJORANA_SLOPE0).'

    if (first_call) then
       ! Computing R73 = sqrt(73)
       R73 = to_fm('73')
       R73 = sqrt(R73)

       ! Computing A1 = 9-sqrt(73)
       A1 = 9-R73

       ! Computing A2 = (6497-755*R73)/152
       A2 = (6497-755*R73)
       A2 = A2/152

       ! Computing R3_16 = cube_root(3/16) = cube_root(3/2)/2 =
       ! cube_root(1.5)/2
       R3_16 = to_fm('1.5')
       call fmrational_power(R3_16,1,3,R3_16)
       R3_16 = R3_16/2

       first_call = .false.
    endif

    associate (sum_val => r)

      a(0:2) = [to_fm('1'), A1, A2]

      do m = 3, nc
         mm1 = m-1
         mm2 = m-2
         mp1 = m+1
         mp3_2 = 2*(m+3)
         mp6 = m+6
         mp7 = m+7
         mp8_2 = 2*(m+8)

         n = mm2
         nm1 = n-1
         np1 = n+1
         np4_2 = 2*(n+4)
         np7 = n+7
         tt(n) = np1*a(np1)-np4_2*a(n)+np7*a(nm1)

         sum_val = to_fm('0')
         do n = 1, mm2
            sum_val = sum_val+a(m-n)*tt(n)
         end do

         ! Partial value
         a(m) = sum_val+a(mm1)*(mp7-mp3_2*A1)+a(mm2)*mp6*A1

         ! Final value
         a(m) = a(m)/(mp8_2-mp1*A1)
      end do

      ! The MU value as computed with Majorana method
      sum_val = R3_16*sum(a(:nc))

    end associate

    if (allocated(tt)) deallocate(tt,STAT=ierr)
    if (ierr /= 0) stop ': Deallocation failure for TT(:) (TFMAJORANA_SLOPE0).'

    if (allocated(a)) deallocate(a,STAT=ierr)
    if (ierr /= 0) stop ': Deallocation failure for A(:) (TFMAJORANA_SLOPE0).'

  end function tfmajorana_slope0

end program tfmajorana_mpc
