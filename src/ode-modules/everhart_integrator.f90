!
! Author: ANGELO GRAZIOSI
!
!   created   : Sep 12, 2014
!   last edit : Sep 13, 2017
!
!   The Everhart integrator module
!   A simple module which tries to re-implement in modern Fortran the
!   Everhart's RADAU integrator.
!
!   Here we use the Whole Array Syntax (WAS). Notice that, in principle,
!   we should use the notation V(1:nv) for arrays created in the caller.
!   Indeed the others ALL have dimensions 1:nv and/or 1:NSTEPS, so that
!   V or V(:) is enough... But if we call RA15 as in
!
!     call ra15(t0,t1,x(1:nv),v(1:nv),h,force)
!
!   then we can use V(:) for all arrays..
!
!   Notice that here we allocate multidimensional arrays as NV x NSTEPS. In
!   this way, expessions like
!
!     g(:,4) = b(:,4)+D(10)*b(:,5)+D(14)*b(:,6)+D(19)*b(:,7)
!
!   SHOULD be evaluated more quickly [2]. "SHOULD" because we did not notice
!   a big difference..
!
! REFERENCES
!
!   1. E. Everhart, An Efficient Integrator That Use Gauss-Radau Spacings,
!      in A. Carusi and G. B. Valsecchi - Dynamics of Comets: Their Origin
!      and Evolution, 185-202. 1985 by D. Reidel Publishing Company.
!   2. http://www.fortran90.org/src/best-practices.html\
!        #multidimensional-arrays
!

module everhart_integrator
  use kind_consts, only: WP

  implicit none
  private

  integer, parameter :: NOR = 15
  integer, parameter :: NSTEPS = 7, NCOEF = (NSTEPS*(NSTEPS-1))/2
  real(WP), parameter :: ZERO = 0, ONE = 1, HALF = ONE/2

  ! These HS(:) values are the Gauss-Radau spacings, scaled to the
  ! range 0 to 1, for integrating to order 15. HS(0) == ZERO always.
  ! The sum of these H-values should be 3.7(3) = 3.733333... = 56/15
  ! (Viete formulas for the polynomial of degree 7 whose root are
  ! HS(1:NSTEPS)-values)
  !
  real(WP), parameter :: HS(0:NSTEPS) = [ ZERO, 0.05626256053692215_WP, &
       0.18024069173689236_WP, 0.35262471711316964_WP, &
       0.54715362633055538_WP, 0.73421017721541053_WP, &
       0.88532094683909577_WP, 0.97752061356128750_WP ]

  abstract interface
     subroutine ode_field(t,y,yp,f)
       import :: WP
       real(WP), intent(in) :: t, y(:), yp(:)
       real(WP), intent(out) :: f(:)
     end subroutine ode_field
  end interface

  integer :: nv = 0, ll = 0, nclass = 0
  procedure(ode_field), pointer :: force => null()
  logical :: save_log = .false., save_data = .false.

  ! Auxiliary variables
  integer :: log_unit = 0, data_unit = 0
  logical :: npq = .false., ncl = .false., nes = .false.

  !
  ! WORK SPACE
  !

  ! WC, UC, WC0, SS, C, D, R are, really, CONSTANTS
  real(WP) :: WC(NSTEPS) = ZERO, UC(NSTEPS) = ZERO, WC0 = ZERO, SS = ZERO
  real(WP) :: C(NCOEF) = ZERO, D(NCOEF) = ZERO, R(NCOEF) = ZERO

  ! The workspace would be NV x 3*NSTEPS+4 = NV x 3*7+4 --> w(NV,25)
  ! (BSG uses w(NEQ,36), being NEQ the number of equations of 1st order)
  real(WP), save, allocatable :: f0(:), fj(:), y(:), yp(:)
  real(WP), save, allocatable :: b(:,:), bd(:,:), g(:,:), e(:,:)

  public :: ra15_on, ra15_run, ra15_off

contains

  subroutine ra15_on(neq,accuracy,eqs_class,eqs_field,log_file,data_file)
    integer, intent(in) :: neq, accuracy, eqs_class
    procedure(ode_field) :: eqs_field
    character(len=*), intent(in), optional :: log_file, data_file

    integer, parameter :: NW(0:NSTEPS)= [ 0, 0, 1, 3, 6, 10, 15, 21 ]

    real(WP) :: temp
    integer :: l, la, lb, lc, ld, le, k, ierr

    ! WORK SPACE ALLOCATION
    allocate(b(neq,NSTEPS),stat=ierr)
    if (ierr /= 0) stop ': Allocation failure for B(:,:) (RA15_ON).'

    allocate(bd(neq,NSTEPS),stat=ierr)
    if (ierr /= 0) stop ': Allocation failure for BD(:,:) (RA15_ON).'

    allocate(g(neq,NSTEPS),stat=ierr)
    if (ierr /= 0) stop ': Allocation failure for G(:,:) (RA15_ON).'

    allocate(e(neq,NSTEPS),stat=ierr)
    if (ierr /= 0) stop ': Allocation failure for E(:,:) (RA15_ON).'

    allocate(f0(neq),stat=ierr)
    if (ierr /= 0) stop ': Allocation failure for F0(:) (RA15_ON).'

    allocate(fj(neq),stat=ierr)
    if (ierr /= 0) stop ': Allocation failure for FJ(:) (RA15_ON).'

    allocate(y(neq),stat=ierr)
    if (ierr /= 0) stop ': Allocation failure for Y(:) (RA15_ON).'

    allocate(yp(neq),stat=ierr)
    if (ierr /= 0) stop ': Allocation failure for YP(:) (RA15_ON).'

    ! Input data initialization
    nv = neq
    ll = accuracy
    nclass = eqs_class
    force => eqs_field

    ! GLOBAL LOGICAL DATA INITIALIZATION
    !
    ! NCL is a flag which says if the equations are of 1st order (.TRUE.) or
    ! of 2nd order (.FALSE.) :
    !
    !   y' = F(t,y),    NCL == .TRUE.
    !   y" = F(t,y),    NCL == .FALSE.
    !   y" = F(t,y,y'), NCL == .FALSE.
    !
    ! NPQ is a flag which says if the equations are of 2nd order general
    ! (.FALSE.) or NOT 2nd order general (.TRUE.), i.e. of 1st order or
    ! 2nd order Special (without y') :
    !
    ! NCLASS ==  1, NPQ == .TRUE.
    ! NCLASS == -2, NPQ == .TRUE.
    ! NCLASS ==  2, NPQ == .FALSE.
    ! NES  is .TRUE.  only if LL is negative. Then the sequence size is H0.
    !
    ncl = (nclass == 1)
    npq = (nclass < 2)
    nes = (ll < 0)

    ! In the old implementation:
    !
    !   foo_flag = .false.
    !   if (present(foo)) foo_flag = foo
    !
    ! foo_flag is set twice when foo is present.
    !
    ! In the implementation below, the flag is set only once
    !
    if (present(log_file)) then
       save_log = .true.
    else
       save_log = .false.
    end if

    if (present(data_file)) then
       save_data = .true.
    else
       save_data = .false.
    end if

    ! ..or, having set those flags to .false. in the module definition
    !if (present(log_file)) save_log = .true.
    !if (present(data_file)) save_data = .true.

    ! CONSTANT coefficients setup
    k = 2
    do l = 1, NSTEPS
       temp = k+k*k
       if (ncl) temp = k
       WC(l) = ONE/temp
       temp = k
       UC(l) = ONE/temp
       k = k+1
    end do

    WC0 = HALF
    if (ncl) WC0 = ONE

    C(1) = -HS(1)
    D(1) = HS(1)
    R(1) = ONE/(HS(2)-HS(1))
    la = 1
    lc = 1
    do k = 3, NSTEPS
       lb = la
       la = lc+1
       lc = NW(k)
       C(la) = -HS(k-1)*C(lb)
       C(lc) = C(la-1)-HS(k-1)

       D(la) = HS(1)*D(lb)
       D(lc) = -C(lc)

       R(la) = ONE/(HS(k)-HS(1))
       R(lc) = ONE/(HS(k)-HS(k-1))

       if (k == 3) cycle

       do l = 4, k
          ld = la+l-3
          le = lb+l-4
          C(ld) = C(le)-HS(k-1)*C(le+1)
          D(ld) = D(le)+HS(l-2)*D(le+1)
          R(ld) = ONE/(HS(k)-HS(l-2))
       end do
    end do

    ! SS is, really, a CONSTANT (like WC, UC, and WC0)
    SS = 10.0_WP ** (-ll)
    !
    ! The statements above are used only once in an integration to set up
    ! the constants. They uses less than a second of execution time.
    !

    ! Opening LOG file: 'log_file' MUST BE a valid file name
    if (save_log) &
         open(newunit=log_unit,file=log_file,status='REPLACE')

    ! Opening DATA file: 'data_file' MUST BE a valid file name
    if (save_data) then
       open(newunit=data_unit,file=data_file,access='STREAM', &
            form='UNFORMATTED',status='REPLACE')
       write(data_unit) nv, ll, nclass
    end if
  end subroutine ra15_on

  subroutine ra15_off()
    integer :: ierr

    ! Closing DATA file
    if (save_data) close(data_unit)

    ! Closing LOG file
    if (save_log) close(log_unit)

    ! Clear (dissociate) the pointer
    nullify(force)

    ! FREEING WORK SPACE
    if (allocated(yp)) deallocate(yp,stat=ierr)
    if (ierr /= 0) stop ': Deallocation failure for YP(:) (RA15_OFF).'

    if (allocated(y)) deallocate(y,stat=ierr)
    if (ierr /= 0) stop ': Deallocation failure for Y(:) (RA15_OFF).'

    if (allocated(fj)) deallocate(fj,stat=ierr)
    if (ierr /= 0) stop ': Deallocation failure for FJ(:) (RA15_OFF).'

    if (allocated(f0)) deallocate(f0,stat=ierr)
    if (ierr /= 0) stop ': Deallocation failure for F0(:) (RA15_OFF).'

    if (allocated(e)) deallocate(e,stat=ierr)
    if (ierr /= 0) stop ': Deallocation failure for E(:,:) (RA15_OFF).'

    if (allocated(g)) deallocate(g,stat=ierr)
    if (ierr /= 0) stop ': Deallocation failure for G(:,:) (RA15_OFF).'

    if (allocated(bd)) deallocate(bd,stat=ierr)
    if (ierr /= 0) stop ': Deallocation failure for BD(:,:) (RA15_OFF).'

    if (allocated(b)) deallocate(b,stat=ierr)
    if (ierr /= 0) stop ': Deallocation failure for B(:,:) (RA15_OFF).'
  end subroutine ra15_off

  subroutine ra15_run(ta,tz,x,v,h0)
    real(WP), intent(in) :: ta, tz
    real(WP), intent(inout) :: x(:), v(:), h0
    !
    ! Integrator by E. Everhart, Physics Department, University of Denver
    ! Revision :  Angelo Graziosi (Sept. 12, 2014)
    !
    ! This 15th-order version is called RA15. Order NOR is 15.
    !
    ! y' = F(t,y)    is NCLASS == 1,   y" = F(t,y) is NCLASS == -2,
    ! y" = F(t,y,y') is NCLASS == 2
    !
    ! TF is t(final)-t(initial). (Negative when integrating backward.)
    ! NV = the number of simultaneous differential equations.
    !
    ! LL controls accuracy. Thus SS = 10.**(-LL) controls the size of
    ! the last term in a series. Try LL = 8 and work up or down from there.
    ! However, if LL < 0, then H0 is the constant sequence size used.
    ! A non-zero H0 sets the size of the first sequence regardless of
    ! LL sign. Zero's and Oh's could look alike here. Use care!
    !
    ! X and V enter as the starting position-velocity vector (values of y
    ! and y' at t = ta) and they output as the final position-velocity vector.
    !
    integer, parameter :: MAX_NCOUNT = 10
    real(WP), parameter :: SR = 1.4_WP, PW = ONE/9, &
         EPS_TF_MATCH = 1.0E-08_WP, &
         Z2 = 2, Z3 = 3, Z4 = 4, Z5 = 5, Z6 = 6, Z7 = 7, &
         Z10 = 10, Z15 = 15, Z20 = 20, Z21 = 21, Z35 = 35
    integer, save :: i, ncount, ns, nf, ni, j
    logical, save :: nsf, nper
    real(WP), save :: tf, hval, hp, tm, tmf, h, h2, s, q, temp, hv, &
         q2, q3, q4, q5, q6, q7

    ! NSF  is .FALSE. on starting sequence, otherwise .TRUE.
    ! NPER is .TRUE.  only on last sequence of integration.
    nsf = .false.
    nper = .false.

    ! Initialize the working space. We need to initialize only B and BD.
    if (ncl) v(:) = ZERO
    b(:,:) = ZERO
    bd(:,:) = ZERO

    !print *, size(x), size(v), size(y), size(yp)

    tf = tz-ta
    h0 = sign(h0,tf)

    ! Now set in an estimate to HP based on experience. Same sign as TF.
    if (h0 /= ZERO) then
       hp = h0
    else
       hp = sign(0.1_WP,tf)
    end if
    if (hp/tf > HALF) hp = HALF*tf

    ! NCOUNT is the number of attempts to find the optimal sequence size.
    ! If NCOUNT > MAX_NCOUNT it returns to the caller: integration failed.
    ncount = 0

    if (save_log) then
       ! An * is the symbol for writing on the monitor. The file is unit
       ! LOG_UNIT.
       write(*,*) ' No. of calls, Every 10th seq.X(1),X(2),H,TM,TF'
    end if

    ! Now the loop regarding the first sequence, aka THE MAIN LOOP, or
    ! if you prefer, the main sequence loop.
    !
    ! NS is the number of sequences done
    ! NF is the number of calls to FORCE subroutine
    ! NI is the number of iterations to predict the B-values. NI is 6 for
    ! the first sequence, 2 after it.
    !
    main_loop: do
       ns = 0
       if (save_log) nf = 0
       ni = 6
       tm = ZERO
       tmf = ta
       call force(tmf,x,v,f0)
       if (save_log) nf = nf+1

       ! Now begins every sequence after the first. First find new
       ! G-values from the predicted B-values, following Eqs. (7) in text.
       every_sequence_loop: do

          g(:,1) = b(:,1)+D(1)*b(:,2)+D(2)*b(:,3)+D(4)*b(:,4)+D(7)*b(:,5) &
               +D(11)*b(:,6)+D(16)*b(:,7)
          g(:,2) = b(:,2)+D(3)*b(:,3)+D(5)*b(:,4)+D(8)*b(:,5) &
               +D(12)*b(:,6)+D(17)*b(:,7)
          g(:,3) = b(:,3)+D(6)*b(:,4)+D(9)*b(:,5)+D(13)*b(:,6)+D(18)*b(:,7)
          g(:,4) = b(:,4)+D(10)*b(:,5)+D(14)*b(:,6)+D(19)*b(:,7)
          g(:,5) = b(:,5)+D(15)*b(:,6)+D(20)*b(:,7)
          g(:,6) = b(:,6)+D(21)*b(:,7)
          g(:,7) = b(:,7)

          !
          ! H    is the sequence size
          ! HP   is the guessed sequence size
          ! HVAL is the absolute value of sequence size
          ! TM   is the current time relative to TA
          ! TMF  is the current time (time to be passed to the force/FCN)
          !
          h = hp
          if (ncl) then
             h2 = h
          else
             h2 = h*h
          end if
          hval = abs(h)

          if (save_log) then
             ! Writing to the screen during the integration lets one monitor
             ! the progress. Values are shown at every 10th sequence.
             if (ns/10*10 == ns) then
                if (nv > 1) then
                   temp = x(2)
                else
                   temp = ZERO
                end if
                write(*,'(1X,2I6,5F12.5)') nf, ns, x(1), temp, h, tm, tf
             end if
          end if

          ! better_B_loop is 6 iterations on first sequence and
          ! 2 iterations therafter
          better_B_loop: do i = 1, ni
             ! This loop is for each substep within a sequence.
             substep_loop: do j = 1, NSTEPS
                s = HS(j)
                q = s
                if (ncl) q = ONE

                ! Here Y is used for the value of y at substep n.
                ! We use Eq. (9). The collapsed series are broken in two part
                ! because an otherwise excellent compiler could not handle the
                ! complicated expression. We use y(:) and yp(:) as "temp"
                ! variable
                !
                y(:) = WC(3)*b(:,3)+s*(WC(4)*b(:,4)+s*(WC(5)*b(:,5) &
                     +s*(WC(6)*b(:,6)+s*WC(7)*b(:,7))))
                y(:) = x(:)+q*(h*v(:)+h2*s*(f0(:)*WC0+s*(WC(1)*b(:,1) &
                     +s*(WC(2)*b(:,2)+s*y(:)))))

                ! If equations are 1st order or 2nd order special
                ! (npq == .true.), i.e. without y', continue..
                if (.not.npq) then
                   ! Next are calculated the velocity predictors if need for
                   ! general Class II. Here YP is used as the value of y' at
                   ! substep n (Eq. (10)).
                   yp(:) = UC(3)*b(:,3)+s*(UC(4)*b(:,4)+s*(UC(5)*b(:,5) &
                        +s*(UC(6)*b(:,6)+s*UC(7)*b(:,7))))
                   yp(:) = v(:)+s*h*(f0(:)+s*(UC(1)*b(:,1) &
                        +s*(UC(2)*b(:,2)+s*yp(:))))
                end if

                ! Find forces at each substep.
                call force(tmf+s*h,y,yp,fj)
                if (save_log) nf = nf+1

                !
                ! Since y(:) and yp(:) are 'free' now, we can use y(:) as "q"
                ! and yp(:) as "temp"
                !
                ! (A)
                ! Find G-values from the force FJ found at current substep.
                ! This section uses Eqs. (4) of text.
                ! Before save in YP(:) (TEMP) the current value.
                !
                ! (B)
                ! YP(:) (TEMP) is now the improvement on G(J,K) over its
                ! former value. Now we upgrade the B-value using this
                ! difference in the one term.
                ! This section is based on Eqs. (5).
                !

                y(:) = (fj(:)-f0(:))/s

                select case (j)
                case (1)
                   ! See comment (A) above...
                   yp(:) = g(:,1)
                   g(:,1) = y(:)

                   ! See comment (B) above...
                   yp(:) = g(:,1)-yp(:)
                   b(:,1) = b(:,1)+yp(:)
                case (2)
                   ! See comment (A) above...
                   yp(:) = g(:,2)
                   g(:,2) = (y(:)-g(:,1))*R(1)

                   ! See comment (B) above...
                   yp(:) = g(:,2)-yp(:)
                   b(:,1) = b(:,1)+C(1)*yp(:)
                   b(:,2) = b(:,2)+yp(:)
                case (3)
                   ! See comment (A) above...
                   yp(:) = g(:,3)
                   g(:,3) = ((y(:)-g(:,1))*R(2)-g(:,2))*R(3)

                   ! See comment (B) above...
                   yp(:) = g(:,3)-yp(:)
                   b(:,1) = b(:,1)+C(2)*yp(:)
                   b(:,2) = b(:,2)+C(3)*yp(:)
                   b(:,3) = b(:,3)+yp(:)
                case (4)
                   ! See comment (A) above...
                   yp(:) = g(:,4)
                   g(:,4) = (((y(:)-g(:,1))*R(4)-g(:,2))*R(5)-g(:,3))*R(6)

                   ! See comment (B) above...
                   yp(:) = g(:,4)-yp(:)
                   b(:,1) = b(:,1)+C(4)*yp(:)
                   b(:,2) = b(:,2)+C(5)*yp(:)
                   b(:,3) = b(:,3)+C(6)*yp(:)
                   b(:,4) = b(:,4)+yp(:)
                case (5)
                   ! See comment (A) above...
                   yp(:) = g(:,5)
                   g(:,5) = ((((y(:)-g(:,1))*R(7)-g(:,2))*R(8)-g(:,3))*R(9) &
                        -g(:,4))*R(10)

                   ! See comment (B) above...
                   yp(:) = g(:,5)-yp(:)
                   b(:,1) = b(:,1)+C(7)*yp(:)
                   b(:,2) = b(:,2)+C(8)*yp(:)
                   b(:,3) = b(:,3)+C(9)*yp(:)
                   b(:,4) = b(:,4)+C(10)*yp(:)
                   b(:,5) = b(:,5)+yp(:)
                case (6)
                   ! See comment (A) above...
                   yp(:) = g(:,6)
                   g(:,6) = (((((y(:)-g(:,1))*R(11)-g(:,2))*R(12) &
                        -g(:,3))*R(13)-g(:,4))*R(14)-g(:,5))*R(15)

                   ! See comment (B) above...
                   yp(:) = g(:,6)-yp(:)
                   b(:,1) = b(:,1)+C(11)*yp(:)
                   b(:,2) = b(:,2)+C(12)*yp(:)
                   b(:,3) = b(:,3)+C(13)*yp(:)
                   b(:,4) = b(:,4)+C(14)*yp(:)
                   b(:,5) = b(:,5)+C(15)*yp(:)
                   b(:,6) = b(:,6)+yp(:)
                case (7)
                   ! See comment (A) above...
                   yp(:) = g(:,7)
                   g(:,7) = ((((((y(:)-g(:,1))*R(16)-g(:,2))*R(17) &
                        -g(:,3))*R(18)-g(:,4))*R(19)-g(:,5))*R(20) &
                        -g(:,6))*R(21)

                   ! See comment (B) above...
                   yp(:) = g(:,7)-yp(:)
                   b(:,1) = b(:,1)+C(16)*yp(:)
                   b(:,2) = b(:,2)+C(17)*yp(:)
                   b(:,3) = b(:,3)+C(18)*yp(:)
                   b(:,4) = b(:,4)+C(19)*yp(:)
                   b(:,5) = b(:,5)+C(20)*yp(:)
                   b(:,6) = b(:,6)+C(21)*yp(:)
                   b(:,7) = b(:,7)+yp(:)
                end select
             end do substep_loop

             if (nes .or. i < ni) cycle better_B_loop

             ! Integration of sequence is over. Next is sequence size control.
             hv = maxval(abs(b(:,7)))
             hv = hv*WC(7)/hval**7
          end do better_B_loop

          ! If this is the 1st sequence... we still have to adjust the
          ! time step
          if (.not. nsf) then
             if (.not. nes) hp = sign((SS/hv)**PW,tf)
             if (nes .or. hp/h > ONE) then
                if (nes) hp = h0
                nsf = .true.
                if (save_data) then
                   write(data_unit) ns, h, tmf, x, v
                end if
             else
                hp = 0.8_WP*hp
                ncount = ncount+1
                if (ncount > MAX_NCOUNT) then
                   write(*,*)
                   write(*,*) '*************************************'
                   write(*,*) 'NCOUNT > ', MAX_NCOUNT
                   write(*,*) 'Cannot find an optimal sequence size.'
                   write(*,*) 'RA15 returns to the caller.'
                   write(*,*) '*************************************'
                   write(*,*)
                   ! Exiting the main loop should be the same as RETURN.
                   ! (Doing so one could close also an LOG_UNIT file if
                   ! it were opened at the beginning of this routine...)
                   exit main_loop
                   !return
                end if

                if (save_log) then
                   if (ncount > 1) &
                        write(log_unit,'(2X,2I2,2ES18.10)') NOR, ncount, h, hp
                end if

                ! Restart with HP = 0.8*H if new HP is smaller than original
                ! H on 1st sequence.
                cycle main_loop
             end if
          end if

          ! This loop finds new X and V values at end of sequence.
          ! Eqs. (11), (12).
          x(:) = x(:)+v(:)*h+h2*(f0(:)*WC0+b(:,1)*WC(1)+b(:,2)*WC(2) &
               +b(:,3)*WC(3)+b(:,4)*WC(4)+b(:,5)*WC(5)+b(:,6)*WC(6) &
               +b(:,7)*WC(7))

          ! If equations are 1st order (npq == .true.), skip to compute y'
          ! (aka V) at end of sequence..
          if (.not.ncl) then
             v(:) = v(:)+h*(f0(:)+b(:,1)*UC(1)+b(:,2)*UC(2)+b(:,3)*UC(3) &
                  +b(:,4)*UC(4)+b(:,5)*UC(5)+b(:,6)*UC(6)+b(:,7)*UC(7))
          end if
          !
          ! We have done a sequence and can update current time and
          ! sequence counter.
          !
          tm = tm+h
          tmf = tmf+h
          ns = ns+1

          if (save_data .and. .not.nper) then
             write(data_unit) ns, h, tmf, x, v
          end if

          ! Return if done.
          if (nper) then
             if (save_log) then
                if (nv > 1) then
                   temp = x(2)
                else
                   temp = ZERO
                end if
                write(*,'(1X,2I6,5F12.5)') nf, ns, x(1), temp, h, tm, tf
                write(log_unit,'(1X,2I6)') nf, ns
             end if

             if (save_data) then
                write(data_unit) ns, h, tmf, x, v
             end if

             ! On exit, H0 contains the last computed (signed) sequence size
             h0 = h

             ! Exiting the main loop should be the same as RETURN.
             ! (Doing so one could close also an LOG_UNIT file if
             ! it were opened at the beginning of this routine...)
             exit main_loop
             !return
          end if

          ! Control on size of next sequence and adjust last sequence to
          ! exactly cover the integration span. NPER = .TRUE. set on last
          ! sequence.
          call force(tmf,x,v,f0)

          if (save_log) nf = nf+1

          if (nes) then
             hp = h0
          else
             hp = sign((SS/hv)**PW,tf)
             if (hp/h > SR) hp = h*SR
          end if
          if (abs(tm+hp) >= abs(tf)-EPS_TF_MATCH) then
             hp = tf-tm
             nper = .true.
          end if

          ! Now predict B-values for next step using Eqs. (13). Values from
          ! the preceding sequence were saved in the E-matrix. The correction
          ! BD is applied in the following loop as described in Sec. 2.5.
          q = hp/h

          ! To avoid re-computation of the same expession (q**2, q**3,...)
          ! for each K...
          q2 = q*q!q**2
          q3 = q*q2!q**3
          q4 = q2*q2!q**4
          q5 = q2*q3!q**5
          q6 = q3*q3!q**6
          q7 = q3*q4!q**7

          ! If we have done at least TWO sequences..
          if (ns /= 1) then
             do j = 1, NSTEPS
                bd(:,j) = b(:,j)-e(:,j)
             end do
          end if

          e(:,1) = q*(b(:,1)+Z2*b(:,2)+Z3*b(:,3)+Z4*b(:,4)+Z5*b(:,5) &
               +Z6*b(:,6)+Z7*b(:,7))
          e(:,2) = q2*(b(:,2)+Z3*b(:,3)+Z6*b(:,4)+Z10*b(:,5)+Z15*b(:,6) &
               +Z21*b(:,7))
          e(:,3) = q3*(b(:,3)+Z4*b(:,4)+Z10*b(:,5)+Z20*b(:,6)+Z35*b(:,7))
          e(:,4) = q4*(b(:,4)+Z5*b(:,5)+Z15*b(:,6)+Z35*b(:,7))
          e(:,5) = q5*(b(:,5)+Z6*b(:,6)+Z21*b(:,7))
          e(:,6) = q6*(b(:,6)+Z7*b(:,7))
          e(:,7) = q7*b(:,7)

          ! Apply the correction.. Notice that when we have done ONLY
          ! one sequence (NS == 1), BD == 0 from its initialization, i.e.
          ! we are doing B = E. It is only when NS > 1 that we are applying
          ! the correction BD.
          !
          do j = 1, NSTEPS
             b(:,j) = e(:,j)+bd(:,j)
          end do

          ! Two iterations for every sequence. (Use 3 for 23rd and 27th
          ! order.)
          ni = 2
       end do every_sequence_loop
    end do main_loop
  end subroutine ra15_run
end module everhart_integrator
