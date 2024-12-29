!
! Author: Angelo Graziosi
!
!   created   : Sep 12, 2014
!   last edit : Jul 27, 2016
!
!  Fortran Interface to the Function Parser Library
!  Test example
!
!
! HOW TO BUILD THE APP (macOS, MSYS2. MSYS2-MINGW64, GNU/Linux)
!
!   wget http://warp.povusers.org/FunctionParser/fparser4.5.2.zip
!   aunpack fparser4.5.2.zip -X fparser-4.5.2/
!   cd fparser-4.5.2
!
!   g++[-mp-4.9] -DFP_SUPPORT_FLOAT_TYPE [-DFP_USE_STRTOLD] \
!     -DFP_SUPPORT_LONG_DOUBLE_TYPE \
!     -DFP_SUPPORT_LONG_INT_TYPE -DFP_SUPPORT_COMPLEX_DOUBLE_TYPE \
!     -DFP_SUPPORT_COMPLEX_FLOAT_TYPE -DFP_SUPPORT_COMPLEX_LONG_DOUBLE_TYPE \
!     -DFP_USE_THREAD_SAFE_EVAL -DFP_USE_THREAD_SAFE_EVAL_WITH_ALLOCA \
!     -c fparser.cc
!
!   g++[-mp-4.9] -DFP_SUPPORT_FLOAT_TYPE [-DFP_USE_STRTOLD] \
!     -DFP_SUPPORT_LONG_DOUBLE_TYPE \
!     -DFP_SUPPORT_LONG_INT_TYPE -DFP_SUPPORT_COMPLEX_DOUBLE_TYPE \
!     -DFP_SUPPORT_COMPLEX_FLOAT_TYPE -DFP_SUPPORT_COMPLEX_LONG_DOUBLE_TYPE \
!     -DFP_USE_THREAD_SAFE_EVAL -DFP_USE_THREAD_SAFE_EVAL_WITH_ALLOCA \
!     -c fpoptimizer.cc
!
!   mv *.o ../fparser-fortran/
!
! It seems that on OSX 10.9 (Mavericks), with g++-mp-4.9 (MacPorts),
! it accepts also DFP_USE_STRTOLD...
!
!   cd ../fparser-fortran
!   g++[-mp-4.9] -I ../fparser-4.5.2 -c cwrapper_fparser.cc
!   ar rcs libFParser.a fparser.o fpoptimizer.o cwrapper_fparser.o
!
!   rm -rf *.o *.mod
!   gfortran[-mp-4.9] ../basic-modules/{kind_consts,getdata,utilities}.f90 \
!     fparser_dp.f90 fparser_cd.f90 fparser_test.f90 \
!     -L . -lFParser -lstdc++ -o fparser_test.out
!

module adding_functions
  use kind_consts, only: WP
  !
  ! The functions of this module could be defined also in the "contains"
  ! section of the main...
  !
  implicit none
  private

  public :: sqr_x, sqr_z

contains

  function sqr_x(p) result (s)
    real(WP), intent(in) :: p(*)
    real(WP) :: s
    s = p(1)*p(1)
  end function sqr_x

  function sqr_z(p) result (s)
    complex(WP), intent(in) :: p(*)
    complex(WP) :: s
    s = p(1)*p(1)
  end function sqr_z
end module adding_functions

program fparser_test
  use kind_consts, only: WP
  use getdata, only: get, MAXLEN
  use adding_functions

  implicit none

  call test_fx()
  call test_fz()

contains

  subroutine test_fx()
    use fparser_dp

    character(len=MAXLEN) :: fcn_str = 'x*x'
    type(FunctionParser_type) fparser, fp2
    integer :: res, use_degrees = 0, ammount
    real(WP) :: vals(2), minx = -5.0_WP, maxx = 5.0_WP, stp = 1.0_WP

    call NewParser(fparser)
    call NewParser(fp2)

    if (AddConstant(fparser,'pi',3.1415926535897932_WP) <= 0) then
       write(*,*) "AddConstant() error..."
       stop
    end if

    if (AddUnit(fparser,'cm',100.0_WP) <= 0) then
       write(*,*) "AddUnit() error..."
       stop
    end if

    if (AddFunction(fparser,'sqr',sqr_x,1) <= 0) then
       write(*,*) "AddFunction() error (Fortran function)..."
       stop
    end if

    do
       call get("Degrees (0 = radians,1 = degrees):",use_degrees)
       call get("f(x) =",fcn_str)

       if (use_degrees > 0) then
          res = Parse(fparser,fcn_str,'x',1)
       else
          res = Parse(fparser,fcn_str,'x')
       end if

       if (res < 0) exit

       write(*,'(A)') 'f(x) = '//trim(fcn_str)
       write(*,'(A)') repeat(' ',res+7)//'^'
       !
       ! Remember : ErrorMsg() is an array of characters...
       !
       write(*,*) ErrorMsg(fparser)
       write(*,*) 'Error type: ',GetParseErrorType(fparser)
       write(*,*)
    end do

    call Optimize(fparser)

    if (AddFunction(fp2,'phi',fparser) <= 0) then
       write(*,*) "AddFunction() error (fparser function)..."
       stop
    end if

    do
       call get("Degrees (0 = radians,1 = degrees):",use_degrees)
       call get("F(x,y) =",fcn_str)

       if (use_degrees > 0) then
          res = Parse(fp2,fcn_str,'x,y',1)
       else
          res = Parse(fp2,fcn_str,'x,y')
       end if

       if (res < 0) exit

       write(*,'(A)') 'F(x,y) = '//trim(fcn_str)
       write(*,'(A)') repeat(' ',res+9)//'^'

       ! Remember : ErrorMsg() is an array of characters...
       write(*,*) ErrorMsg(fp2)
       write(*,*) 'Error type: ',GetParseErrorType(fp2)
       write(*,*)
    end do

    call get("min x:",minx)
    call get("max x:",maxx)
    call get("step:",stp)

    vals(1) = minx
    do while(vals(1) <= maxx)
       write(*,*) 'f(',vals(1),') = ',Eval(fparser,vals)
       res = EvalError(fparser)
       if (res > 0) then
          write(*,*) 'Eval() error: ',res
          stop
       end if
       vals(1) = vals(1)+stp
    end do

    vals(1) = minx
    vals(2) = 1.0_WP
    do while(vals(1) <= maxx)
       write(*,*) 'F(',vals(1),',',vals(2),') = ',Eval(fp2,vals)
       res = EvalError(fp2)
       if (res > 0) then
          write(*,*) 'Eval() error: ',res
          stop
       end if
       vals(1) = vals(1)+stp
    end do

    call set_epsilon(1E-7_WP)
    write(*,*) 'current eps= ',get_epsilon()

    write(*,*) 'freeing memory...'
    call DeleteParser(fp2)
    call DeleteParser(fparser)

    write(*,*) 'NOW TESTING setDelimiterChar...'
    write(*,*)

    call NewParser(fparser)

    call setDelimiterChar(fparser,'}')

    res = Parse(fparser,'(y*y)+1 }','y')

    ! res = Parse(fparser,'(y*y)+1','y')
    ! vals(1) = 2.0_WP
    ! write(*,*) 'f(y) = ', Eval(fparser,vals)

    write(*,*) 'The } is at position ', res

    call DeleteParser(fparser)

    !
    ! Quick tests
    !
    call NewParser(fparser)

    res = AddConstant(fparser,'pi',3.1415926535897932_WP)
    res = ParseAndDeduceVariables(fparser,'x*sin(pi/2)')

    vals(1) = 2.71828_WP
    write(*,*) 'fun() = ',Eval(fparser,vals)

    call DeleteParser(fparser)

    call NewParser(fparser)

    res = AddConstant(fparser,'pi',3.1415926535897932_WP)
    res = ParseAndDeduceVariables(fparser,'x*sin(pi/2)',ammount)

    vals(1) = 2.71828_WP
    write(*,*) 'fun() = ',Eval(fparser,vals)
    write(*,*) 'ammount = ',ammount

    call DeleteParser(fparser)

    call NewParser(fparser)

    res = AddConstant(fparser,'pi',180.0_WP)
    res = ParseAndDeduceVariables(fparser,'x*sin(pi/2)',ammount,1)

    vals(1) = 2.71828_WP
    write(*,*) 'fun_deg() = ',Eval(fparser,vals)
    write(*,*) 'ammount = ',ammount

    call DeleteParser(fparser)
    call NewParser(fparser)

    res = AddConstant(fparser,'pi',180.0_WP)
    res = ParseAndDeduceVariables(fparser,'x*y*sin(pi/2)',ammount,1)

    vals(1) = 2.71828_WP
    vals(2) = 2.0_WP
    write(*,*) 'fun_deg() = ',Eval(fparser,vals)
    write(*,*) 'ammount = ',ammount

    call DeleteParser(fparser)
    call NewParser(fparser)

    res = AddConstant(fparser,'pi',180.0_WP)
    res = ParseAndDeduceVariables(fparser,'phy*chi*sin(pi/2)',ammount,1)

    vals(1) = 2.71828_WP
    vals(2) = 3.0_WP
    write(*,*) 'fun_deg() = ',Eval(fparser,vals)
    write(*,*) 'ammount = ',ammount

    call DeleteParser(fparser)
  end subroutine test_fx

  subroutine test_fz()
    use fparser_cd

    character(len=MAXLEN) :: fcn_str = 'z*z'
    type(FunctionParser_cd_type) fparser, fp2
    integer :: res, use_degrees = 0, ammount
    complex(WP) :: vals(2)
    complex(WP), parameter :: JJ = (0,1)
    real(WP) :: minx = -5.0_WP, maxx = 5.0_WP, stp = 1.0_WP, x, y = 1.0_WP

    call NewParser(fparser)
    call NewParser(fp2)

    if (AddConstant(fparser,'j',JJ) <= 0) then
       write(*,*) "AddConstant() error (j complex)..."
       stop
    end if

    if (AddConstant(fparser,'pi',(3.1415926535897932_WP,0.0_WP)) <= 0) then
       write(*,*) "AddConstant() error (pi complex)..."
       stop
    end if

    if (AddUnit(fparser,'cm',(100.0_WP,100.0_WP)) <= 0) then
       write(*,*) "AddUnit() error (complex)..."
       stop
    end if

    if (AddFunction(fparser,'sqr',sqr_z,1) <= 0) then
       write(*,*) "AddFunction() error (complex Fortran function)..."
       stop
    end if

    if (RemoveIdentifier(fparser,'pi') <= 0) then
       write(*,*) "RemoveIdentifier() error (fparser complex function)..."
       stop
    end if

    do
       call get("Degrees (0 = radians,1 = degrees):",use_degrees)
       call get("f(z) =",fcn_str)

       if (use_degrees > 0) then
          res = Parse(fparser,fcn_str,'z',1)
       else
          res = Parse(fparser,fcn_str,'z')
       end if

       if (res < 0) exit

       write(*,'(A)') 'f(z) = '//trim(fcn_str)
       write(*,'(A)') repeat(' ',res+7)//'^'
       !
       ! Remember : ErrorMsg() is an array of characters...
       !
       write(*,*) ErrorMsg(fparser)
       write(*,*) 'Error type: ',GetParseErrorType(fparser)
       write(*,*)
    end do

    call Optimize(fparser)

    if (AddFunction(fp2,'phi',fparser) <= 0) then
       write(*,*) "AddFunction() error (fparser complex function)..."
       stop
    end if

    do
       call get("Degrees (0 = radians,1 = degrees):",use_degrees)
       call get("F(z,w) =",fcn_str)

       if (use_degrees > 0) then
          res = Parse(fp2,fcn_str,'z,w',1)
       else
          res = Parse(fp2,fcn_str,'z,w')
       end if

       if (res < 0) exit

       write(*,'(A)') 'F(z,w) = '//trim(fcn_str)
       write(*,'(A)') repeat(' ',res+9)//'^'

       ! Remember : ErrorMsg() is an array of characters...
       write(*,*) ErrorMsg(fp2)
       write(*,*) 'Error type: ',GetParseErrorType(fp2)
       write(*,*)
    end do

    call get("min x:",minx)
    call get("max x:",maxx)
    call get("y:",y)
    call get("step:",stp)

    x = minx
    do while(x <= maxx)
       vals(1) = x+JJ*y
       write(*,*) 'f(',vals(1),') = ',Eval(fparser,vals)
       res = EvalError(fparser)
       if (res > 0) then
          write(*,*) 'Eval() error: ',res
          stop
       end if
       x = x+stp
    end do

    x = minx
    vals(2) = 1.0_WP+JJ*1.0_WP
    do while(x <= maxx)
       vals(1) = x+JJ*y
       write(*,*) 'F(',vals(1),',',vals(2),') = ',Eval(fp2,vals)
       res = EvalError(fp2)
       if (res > 0) then
          write(*,*) 'Eval() error: ',res
          stop
       end if
       x = x+stp
    end do

    call set_epsilon(1E-7_WP*(1.0_WP+JJ))
    write(*,*) 'current eps= ',get_epsilon()

    write(*,*) 'freeing memory...'
    call DeleteParser(fp2)
    call DeleteParser(fparser)

    write(*,*) 'NOW TESTING setDelimiterChar...'
    write(*,*)

    call NewParser(fparser)

    call setDelimiterChar(fparser,'}')

    res = Parse(fparser,'(y*y)+1 }','y')

    ! res = Parse(fparser,'(y*y)+1','y')
    ! vals(1) = 2.0_WP+JJ*0.0_WP
    ! write(*,*) 'f(y) = ', Eval(fparser,vals)

    write(*,*) 'The } is at position ', res

    call DeleteParser(fparser)

    !
    ! Quick tests
    !
    call NewParser(fparser)

    res = AddConstant(fparser,'pi',3.1415926535897932_WP+JJ*0)
    res = ParseAndDeduceVariables(fparser,'z*sin(pi/2)')

    vals(1) = 2.71828_WP+JJ*0
    write(*,*) 'fun() = ',Eval(fparser,vals)

    call DeleteParser(fparser)

    call NewParser(fparser)

    res = AddConstant(fparser,'pi',(3.1415926535897932_WP,0.0_WP))
    res = ParseAndDeduceVariables(fparser,'z*sin(pi/2)',ammount)

    vals(1) = 2.71828_WP
    write(*,*) 'fun() = ',Eval(fparser,vals)
    write(*,*) 'ammount = ',ammount

    call DeleteParser(fparser)

    call NewParser(fparser)

    res = AddConstant(fparser,'pi',(180.0_WP,0.0_WP))
    res = ParseAndDeduceVariables(fparser,'z*sin(pi/2)',ammount,1)

    vals(1) = 2.71828_WP
    write(*,*) 'fun_deg() = ',Eval(fparser,vals)
    write(*,*) 'ammount = ',ammount

    call DeleteParser(fparser)
    call NewParser(fparser)

    res = AddConstant(fparser,'pi',180.0_WP+JJ*0)
    res = ParseAndDeduceVariables(fparser,'x*y*sin(pi/2)',ammount,1)

    vals(1) = 2.71828_WP
    vals(2) = 2.0_WP
    write(*,*) 'fun_deg() = ',Eval(fparser,vals)
    write(*,*) 'ammount = ',ammount

    call DeleteParser(fparser)
    call NewParser(fparser)

    res = AddConstant(fparser,'pi',180.0_WP+JJ*0)
    res = ParseAndDeduceVariables(fparser,'phy*chi*sin(pi/2)',ammount,1)

    vals(1) = 2.71828_WP
    vals(2) = 3.0_WP
    write(*,*) 'fun_deg() = ',Eval(fparser,vals)
    write(*,*) 'ammount = ',ammount

    call DeleteParser(fparser)
  end subroutine test_fz
end program fparser_test
