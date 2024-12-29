!
! Author: Angelo Graziosi
!
!   created   : Sep 12, 2014
!   last edit : Jul 27, 2016
!
!   Fortran Interface to the Function Parser Library
!   The simplest interface (complex double precision) to the
!   Function Parser Library for C++ (http://warp.povusers.org/FunctionParser)
!
! References
!
!   http://fortranwiki.org/fortran/show/Fortran+and+Cpp+objects
!   http://fortranwiki.org/fortran/show/Generating+C+Interfaces
!
! See also:
!
!   http://fortranwiki.org/fortran/show/c_interface_module
!
! N.B.
!
!   Since these module is an interface to C/C++ routines, a few routines
!   handling characters (string) follow the "C rules".
!   So Parse() when used in associacition with setDelimiterChar() will
!   return position in C style, i.e. starting from 0 (zero)...
!

module fparser_cd
  use kind_consts, only: DP
  use, intrinsic :: iso_c_binding, only: C_INT, C_PTR, C_NULL_PTR, &
       C_CHAR, C_DOUBLE_COMPLEX, C_NULL_CHAR, C_SIZE_T, C_FUNPTR, &
       c_funloc!, c_f_pointer

  implicit none
  private

  type FunctionParser_cd_type
     private
     type(C_PTR) :: object = C_NULL_PTR
  end type FunctionParser_cd_type

  interface

     function C_FunctionParser_cd__new() result(this) &
          bind(C,name='FunctionParser_cd__new')
       import :: C_PTR
       type(C_PTR) :: this
     end function C_FunctionParser_cd__new

     function C_FunctionParser_cd__Parse(this,fcn,vars,useDegrees) &
          result(Parse) bind(C,name='FunctionParser_cd__Parse')
       import :: C_CHAR, C_INT, C_PTR
       integer(C_INT) :: Parse
       type(C_PTR), value :: this
       character(C_CHAR), intent(in) :: fcn(*), vars(*)
       integer(C_INT), value :: useDegrees
     end function C_FunctionParser_cd__Parse

     subroutine C_FunctionParser_cd__setDelimiterChar(this,c) &
          bind(C,name='FunctionParser_cd__setDelimiterChar')
       import :: C_CHAR, C_PTR
       type(C_PTR), value :: this
       character(C_CHAR), value :: c
     end subroutine C_FunctionParser_cd__setDelimiterChar

     function C_static_FunctionParser_cd__epsilon() result(epsilon) &
          bind(C,name='static_FunctionParser_cd__epsilon')
       import :: C_DOUBLE_COMPLEX
       complex(C_DOUBLE_COMPLEX) :: epsilon
     end function C_static_FunctionParser_cd__epsilon

     subroutine C_static_FunctionParser_cd__setEpsilon(e) &
          bind(C,name='static_FunctionParser_cd__setEpsilon')
       import :: C_DOUBLE_COMPLEX
       complex(C_DOUBLE_COMPLEX), value :: e
     end subroutine C_static_FunctionParser_cd__setEpsilon

     function C_FunctionParser_cd__ErrorMsg(this) result(ErrorMsg) &
          bind(C,name='FunctionParser_cd__ErrorMsg')
       import :: C_PTR
       type(C_PTR) :: ErrorMsg
       type(C_PTR), value :: this
     end function C_FunctionParser_cd__ErrorMsg

     function C_FunctionParser_cd__GetParseErrorType(this) &
          result(GetParseErrorType) &
          bind(C,name='FunctionParser_cd__GetParseErrorType')
       import :: C_INT, C_PTR
       integer(C_INT) :: GetParseErrorType
       type(C_PTR), value :: this
     end function C_FunctionParser_cd__GetParseErrorType

     function C_FunctionParser_cd__Eval(this,vars) result(Eval) &
          bind(C,name='FunctionParser_cd__Eval')
       import :: C_DOUBLE_COMPLEX, C_PTR
       complex(C_DOUBLE_COMPLEX) :: Eval
       type(C_PTR), value :: this
       complex(C_DOUBLE_COMPLEX), intent(in) :: vars(*)
     end function C_FunctionParser_cd__Eval

     function C_FunctionParser_cd__EvalError(this) &
          result(EvalError) &
          bind(C,name='FunctionParser_cd__EvalError')
       import :: C_INT, C_PTR
       integer(C_INT) :: EvalError
       type(C_PTR), value :: this
     end function C_FunctionParser_cd__EvalError

     subroutine C_FunctionParser_cd__Optimize(this) &
          bind(C,name='FunctionParser_cd__Optimize')
       import :: C_PTR
       type(C_PTR), value :: this
     end subroutine C_FunctionParser_cd__Optimize

     function C_FunctionParser_cd__AddConstant(this,name,val) &
          result(AddConstant) bind(C,name='FunctionParser_cd__AddConstant')
       import :: C_CHAR, C_DOUBLE_COMPLEX, C_INT, C_PTR
       integer(C_INT) :: AddConstant
       type(C_PTR), value :: this
       character(C_CHAR), intent(in) :: name(*)
       complex(C_DOUBLE_COMPLEX), value :: val
     end function C_FunctionParser_cd__AddConstant

     function C_FunctionParser_cd__AddUnit(this,name,val) result(AddUnit) &
          bind(C,name='FunctionParser_cd__AddUnit')
       import :: C_CHAR, C_DOUBLE_COMPLEX, C_INT, C_PTR
       integer(C_INT) :: AddUnit
       type(C_PTR), value :: this
       character(C_CHAR), intent(in) :: name(*)
       complex(C_DOUBLE_COMPLEX), value :: val
     end function C_FunctionParser_cd__AddUnit

     function C_FunctionParser_cd__AddFunction(this,name,functionPtr, &
          paramsAmount) result(AddFunction) &
          bind(C,name='FunctionParser_cd__AddFunction')
       import :: C_CHAR, C_FUNPTR, C_INT, C_PTR
       integer(C_INT) :: AddFunction
       type(C_PTR), value :: this
       character(C_CHAR), intent(in) :: name(*)
       type(C_FUNPTR), value :: functionPtr
       integer(C_INT), value :: paramsAmount
     end function C_FunctionParser_cd__AddFunction

     function C_FunctionParser_cd__AddFunction2(this,name,fp) &
          result(AddFunction2) &
          bind(C,name='FunctionParser_cd__AddFunction2')
       import :: C_CHAR, C_INT, C_PTR
       integer(C_INT) :: AddFunction2
       type(C_PTR), value :: this
       character(C_CHAR), intent(in) :: name(*)
       type(C_PTR), value :: fp
     end function C_FunctionParser_cd__AddFunction2

     function C_FunctionParser_cd__RemoveIdentifier(this,name) &
          result(RemoveIdentifier) &
          bind(C,name='FunctionParser_cd__RemoveIdentifier')
       import :: C_CHAR, C_INT, C_PTR
       integer(C_INT) :: RemoveIdentifier
       type(C_PTR), value :: this
       character(C_CHAR), intent(in) :: name(*)
     end function C_FunctionParser_cd__RemoveIdentifier

     function C_FunctionParser_cd__ParseAndDeduceVariables(this,fcn, &
          amountOfVariablesFound,useDegrees) &
          result(ParseAndDeduceVariables) &
          bind(C,name='FunctionParser_cd__ParseAndDeduceVariables')
       import :: C_CHAR, C_INT, C_PTR
       integer(C_INT) :: ParseAndDeduceVariables
       type(C_PTR), value :: this
       character(C_CHAR), intent(in) :: fcn(*)
       integer(C_INT), intent(out) :: amountOfVariablesFound
       integer(C_INT), value :: useDegrees
     end function C_FunctionParser_cd__ParseAndDeduceVariables

     subroutine C_FunctionParser_cd__delete(this) &
          bind(C,name='FunctionParser_cd__delete')
       import :: C_PTR
       type(C_PTR), value :: this
     end subroutine C_FunctionParser_cd__delete

  end interface

  interface NewParser
     module procedure FunctionParser_cd__new
  end interface NewParser

  interface Parse
     module procedure FunctionParser_cd__Parse
  end interface Parse

  interface setDelimiterChar
     module procedure FunctionParser_cd__setDelimiterChar
  end interface setDelimiterChar

  interface get_epsilon
     module procedure static_FunctionParser_cd__epsilon
  end interface get_epsilon

  interface set_epsilon
     module procedure static_FunctionParser_cd__setEpsilon
  end interface set_epsilon

  interface ErrorMsg
     module procedure FunctionParser_cd__ErrorMsg
  end interface ErrorMsg

  interface GetParseErrorType
     module procedure FunctionParser_cd__GetParseErrorType
  end interface GetParseErrorType

  interface Eval
     module procedure FunctionParser_cd__Eval
  end interface Eval

  interface EvalError
     module procedure FunctionParser_cd__EvalError
  end interface EvalError

  interface Optimize
     module procedure FunctionParser_cd__Optimize
  end interface Optimize

  interface AddConstant
     module procedure FunctionParser_cd__AddConstant
  end interface AddConstant

  interface AddUnit
     module procedure FunctionParser_cd__AddUnit
  end interface AddUnit

  interface AddFunction
     module procedure FunctionParser_cd__AddFunction, &
          FunctionParser_cd__AddFunction2
  end interface AddFunction

  interface RemoveIdentifier
     module procedure FunctionParser_cd__RemoveIdentifier
  end interface RemoveIdentifier

  interface ParseAndDeduceVariables
     module procedure FunctionParser_cd__ParseAndDeduceVariables
  end interface ParseAndDeduceVariables

  interface DeleteParser
     module procedure FunctionParser_cd__delete
  end interface DeleteParser

  public:: AddConstant, AddFunction, AddUnit, DeleteParser, ErrorMsg, Eval, &
       EvalError, FunctionParser_cd_type, get_epsilon, GetParseErrorType, &
       NewParser, Optimize, Parse, ParseAndDeduceVariables, &
       RemoveIdentifier, setDelimiterChar, set_epsilon

contains

  ! Fortran wrapper routines to interface C wrappers
  subroutine FunctionParser_cd__new(this)
    type(FunctionParser_cd_type), intent(out) :: this
    this%object = C_FunctionParser_cd__new()
  end subroutine FunctionParser_cd__new

  function FunctionParser_cd__Parse(this,fcn,vars,useDegrees) result(Parse)
    type(FunctionParser_cd_type), intent(in) :: this
    character(len=*), intent(in) :: fcn, vars
    integer, intent(in), optional :: useDegrees
    integer :: Parse
    integer :: degrees = 0

    if (present(useDegrees)) degrees = useDegrees

    Parse = C_FunctionParser_cd__Parse(this%object,trim(fcn)//C_NULL_CHAR, &
         trim(vars)//C_NULL_CHAR,degrees)
  end function FunctionParser_cd__Parse

  subroutine FunctionParser_cd__setDelimiterChar(this,c)
    type(FunctionParser_cd_type), intent(in) :: this
    character(len=1), intent(in) :: c
    call C_FunctionParser_cd__setDelimiterChar(this%object,c)
  end subroutine FunctionParser_cd__setDelimiterChar

  function static_FunctionParser_cd__epsilon() result(epsilon)
    complex(DP) :: epsilon
    epsilon = C_static_FunctionParser_cd__epsilon()
  end function static_FunctionParser_cd__epsilon

  subroutine static_FunctionParser_cd__setEpsilon(e)
    complex(DP), intent(in) :: e
    call C_static_FunctionParser_cd__setEpsilon(e)
  end subroutine static_FunctionParser_cd__setEpsilon

  ! function FunctionParser_cd__ErrorMsg(this) result(ErrorMsg)
  !   use utilities, only: str_len
  !   type(FunctionParser_cd_type), intent(in) :: this
  !   type(C_PTR) :: cstr
  !   integer :: len
  !   character, pointer :: str
  !   character(len=70) :: ErrorMsg
  !   cstr = C_FunctionParser_cd__ErrorMsg(this%object)
  !   len = str_len(cstr)
  !   call c_f_pointer(cstr,str)
  !   ErrorMsg = trim(adjustl(str(1:len)))
  ! end function FunctionParser_cd__ErrorMsg

  function FunctionParser_cd__ErrorMsg(this) result(ErrorMsg)
    use utilities, only: c_f_string
    type(FunctionParser_cd_type), intent(in) :: this
    character, dimension(:), pointer :: ErrorMsg
    ErrorMsg => c_f_string(C_FunctionParser_cd__ErrorMsg(this%object))
  end function FunctionParser_cd__ErrorMsg

  function FunctionParser_cd__GetParseErrorType(this) &
       result(GetParseErrorType)
    type(FunctionParser_cd_type), intent(in) :: this
    integer :: GetParseErrorType
    GetParseErrorType = C_FunctionParser_cd__GetParseErrorType(this%object)
  end function FunctionParser_cd__GetParseErrorType

  function FunctionParser_cd__Eval(this,vars) result(Eval)
    type(FunctionParser_cd_type), intent(in) :: this
    complex(DP), intent(in) :: vars(:)
    complex(DP) :: Eval
    Eval = C_FunctionParser_cd__Eval(this%object,vars)
  end function FunctionParser_cd__Eval

  function FunctionParser_cd__EvalError(this) result(EvalError)
    type(FunctionParser_cd_type), intent(in) :: this
    integer :: EvalError
    EvalError = C_FunctionParser_cd__EvalError(this%object)
  end function FunctionParser_cd__EvalError

  subroutine FunctionParser_cd__Optimize(this)
    type(FunctionParser_cd_type), intent(in) :: this
    call C_FunctionParser_cd__Optimize(this%object)
  end subroutine FunctionParser_cd__Optimize

  function FunctionParser_cd__AddConstant(this,name,val) result(AddConstant)
    type(FunctionParser_cd_type), intent(in) :: this
    character(len=*), intent(in) :: name
    complex(DP), intent(in) :: val
    integer :: AddConstant
    AddConstant = C_FunctionParser_cd__AddConstant(this%object, &
         trim(name)//C_NULL_CHAR,val)
  end function FunctionParser_cd__AddConstant

  function FunctionParser_cd__AddUnit(this,name,val) result(AddUnit)
    type(FunctionParser_cd_type), intent(in) :: this
    character(len=*), intent(in) :: name
    complex(DP), intent(in) :: val
    integer :: AddUnit
    AddUnit = C_FunctionParser_cd__AddUnit(this%object, &
         trim(name)//C_NULL_CHAR,val)
  end function FunctionParser_cd__AddUnit

  function FunctionParser_cd__AddFunction(this,name,fcn,paramsAmount) &
       result(AddFunction)
    type(FunctionParser_cd_type), intent(in) :: this
    character(len=*), intent(in) :: name

    interface
       function fcn(z) bind(C)
         use iso_c_binding, only: C_DOUBLE_COMPLEX
         complex(C_DOUBLE_COMPLEX), intent(in) :: z(*)
         complex(C_DOUBLE_COMPLEX) :: fcn
       end function fcn
    end interface

    integer, intent(in) :: paramsAmount
    integer :: AddFunction

    AddFunction = C_FunctionParser_cd__AddFunction(this%object, &
         trim(name)//C_NULL_CHAR,c_funloc(fcn),paramsAmount)
  end function FunctionParser_cd__AddFunction

  function FunctionParser_cd__AddFunction2(this,name,fp) &
       result(AddFunction2)
    type(FunctionParser_cd_type), intent(in) :: this
    character(len=*), intent(in) :: name
    type(FunctionParser_cd_type), intent(in) :: fp
    integer :: AddFunction2

    AddFunction2 = C_FunctionParser_cd__AddFunction2(this%object, &
         trim(name)//C_NULL_CHAR,fp%object)
  end function FunctionParser_cd__AddFunction2

  function FunctionParser_cd__RemoveIdentifier(this,name) &
       result(RemoveIdentifier)
    type(FunctionParser_cd_type), intent(in) :: this
    character(len=*), intent(in) :: name
    integer :: RemoveIdentifier

    RemoveIdentifier = C_FunctionParser_cd__RemoveIdentifier(this%object, &
         trim(name)//C_NULL_CHAR)
  end function FunctionParser_cd__RemoveIdentifier

  function FunctionParser_cd__ParseAndDeduceVariables(this,fcn, &
       amountOfVariablesFound,useDegrees) result(ParseAndDeduceVariables)
    type(FunctionParser_cd_type), intent(in) :: this
    character(len=*), intent(in) :: fcn
    integer, intent(out), optional :: amountOfVariablesFound
    integer, intent(in), optional :: useDegrees
    integer :: ParseAndDeduceVariables
    integer :: ammount = -1, degrees = 0

    if (present(amountOfVariablesFound)) ammount = amountOfVariablesFound
    if (present(useDegrees)) degrees = useDegrees

    ParseAndDeduceVariables = &
         C_FunctionParser_cd__ParseAndDeduceVariables(this%object, &
         trim(fcn)//C_NULL_CHAR,ammount,degrees)

    if (present(amountOfVariablesFound)) amountOfVariablesFound = ammount
  end function FunctionParser_cd__ParseAndDeduceVariables

  subroutine FunctionParser_cd__delete(this)
    type(FunctionParser_cd_type), intent(inout) :: this
    call C_FunctionParser_cd__delete(this%object)
    this%object = C_NULL_PTR
  end subroutine FunctionParser_cd__delete
end module fparser_cd
