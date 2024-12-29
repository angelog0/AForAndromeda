!
! Author: Angelo Graziosi
!
!   created   : Feb 10, 2014
!   last edit : Aug 10, 2016
!
!   (Partial) Fortran Interface to the Windows API Library
!   The main module
!
!
! DESCRIPTION
!
!   This is the 'win32' module.
!   Just to start with Windows Fortran Applications...
!
! NOTE
!   For Microsoft, the type "long" is always (in Windows 32 and 64 systems)
!   a 32 bit integer.
!   For GNU/Linux it is a 32/64 bit integer for systems 32/64 rispectively.
!   So we have to adopt: C_LONG --> C_INT, being C_INT a 32 bit integer
!   in any case.
!
!   See the thread: http://gcc.gnu.org/ml/fortran/2013-07/msg00087.html
!   See also: http://cygwin.com/cygwin-ug-net/programming.html#gcc-64
!
! BTW
!   The usage of iany() (Fortran 2008) need of GCC >= 4.7
!
!   Notice that:
!
!       int(0,UINT_T)    -->  0_UINT_T
!       int(0,WPARAM_T)  -->  0_WPARAM_T
!       int(0,LPARAM_T)  -->  0_LPARAM_T
!       ...

module win32
  use, intrinsic :: iso_c_binding, only: C_CHAR, c_f_pointer, C_FUNPTR, &
       C_INT, C_INT8_T, C_INTPTR_T, C_LONG, C_NEW_LINE, C_NULL_CHAR, &
       C_NULL_PTR, C_PTR, C_SHORT

  implicit none
  private

  ! Common useful variables
  integer, public :: dummy = 0   ! dummy = WindowsFunctionCalled()

  ! Common useful constants
  integer, parameter, public :: MAX_LEN = 256

  ! Default format descriptors for output in these apps..
  character(len=*), parameter, public :: I_FMT = '(I0)'
  character(len=*), parameter, public :: R_FMT = '(1PG12.5)'

  ! =================
  !   WIN32 ALIASES
  ! =================

  ! Using directly C_INT to define DWORD_T and LONG_T maybe misleading,
  ! so we adopt the Tobias tips
  ! (http://gcc.gnu.org/ml/fortran/2013-07/msg00090.html).
  !
  integer, parameter :: C_MS_LONG = C_INT

  integer, parameter, public :: BYTE_T = C_INT8_T
  integer, parameter, public :: DWORD_T = C_MS_LONG
  integer, parameter, public :: HANDLE_T = C_INTPTR_T
  integer, parameter, public :: INT_T = C_INT
  integer, parameter, public :: INT_PTR_T = C_INTPTR_T
  integer, parameter, public :: LONG_T = C_MS_LONG
  integer, parameter, public :: LONG_PTR_T = C_INTPTR_T
  integer, parameter, public :: SHORT_T = C_SHORT
  integer, parameter, public :: UINT_PTR_T = C_INTPTR_T
  integer, parameter, public :: WORD_T = C_SHORT

  integer, parameter, public :: ATOM_T = WORD_T
  integer, parameter, public :: BOOL_T = INT_T
  integer, parameter, public :: COLORREF_T = DWORD_T
  integer, parameter, public :: HBITMAP_T = HANDLE_T
  integer, parameter, public :: HBRUSH_T = HANDLE_T
  integer, parameter, public :: HCURSOR_T = HANDLE_T
  integer, parameter, public :: HDC_T = HANDLE_T
  integer, parameter, public :: HGDIOBJ_T = HANDLE_T
  integer, parameter, public :: HICON_T = HANDLE_T
  integer, parameter, public :: HINSTANCE_T = HANDLE_T
  integer, parameter, public :: HMENU_T = HANDLE_T
  integer, parameter, public :: HMODULE_T = HINSTANCE_T
  integer, parameter, public :: HMONITOR_T = HANDLE_T
  integer, parameter, public :: HPEN_T = HANDLE_T
  integer, parameter, public :: HWND_T = HANDLE_T
  integer, parameter, public :: LPARAM_T = LONG_PTR_T
  integer, parameter, public :: LRESULT_T = LONG_PTR_T
  integer, parameter, public :: UINT_T = INT_T
  integer, parameter, public :: WPARAM_T = UINT_PTR_T

  ! ===============
  !   WIN32 TYPES
  ! ===============

  type, public, bind(C) :: WNDCLASSEX_T
     integer(UINT_T) :: cbSize
     integer(UINT_T) :: style
     type(C_FUNPTR) :: lpfnWndProc ! WNDPROC
     integer(INT_T) :: cbClsExtra
     integer(INT_T) :: cbWndExtra
     integer(HINSTANCE_T) :: hInstance
     integer(HICON_T) :: hIcon
     integer(HCURSOR_T) :: hCursor
     integer(HBRUSH_T) :: hbrBackground
     type(C_PTR) :: lpszMenuName  ! LPCTSTR
     type(C_PTR) :: lpszClassName ! LPCTSTR
     integer(HICON_T) :: hIconSm
  end type WNDCLASSEX_T

  type, public, bind(C) :: POINT_T
     integer(LONG_T) :: x
     integer(LONG_T) :: y
  end type POINT_T

  type, public, bind(C) :: MSG_T
     integer(HWND_T) :: hWnd
     integer(UINT_T) :: message
     integer(WPARAM_T) :: wParam
     integer(LPARAM_T) :: lParam
     integer(DWORD_T) :: time
     type(POINT_T) :: pt
  end type MSG_T

  type, public, bind(C) :: RECT_T
     integer(LONG_T) :: left
     integer(LONG_T) :: top
     integer(LONG_T) :: right
     integer(LONG_T) :: bottom
  end type RECT_T

  type, public, bind(C) :: PAINTSTRUCT_T
     integer(HDC_T) :: hdc
     integer(BOOL_T) :: fErase
     type(RECT_T) :: rcPaint
     integer(BOOL_T) :: fRestore
     integer(BOOL_T) :: fIncUpdate
     integer(BYTE_T) :: rgbReserved(32)
  end type PAINTSTRUCT_T

  type, public, bind(C) :: MONITORINFO_T
     integer(DWORD_T) :: cbSize
     type(RECT_T) :: rcMonitor
     type(RECT_T) :: rcWork
     integer(DWORD_T) :: dwFlags
  end type MONITORINFO_T

  ! =================================
  !   WIN32 CONSTANTS AND VARIABLES
  ! =================================

  ! An alternative to the function null_p()
  character(C_CHAR), pointer, public :: NULL_LPSTR(:) => null()

  type(RECT_T), pointer, public :: NULL_RECT_T => null()

  integer(HANDLE_T), parameter, public :: NULL_T = 0
  integer(BOOL_T), parameter, public :: FALSE_T = 0
  integer(BOOL_T), parameter, public :: TRUE_T = 1

  type(C_PTR), parameter, public :: NULL_PTR_T = C_NULL_PTR

  ! C string constants alaises using the ASCII name.
  character(C_CHAR), parameter, public :: NUL = C_NULL_CHAR
  character(C_CHAR), parameter, public :: NL = C_NEW_LINE

  ! COLORREF (Z'00BBGGRR') constants
  integer(COLORREF_T), parameter, public :: BLACK_COLOR = 0        ! Z'00000000'
  integer(COLORREF_T), parameter, public :: CYAN_COLOR = 16776960  ! Z'00FFFF00'
  integer(COLORREF_T), parameter, public :: GREEN_COLOR = 65280    ! Z'0000FF00'
  integer(COLORREF_T), parameter, public :: RED_COLOR = 255        ! Z'000000FF'
  integer(COLORREF_T), parameter, public :: YELLOW_COLOR = 65535   ! Z'0000FFFF'
  integer(COLORREF_T), parameter, public :: WHITE_COLOR = 16777215 ! Z'00FFFFFF'

  ! Device-specific information index (/usr/include/w32api/wingdi.h)
  integer(INT_T), parameter, public :: ASPECTX = 40
  integer(INT_T), parameter, public :: ASPECTY = 42
  integer(INT_T), parameter, public :: ASPECTXY = 44

  ! Window default position and/or dimension.
  ! The C/C++ definition is ((int)0x80000000), i.e. (int)2147483648.
  ! Given the range of int (4 bytes) is [-2147483648,2147483647], 2147483648
  ! means -2147483648. The right way to obtain this is as follows:
  ! One cannot use CW_USEDEFAULT = -2147483648, because it would use an
  ! unary minus operator on the integer constant (+)2147483648, which
  ! does not exist! (the maximum is 2147483647!)
  ! See also this explanation
  ! http://gcc.gnu.org/ml/fortran/2013-12/msg00083.html,
  ! and the relative thread, for a similar question.
  !
  integer(INT_T), parameter, public :: CW_USEDEFAULT = -2147483647-1

  ! Class styles (/usr/include/w32api/winuser.h)
  integer(UINT_T), parameter, public :: CS_VREDRAW = 1     ! Z'00000001'
  integer(UINT_T), parameter, public :: CS_HREDRAW = 2     ! Z'00000002'
  integer(UINT_T), parameter, public :: CS_SAVEBITS = 2048 ! Z'00000800'

  ! DrawText formats (/usr/include/w32api/winuser.h)
  integer(UINT_T), parameter, public :: DT_CENTER = 1
  integer(UINT_T), parameter, public :: DT_VCENTER = 4
  integer(UINT_T), parameter, public :: DT_SINGLELINE = 32

  ! Hatch style of the brush (/usr/include/w32api/wingdi.h)
  integer(INT_T), parameter, public :: HS_DIAGCROSS = 5

  ! IDC_* definitions for make_int_resource() (/usr/include/w32api/winuser.h)
  integer(WORD_T), parameter, public :: IDC_ARROW = 32512
  integer(WORD_T), parameter, public :: IDC_CROSS = 32515
  integer(WORD_T), parameter, public :: IDC_HAND = 32649
  integer(WORD_T), parameter, public :: IDC_WAIT = 32514

  ! IDI_* definitions for make_int_resource() (/usr/include/w32api/winuser.h)
  integer(WORD_T), parameter, public :: IDI_APPLICATION = 32512
  integer(WORD_T), parameter, public :: IDI_ASTERISK = 32516
  integer(WORD_T), parameter, public :: IDI_ERROR = 32513
  integer(WORD_T), parameter, public :: IDI_EXCLAMATION = 32515
  integer(WORD_T), parameter, public :: IDI_HAND = 32513
  integer(WORD_T), parameter, public :: IDI_INFORMATION = 32516
  integer(WORD_T), parameter, public :: IDI_QUESTION = 32514
  integer(WORD_T), parameter, public :: IDI_WARNING = 32515
  integer(WORD_T), parameter, public :: IDI_WINLOGO = 32517

  ! MessageBox() buttons and return values (/usr/include/w32api/winuser.h)
  integer(UINT_T), parameter, public :: MB_ICONASTERISK = 64    ! Z'00000040'
  integer(UINT_T), parameter, public :: MB_ICONHAND = 16        ! Z'00000010'
  integer(UINT_T), parameter, public :: MB_ICONERROR = MB_ICONHAND
  integer(UINT_T), parameter, public :: MB_ICONEXCLAMATION = 48 ! Z'00000030'
  integer(UINT_T), parameter, public :: MB_ICONINFORMATION = MB_ICONASTERISK
  integer(UINT_T), parameter, public :: MB_ICONQUESTION = 32    ! Z'00000020'
  integer(UINT_T), parameter, public :: MB_OK = 0               ! Z'00000000'
  integer(UINT_T), parameter, public :: MB_YESNO = 4            ! Z'00000004'
  integer(UINT_T), parameter, public :: MB_YESNOCANCEL = 3      ! Z'00000003'
  !
  integer(INT_T), parameter, public :: IDCANCEL = 2
  integer(INT_T), parameter, public :: IDOK = 1
  integer(INT_T), parameter, public :: IDYES = 6

  ! EnableMenuItem() constants (/usr/include/w32api/winuser.h)
  integer(UINT_T), parameter, public :: MF_BYCOMMAND = 0     ! Z'00000000'
  integer(UINT_T), parameter, public :: MF_BYPOSITION = 1024 ! Z'00000400'
  integer(UINT_T), parameter, public :: MF_DISABLED = 2      ! Z'00000002'
  integer(UINT_T), parameter, public :: MF_ENABLED = 0       ! Z'00000000'
  integer(UINT_T), parameter, public :: MF_GRAYED = 1        ! Z'00000001'

  ! Specifies how messages are to be handled (/usr/include/w32api/winuser.h)
  integer(UINT_T), parameter, public :: PM_NOREMOVE = 0  ! Z'00000000'
  integer(UINT_T), parameter, public :: PM_REMOVE = 1    ! Z'00000001'
  integer(UINT_T), parameter, public :: PM_NOYIELD = 2   ! Z'00000002'

  ! Pen styles (/usr/include/w32api/wingdi.h)
  integer(INT_T), parameter, public :: PS_SOLID = 0
  integer(INT_T), parameter, public :: PS_DASH = 1
  integer(INT_T), parameter, public :: PS_DOT = 2
  integer(INT_T), parameter, public :: PS_DASHDOT = 3
  integer(INT_T), parameter, public :: PS_DASHDOTDOT = 4
  integer(INT_T), parameter, public :: PS_NULL = 5
  integer(INT_T), parameter, public :: PS_INSIDEFRAME = 6
  integer(INT_T), parameter, public :: PS_USERSTYLE = 7
  integer(INT_T), parameter, public :: PS_ALTERNATE = 8

  ! Background modes (/usr/include/w32api/wingdi.h)
  integer(INT_T), parameter, public :: OPAQUE = 1
  integer(INT_T), parameter, public :: TRANSPARENT = 1

  ! Foreground mix modes (/usr/include/w32api/wingdi.h)
  integer(INT_T), parameter, public :: R2_BLACK = 1
  integer(INT_T), parameter, public :: R2_NOTMERGEPEN = 2
  integer(INT_T), parameter, public :: R2_MASKNOTPEN = 3
  integer(INT_T), parameter, public :: R2_NOTCOPYPEN = 4
  integer(INT_T), parameter, public :: R2_MASKPENNOT = 5
  integer(INT_T), parameter, public :: R2_NOT = 6
  integer(INT_T), parameter, public :: R2_XORPEN = 7
  integer(INT_T), parameter, public :: R2_NOTMASKPEN = 8
  integer(INT_T), parameter, public :: R2_MASKPEN = 9
  integer(INT_T), parameter, public :: R2_NOTXORPEN = 10
  integer(INT_T), parameter, public :: R2_NOP = 11
  integer(INT_T), parameter, public :: R2_MERGENOTPEN = 12
  integer(INT_T), parameter, public :: R2_COPYPEN = 13
  integer(INT_T), parameter, public :: R2_MERGEPENNOT=  14
  integer(INT_T), parameter, public :: R2_MERGEPEN = 15
  integer(INT_T), parameter, public :: R2_WHITE = 16
  integer(INT_T), parameter, public :: R2_LAST = 16

  ! Raster-operation codes (/usr/include/w32api/wingdi.h)
  integer(DWORD_T), parameter, public :: BLACKNESS = 66       ! Z'00000042'
  integer(DWORD_T), parameter, public :: SRCCOPY = 13369376   ! Z'00CC0020'
  integer(DWORD_T), parameter, public :: WHITENESS = 16711778 ! Z'00FF0062'

  ! Flags for playing the sound (/usr/include/w32api/mmsystem.h)
  integer(DWORD_T), parameter, public :: SND_ALIAS = 65536 ! Z'00010000'

  ! Show window constants (/usr/include/w32api/winuser.h)
  integer(INT_T), parameter, public :: SW_SHOWDEFAULT = 10
  integer(INT_T), parameter, public :: SW_SHOW = 5

  ! Text alignments (/usr/include/w32api/wingdi.h)
  integer(INT_T), parameter, public :: TA_NOUPDATECP = 0
  integer(INT_T), parameter, public :: TA_UPDATECP = 1
  integer(INT_T), parameter, public :: TA_LEFT = 0
  integer(INT_T), parameter, public :: TA_RIGHT = 2
  integer(INT_T), parameter, public :: TA_CENTER = 6
  integer(INT_T), parameter, public :: TA_TOP = 0
  integer(INT_T), parameter, public :: TA_BOTTOM = 8
  integer(INT_T), parameter, public :: TA_BASELINE = 24
  integer(INT_T), parameter, public :: TA_RTLREADING = 256
  integer(INT_T), parameter, public :: TA_MASK = &
       (TA_BASELINE+TA_CENTER+TA_UPDATECP+TA_RTLREADING)
  integer(INT_T), parameter, public :: VTA_BASELINE = TA_BASELINE
  integer(INT_T), parameter, public :: VTA_LEFT = TA_BOTTOM
  integer(INT_T), parameter, public :: VTA_RIGHT = TA_TOP
  integer(INT_T), parameter, public :: VTA_CENTER = TA_CENTER
  integer(INT_T), parameter, public :: VTA_BOTTOM = TA_RIGHT
  integer(INT_T), parameter, public :: VTA_TOP = TA_LEFT

  ! Stock objects brushes (/usr/include/w32api/wingdi.h)
  integer(INT_T), parameter, public :: BLACK_BRUSH = 4
  integer(INT_T), parameter, public :: DC_BRUSH = 18
  integer(INT_T), parameter, public :: DKGRAY_BRUSH = 3
  integer(INT_T), parameter, public :: GRAY_BRUSH = 2
  integer(INT_T), parameter, public :: HOLLOW_BRUSH = 5
  integer(INT_T), parameter, public :: LTGRAY_BRUSH = 1
  integer(INT_T), parameter, public :: NULL_BRUSH = 5
  integer(INT_T), parameter, public :: OBJ_BRUSH = 2
  integer(INT_T), parameter, public :: WHITE_BRUSH = 0

  ! Virtual key codes (/usr/include/w32api/winuser.h)
  integer(INT_T), parameter, public :: VK_ESCAPE = 27 ! Z'0000001B'

  ! Windows messages (/usr/include/w32api/winuser.h)
  integer(UINT_T), parameter, public :: WM_LBUTTONDOWN = 513 ! Z'00000201'
  integer(UINT_T), parameter, public :: WM_CHAR = 258        ! Z'00000102'
  integer(UINT_T), parameter, public :: WM_CLOSE = 16        ! Z'00000010'
  integer(UINT_T), parameter, public :: WM_COMMAND = 273     ! Z'00000111'
  integer(UINT_T), parameter, public :: WM_CREATE = 1        ! Z'00000001'
  integer(UINT_T), parameter, public :: WM_DESTROY = 2       ! Z'00000002'
  integer(UINT_T), parameter, public :: WM_ERASEBKGND = 20   ! Z'00000014'
  integer(UINT_T), parameter, public :: WM_INITDIALOG = 272  ! Z'00000110'
  integer(UINT_T), parameter, public :: WM_PAINT = 15        ! Z'0000000F'
  integer(UINT_T), parameter, public :: WM_PRINTCLIENT = 792 ! Z'00000318'
  integer(UINT_T), parameter, public :: WM_QUIT = 18         ! Z'00000012'
  integer(UINT_T), parameter, public :: WM_SIZE = 5          ! Z'00000005'
  integer(UINT_T), parameter, public :: WM_TIMER = 275       ! Z'00000113'

  ! Windows styles (/usr/include/w32api/winuser.h)
  integer(DWORD_T), parameter, public :: WS_CAPTION = 12582912   ! Z'00C00000'
  integer(DWORD_T), parameter, public :: &
       WS_CLIPCHILDREN = 33554432 ! Z'02000000'
  integer(DWORD_T), parameter, public :: &
       WS_CLIPSIBLINGS = 67108864 ! Z'04000000'
  integer(DWORD_T), parameter, public :: WS_MAXIMIZEBOX = 65536  ! Z'00010000'
  integer(DWORD_T), parameter, public :: WS_MINIMIZEBOX = 131072 ! Z'00020000'
  integer(DWORD_T), parameter, public :: WS_SYSMENU = 524288     ! Z'00080000'
  integer(DWORD_T), parameter, public :: WS_THICKFRAME = 262144  ! Z'00040000'
  integer(DWORD_T), parameter, public :: WS_OVERLAPPED = 0       ! Z'00000000'
  integer(DWORD_T), parameter, public :: WS_OVERLAPPEDWINDOW = &
       iany([ WS_OVERLAPPED, WS_CAPTION, WS_SYSMENU, WS_THICKFRAME, &
       WS_MINIMIZEBOX, WS_MAXIMIZEBOX ]) ! 13565952

  ! Windows styles extended (/usr/include/w32api/winuser.h)
  integer(DWORD_T), parameter, public :: WS_EX_CLIENTEDGE = 512  ! Z'00000200'

  ! ===================
  !   WIN32 INTERFACE
  ! ===================

  interface

     function BeginPaint(hWnd,lpPaint) bind(C, name='BeginPaint')
       import :: HDC_T, HWND_T, PAINTSTRUCT_T
       !GCC$ ATTRIBUTES STDCALL :: BeginPaint
       integer(HDC_T) :: BeginPaint
       integer(HWND_T), value :: hWnd
       type(PAINTSTRUCT_T), intent(out) :: lpPaint
     end function BeginPaint

     function BitBlt(hdcDest,nXDest,nYDest,nWidth,nHeight,hdcSrc, &
          nXSrc,nYSrc,dwRop) bind(C, name='BitBlt')
       import :: BOOL_T, DWORD_T, HDC_T, INT_T
       !GCC$ ATTRIBUTES STDCALL :: BitBlt
       integer(BOOL_T) :: BitBlt
       integer(HDC_T), value :: hdcDest
       integer(INT_T), value :: nXDest
       integer(INT_T), value :: nYDest
       integer(INT_T), value :: nWidth
       integer(INT_T), value :: nHeight
       integer(HDC_T), value :: hdcSrc
       integer(INT_T), value :: nXSrc
       integer(INT_T), value :: nYSrc
       integer(DWORD_T), value :: dwRop
     end function BitBlt

     function CheckRadioButton(hDlg,nIDFirstButton,nIDLastButton, &
          nIDCheckButton) bind(C, name='CheckRadioButton')
       import :: BOOL_T, HWND_T, INT_T
       !GCC$ ATTRIBUTES STDCALL :: CheckRadioButton
       integer(BOOL_T) :: CheckRadioButton
       integer(HWND_T), value :: hDlg
       integer(INT_T), value :: nIDFirstButton
       integer(INT_T), value :: nIDLastButton
       integer(INT_T), value :: nIDCheckButton
     end function CheckRadioButton

     function CreateCompatibleBitmap(hdc,nWidth,nHeight) &
          bind(C, name='CreateCompatibleBitmap')
       import :: HBITMAP_T, HDC_T, INT_T
       !GCC$ ATTRIBUTES STDCALL :: CreateCompatibleBitmap
       integer(HBITMAP_T) :: CreateCompatibleBitmap
       integer(HDC_T), value :: hdc
       integer(INT_T), value :: nWidth
       integer(INT_T), value :: nHeight
     end function CreateCompatibleBitmap

     function CreateCompatibleDC(hdc) bind(C, name='CreateCompatibleDC')
       import :: HDC_T
       !GCC$ ATTRIBUTES STDCALL :: CreateCompatibleDC
       integer(HDC_T) :: CreateCompatibleDC
       integer(HDC_T), value :: hdc
     end function CreateCompatibleDC

     function CreatePen(fnPenStyle,nWidth,crColor) bind(C, name='CreatePen')
       import :: COLORREF_T, HPEN_T, INT_T
       !GCC$ ATTRIBUTES STDCALL :: CreatePen
       integer(HPEN_T) :: CreatePen
       integer(INT_T), value :: fnPenStyle
       integer(INT_T), value :: nWidth
       integer(COLORREF_T), value :: crColor
     end function CreatePen

     function CreateSolidBrush(crColor) bind(C, name='CreateSolidBrush')
       import :: COLORREF_T, HBRUSH_T
       !GCC$ ATTRIBUTES STDCALL :: CreateSolidBrush
       integer(HBRUSH_T) :: CreateSolidBrush
       integer(COLORREF_T), value :: crColor
     end function CreateSolidBrush

     function CreateHatchBrush(fnStyle,clrref) &
          bind(C, name='CreateHatchBrush')
       import :: COLORREF_T, HBRUSH_T, INT_T
       !GCC$ ATTRIBUTES STDCALL :: CreateHatchBrush
       integer(HBRUSH_T) :: CreateHatchBrush
       integer(INT_T), value :: fnStyle
       integer(COLORREF_T), value :: clrref
     end function CreateHatchBrush

     function CreateWindowEx(dwExStyle,lpClassName,lpWindowName,dwStyle, &
          x,y,nWidth,nHeight, &
          hWndParent,hMenu,hInstance,lpParam) bind(C, name='CreateWindowExA')
       import :: C_CHAR, C_PTR, DWORD_T, HINSTANCE_T, HMENU_T, HWND_T, INT_T
       !GCC$ ATTRIBUTES STDCALL :: CreateWindowEx
       integer(HWND_T) :: CreateWindowEx
       integer(DWORD_T), value :: dwExStyle
       character(C_CHAR), intent(in) :: lpClassName(*)  ! LPCTSTR
       character(C_CHAR), intent(in) :: lpWindowName(*) ! LPCTSTR
       integer(DWORD_T), value :: dwStyle
       integer(INT_T), value :: x
       integer(INT_T), value :: y
       integer(INT_T), value :: nWidth
       integer(INT_T), value :: nHeight
       integer(HWND_T), value :: hWndParent
       integer(HMENU_T), value :: hMenu
       integer(HINSTANCE_T), value :: hInstance
       type(C_PTR), value :: lpParam
     end function CreateWindowEx

     function DeleteDC(hdc) bind(C, name='DeleteDC')
       import :: BOOL_T, HDC_T
       !GCC$ ATTRIBUTES STDCALL :: DeleteDC
       integer(BOOL_T) :: DeleteDC
       integer(HDC_T), value :: hdc
     end function DeleteDC

     function DeleteObject(hObject) bind(C, name='DeleteObject')
       import :: BOOL_T, HGDIOBJ_T
       !GCC$ ATTRIBUTES STDCALL :: DeleteObject
       integer(BOOL_T) :: DeleteObject
       integer(HGDIOBJ_T), value :: hObject
     end function DeleteObject

     function DestroyWindow(hWnd) bind(C, name='DestroyWindow')
       import :: BOOL_T, HWND_T
       !GCC$ ATTRIBUTES STDCALL :: DestroyWindow
       integer(BOOL_T) :: DestroyWindow
       integer(HWND_T), value :: hWnd
     end function DestroyWindow

     function DefWindowProc(hWnd,Msg,wParam,lParam) &
          bind(C, name='DefWindowProcA')
       import :: HWND_T, LPARAM_T, LRESULT_T, UINT_T, WPARAM_T
       !GCC$ ATTRIBUTES STDCALL :: DefWindowProc
       integer(LRESULT_T) :: DefWindowProc
       integer(HWND_T), value :: hWnd
       integer(UINT_T), value :: Msg
       integer(WPARAM_T), value :: wParam
       integer(LPARAM_T), value :: lParam
     end function DefWindowProc

     function DialogBoxParam(hInstance,lpTemplate,hWndParent,lpDialogFunc, &
          dwInitParam) bind(C, name='DialogBoxParamA')
       import :: C_CHAR, C_FUNPTR, HINSTANCE_T, HWND_T, INT_PTR_T, LPARAM_T
       !GCC$ ATTRIBUTES STDCALL :: DialogBoxParam
       integer(INT_PTR_T) :: DialogBoxParam
       integer(HINSTANCE_T), value :: hInstance
       character(C_CHAR), intent(in) :: lpTemplate(*) ! LPCTSTR
       !type(C_PTR), value :: lpTemplate ! LPCTSTR
       integer(HWND_T), value :: hWndParent
       type(C_FUNPTR), value :: lpDialogFunc ! DLGPROC
       integer(LPARAM_T), value :: dwInitParam
     end function DialogBoxParam

     function DispatchMessage(lpMsg) bind(C, name='DispatchMessageA')
       import :: LRESULT_T, MSG_T
       !GCC$ ATTRIBUTES STDCALL :: DispatchMessage
       integer(LRESULT_T) :: DispatchMessage
       type(MSG_T), intent(in) :: lpMsg
     end function DispatchMessage

     function DrawText(hdc,lpString,nCount,lpRect,uFormat) &
          bind(C, name='DrawTextA')
       import :: C_CHAR, HDC_T, INT_T, RECT_T, UINT_T
       !GCC$ ATTRIBUTES STDCALL :: DrawText
       integer(INT_T) :: DrawText
       integer(HDC_T), value :: hdc
       character(C_CHAR), intent(inout) :: lpString(*) ! LPCTSTR
       integer(INT_T), value :: nCount
       type(RECT_T), intent(inout) :: lpRect
       integer(UINT_T), value :: uFormat
     end function DrawText

     function Ellipse(hdc,nLeftRect,nTopRect,nRightRect,nBottomRect) &
          bind(C, name='Ellipse')
       import :: BOOL_T, HDC_T, INT_T
       !GCC$ ATTRIBUTES STDCALL :: Ellipse
       integer(BOOL_T) :: Ellipse
       integer(HDC_T), value :: hdc
       integer(INT_T), value :: nLeftRect
       integer(INT_T), value :: nTopRect
       integer(INT_T), value :: nRightRect
       integer(INT_T), value :: nBottomRect
     end function Ellipse

     function EnableMenuItem(hMenu,uIDEnableItem,uEnable) &
          bind(C, name='EnableMenuItem')
       import :: BOOL_T, HMENU_T, UINT_T
       !GCC$ ATTRIBUTES STDCALL :: EnableMenuItem
       integer(BOOL_T) :: EnableMenuItem
       integer(HMENU_T), value :: hMenu
       integer(UINT_T), value :: uIDEnableItem
       integer(UINT_T), value :: uEnable
     end function EnableMenuItem

     function EndDialog(hWnd,nResult) bind(C, name='EndDialog')
       import :: BOOL_T, HWND_T, INT_PTR_T
       !GCC$ ATTRIBUTES STDCALL :: EndDialog
       integer(BOOL_T) :: EndDialog
       integer(HWND_T), value :: hWnd
       integer(INT_PTR_T), value :: nResult
     end function EndDialog

     function EndPaint(hWnd,lpPaint) bind(C, name='EndPaint')
       import :: BOOL_T, HWND_T, PAINTSTRUCT_T
       !GCC$ ATTRIBUTES STDCALL :: EndPaint
       integer(BOOL_T) :: EndPaint
       integer(HWND_T), value :: hWnd
       type(PAINTSTRUCT_T), intent(in) :: lpPaint
     end function EndPaint

     subroutine ExitProcess(uExitCode) bind(C, name='ExitProcess')
       import :: UINT_T
       !GCC$ ATTRIBUTES STDCALL :: ExitProcess
       integer(UINT_T), value :: uExitCode
     end subroutine ExitProcess

     function FillRect(hdc,lprc,hbr) bind(C, name='FillRect')
       import :: HBRUSH_T, HDC_T, INT_T, RECT_T
       !GCC$ ATTRIBUTES STDCALL :: FillRect
       integer(INT_T) :: FillRect
       integer(HDC_T),value :: hdc
       type(RECT_T), intent(in) :: lprc
       integer(HBRUSH_T), value :: hbr
     end function FillRect

     function GetBkColor(hdc) bind(C, name='GetBkColor')
       import :: COLORREF_T, HDC_T
       !GCC$ ATTRIBUTES STDCALL :: GetBkColor
       integer(COLORREF_T) :: GetBkColor
       integer(HDC_T), value :: hdc
     end function GetBkColor

     function GetClientRect(hWnd,lpRect) bind(C, name='GetClientRect')
       import :: BOOL_T, HWND_T, RECT_T
       !GCC$ ATTRIBUTES STDCALL :: GetClientRect
       integer(BOOL_T) :: GetClientRect
       integer(HWND_T), value :: hWnd
       type(RECT_T), intent(out) :: lpRect
     end function GetClientRect

     function GetCommandLine() bind(C, name='GetCommandLineA')
       import :: C_PTR
       !GCC$ ATTRIBUTES STDCALL :: GetCommandLine
       type(C_PTR) :: GetCommandLine ! LPCTSTR
     end function GetCommandLine

     function GetDC(hWnd) bind(C, name='GetDC')
       import :: HDC_T, HWND_T
       !GCC$ ATTRIBUTES STDCALL :: GetDC
       integer(HDC_T) :: GetDC
       integer(HWND_T), value :: hWnd
     end function GetDC

     function GetDCBrushColor(hdc) bind(C, name='GetDCBrushColor')
       import :: COLORREF_T, HDC_T
       !GCC$ ATTRIBUTES STDCALL :: GetDCBrushColor
       integer(COLORREF_T) :: GetDCBrushColor
       integer(HDC_T), value :: hdc
     end function GetDCBrushColor

     function GetDeviceCaps(hdc,nIndex) bind(C, name='GetDeviceCaps')
       import :: HDC_T, INT_T
       !GCC$ ATTRIBUTES STDCALL :: GetDeviceCaps
       integer(INT_T) :: GetDeviceCaps
       integer(HDC_T), value :: hdc
       integer(INT_T), value :: nIndex
     end function GetDeviceCaps

     function GetDlgItemText(hDlg,nIDDlgItem,lpString,nMaxCount) &
          bind(C, name='GetDlgItemTextA')
       import :: C_CHAR, HWND_T, INT_T, UINT_T
       !GCC$ ATTRIBUTES STDCALL :: GetDlgItemText
       integer(UINT_T) :: GetDlgItemText
       integer(HWND_T), value :: hDlg
       integer(INT_T), value :: nIDDlgItem
       character(C_CHAR), intent(out) :: lpString(*) ! LPTSTR
       ! This works too... but it is more complicated :-(
       ! Notice that the C string pointer is of type value:
       ! it is the content to which it points that is an 'output'
       !
       !type(C_PTR), value :: lpString
       integer(INT_T), value :: nMaxCount
     end function GetDlgItemText

     function GetKeyState(nVirtKey) bind(C, name='GetKeyState')
       import :: INT_T, SHORT_T
       !GCC$ ATTRIBUTES STDCALL :: GetKeyState
       integer(SHORT_T) :: GetKeyState
       integer(INT_T), value :: nVirtKey
     end function GetKeyState

     function GetLastError() bind(C, name='GetLastError')
       import :: DWORD_T
       !GCC$ ATTRIBUTES STDCALL :: GetLastError
       integer(DWORD_T) :: GetLastError
     end function GetLastError

     function GetMenu(hWnd) bind(C, name='GetMenu')
       import :: HMENU_T, HWND_T
       !GCC$ ATTRIBUTES STDCALL :: GetMenu
       integer(HMENU_T) :: GetMenu
       integer(HWND_T), value :: hWnd
     end function GetMenu

     function GetMessage(lpMsg,hWnd,wMsgFilterMin,wMsgFilterMax) &
          bind(C, name='GetMessageA')
       import :: BOOL_T, HWND_T, MSG_T, UINT_T
       !GCC$ ATTRIBUTES STDCALL :: GetMessage
       integer(BOOL_T) :: GetMessage
       type(MSG_T), intent(out) :: lpMsg
       integer(HWND_T), value :: hWnd
       integer(UINT_T), value :: wMsgFilterMin
       integer(UINT_T), value :: wMsgFilterMax
     end function GetMessage

     function GetModuleHandle(lpModuleName) bind(C, name='GetModuleHandleA')
       import :: C_CHAR, HMODULE_T
       !GCC$ ATTRIBUTES STDCALL :: GetModuleHandle
       integer(HMODULE_T) :: GetModuleHandle
       character(C_CHAR), intent(in) :: lpModuleName(*) ! LPCTSTR
     end function GetModuleHandle

     function GetMonitorInfo(hMonitor,lpmi) bind(C, name='GetMonitorInfoA')
       import :: BOOL_T, HMONITOR_T, MONITORINFO_T
       !GCC$ ATTRIBUTES STDCALL :: GetMonitorInfo
       integer(BOOL_T) :: GetMonitorInfo
       integer(HMONITOR_T), value :: hMonitor
       type(MONITORINFO_T), intent(out) :: lpmi
     end function GetMonitorInfo

     function GetStockObject(fnObject) bind(C, name='GetStockObject')
       import :: HGDIOBJ_T, INT_T
       !GCC$ ATTRIBUTES STDCALL :: GetStockObject
       integer(HGDIOBJ_T) :: GetStockObject
       integer(INT_T), value :: fnObject
     end function GetStockObject

     function GetTextColor(hdc) bind(C, name='GetTextColor')
       import :: COLORREF_T, HDC_T
       !GCC$ ATTRIBUTES STDCALL :: GetTextColor
       integer(COLORREF_T) :: GetTextColor
       integer(HDC_T), value :: hdc
     end function GetTextColor

     function InvalidateRect(hWnd,lpRect,bErase) &
          bind(C, name='InvalidateRect')
       import :: BOOL_T, HWND_T, RECT_T
       !GCC$ ATTRIBUTES STDCALL :: InvalidateRect
       integer(BOOL_T) :: InvalidateRect
       integer(HWND_T), value :: hWnd
       type(RECT_T), intent(in) :: lpRect
       integer(BOOL_T), value :: bErase
     end function InvalidateRect

     function KillTimer(hWnd,uIDEvent) bind(C, name='KillTimer')
       import :: BOOL_T, HWND_T, UINT_PTR_T
       !GCC$ ATTRIBUTES STDCALL :: KillTimer
       integer(BOOL_T) :: KillTimer
       integer(HWND_T), value :: hWnd
       integer(UINT_PTR_T), value :: uIDEvent
     end function KillTimer

     function LineTo(hdc,nXEnd,nYEnd) bind(C, name='LineTo')
       import :: BOOL_T, HDC_T, INT_T
       !GCC$ ATTRIBUTES STDCALL :: LineTo
       integer(BOOL_T) :: LineTo
       integer(HDC_T),value :: hdc
       integer(INT_T), value :: nXEnd
       integer(INT_T), value :: nYEnd
     end function LineTo

     function LoadCursor(hInstance,lpCursorName) bind(C, name='LoadCursorA')
       import :: C_CHAR, HCURSOR_T, HINSTANCE_T
       !GCC$ ATTRIBUTES STDCALL :: LoadCursor
       integer(HCURSOR_T) :: LoadCursor
       integer(HINSTANCE_T), value :: hInstance
       character(C_CHAR), intent(in) :: lpCursorName(*) ! LPCTSTR
     end function LoadCursor

     function LoadIcon(hInstance,lpIconName) bind(C, name='LoadIconA')
       import :: C_CHAR, HICON_T, HINSTANCE_T
       !GCC$ ATTRIBUTES STDCALL :: LoadIcon
       integer(HICON_T) :: LoadIcon
       integer(HINSTANCE_T), value :: hInstance
       character(C_CHAR), intent(in) :: lpIconName(*) ! LPCTSTR
     end function LoadIcon

     function MessageBeep(uType) bind(C, name='MessageBeep')
       import :: BOOL_T, UINT_T
       !GCC$ ATTRIBUTES STDCALL :: MessageBeep
       integer(BOOL_T) :: MessageBeep
       integer(UINT_T), value :: uType
     end function MessageBeep

     function MessageBox(hWnd,lpText,lpCaption,uType) &
          bind(C, name='MessageBoxA')
       import :: C_CHAR, HWND_T, INT_T, UINT_T
       !GCC$ ATTRIBUTES STDCALL :: MessageBox
       integer(INT_T) :: MessageBox
       integer(HWND_T), value :: hWnd
       character(C_CHAR), intent(in) :: lpText(*)    ! LPCTSTR
       character(C_CHAR), intent(in) :: lpCaption(*) ! LPCTSTR
       integer(UINT_T), value :: uType
     end function MessageBox

     function MonitorFromPoint(pt,dwFlags) bind(C, name='MonitorFromPoint')
       import :: DWORD_T, HMONITOR_T, POINT_T
       !GCC$ ATTRIBUTES STDCALL :: MonitorFromPoint
       integer(HMONITOR_T) :: MonitorFromPoint
       type(POINT_T), value :: pt
       integer(DWORD_T), value :: dwFlags
     end function MonitorFromPoint

     function MoveToEx(hdc,X,Y,lpPoint) bind(C, name='MoveToEx')
       import :: BOOL_T, HDC_T, INT_T, POINT_T
       !GCC$ ATTRIBUTES STDCALL :: MoveToEx
       integer(BOOL_T) :: MoveToEx
       integer(HDC_T),value :: hdc
       integer(INT_T), value :: X
       integer(INT_T), value :: Y
       type(POINT_T), intent(out) :: lpPoint
     end function MoveToEx

     function PlaySound(pszSound,hmod,fdwSound) bind(C, name='PlaySoundA')
       import :: BOOL_T, C_CHAR, DWORD_T, HMODULE_T
       !GCC$ ATTRIBUTES STDCALL :: PlaySound
       integer(BOOL_T) :: PlaySound
       character(C_CHAR), intent(in) :: pszSound(*) ! LPCTSTR
       integer(HMODULE_T), value :: hmod
       integer(DWORD_T), value :: fdwSound
     end function PlaySound

     function PeekMessage(lpMsg,hWnd,wMsgFilterMin,wMsgFilterMax,wRemoveMsg) &
          bind(C, name='PeekMessageA')
       import :: BOOL_T, HWND_T, MSG_T, UINT_T
       !GCC$ ATTRIBUTES STDCALL :: PeekMessage
       integer(BOOL_T) :: PeekMessage
       type(MSG_T), intent(out) :: lpMsg
       integer(HWND_T), value :: hWnd
       integer(UINT_T), value :: wMsgFilterMin
       integer(UINT_T), value :: wMsgFilterMax
       integer(UINT_T), value :: wRemoveMsg
     end function PeekMessage

     function Polygon(hdc,lpPoints,nCount) bind(C, name='Polygon')
       import :: BOOL_T, HDC_T, INT_T, POINT_T
       !GCC$ ATTRIBUTES STDCALL :: Polygon
       integer(BOOL_T) :: Polygon
       integer(HDC_T),value :: hdc
       type(POINT_T), intent(in) :: lpPoints(*)
       integer(INT_T), value :: nCount
     end function Polygon

     function PostMessage(hWnd,Msg,wParam,lParam) bind(C, name='PostMessageA')
       import :: BOOL_T, HWND_T, LPARAM_T, UINT_T, WPARAM_T
       !GCC$ ATTRIBUTES STDCALL :: PostMessage
       integer(BOOL_T) :: PostMessage
       integer(HWND_T), value :: hWnd
       integer(UINT_T), value :: Msg
       integer(WPARAM_T), value :: wParam
       integer(LPARAM_T), value :: lParam
     end function PostMessage

     subroutine PostQuitMessage(nExitCode) bind(C, name='PostQuitMessage')
       import :: INT_T
       !GCC$ ATTRIBUTES STDCALL :: PostQuitMessage
       integer(INT_T), value :: nExitCode
     end subroutine PostQuitMessage

     function Rectangle(hdc,nLeftRect,nTopRect,nRightRect,nBottomRect) &
          bind(C, name='Rectangle')
       import :: BOOL_T, HDC_T, INT_T
       !GCC$ ATTRIBUTES STDCALL :: Rectangle
       integer(BOOL_T) :: Rectangle
       integer(HDC_T),value :: hdc
       integer(INT_T), value :: nLeftRect
       integer(INT_T), value :: nTopRect
       integer(INT_T), value :: nRightRect
       integer(INT_T), value :: nBottomRect
     end function Rectangle

     function RegisterClassEx(WndClass) bind(C, name='RegisterClassExA')
       import :: ATOM_T, WNDCLASSEX_T
       !GCC$ ATTRIBUTES STDCALL :: RegisterClassEx
       integer(ATOM_T) :: RegisterClassEx
       type(WNDCLASSEX_T), intent(in) :: WndClass
     end function RegisterClassEx

     function ReleaseDC(hWnd,hdc) bind(C, name='ReleaseDC')
       import :: HDC_T, HWND_T, INT_T
       !GCC$ ATTRIBUTES STDCALL :: ReleaseDC
       integer(INT_T) :: ReleaseDC
       integer(HWND_T), value :: hWnd
       integer(HDC_T), value :: hdc
     end function ReleaseDC

     function SelectObject(hdc,hgdiobj) bind(C, name='SelectObject')
       import :: HDC_T, HGDIOBJ_T
       !GCC$ ATTRIBUTES STDCALL :: SelectObject
       integer(HGDIOBJ_T) :: SelectObject
       integer(HDC_T), value :: hdc
       integer(HGDIOBJ_T), value :: hgdiobj
     end function SelectObject

     function SendMessage(hWnd,Msg,wParam,lParam) bind(C, name='SendMessageA')
       import :: HWND_T, LPARAM_T, LRESULT_T, UINT_T, WPARAM_T
       !GCC$ ATTRIBUTES STDCALL :: SendMessage
       integer(LRESULT_T) :: SendMessage
       integer(HWND_T), value :: hWnd
       integer(UINT_T), value :: Msg
       integer(WPARAM_T), value :: wParam
       integer(LPARAM_T), value :: lParam
     end function SendMessage

     function SendNotifyMessage(hWnd,Msg,wParam,lParam) &
          bind(C, name='SendNotifyMessageA')
       import :: HWND_T, LPARAM_T, LRESULT_T, UINT_T, WPARAM_T
       !GCC$ ATTRIBUTES STDCALL :: SendNotifyMessage
       integer(LRESULT_T) :: SendNotifyMessage
       integer(HWND_T), value :: hWnd
       integer(UINT_T), value :: Msg
       integer(WPARAM_T), value :: wParam
       integer(LPARAM_T), value :: lParam
     end function SendNotifyMessage

     function SetBkColor(hdc,crColor) bind(C, name='SetBkColor')
       import :: COLORREF_T, HDC_T
       !GCC$ ATTRIBUTES STDCALL :: SetBkColor
       integer(COLORREF_T) :: SetBkColor
       integer(HDC_T), value :: hdc
       integer(COLORREF_T), value :: crColor
     end function SetBkColor

     function SetBkMode(hdc,iBkMode) bind(C, name='SetBkMode')
       import :: HDC_T, INT_T
       !GCC$ ATTRIBUTES STDCALL :: SetBkMode
       integer(INT_T) :: SetBkMode
       integer(HDC_T), value :: hdc
       integer(INT_T), value :: iBkMode
     end function SetBkMode

     function SetCursor(hCursor) bind(C, name='SetCursor')
       import :: HCURSOR_T
       !GCC$ ATTRIBUTES STDCALL :: SetCursor
       integer(HCURSOR_T) :: SetCursor
       integer(HCURSOR_T), value :: hCursor
     end function SetCursor

     function SetDCBrushColor(hdc,crColor) bind(C, name='SetDCBrushColor')
       import :: COLORREF_T, HDC_T
       !GCC$ ATTRIBUTES STDCALL :: SetDCBrushColor
       integer(COLORREF_T) :: SetDCBrushColor
       integer(HDC_T), value :: hdc
       integer(COLORREF_T), value :: crColor
     end function SetDCBrushColor

     function SetDlgItemText(hDlg,nIDDlgItem,lpString) &
          bind(C, name='SetDlgItemTextA')
       import :: BOOL_T, C_CHAR, HWND_T, INT_T
       !GCC$ ATTRIBUTES STDCALL :: SetDlgItemText
       integer(BOOL_T) :: SetDlgItemText
       integer(HWND_T), value :: hDlg
       integer(INT_T), value :: nIDDlgItem
       character(C_CHAR), intent(in) :: lpString(*) ! LPCTSTR
     end function SetDlgItemText

     function SetPixel(hdc,X,Y,crColor) bind(C, name='SetPixel')
       import :: COLORREF_T, HDC_T, INT_T
       !GCC$ ATTRIBUTES STDCALL :: SetPixel
       integer(COLORREF_T) :: SetPixel
       integer(HDC_T), value :: hdc
       integer(INT_T), value :: X
       integer(INT_T), value :: Y
       integer(COLORREF_T), value :: crColor
     end function SetPixel

     function SetROP2(hdc,fnDrawMode) bind(C, name='SetROP2')
       import :: HDC_T, INT_T
       !GCC$ ATTRIBUTES STDCALL :: SetROP2
       integer(INT_T) :: SetROP2
       integer(HDC_T), value :: hdc
       integer(INT_T), value :: fnDrawMode
     end function SetROP2

     function SetTextAlign(hdc,fMode) bind(C, name='SetTextAlign')
       import :: HDC_T, UINT_T
       !GCC$ ATTRIBUTES STDCALL :: SetTextAlign
       integer(UINT_T) :: SetTextAlign
       integer(HDC_T), value :: hdc
       integer(UINT_T), value :: fMode
     end function SetTextAlign

     function SetTextColor(hdc,crColor) bind(C, name='SetTextColor')
       import :: COLORREF_T, HDC_T
       !GCC$ ATTRIBUTES STDCALL :: SetTextColor
       integer(COLORREF_T) :: SetTextColor
       integer(HDC_T), value :: hdc
       integer(COLORREF_T), value :: crColor
     end function SetTextColor

     function SetTimer(hWnd,nIDEvent,uElapse,lpTimerFunc) &
          bind(C,name='SetTimer')
       import :: C_FUNPTR, HWND_T, UINT_T, UINT_PTR_T
       !GCC$ ATTRIBUTES STDCALL :: SetTimer
       integer(UINT_PTR_T) :: SetTimer
       integer(HWND_T), value :: hWnd
       integer(UINT_PTR_T), value :: nIDEvent
       integer(UINT_T), value :: uElapse
       type(C_FUNPTR), value :: lpTimerFunc ! TIMERPROC
     end function SetTimer

     function ShowWindow(hWnd,nCmdShow) bind(C, name='ShowWindow')
       import :: BOOL_T, HWND_T, INT_T
       !GCC$ ATTRIBUTES STDCALL :: ShowWindow
       integer(BOOL_T) :: ShowWindow
       integer(HWND_T), value :: hWnd
       integer(INT_T), value :: nCmdShow
     end function ShowWindow

     function TextOut(hdc,nXStart,nYStart,lpString,cchString) &
          bind(C, name='TextOutA')
       import :: BOOL_T, C_CHAR, HDC_T, INT_T
       !GCC$ ATTRIBUTES STDCALL :: TextOut
       integer(BOOL_T) :: TextOut
       integer(HDC_T), value :: hdc
       integer(INT_T), value :: nXStart
       integer(INT_T), value :: nYStart
       character(C_CHAR), intent(in) :: lpString(*) ! LPCTSTR
       integer(INT_T), value :: cchString
     end function TextOut

     function TranslateMessage(lpMsg) bind(C, name='TranslateMessage')
       import :: BOOL_T, MSG_T
       !GCC$ ATTRIBUTES STDCALL :: TranslateMessage
       integer(BOOL_T) :: TranslateMessage
       type(MSG_T), intent(in) :: lpMsg
     end function TranslateMessage

     function UpdateWindow(hWnd) bind(C, name='UpdateWindow')
       import :: BOOL_T, HWND_T
       !GCC$ ATTRIBUTES STDCALL :: UpdateWindow
       integer(BOOL_T) :: UpdateWindow
       integer(HWND_T), value :: hWnd
     end function UpdateWindow
  end interface

  ! Interface routines
  public :: BeginPaint, BitBlt, CheckRadioButton, CreateCompatibleBitmap, &
       CreateCompatibleDC, CreatePen, CreateSolidBrush, CreateHatchBrush, &
       CreateWindowEx, DeleteDC, DeleteObject, DestroyWindow, DefWindowProc, &
       DialogBoxParam, DispatchMessage, DrawText, Ellipse, EnableMenuItem, &
       EndDialog, &
       EndPaint, ExitProcess, FillRect, GetBkColor, GetClientRect, &
       GetCommandLine, GetDC, GetDCBrushColor, GetDeviceCaps, &
       GetDlgItemText, GetKeyState, GetLastError, GetMenu, GetMessage, &
       GetModuleHandle, GetMonitorInfo, GetStockObject, GetTextColor, &
       InvalidateRect, KillTimer, LineTo, LoadCursor, LoadIcon, MessageBeep, &
       MessageBox, MonitorFromPoint, MoveToEx, PlaySound, PeekMessage, &
       Polygon, PostMessage, PostQuitMessage, Rectangle, RegisterClassEx, &
       ReleaseDC, SelectObject, SendMessage, SendNotifyMessage, SetBkColor, &
       SetBkMode, SetCursor, SetDCBrushColor, SetDlgItemText, SetPixel, &
       SetROP2, SetTextAlign, SetTextColor, SetTimer, ShowWindow, TextOut, &
       TranslateMessage, UpdateWindow

  ! Auxiliary routines
  public :: arrow_cursor, cross_cursor, hand_cursor, wait_cursor, &
       application_icon, asterisk_icon, error_icon, exclamation_icon, &
       hand_icon, information_icon, question_icon, warning_icon, &
       winlogo_icon, ask_confirmation, dialog_box, hi_word, lo_word, &
       make_int_resource, make_int_resource_C_PTR, null_p, RGB, error_msg

contains

  function arrow_cursor() result(s)
    character(C_CHAR), pointer :: s(:)
    s => make_int_resource(IDC_ARROW)
  end function arrow_cursor

  function cross_cursor() result(s)
    character(C_CHAR), pointer :: s(:)
    s => make_int_resource(IDC_CROSS)
  end function cross_cursor

  function hand_cursor() result(s)
    character(C_CHAR), pointer :: s(:)
    s => make_int_resource(IDC_HAND)
  end function hand_cursor

  function wait_cursor() result(s)
    character(C_CHAR), pointer :: s(:)
    s => make_int_resource(IDC_WAIT)
  end function wait_cursor

  function application_icon() result(s)
    character(C_CHAR), pointer :: s(:)
    s => make_int_resource(IDI_APPLICATION)
  end function application_icon

  function asterisk_icon() result(s)
    character(C_CHAR), pointer :: s(:)
    s => make_int_resource(IDI_ASTERISK)
  end function asterisk_icon

  function error_icon() result(s)
    character(C_CHAR), pointer :: s(:)
    s => make_int_resource(IDI_ERROR)
  end function error_icon

  function exclamation_icon() result(s)
    character(C_CHAR), pointer :: s(:)
    s => make_int_resource(IDI_EXCLAMATION)
  end function exclamation_icon

  function hand_icon() result(s)
    character(C_CHAR), pointer :: s(:)
    s => make_int_resource(IDI_HAND)
  end function hand_icon

  function information_icon() result(s)
    character(C_CHAR), pointer :: s(:)
    s => make_int_resource(IDI_INFORMATION)
  end function information_icon

  function question_icon() result(s)
    character(C_CHAR), pointer :: s(:)
    s => make_int_resource(IDI_QUESTION)
  end function question_icon

  function warning_icon() result(s)
    character(C_CHAR), pointer :: s(:)
    s => make_int_resource(IDI_WARNING)
  end function warning_icon

  function winlogo_icon() result(s)
    character(C_CHAR), pointer :: s(:)
    s => make_int_resource(IDI_WINLOGO)
  end function winlogo_icon

  function ask_confirmation(hWnd,text,caption)
    integer(INT_T) :: ask_confirmation
    integer(HWND_T), intent(in) :: hWnd
    character(len=*), intent(in) :: text ! LPCTSTR
    character(len=*), intent(in) :: caption ! LPCTSTR
    ask_confirmation = MessageBox(hWnd,text//NUL,caption//NUL, &
         ior(MB_YESNO,MB_ICONQUESTION))
  end function ask_confirmation

  function dialog_box(hInstance,lpTemplate,hWndParent,lpDialogFunc)
    integer(INT_PTR_T) :: dialog_box
    integer(HINSTANCE_T), intent(in) :: hInstance
    character(C_CHAR), intent(in) :: lpTemplate(*) ! LPCTSTR
    !type(C_PTR), intent(in) :: lpTemplate ! LPCTSTR
    integer(HWND_T), intent(in) :: hWndParent
    type(C_FUNPTR), intent(in) :: lpDialogFunc ! DLGPROC
    dialog_box = DialogBoxParam(hInstance,lpTemplate,hWndParent, &
         lpDialogFunc,NULL_T)
  end function dialog_box

  function hi_word(dwValue)
    integer(WORD_T) :: hi_word
    integer(DWORD_T), intent(in) :: dwValue
    ! (/usr/include/w32api/windef.h)
    hi_word = int(ishft(dwValue,-16),WORD_T)
  end function hi_word

  function lo_word(dwValue)
    integer(WORD_T) :: lo_word
    integer(DWORD_T), intent(in) :: dwValue
    ! (/usr/include/w32api/windef.h)
    lo_word = int(iand(dwValue,65535),WORD_T) ! iand(dwValue,Z'0000FFFF')
  end function lo_word

  function make_int_resource(i) result(s)
    integer(WORD_T), intent(in) :: i
    character(C_CHAR), pointer :: s(:) ! LPTSTR
    call c_f_pointer(make_int_resource_C_PTR(i),s,[0])
  end function make_int_resource

  function make_int_resource_C_PTR(i) result(s)
    integer(WORD_T), intent(in) :: i
    type(C_PTR) :: s
    s = transfer(int(i,HANDLE_T),NULL_PTR_T)
  end function make_int_resource_C_PTR

  function null_p() result(s)
    character(C_CHAR), pointer :: s(:) ! LPTSTR
    s => make_int_resource(0_WORD_T)
  end function null_p

  function RGB(r,g,b)
    integer(COLORREF_T) :: RGB
    integer(INT_T), intent(in) :: r, g, b
    RGB = (ior(ior((r),ishft((g),8)),ishft((b),16)))
  end function RGB

  subroutine error_msg(text)
    character(len=*), intent(in) :: text ! LPCTSTR

    dummy = MessageBox(NULL_T,text//NUL,NULL_LPSTR, &
         ior(MB_ICONEXCLAMATION,MB_OK))
  end subroutine error_msg
end module win32
