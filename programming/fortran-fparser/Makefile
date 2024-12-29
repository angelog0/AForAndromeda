.POSIX:
.SUFFIXES:

CXX         = g++
CXXFLAGS    = -DFP_SUPPORT_FLOAT_TYPE -DFP_SUPPORT_LONG_DOUBLE_TYPE \
              -DFP_SUPPORT_LONG_INT_TYPE -DFP_SUPPORT_COMPLEX_DOUBLE_TYPE \
              -DFP_SUPPORT_COMPLEX_FLOAT_TYPE \
              -DFP_SUPPORT_COMPLEX_LONG_DOUBLE_TYPE \
              -DFP_USE_THREAD_SAFE_EVAL -DFP_USE_THREAD_SAFE_EVAL_WITH_ALLOCA

FC          = gfortran
FFLAGS      = -march=native -Wall -fmax-errors=1 # -O2

SRCSCC = ../fparser-4.5.2/fparser.cc \
       ../fparser-4.5.2/fpoptimizer.cc
OBJSCC = fparser.o fpoptimizer.o

SRCCWRAP = cwrapper_fparser.cc
OBJCWRAP = cwrapper_fparser.o

SRCS = fparser_dp.f90 fparser_cd.f90
OBJS = $(SRCS:.f90=.o)

LIBNAMECC = fpc++
LIBNAME = fortran-fparser

LIBRARYCC = lib$(LIBNAMECC).a
LIBRARY = lib$(LIBNAME).a

.PHONY: all clean

all: $(LIBRARYCC) $(OBJCWRAP) $(LIBRARY)

$(LIBRARYCC): $(SRCSCC)
	$(CXX) $(CXXFLAGS) -I ../fparser-4.5.2 -fPIC -c $(SRCSCC)
	ar rcs $(LIBRARYCC) $(OBJSCC)

$(OBJCWRAP): $(SRCCWRAP)
	$(CXX) -I ../fparser-4.5.2 -fPIC -c $(SRCCWRAP)

$(LIBRARY): $(SRCS)
	$(FC) $(FFLAGS) -I ../finclude -fPIC -c $(SRCS)
	ar rcs $(LIBRARY) $(OBJS) $(OBJCWRAP)

# Delete *.mod, *.a, *.o, and all compiled examples.
clean:
	if [ `ls -1 *.mod 2>/dev/null | wc -l` -gt 0 ]; then rm *.mod; fi
	if [ `ls -1 *.a 2>/dev/null | wc -l` -gt 0 ]; then rm *.a; fi
	if [ `ls -1 *.o 2>/dev/null | wc -l` -gt 0 ]; then rm *.o; fi
