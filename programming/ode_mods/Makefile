.POSIX:
.SUFFIXES:

FC         = gfortran
FFLAGS     = -march=native -Wall -std=f2008 -fmax-errors=1 # -O2

SRCS = cernlib_integrators.f90 everhart_integrator.f90 ode_integrator.f90
OBJS = $(SRCS:.f90=.o)

LIBNAME = ode_mods

LIBRARY = lib$(LIBNAME).a

.PHONY: all clean

all: $(LIBRARY)

$(LIBRARY): $(SRCS)
	$(FC) $(FFLAGS) -Wno-unused-dummy-argument -I ../finclude -fPIC -c $(SRCS)
	ar rcs $(LIBRARY) $(OBJS)

# Delete *.mod, *.a, *.o, and all compiled examples.
clean:
	if [ `ls -1 *.mod 2>/dev/null | wc -l` -gt 0 ]; then rm *.mod; fi
	if [ `ls -1 *.a 2>/dev/null | wc -l` -gt 0 ]; then rm *.a; fi
	if [ `ls -1 *.o 2>/dev/null | wc -l` -gt 0 ]; then rm *.o; fi
