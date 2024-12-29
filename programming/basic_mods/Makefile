.POSIX:
.SUFFIXES:

FC         = gfortran
FFLAGS     = -march=native -Wall -std=f2008 -fmax-errors=1 # -O2

SRCS = kind_consts.f90 math_consts.f90 getdata.f90 shading_colors.f90 \
       utilities.f90 additional_functions.f90 ft_timer_m.f90 \
       contour_plots.f90 julian_dates.f90 camera_view_m.f90
OBJS = $(SRCS:.f90=.o)

LIBNAME = basic_mods

LIBRARY = lib$(LIBNAME).a

.PHONY: all clean

all: $(LIBRARY)

$(LIBRARY): $(SRCS)
	$(FC) $(FFLAGS) -fPIC -c $(SRCS)
	ar rcs $(LIBRARY) $(OBJS)

# Delete *.mod, *.a, *.o, and all compiled examples.
clean:
	if [ `ls -1 *.mod 2>/dev/null | wc -l` -gt 0 ]; then rm *.mod; fi
	if [ `ls -1 *.a 2>/dev/null | wc -l` -gt 0 ]; then rm *.a; fi
	if [ `ls -1 *.o 2>/dev/null | wc -l` -gt 0 ]; then rm *.o; fi
