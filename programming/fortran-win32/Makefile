.POSIX:
.SUFFIXES:

FC          = gfortran
FFLAGS      = -march=native -Wall -fmax-errors=1 # -O2

SRCW32 =  win32.f90
OBJW32 = $(SRCW32:.f90=.o)

SRCWBOX =  basic_box_m.f90 about_box_m.f90 radio_box_m.f90 x_box_m.f90 xy_box_m.f90 xyz_box_m.f90
OBJWBOX = $(SRCWBOX:.f90=.o)

SRCWAPP =  win32app.f90
OBJWAPP = $(SRCWAPP:.f90=.o)

LIBNAMEW32 = fortran-win32
LIBNAMEWBOX = fortran-win32box
LIBNAMEWAPP = fortran-win32app

LIBRARYW32 = lib$(LIBNAMEW32).a
LIBRARYWBOX = lib$(LIBNAMEWBOX).a
LIBRARYWAPP = lib$(LIBNAMEWAPP).a

.PHONY: all clean

all: $(LIBRARYW32) $(LIBRARYWBOX) $(LIBRARYWAPP)

$(LIBRARYW32): $(SRCW32)
	$(FC) $(FFLAGS) -I ../finclude -fPIC -c $(SRCW32)
	ar rcs $(LIBRARYW32) $(OBJW32)

$(LIBRARYWBOX): $(SRCWBOX)
	$(FC) $(FFLAGS) -Wno-unused-dummy-argument -I ../finclude -fPIC -c $(SRCWBOX)
	ar rcs $(LIBRARYWBOX) $(OBJWBOX)

$(LIBRARYWAPP): $(SRCWAPP)
	$(FC) $(FFLAGS) -I ../finclude -fPIC -c $(SRCWAPP)
	ar rcs $(LIBRARYWAPP) $(OBJWAPP)

# Delete *.mod, *.a, *.o, and all compiled examples.
clean:
	if [ `ls -1 *.mod 2>/dev/null | wc -l` -gt 0 ]; then rm *.mod; fi
	if [ `ls -1 *.a 2>/dev/null | wc -l` -gt 0 ]; then rm *.a; fi
	if [ `ls -1 *.o 2>/dev/null | wc -l` -gt 0 ]; then rm *.o; fi
