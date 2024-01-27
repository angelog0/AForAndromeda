#!/bin/bash
##
## USAGE
##
##   From AForAndromeda directory:
##
##     ./build-all.sh
##
##   or (on macOS, for example after installing the right bash > 4.0)
##
##     FC=gfortran-mp-12 CXX=g++-mp-12 bash ./build-all.sh
##
##   We have removed the '-march=native' flag because it makes
##   binaries NOT PORTABLE across the same OS with different
##   processors. If you need it, maybe you ca run
##
##     FC='gfortran -march=native' ./build-all.sh
##

: ${FC=gfortran}
: ${CXX=g++}
: ${RC=windres}

AFORANDROMEDA_DIR="$(pwd)"

AFORANDROMEDA_SRCDIR="${AFORANDROMEDA_DIR}/src"

BMODS_DIR="${AFORANDROMEDA_SRCDIR}/basic-modules"
OMODS_DIR="${AFORANDROMEDA_SRCDIR}/ode-modules"

FORTRAN_SDL2_DIR="${AFORANDROMEDA_DIR}/fortran-sdl2"
BIN_DIR="${AFORANDROMEDA_DIR}/bin"
FPARSER_DIR="${AFORANDROMEDA_DIR}/fparser-4.5.2"

SDL2_FORTRAN_APPS_DIR="${AFORANDROMEDA_SRCDIR}/sdl2-fortran.apps"
WIN32_FORTRAN_APPS_DIR="${AFORANDROMEDA_SRCDIR}/win32-fortran.apps"

NBODY_DIR="${AFORANDROMEDA_SRCDIR}/N-Body"
ODETS_DIR="${AFORANDROMEDA_SRCDIR}/ODE-Tests"
TF_DIR="${AFORANDROMEDA_SRCDIR}/Thomas-Fermi"
FPFORTRAN_DIR="${AFORANDROMEDA_SRCDIR}/fparser-fortran"

SDL2F90=(${FORTRAN_SDL2_DIR}/src/{c_util,sdl2/{sdl2_stdinc,sdl2_audio,sdl2_blendmode,sdl2_cpuinfo,sdl2_gamecontroller,sdl2_error,sdl2_events,sdl2_filesystem,sdl2_hints,sdl2_joystick,sdl2_keyboard,sdl2_log,sdl2_messagebox,sdl2_rect,sdl2_pixels,sdl2_platform,sdl2_scancode,sdl2_surface,sdl2_render,sdl2_keycode,sdl2_mouse,sdl2_rwops,sdl2_thread,sdl2_timer,sdl2_version,sdl2_video,sdl2_opengl},sdl2}.f90)

SDL2F90=${SDL2F90[@]}

MINGW64_OS=`uname -s | grep MINGW64`

if [ "${MINGW64_OS}" != "" ] ; then
    ## MinGW64 static build
    LIBS="-static -lmingw32 -lSDL2main -lSDL2 -lws2_32 -ldinput8 -ldxguid -ldxerr8 -luser32 -lgdi32 -lwinmm -limm32 -lole32 -loleaut32 -lshell32 -lversion -luuid -lcomdlg32 -lhid -lsetupapi"
    EXE=.exe
else
    ## GNU/Linux / macOS build
    LIBS=`sdl2-config --libs`
    EXE=.out
fi

if [ ! -e "${FORTRAN_SDL2_DIR}" ] ; then
    echo
    echo -n "Cloning ${FORTRAN_SDL2_DIR} ... "

    ## ${FORTRAN_SDL2_DIR##*/}: remove the longest string "*/" from
    ## the front of ${FORTRAN_SDL2_DIR}
    git clone -q https://github.com/interkosmos/${FORTRAN_SDL2_DIR##*/}.git

    echo "done."
    echo
fi

if [ ! -e "${BIN_DIR}" ] ; then
    mkdir -p "${BIN_DIR}"
fi

if [ ! -e "${FPARSER_DIR}" ] ; then
    echo
    echo -n "Downloading FPARSER source package ... "

    wget -q http://warp.povusers.org/FunctionParser/fparser4.5.2.zip
    aunpack -q fparser4.5.2.zip -X fparser-4.5.2/ > /dev/null

    rm -rf fparser4.5.2.zip

    echo "done."

    echo -n "Building FPARSER package ... "

    cd fparser-4.5.2

    ${CXX} -DFP_SUPPORT_FLOAT_TYPE \
       -DFP_SUPPORT_LONG_DOUBLE_TYPE \
       -DFP_SUPPORT_LONG_INT_TYPE -DFP_SUPPORT_COMPLEX_DOUBLE_TYPE \
       -DFP_SUPPORT_COMPLEX_FLOAT_TYPE -DFP_SUPPORT_COMPLEX_LONG_DOUBLE_TYPE \
       -DFP_USE_THREAD_SAFE_EVAL -DFP_USE_THREAD_SAFE_EVAL_WITH_ALLOCA \
       -c fparser.cc

    ${CXX} -DFP_SUPPORT_FLOAT_TYPE \
       -DFP_SUPPORT_LONG_DOUBLE_TYPE \
       -DFP_SUPPORT_LONG_INT_TYPE -DFP_SUPPORT_COMPLEX_DOUBLE_TYPE \
       -DFP_SUPPORT_COMPLEX_FLOAT_TYPE -DFP_SUPPORT_COMPLEX_LONG_DOUBLE_TYPE \
       -DFP_USE_THREAD_SAFE_EVAL -DFP_USE_THREAD_SAFE_EVAL_WITH_ALLOCA \
       -c fpoptimizer.cc

    mv *.o ../src/fparser-fortran/

    cd ../src/fparser-fortran
    ${CXX} -I ../../fparser-4.5.2 -c cwrapper_fparser.cc
    ar rcs libFParser.a fparser.o fpoptimizer.o cwrapper_fparser.o

    echo "done."
    echo
fi

cd "${AFORANDROMEDA_SRCDIR}"

## First, build in SRC directory

program_name="brooks_matelski"
if [ ! -f "${BIN_DIR}/${program_name}${EXE}" ] ; then
    ## Just an alternative (almost) is
    ##
    ##   echo -n "Building ${program_name} ... " | tr '[:lower:]' '[:upper:]'
    ##
    ## because the following does NOT work OB on macOS
    echo -n "Building ${program_name^^} ... "
    rm -rf *.mod
    ${FC} -std=f2018 -O3 -Wall ${program_name}.f90 -o ${program_name}${EXE}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    echo "done."
fi

program_name="cbrt_test"
if [ ! -f "${BIN_DIR}/${program_name}${EXE}" ] ; then
    echo -n "Building ${program_name^^} ... "
    rm -rf *.mod
    ${FC} -std=f2018 -O3 -Wall ${program_name}.f90 -o ${program_name}${EXE}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    echo "done."
fi

program_name="plnml_test"
if [ ! -f "${BIN_DIR}/${program_name}${EXE}" ] ; then
    echo -n "Building ${program_name^^} ... "
    rm -rf *.mod
    ${FC} -std=f2018 -O3 -Wall ${program_name}.f90 -o ${program_name}${EXE}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    echo "done."
fi

program_name="solve_equation"
if [ ! -f "${BIN_DIR}/${program_name}${EXE}" ] ; then
    echo -n "Building ${program_name^^} ... "
    rm -rf *.mod
    ${FC} -std=f2018 -O3 -Wall -Wno-unused-function \
          ${program_name}.f90 -o ${program_name}${EXE}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    echo "done."
fi

cd "${SDL2_FORTRAN_APPS_DIR}"

program_name="display_ppm"
if [ ! -f "${BIN_DIR}/${program_name}${EXE}" ] ; then
    echo -n "Building ${program_name^^} ... "
    rm -rf *.mod
    ${FC} -std=f2018 -O3 -Wall \
          ${BMODS_DIR}/{{kind,math}_consts,nicelabels}.f90 ${SDL2F90} \
          SDL2_app.f90 ${program_name}.f90 ${LIBS} -o ${program_name}${EXE}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    echo "done."
fi

program_name="koch_snowflake"
if [ ! -f "${BIN_DIR}/${program_name}${EXE}" ] ; then
    echo -n "Building ${program_name^^} ... "
    rm -rf *.mod
    ${FC} -std=f2018 -O3 -Wall \
          ${BMODS_DIR}/{{kind,math}_consts,nicelabels}.f90 ${SDL2F90} \
          SDL2_app.f90 ${program_name}.f90 ${LIBS} -o ${program_name}${EXE}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    echo "done."
fi

program_name="star_walk_2d"
if [ ! -f "${BIN_DIR}/${program_name}${EXE}" ] ; then
    echo -n "Building ${program_name^^} ... "
    rm -rf *.mod
    ${FC} -std=f2018 -O3 -Wall \
          ${BMODS_DIR}/{{kind,math}_consts,getdata,nicelabels}.f90 ${SDL2F90} \
          SDL2_{app,shading}.f90 ${program_name}.f90 ${LIBS} \
          -o ${program_name}${EXE}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    echo "done."
fi

program_name="colour_map"
if [ ! -f "${BIN_DIR}/${program_name}${EXE}" ] ; then
    echo -n "Building ${program_name^^} ... "
    rm -rf *.mod
    ${FC} -std=f2018 -O3 -Wall \
          ${BMODS_DIR}/{{kind,math}_consts,nicelabels}.f90 ${SDL2F90} \
          SDL2_{app,shading}.f90 \
          ${program_name}.f90 ${LIBS} -o ${program_name}${EXE}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    echo "done."
fi

program_name="bouncing"
if [ ! -f "${BIN_DIR}/${program_name}${EXE}" ] ; then
    echo -n "Building ${program_name^^} ... "
    rm -rf *.mod
    ${FC} -std=f2018 -O3 -Wall \
          ${BMODS_DIR}/{{kind,math}_consts,getdata,nicelabels}.f90 ${SDL2F90} \
          SDL2_{app,shading}.f90 \
          ${program_name}.f90 ${LIBS} -o ${program_name}${EXE}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    echo "done."
fi

program_name="plot_ellipse"
if [ ! -f "${BIN_DIR}/${program_name}${EXE}" ] ; then
    echo -n "Building ${program_name^^} ... "
    rm -rf *.mod
    ${FC} -std=f2018 -O3 -Wall \
          ${BMODS_DIR}/{{kind,math}_consts,getdata,nicelabels}.f90 ${SDL2F90} \
          SDL2_app.f90 \
          ${program_name}.f90 ${LIBS} -o ${program_name}${EXE}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    echo "done."
fi

cd brkmtlsk

program_name="brkmtlsk_calc"
if [ ! -f "${BIN_DIR}/${program_name}${EXE}" ] ; then
    echo -n "Building ${program_name^^} ... "
    rm -rf *.mod
    ${FC} -std=f2018 -O3 -Wall ${program_name}.f90 -o ${program_name}${EXE}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    echo "done."
fi

program_name="display_brkmtlsk"
if [ ! -f "${BIN_DIR}/${program_name}${EXE}" ] ; then
    echo -n "Building ${program_name^^} ... "
    rm -rf *.mod
    ${FC} -std=f2018 -O3 -Wall \
          ${BMODS_DIR}/{{kind,math}_consts,nicelabels}.f90 ${SDL2F90} \
          ../SDL2_app.f90 ${program_name}.f90 ${LIBS} -o ${program_name}${EXE}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    echo "done."
fi

cd ../mandelzoom

program_name="mandelzoom"
if [ ! -f "${BIN_DIR}/${program_name}${EXE}" ] ; then
    echo -n "Building ${program_name^^} ... "
    rm -rf *.mod
    ${FC} -std=f2018 -O3 -Wall \
          ${BMODS_DIR}/{{kind,math}_consts,ft_timer_m,getdata,nicelabels}.f90 \
          ${SDL2F90} ../SDL2_app.f90 \
          ${program_name}.f90 ${LIBS} -o ${program_name}${EXE}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    echo "done."
fi

cd ../poisson2D_solver

program_name="poisson2D_solver"
if [ ! -f "${BIN_DIR}/${program_name}${EXE}" ] ; then
    echo -n "Building ${program_name^^} ... "
    rm -rf *.mod
    ${FC} -std=f2018 -O3 -Wall \
          ${BMODS_DIR}/{{kind,math}_consts,getdata,nicelabels}.f90 \
          ${SDL2F90} ../SDL2_{app,shading}.f90 \
          ${program_name}.f90 ${LIBS} -o ${program_name}${EXE}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    echo "done."
fi

cd ../heat1D_solver

program_name="heat1D_solver"
if [ ! -f "${BIN_DIR}/${program_name}${EXE}" ] ; then
    echo -n "Building ${program_name^^} ... "
    rm -rf *.mod
    ${FC} -std=f2018 -O3 -Wall \
          ${BMODS_DIR}/{{kind,math}_consts,additional_functions,utilities,getdata,nicelabels}.f90 ${SDL2F90} \
          ../../fparser-fortran/fparser_dp.f90 ../SDL2_{app,shading}.f90 \
          ${program_name}.f90 ${LIBS} \
          -L ../../fparser-fortran -lFParser -lstdc++ \
          -o ${program_name}${EXE}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    echo "done."
fi

cd ../complex-dynamics

program_name="complexplot"
if [ ! -f "${BIN_DIR}/${program_name}${EXE}" ] ; then
    echo -n "Building ${program_name^^} ... "
    rm -rf *.mod
    ${FC} -std=f2018 -O3 -Wall \
          ${BMODS_DIR}/{{kind,math}_consts,additional_functions,ft_timer_m,utilities,getdata,nicelabels}.f90 ${SDL2F90} \
          ../../fparser-fortran/fparser_cd.f90 ../SDL2_app.f90 \
          ${program_name}.f90 ${LIBS} \
          -L ../../fparser-fortran -lFParser -lstdc++ \
          -o ${program_name}${EXE}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    echo "done."
fi

program_name="flux_stream"
if [ ! -f "${BIN_DIR}/${program_name}${EXE}" ] ; then
    echo -n "Building ${program_name^^} ... "
    rm -rf *.mod
    ${FC} -std=f2018 -O3 -Wall \
          ${BMODS_DIR}/{{kind,math}_consts,additional_functions,ft_timer_m,utilities,getdata,nicelabels,contour_plots}.f90 ${SDL2F90} \
          ../../fparser-fortran/fparser_cd.f90 ../SDL2_app.f90 \
          ${program_name}.f90 ${LIBS} \
          -L ../../fparser-fortran -lFParser -lstdc++ \
          -o ${program_name}${EXE}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    echo "done."
fi

cd ../pendulums

program_name="double_pendulum"
if [ ! -f "${BIN_DIR}/${program_name}${EXE}" ] ; then
    echo -n "Building ${program_name^^} ... "
    rm -rf *.mod
    ${FC} -std=f2018 -O3 -Wall -Wno-unused-dummy-argument \
          ${BMODS_DIR}/{{kind,math}_consts,getdata,nicelabels}.f90 \
          ${OMODS_DIR}/{everhart,ode}_integrator.f90 \
          ${SDL2F90} ../SDL2_app.f90 \
          ${program_name}.f90 ${LIBS} -o ${program_name}${EXE}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    echo "done."
fi

cd ../lorenz

program_name="lorenz_attractor"
if [ ! -f "${BIN_DIR}/${program_name}${EXE}" ] ; then
    echo -n "Building ${program_name^^} ... "
    rm -rf *.mod
    ${FC} -std=f2018 -O3 -Wall -Wno-unused-dummy-argument \
          ${BMODS_DIR}/{{kind,math}_consts,getdata,nicelabels}.f90 \
          ${OMODS_DIR}/{everhart,ode}_integrator.f90 \
          ${SDL2F90} ../SDL2_{app,shading}.f90 \
          ${program_name}.f90 ${LIBS} -o ${program_name}${EXE}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    echo "done."
fi

cd "${NBODY_DIR}"

program_name="calc_orbits"
if [ ! -f "${BIN_DIR}/${program_name}${EXE}" ] ; then
    echo -n "Building ${program_name^^} ... "
    rm -rf *.mod
    ${FC} -std=f2018 -O3 -funroll-loops -Wall \
          -Wno-unused-dummy-argument \
          ${BMODS_DIR}/{{kind,math}_consts,ft_timer_m,getdata}.f90 \
          ${OMODS_DIR}/everhart_integrator.f90 \
          ${program_name}.f90 -o ${program_name}${EXE}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    rsync -a close_encounters.cards "${BIN_DIR}/"
    echo "done."
fi

program_name="process_orbits"
if [ ! -f "${BIN_DIR}/${program_name}${EXE}" ] ; then
    echo -n "Building ${program_name^^} ... "
    rm -rf *.mod
    ${FC} -std=f2018 -O3 -funroll-loops -Wall \
          -Wno-unused-dummy-argument ${BMODS_DIR}/{kind,math}_consts.f90 \
          ${BMODS_DIR}/{ft_timer_m,getdata,julian_dates}.f90 \
          ${program_name}.f90 -o ${program_name}${EXE}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    echo "done."
fi

program_name="test_jsunp"
if [ ! -f "${BIN_DIR}/${program_name}${EXE}" ] ; then
    echo -n "Building ${program_name^^} ... "
    rm -rf *.mod
    ${FC} -std=f2018 -O3 -funroll-loops -Wall \
          -Wno-unused-dummy-argument \
          ${BMODS_DIR}/{kind,math}_consts.f90 \
          ${OMODS_DIR}/{everhart_integrator,cernlib_integrators}.f90 \
          ${program_name}.f90 -o ${program_name}${EXE}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    echo "done."
fi

cd apophis

program_name="apophis"
if [ ! -f "${BIN_DIR}/${program_name}${EXE}" ] ; then
    echo -n "Building ${program_name^^} ... "
    rm -rf *.mod
    ${FC} -std=f2018 -O3 -funroll-loops -Wall \
          -Wno-unused-dummy-argument \
          ${BMODS_DIR}/{{kind,math}_consts,getdata,julian_dates,nicelabels,camera_view_m}.f90 \
          ${OMODS_DIR}/everhart_integrator.f90 \
          ${SDL2F90} ../../sdl2-fortran.apps/SDL2_{app,shading}.f90 \
          ${program_name}.f90 ${LIBS} -o ${program_name}${EXE}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    echo "done."
fi

cd ../three_bodies

program_name="three_bodies"
if [ ! -f "${BIN_DIR}/${program_name}${EXE}" ] ; then
    echo -n "Building ${program_name^^} ... "
    rm -rf *.mod
    ${FC} -std=f2018 -O3 -funroll-loops -Wall \
          -Wno-unused-dummy-argument \
          ${BMODS_DIR}/{{kind,math}_consts,ft_timer_m,getdata,nicelabels,camera_view_m}.f90 \
          ${OMODS_DIR}/{everhart,ode}_integrator.f90 \
          ${SDL2F90} ../../sdl2-fortran.apps/SDL2_{app,shading}.f90 \
          ${program_name}.f90 ${LIBS} -o ${program_name}${EXE}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    echo "done."
fi

cd "${ODETS_DIR}"

program_name="Euler"
if [ ! -f "${BIN_DIR}/${program_name}${EXE}" ] ; then
    echo -n "Building ${program_name^^} ... "
    rm -rf *.mod
    ${FC} -std=f2018 -O3 -funroll-loops -Wall \
          -Wno-unused-dummy-argument \
          ${BMODS_DIR}/{{kind,math}_consts,getdata}.f90 \
          ${OMODS_DIR}/{everhart,ode}_integrator.f90 \
          ${program_name}.f90 -o ${program_name}${EXE}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    echo "done."
fi

program_name="Filippi"
if [ ! -f "${BIN_DIR}/${program_name}${EXE}" ] ; then
    echo -n "Building ${program_name^^} ... "
    rm -rf *.mod
    ${FC} -std=f2018 -O3 -funroll-loops -Wall \
          -Wno-unused-dummy-argument \
          ${BMODS_DIR}/{{kind,math}_consts,getdata}.f90 \
          ${OMODS_DIR}/{everhart,ode}_integrator.f90 \
          ${program_name}.f90 -o ${program_name}${EXE}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    echo "done."
fi

program_name="decay_field"
if [ ! -f "${BIN_DIR}/${program_name}${EXE}" ] ; then
    echo -n "Building ${program_name^^} ... "
    rm -rf *.mod
    ${FC} -std=f2018 -O3 -funroll-loops -Wall \
          -Wno-unused-dummy-argument \
          ${BMODS_DIR}/kind_consts.f90 \
          ${OMODS_DIR}/{everhart,ode}_integrator.f90 \
          ${program_name}.f90 -o ${program_name}${EXE}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    echo "done."
fi

cd "${TF_DIR}"

program_name="thomas_fermi-ra15"
if [ ! -f "${BIN_DIR}/${program_name}${EXE}" ] ; then
    echo -n "Building ${program_name^^} ... "
    rm -rf *.mod
    ${FC} -std=f2018 -O3 -funroll-loops -Wall \
          -Wno-unused-dummy-argument \
          ${BMODS_DIR}/{{kind,math}_consts,ft_timer_m,getdata}.f90 \
          ${OMODS_DIR}/everhart_integrator.f90 \
          ${program_name}.f90 -o ${program_name}${EXE}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    echo "done."
fi

cd "${FPFORTRAN_DIR}"

program_name="fparser_test"
if [ ! -f "${BIN_DIR}/${program_name}${EXE}" ] ; then
    echo -n "Building ${program_name^^} ... "
    rm -rf *.mod
    ${FC} -std=f2018 -Wall ${BMODS_DIR}/{kind_consts,getdata,utilities}.f90 \
          fparser_dp.f90 fparser_cd.f90  ${program_name}.f90 \
          -L . -lFParser -lstdc++ -o ${program_name}${EXE}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    echo "done."
fi

## Windows apps

cd "${WIN32_FORTRAN_APPS_DIR}"

cd poisson2D.app

program_name="poisson2D"
if [ ! -f "${BIN_DIR}/${program_name}.exe" ] ; then
    echo -n "Building ${program_name^^} ... "
    rm -rf {*.mod,*.res}
    ${RC} ${program_name}.rc -O coff -o ${program_name}.res
    ${FC} -std=f2018 -O3 -Wall -Wall \
          -Wno-unused-dummy-argument -Wno-maybe-uninitialized \
          -static -mwindows ${BMODS_DIR}/{kind,math}_consts.f90 \
          ../{win32,{basic,about,x,xy,radio}_box_m,win32app}.f90 \
          ${program_name}.f90 ${program_name}.res -o ${program_name}.exe
    rm -rf {*.mod,*.res}
    mv ${program_name}.exe "${BIN_DIR}"
    echo "done."
fi
