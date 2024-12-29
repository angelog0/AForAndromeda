#!/bin/bash
##
## USAGE
##
##   From AForAndromeda directory:
##
##     ./build.sh
##
##   or (on macOS, for example after installing the right bash > 4.0)
##
##     FC=gfortran-mp-12 bash ./build-all.sh
##
##   We have removed the '-march=native' flag because it makes
##   binaries NOT PORTABLE across the same OS with different
##   processors. If you need it, maybe you ca run
##
##     FC='gfortran -march=native' ./build-all.sh
##
##   Then to start a program:
##
##     cd bin
##     ./program
##

: ${FC=gfortran}
: ${RC=windres}

PROGRAMMING_DIR="$(pwd)"

BIN_DIR="${PROGRAMMING_DIR}/bin"
LIB_DIR="${PROGRAMMING_DIR}/lib"
INC_DIR="${PROGRAMMING_DIR}/finclude"

FORTRANSDL2_DIR="${PROGRAMMING_DIR}/fortran-sdl2"
FORTRANSDL2_LIB="${LIB_DIR}/libfortran-sdl2.a"

BASICMODS_DIR="${PROGRAMMING_DIR}/basic_mods"
BASICMODS_LIB="${LIB_DIR}/libbasic_mods.a"

FORTRANSDL2APPS_DIR="${PROGRAMMING_DIR}/fortran-sdl2apps"
FORTRANSDL2APPS_LIB="${LIB_DIR}/libfortran-sdl2apps.a"

FPARSER452_DIR="${PROGRAMMING_DIR}/fparser-4.5.2"
FORTRANFPARSER_DIR="${PROGRAMMING_DIR}/fortran-fparser"
FORTRANFPARSER_LIB="${LIB_DIR}/libfortran-fparser.a"
FPCPP_LIB="${LIB_DIR}/libfpc++.a"

FORTRANW32_DIR="${PROGRAMMING_DIR}/fortran-win32"
FORTRANW32_LIB="${LIB_DIR}/libfortran-win32.a"
FORTRANWBOX_LIB="${LIB_DIR}/libfortran-win32box.a"
FORTRANWAPP_LIB="${LIB_DIR}/libfortran-win32app.a"

ODEMODS_DIR="${PROGRAMMING_DIR}/ode_mods"
ODEMODS_LIB="${LIB_DIR}/libode_mods.a"

if [ ! -e "${BIN_DIR}" ] ; then
    mkdir -p "${BIN_DIR}"
fi

if [ ! -e "${LIB_DIR}" ] ; then
    mkdir -p "${LIB_DIR}"
fi

if [ ! -e "${INC_DIR}" ] ; then
    mkdir -p "${INC_DIR}"
fi

## MSYS2/UCRT64 _is_ MINGW64!!!
MINGW64_OS=`uname -s | grep MINGW64`

if [ "${MINGW64_OS}" != "" ] ; then
    ## MINGW64 static build
    LIBS="-static -lmingw32 -lSDL2main -lSDL2 -lws2_32 -ldinput8 -ldxguid -ldxerr8 -luser32 -lgdi32 -lwinmm -limm32 -lole32 -loleaut32 -lshell32 -lversion -luuid -lcomdlg32 -lhid -lsetupapi"
    EXE=.exe
else
    ## GNU/Linux / macOS build
    LIBS=`sdl2-config --libs`
    EXE=.out
fi

## ------------------------------------------------------------
##  S T A R T  T H E  B U I L D  O F  T H E  L I B R A R I E S
## ------------------------------------------------------------

if [ ! -f "${BASICMODS_LIB}" ] ; then

    echo "Building ${BASICMODS_LIB##*/} ..."
    echo

    cd "${BASICMODS_DIR}"

    make FFLAGS='-Wall -std=f2018 -fmax-errors=1 -O3' all
    mv ${BASICMODS_LIB##*/} "${LIB_DIR}"
    mv *.mod "${INC_DIR}"
    make clean
    echo
fi

if [ ! -f "${ODEMODS_LIB}" ] ; then

    echo "Building ${ODEMODS_LIB##*/} ..."
    echo

    cd "${ODEMODS_DIR}"

    make FFLAGS='-Wall -std=f2018 -fmax-errors=1 -O3' all
    mv ${ODEMODS_LIB##*/} "${LIB_DIR}"
    mv *.mod "${INC_DIR}"
    make clean
    echo
fi

if [ ! -f "${FORTRANW32_LIB}" ] || [ ! -f "${FORTRANWBOX_LIB}" ] || [ ! -f "${FORTRANWAPP_LIB}" ]; then

    echo "Building ${FORTRANW32_LIB##*/}, ${FORTRANWBOX_LIB##*/} and ${FORTRANWAPP_LIB##*/} ..."
    echo

    cd "${FORTRANW32_DIR}"

    make FFLAGS='-Wall -std=f2018 -fmax-errors=1 -O3' all
    mv *.a "${LIB_DIR}"
    mv *.mod "${INC_DIR}"
    make clean
    echo
fi

## ${FORTRANSDL2_DIR##*/} and friends: remove the longest string "*/" from the
## front of ${FORTRANSDL2_DIR}
if [ ! -f "${FORTRANSDL2_LIB}" ] ; then

    echo "Building ${FORTRANSDL2_LIB##*/} ..."
    echo

    if [ ! -e "${FORTRANSDL2_DIR}" ] ; then
        echo
        echo -n "Cloning ${FORTRANSDL2_DIR##*/} ... "

        cd "${PROGRAMMING_DIR}"

        git clone -q https://github.com/interkosmos/${FORTRANSDL2_DIR##*/}.git

        echo "done."
        echo
    fi

    cd "${FORTRANSDL2_DIR}"

    make FFLAGS='-Wall -std=f2018 -fmax-errors=1 $(SDL_CFLAGS) -O3' all
    mv ${FORTRANSDL2_LIB##*/} "${LIB_DIR}"
    mv c_util.mod glu.mod sdl2*.mod "${INC_DIR}"
    make clean
    echo
fi

if [ ! -f "${FORTRANSDL2APPS_LIB}" ] ; then

    echo "Building ${FORTRANSDL2APPS_LIB##*/} ..."
    echo

    cd "${FORTRANSDL2APPS_DIR}"

    make FFLAGS='-Wall -std=f2018 -fmax-errors=1 -O3' all
    mv ${FORTRANSDL2APPS_LIB##*/} "${LIB_DIR}"
    mv *.mod "${INC_DIR}"
    make clean
    echo
fi

if [ ! -f "${FORTRANFPARSER_LIB}" ] || [ ! -f "${FPCPP_LIB}" ]; then

    echo "Building ${FORTRANFPARSER_LIB##*/} and ${FPCPP_LIB##*/} ..."
    echo

    if [ ! -e "${FPARSER452_DIR}" ] ; then
        echo
        echo -n "Downloading fparser4.5.2.zip ... "

        cd "${PROGRAMMING_DIR}"

        wget -q http://warp.povusers.org/FunctionParser/fparser4.5.2.zip
        aunpack -q fparser4.5.2.zip -X fparser-4.5.2/
        rm -rf fparser4.5.2.zip

        echo "done."
        echo
    fi

    cd "${FORTRANFPARSER_DIR}"

    make FFLAGS='-Wall -std=f2018 -fmax-errors=1 -O3' all
    mv *.a "${LIB_DIR}"
    mv *.mod "${INC_DIR}"
    make clean
    echo
fi

## ----------------------------------------------------------
##  S T A R T  T H E  B U I L D  O F  T H E  P R O G R A M S
## ----------------------------------------------------------

cd "${PROGRAMMING_DIR}/apophis"

program_name="apophis"
if [ ! -f "${BIN_DIR}/${program_name}${EXE}" ] ; then
    ## Just an alternative (almost) is
    ##
    ##   echo -n "Building ${program_name} ... " | tr '[:lower:]' '[:upper:]'
    ##
    ## because the following does NOT work OB on macOS
    echo -n "Building ${program_name^^} ... "
    rm -rf *.mod
    ${FC} -std=f2018 -O3 -Wall -Wno-unused-dummy-argument -I ../finclude ${program_name}.f90 -o ${program_name}${EXE} -L ../lib -lbasic_mods -lode_mods -lfortran-sdl2apps -lfortran-sdl2 ${LIBS}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    echo "done."
fi

## ----------------------------------------------------------

cd "${PROGRAMMING_DIR}/black_hole"

program_name="black_hole"
if [ ! -f "${BIN_DIR}/${program_name}${EXE}" ] ; then
    ## Just an alternative (almost) is
    ##
    ##   echo -n "Building ${program_name} ... " | tr '[:lower:]' '[:upper:]'
    ##
    ## because the following does NOT work OB on macOS
    echo -n "Building ${program_name^^} ... "
    rm -rf *.mod
    ${FC} -std=f2018 -O3 -Wall -Wno-unused-dummy-argument -I ../finclude ${program_name}.f90 -o ${program_name}${EXE} -L ../lib -lbasic_mods -lfortran-sdl2apps -lfortran-sdl2 ${LIBS}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    echo "done."
fi

## ----------------------------------------------------------

cd "${PROGRAMMING_DIR}/brkmtlsk"

program_name="brooks_matelski"
if [ ! -f "${BIN_DIR}/${program_name}${EXE}" ] ; then
    ## Just an alternative (almost) is
    ##
    ##   echo -n "Building ${program_name} ... " | tr '[:lower:]' '[:upper:]'
    ##
    ## because the following does NOT work OB on macOS
    echo -n "Building ${program_name^^} ... "
    rm -rf *.mod
    ## We do not need _all this_ but we use it to get a static build
    ${FC} -std=f2018 -O3 -Wall ${program_name}.f90 -o ${program_name}${EXE} ${LIBS}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    echo "done."
fi

program_name="brkmtlsk_calc"
if [ ! -f "${BIN_DIR}/${program_name}${EXE}" ] ; then
    ## Just an alternative (almost) is
    ##
    ##   echo -n "Building ${program_name} ... " | tr '[:lower:]' '[:upper:]'
    ##
    ## because the following does NOT work OB on macOS
    echo -n "Building ${program_name^^} ... "
    rm -rf *.mod
    ## We do not need _all this_ but we use it to get a static build
    ${FC} -std=f2018 -O3 -Wall -I ../finclude ${program_name}.f90 -o ${program_name}${EXE} -L ../lib -lbasic_mods ${LIBS}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    echo "done."
fi

program_name="display_brkmtlsk"
if [ ! -f "${BIN_DIR}/${program_name}${EXE}" ] ; then
    ## Just an alternative (almost) is
    ##
    ##   echo -n "Building ${program_name} ... " | tr '[:lower:]' '[:upper:]'
    ##
    ## because the following does NOT work OB on macOS
    echo -n "Building ${program_name^^} ... "
    rm -rf *.mod
    ${FC} -std=f2018 -O3 -Wall -I ../finclude ${program_name}.f90 -o ${program_name}${EXE} -L ../lib -lbasic_mods -lfortran-sdl2apps -lfortran-sdl2 ${LIBS}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    echo "done."
fi

## ----------------------------------------------------------

cd "${PROGRAMMING_DIR}/circle_3p"

program_name="circle_3p"
if [ ! -f "${BIN_DIR}/${program_name}${EXE}" ] ; then
    ## Just an alternative (almost) is
    ##
    ##   echo -n "Building ${program_name} ... " | tr '[:lower:]' '[:upper:]'
    ##
    ## because the following does NOT work OB on macOS
    echo -n "Building ${program_name^^} ... "
    rm -rf *.mod
    ${FC} -std=f2018 -O3 -Wall -I ../finclude ${program_name}.f90 -o ${program_name}${EXE} -L ../lib -lbasic_mods -lfortran-sdl2apps -lfortran-sdl2 ${LIBS}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    echo "done."
fi

## ----------------------------------------------------------

cd "${PROGRAMMING_DIR}/close_encounters"

program_name="calc_orbits"
if [ ! -f "${BIN_DIR}/${program_name}${EXE}" ] ; then
    ## Just an alternative (almost) is
    ##
    ##   echo -n "Building ${program_name} ... " | tr '[:lower:]' '[:upper:]'
    ##
    ## because the following does NOT work OB on macOS
    echo -n "Building ${program_name^^} ... "
    rm -rf *.mod
    ${FC} -std=f2018 -O3 -Wall -Wno-unused-dummy-argument -I ../finclude ${program_name}.f90 -o ${program_name}${EXE} -L ../lib -lbasic_mods -lode_mods ${LIBS}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    echo "done."
fi

program_name="process_orbits"
if [ ! -f "${BIN_DIR}/${program_name}${EXE}" ] ; then
    ## Just an alternative (almost) is
    ##
    ##   echo -n "Building ${program_name} ... " | tr '[:lower:]' '[:upper:]'
    ##
    ## because the following does NOT work OB on macOS
    echo -n "Building ${program_name^^} ... "
    rm -rf *.mod
    ${FC} -std=f2018 -O3 -Wall -I ../finclude ${program_name}.f90 -o ${program_name}${EXE} -L ../lib -lbasic_mods ${LIBS}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    echo "done."
fi

program_name="test_jsunp"
if [ ! -f "${BIN_DIR}/${program_name}${EXE}" ] ; then
    ## Just an alternative (almost) is
    ##
    ##   echo -n "Building ${program_name} ... " | tr '[:lower:]' '[:upper:]'
    ##
    ## because the following does NOT work OB on macOS
    echo -n "Building ${program_name^^} ... "
    rm -rf *.mod
    ${FC} -std=f2018 -O3 -Wall -Wno-unused-dummy-argument -I ../finclude ${program_name}.f90 -o ${program_name}${EXE} -L ../lib -lbasic_mods -lode_mods ${LIBS}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    echo "done."
fi

## ----------------------------------------------------------

cd "${PROGRAMMING_DIR}/complex-dynamics"

program_name="complexplot"
if [ ! -f "${BIN_DIR}/${program_name}${EXE}" ] ; then
    ## Just an alternative (almost) is
    ##
    ##   echo -n "Building ${program_name} ... " | tr '[:lower:]' '[:upper:]'
    ##
    ## because the following does NOT work OB on macOS
    echo -n "Building ${program_name^^} ... "
    rm -rf *.mod
    ${FC} -std=f2018 -O3 -Wall -I ../finclude ${program_name}.f90 -o ${program_name}${EXE} -L ../lib -lfortran-fparser -lbasic_mods -lfortran-sdl2apps -lfortran-sdl2 -lfpc++ -lstdc++ ${LIBS}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    echo "done."
fi

program_name="flux_stream"
if [ ! -f "${BIN_DIR}/${program_name}${EXE}" ] ; then
    ## Just an alternative (almost) is
    ##
    ##   echo -n "Building ${program_name} ... " | tr '[:lower:]' '[:upper:]'
    ##
    ## because the following does NOT work OB on macOS
    echo -n "Building ${program_name^^} ... "
    rm -rf *.mod
    ${FC} -std=f2018 -O3 -Wall -I ../finclude ${program_name}.f90 -o ${program_name}${EXE} -L ../lib -lfortran-fparser -lbasic_mods -lfortran-sdl2apps -lfortran-sdl2 -lfpc++ -lstdc++ ${LIBS}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    echo "done."
fi

## ----------------------------------------------------------

cd "${PROGRAMMING_DIR}/double_pendulum"

program_name="double_pendulum"
if [ ! -f "${BIN_DIR}/${program_name}${EXE}" ] ; then
    ## Just an alternative (almost) is
    ##
    ##   echo -n "Building ${program_name} ... " | tr '[:lower:]' '[:upper:]'
    ##
    ## because the following does NOT work OB on macOS
    echo -n "Building ${program_name^^} ... "
    rm -rf *.mod
    ${FC} -std=f2018 -O3 -Wall -Wno-unused-dummy-argument -I ../finclude ${program_name}.f90 -o ${program_name}${EXE} -L ../lib -lbasic_mods -lode_mods -lfortran-sdl2apps -lfortran-sdl2 ${LIBS}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    echo "done."
fi

## ----------------------------------------------------------

cd "${PROGRAMMING_DIR}/forces"

program_name="forcesfp"
if [ ! -f "${BIN_DIR}/${program_name}${EXE}" ] ; then
    ## Just an alternative (almost) is
    ##
    ##   echo -n "Building ${program_name} ... " | tr '[:lower:]' '[:upper:]'
    ##
    ## because the following does NOT work OB on macOS
    echo -n "Building ${program_name^^} ... "
    rm -rf *.mod
    ${FC} -std=f2018 -O3 -Wall -I ../finclude ${program_name}.f90 -o ${program_name}${EXE} -L ../lib -lfortran-fparser -lbasic_mods -lfortran-sdl2apps -lfortran-sdl2 -lfpc++ -lstdc++ ${LIBS}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    echo "done."
fi

## ----------------------------------------------------------

cd "${PROGRAMMING_DIR}/foucault"

program_name="foucault"
if [ ! -f "${BIN_DIR}/${program_name}${EXE}" ] ; then
    ## Just an alternative (almost) is
    ##
    ##   echo -n "Building ${program_name} ... " | tr '[:lower:]' '[:upper:]'
    ##
    ## because the following does NOT work OB on macOS
    echo -n "Building ${program_name^^} ... "
    rm -rf *.mod
    ${FC} -std=f2018 -O3 -I ../finclude ${program_name}.f90 -o ${program_name}${EXE} -L ../lib -lbasic_mods -lfortran-sdl2apps -lfortran-sdl2 ${LIBS}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    echo "done."
fi

## ----------------------------------------------------------

cd "${PROGRAMMING_DIR}/heat1D_solver"

program_name="heat1D_solver"
if [ ! -f "${BIN_DIR}/${program_name}${EXE}" ] ; then
    ## Just an alternative (almost) is
    ##
    ##   echo -n "Building ${program_name} ... " | tr '[:lower:]' '[:upper:]'
    ##
    ## because the following does NOT work OB on macOS
    echo -n "Building ${program_name^^} ... "
    rm -rf *.mod
    ${FC} -std=f2018 -O3 -Wall -I ../finclude ${program_name}.f90 -o ${program_name}${EXE} -L ../lib -lfortran-fparser -lbasic_mods -lfortran-sdl2apps -lfortran-sdl2 -lfpc++ -lstdc++ ${LIBS}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    echo "done."
fi

## ----------------------------------------------------------

cd "${PROGRAMMING_DIR}/joke"

program_name="bouncing"
if [ ! -f "${BIN_DIR}/${program_name}${EXE}" ] ; then
    ## Just an alternative (almost) is
    ##
    ##   echo -n "Building ${program_name} ... " | tr '[:lower:]' '[:upper:]'
    ##
    ## because the following does NOT work OB on macOS
    echo -n "Building ${program_name^^} ... "
    rm -rf *.mod
    ${FC} -std=f2018 -O3 -Wall -I ../finclude ${program_name}.f90 -o ${program_name}${EXE} -L ../lib -lbasic_mods -lfortran-sdl2apps -lfortran-sdl2 ${LIBS}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    echo "done."
fi

program_name="colour_map"
if [ ! -f "${BIN_DIR}/${program_name}${EXE}" ] ; then
    ## Just an alternative (almost) is
    ##
    ##   echo -n "Building ${program_name} ... " | tr '[:lower:]' '[:upper:]'
    ##
    ## because the following does NOT work OB on macOS
    echo -n "Building ${program_name^^} ... "
    rm -rf *.mod
    ${FC} -std=f2018 -O3 -Wall -I ../finclude ${program_name}.f90 -o ${program_name}${EXE} -L ../lib -lbasic_mods -lfortran-sdl2apps -lfortran-sdl2 ${LIBS}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    echo "done."
fi

program_name="display_ppm"
if [ ! -f "${BIN_DIR}/${program_name}${EXE}" ] ; then
    ## Just an alternative (almost) is
    ##
    ##   echo -n "Building ${program_name} ... " | tr '[:lower:]' '[:upper:]'
    ##
    ## because the following does NOT work OB on macOS
    echo -n "Building ${program_name^^} ... "
    rm -rf *.mod
    ${FC} -std=f2018 -O3 -Wall -I ../finclude ${program_name}.f90 -o ${program_name}${EXE} -L ../lib -lfortran-sdl2apps -lfortran-sdl2 ${LIBS}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    echo "done."
fi

program_name="koch_snowflake"
if [ ! -f "${BIN_DIR}/${program_name}${EXE}" ] ; then
    ## Just an alternative (almost) is
    ##
    ##   echo -n "Building ${program_name} ... " | tr '[:lower:]' '[:upper:]'
    ##
    ## because the following does NOT work OB on macOS
    echo -n "Building ${program_name^^} ... "
    rm -rf *.mod
    ${FC} -std=f2018 -O3 -Wall -I ../finclude ${program_name}.f90 -o ${program_name}${EXE} -L ../lib -lbasic_mods -lfortran-sdl2apps -lfortran-sdl2 ${LIBS}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    echo "done."
fi

program_name="plot_ellipse"
if [ ! -f "${BIN_DIR}/${program_name}${EXE}" ] ; then
    ## Just an alternative (almost) is
    ##
    ##   echo -n "Building ${program_name} ... " | tr '[:lower:]' '[:upper:]'
    ##
    ## because the following does NOT work OB on macOS
    echo -n "Building ${program_name^^} ... "
    rm -rf *.mod
    ${FC} -std=f2018 -O3 -Wall -I ../finclude ${program_name}.f90 -o ${program_name}${EXE} -L ../lib -lbasic_mods -lfortran-sdl2apps -lfortran-sdl2 ${LIBS}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    echo "done."
fi

## ----------------------------------------------------------

cd "${PROGRAMMING_DIR}/lorenz_attractor"

program_name="lorenz_attractor"
if [ ! -f "${BIN_DIR}/${program_name}${EXE}" ] ; then
    ## Just an alternative (almost) is
    ##
    ##   echo -n "Building ${program_name} ... " | tr '[:lower:]' '[:upper:]'
    ##
    ## because the following does NOT work OB on macOS
    echo -n "Building ${program_name^^} ... "
    rm -rf *.mod
    ${FC} -std=f2018 -O3 -Wall -Wno-unused-dummy-argument -I ../finclude ${program_name}.f90 -o ${program_name}${EXE} -L ../lib -lbasic_mods -lode_mods -lfortran-sdl2apps -lfortran-sdl2 ${LIBS}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    echo "done."
fi

## ----------------------------------------------------------

cd "${PROGRAMMING_DIR}/mandelzoom"

program_name="mandelzoom"
if [ ! -f "${BIN_DIR}/${program_name}${EXE}" ] ; then
    ## Just an alternative (almost) is
    ##
    ##   echo -n "Building ${program_name} ... " | tr '[:lower:]' '[:upper:]'
    ##
    ## because the following does NOT work OB on macOS
    echo -n "Building ${program_name^^} ... "
    rm -rf *.mod
    ${FC} -std=f2018 -O3 -Wall -I ../finclude ${program_name}.f90 -o ${program_name}${EXE} -L ../lib -lbasic_mods -lfortran-sdl2apps -lfortran-sdl2 ${LIBS}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    echo "done."
fi

## ----------------------------------------------------------

cd "${PROGRAMMING_DIR}/maps_attractors"

program_name="henon_map"
if [ ! -f "${BIN_DIR}/${program_name}${EXE}" ] ; then
    ## Just an alternative (almost) is
    ##
    ##   echo -n "Building ${program_name} ... " | tr '[:lower:]' '[:upper:]'
    ##
    ## because the following does NOT work OB on macOS
    echo -n "Building ${program_name^^} ... "
    rm -rf *.mod
    ${FC} -std=f2018 -O3 -Wall -I ../finclude ${program_name}.f90 -o ${program_name}${EXE} -L ../lib -lbasic_mods -lfortran-sdl2apps -lfortran-sdl2 ${LIBS}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    echo "done."
fi

program_name="logistic_map"
if [ ! -f "${BIN_DIR}/${program_name}${EXE}" ] ; then
    ## Just an alternative (almost) is
    ##
    ##   echo -n "Building ${program_name} ... " | tr '[:lower:]' '[:upper:]'
    ##
    ## because the following does NOT work OB on macOS
    echo -n "Building ${program_name^^} ... "
    rm -rf *.mod
    ${FC} -std=f2018 -O3 -Wall -I ../finclude ${program_name}.f90 -o ${program_name}${EXE} -L ../lib -lbasic_mods -lfortran-sdl2apps -lfortran-sdl2 ${LIBS}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    echo "done."
fi

## ----------------------------------------------------------

cd "${PROGRAMMING_DIR}/math_test"

program_name="cbrt_test"
if [ ! -f "${BIN_DIR}/${program_name}${EXE}" ] ; then
    ## Just an alternative (almost) is
    ##
    ##   echo -n "Building ${program_name} ... " | tr '[:lower:]' '[:upper:]'
    ##
    ## because the following does NOT work OB on macOS
    echo -n "Building ${program_name^^} ... "
    rm -rf *.mod
    ## We do not need _all this_ but we use it to get a static build
    ${FC} -std=f2018 -O3 -Wall ${program_name}.f90 -o ${program_name}${EXE} ${LIBS}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    echo "done."
fi

program_name="plnml_test"
if [ ! -f "${BIN_DIR}/${program_name}${EXE}" ] ; then
    ## Just an alternative (almost) is
    ##
    ##   echo -n "Building ${program_name} ... " | tr '[:lower:]' '[:upper:]'
    ##
    ## because the following does NOT work OB on macOS
    echo -n "Building ${program_name^^} ... "
    rm -rf *.mod
    ## We do not need _all this_ but we use it to get a static build
    ${FC} -std=f2018 -O3 -Wall ${program_name}.f90 -o ${program_name}${EXE} ${LIBS}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    echo "done."
fi

program_name="solve_equation"
if [ ! -f "${BIN_DIR}/${program_name}${EXE}" ] ; then
    ## Just an alternative (almost) is
    ##
    ##   echo -n "Building ${program_name} ... " | tr '[:lower:]' '[:upper:]'
    ##
    ## because the following does NOT work OB on macOS
    echo -n "Building ${program_name^^} ... "
    rm -rf *.mod
    ## We do not need _all this_ but we use it to get a static build
    ${FC} -std=f2018 -O3 -Wall -Wno-unused-function ${program_name}.f90 -o ${program_name}${EXE} ${LIBS}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    echo "done."
fi

## ----------------------------------------------------------

cd "${PROGRAMMING_DIR}/poisson2D_solver"

program_name="poisson2D_solver"
if [ ! -f "${BIN_DIR}/${program_name}${EXE}" ] ; then
    ## Just an alternative (almost) is
    ##
    ##   echo -n "Building ${program_name} ... " | tr '[:lower:]' '[:upper:]'
    ##
    ## because the following does NOT work OB on macOS
    echo -n "Building ${program_name^^} ... "
    rm -rf *.mod
    ${FC} -std=f2018 -O3 -Wall -I ../finclude ${program_name}.f90 -o ${program_name}${EXE} -L ../lib -lbasic_mods -lfortran-sdl2apps -lfortran-sdl2 ${LIBS}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    echo "done."
fi

## ----------------------------------------------------------

cd "${PROGRAMMING_DIR}/poisson2D-win32"

program_name="poisson2D_win32"
if [ ! -f "${BIN_DIR}/${program_name}${EXE}" ] ; then
    ## Just an alternative (almost) is
    ##
    ##   echo -n "Building ${program_name} ... " | tr '[:lower:]' '[:upper:]'
    ##
    ## because the following does NOT work OB on macOS
    echo -n "Building ${program_name^^} ... "
    rm -rf {*.mod,*.res}
    ${RC} ${program_name}.rc -O coff -o ${program_name}.res
    ${FC} -std=f2018 -O3 -Wall -Wno-unused-dummy-argument -Wno-maybe-uninitialized -I ../finclude -static -mwindows ${program_name}.f90 -o ${program_name}${EXE} ${program_name}.res -L ../lib -lbasic_mods -lfortran-win32box -lfortran-win32app -lfortran-win32
    rm -rf {*.mod,*.res}
    mv ${program_name}${EXE} "${BIN_DIR}"
    echo "done."
fi

## ----------------------------------------------------------

cd "${PROGRAMMING_DIR}/star_walk"

program_name="star_walk"
if [ ! -f "${BIN_DIR}/${program_name}${EXE}" ] ; then
    ## Just an alternative (almost) is
    ##
    ##   echo -n "Building ${program_name} ... " | tr '[:lower:]' '[:upper:]'
    ##
    ## because the following does NOT work OB on macOS
    echo -n "Building ${program_name^^} ... "
    rm -rf *.mod
    ${FC} -std=f2018 -O3 -Wall -I ../finclude ${program_name}.f90 -o ${program_name}${EXE} -L ../lib -lbasic_mods -lfortran-sdl2apps -lfortran-sdl2 ${LIBS}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    echo "done."
fi

## ----------------------------------------------------------

cd "${PROGRAMMING_DIR}/thomas-fermi"

program_name="thomas_fermi"
if [ ! -f "${BIN_DIR}/${program_name}${EXE}" ] ; then
    ## Just an alternative (almost) is
    ##
    ##   echo -n "Building ${program_name} ... " | tr '[:lower:]' '[:upper:]'
    ##
    ## because the following does NOT work OB on macOS
    echo -n "Building ${program_name^^} ... "
    rm -rf *.mod
    ${FC} -std=f2018 -O3 -Wall -I ../finclude ${program_name}.f90 -o ${program_name}${EXE} -L ../lib -lbasic_mods -lode_mods ${LIBS}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    echo "done."
fi

## ----------------------------------------------------------

cd "${PROGRAMMING_DIR}/three_body_problem"

program_name="three_body_problem"
if [ ! -f "${BIN_DIR}/${program_name}${EXE}" ] ; then
    ## Just an alternative (almost) is
    ##
    ##   echo -n "Building ${program_name} ... " | tr '[:lower:]' '[:upper:]'
    ##
    ## because the following does NOT work OB on macOS
    echo -n "Building ${program_name^^} ... "
    rm -rf *.mod
    ${FC} -std=f2018 -O3 -Wall -Wno-unused-dummy-argument -I ../finclude ${program_name}.f90 -o ${program_name}${EXE} -L ../lib -lbasic_mods -lode_mods -lfortran-sdl2apps -lfortran-sdl2 ${LIBS}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    echo "done."
fi
