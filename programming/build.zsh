#!/usr/bin/env zsh

## Disable the “no match” error for globs so that rm -rf *.mod does
##   not complain. You need to add it to the ZSH scripts. Suggested by
##   duck.ai
setopt +o nomatch

##
## USAGE
##
##   From AForAndromeda/programming directory:
##
##     ./build.zsh
##
##   We have removed the '-march=native' flag because it makes
##   binaries NOT PORTABLE across the same OS with different
##   processors. If you need it, maybe you ca run
##
##     FC='gfortran -march=native' ./build.zsh
##
##   On macOS+Homebrew+zsh:
##
##     CXX=g++-16 CC=gcc-16 ./build.zsh
##
##   Then to start a program:
##
##     cd bin
##     ./program
##

: ${FC:='gfortran'}
: ${CXX:='g++'}
: ${RC:='windres'}

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

FMZM_DIR="${PROGRAMMING_DIR}/FM_files"
FMZM_LIB="${LIB_DIR}/libfm.a"

## Create dirs if missing
[[ ! -e "${BIN_DIR}" ]] && mkdir -p "${BIN_DIR}"
[[ ! -e "${LIB_DIR}" ]] && mkdir -p "${LIB_DIR}"
[[ ! -e "${INC_DIR}" ]] && mkdir -p "${INC_DIR}"

## MSYS2/UCRT64 is MINGW64!!!
MINGW64_OS="$(uname -s | grep MINGW64 || true)"

DARWIN_OS="$(uname -s | grep Darwin || true)"

## For GNU/Linux
LIBGL=`pkg-config --libs --cflags gl`
LIBGLU=`pkg-config --libs --cflags glu`

if [[ -n "${MINGW64_OS}" ]]; then
    LIBGL=-lopengl32
    LIBGLU=-lglu32

    ## MINGW64 static build
    LIBS=( -static -lmingw32 -lSDL2main -lSDL2 -lws2_32 -ldinput8 -ldxguid -ldxerr8 -luser32 -lgdi32 -lwinmm -limm32 -lole32 -loleaut32 -lshell32 -lversion -luuid -lcomdlg32 -lhid -lsetupapi )
    EXE=.exe
else
    if [[ -n "${DARWIN_OS}" ]]; then
        LIBGL="-framework OpenGL"
        LIBGLU=
    fi

    ## GNU/Linux / macOS build. LIBGL, LIBGLU already defined for GNU/Linux
    LIBS=( $(sdl2-config --libs 2>/dev/null || true) )
    EXE=.out
fi

## ------------------------------------------------------------
##  S T A R T  T H E  B U I L D  O F  T H E  L I B R A R I E S
## ------------------------------------------------------------

if [[ ! -f "${BASICMODS_LIB}" ]]; then
    print -r -- "Building ${${BASICMODS_LIB##*/}} ..."
    print

    cd "${BASICMODS_DIR}" || exit 1

    make CXX=${CXX} FC=${FC} FFLAGS='-Wall -std=f2023 -fmax-errors=1 -O3' all
    mv ${BASICMODS_LIB##*/} "${LIB_DIR}"
    mv -- *.mod "${INC_DIR}" 2>/dev/null || true
    make clean
    print
fi

if [[ ! -f "${ODEMODS_LIB}" ]]; then
    print -r -- "Building ${${ODEMODS_LIB##*/}} ..."
    print

    cd "${ODEMODS_DIR}" || exit 1

    make CXX=${CXX} FC=${FC} FFLAGS='-Wall -std=f2023 -fmax-errors=1 -O3' all
    mv ${ODEMODS_LIB##*/} "${LIB_DIR}"
    mv -- *.mod "${INC_DIR}" 2>/dev/null || true
    make clean
    print
fi

## Build Windows apps only on MINGW64/UCRT64
if [[ -n "${MINGW64_OS}" ]]; then
    if [[ ! -f "${FORTRANW32_LIB}" ]] || [[ ! -f "${FORTRANWBOX_LIB}" ]] || [[ ! -f "${FORTRANWAPP_LIB}" ]]; then

        print -r -- "Building ${${FORTRANW32_LIB##*/}}, ${${FORTRANWBOX_LIB##*/}} and ${${FORTRANWAPP_LIB##*/}} ..."
        print

        cd "${FORTRANW32_DIR}" || exit 1

        make CXX=${CXX} FC=${FC} FFLAGS='-Wall -std=f2023 -fmax-errors=1 -O3' all
        mv -- *.a "${LIB_DIR}" 2>/dev/null || true
        mv -- *.mod "${INC_DIR}" 2>/dev/null || true
        make clean
        print
    fi
fi

## ${FORTRANSDL2_DIR##*/} and friends: remove the longest string "*/" from the
## front of ${FORTRANSDL2_DIR}
if [[ ! -f "${FORTRANSDL2_LIB}" ]]; then

    print -r -- "Building ${${FORTRANSDL2_LIB##*/}} ..."
    print

    if [[ ! -e "${FORTRANSDL2_DIR}" ]]; then
        print
        print -n -- "Cloning ${FORTRANSDL2_DIR##*/} ... "

        cd "${PROGRAMMING_DIR}" || exit 1

        git clone -q https://github.com/interkosmos/${FORTRANSDL2_DIR##*/}.git

        print "done."
        print
    fi

    cd "${FORTRANSDL2_DIR}" || exit 1

    make CXX=${CXX} FC=${FC} LIBGL=${LIBGL} LIBGLU=${LIBGLU} FFLAGS='-Wall -Wno-character-truncation -Wno-unused-function -std=f2023 -fmax-errors=1 `sdl2-config --cflags` -O3' all

    mv ${FORTRANSDL2_LIB##*/} "${LIB_DIR}"
    mv -- glu.mod sdl2.mod "${INC_DIR}" 2>/dev/null || true
    make clean
    print
fi

if [[ ! -f "${FORTRANSDL2APPS_LIB}" ]]; then

    print -r -- "Building ${${FORTRANSDL2APPS_LIB##*/}} ..."
    print

    cd "${FORTRANSDL2APPS_DIR}" || exit 1

    make CXX=${CXX} FC=${FC} FFLAGS='-Wall -std=f2023 -fmax-errors=1 -O3' all
    mv ${FORTRANSDL2APPS_LIB##*/} "${LIB_DIR}"
    mv -- *.mod "${INC_DIR}" 2>/dev/null || true
    make clean
    print
fi

if [[ ! -f "${FORTRANFPARSER_LIB}" ]] || [[ ! -f "${FPCPP_LIB}" ]]; then

    print -r -- "Building ${${FORTRANFPARSER_LIB##*/}} and ${${FPCPP_LIB##*/}} ..."
    print

    if [[ ! -e "${FPARSER452_DIR}" ]]; then
        print
        print -n -- "Downloading fparser4.5.2.zip ... "

        cd "${PROGRAMMING_DIR}" || exit 1

        wget -q http://warp.povusers.org/FunctionParser/fparser4.5.2.zip
        aunpack -q fparser4.5.2.zip -X fparser-4.5.2/
        rm -rf fparser4.5.2.zip

        print "done."
        print
    fi

    cd "${FORTRANFPARSER_DIR}" || exit 1

    make CXX=${CXX} FC=${FC} FFLAGS='-Wall -std=f2023 -fmax-errors=1 -O3' all
    mv -- *.a "${LIB_DIR}" 2>/dev/null || true
    mv -- *.mod "${INC_DIR}" 2>/dev/null || true
    make clean
    print
fi

if [[ ! -f "${FMZM_LIB}" ]]; then

    print -r -- "Building ${${FMZM_LIB##*/}} ..."
    print

    if [[ ! -e "${FMZM_DIR}" ]]; then
        print
        print -n -- "Downloading FM_files.zip ... "

        cd "${PROGRAMMING_DIR}" || exit 1

        wget -q https://dmsmith.lmu.build/FM1.4/FM_files.zip
        aunpack -q FM_files.zip -X FM.tmp/
        mv FM.tmp/FM_files .
        rm -rf FM.tmp FM_files.zip

        print "done."
        print
    fi

    print
    print -n -- "Wait a moment ... "

    cd "${FMZM_DIR}" || exit 1

    ${FC} -O3 -I./ -o fmsave.o -c fmsave.f95
    ${FC} -O3 -I./ -o fm.o -c fm.f95
    ${FC} -O3 -I./ -o fmzm90.o -c fmzm90.f95
    ar rcs libfm.a fmsave.o fmzm90.o fm.o
    print "done."
    print
    mv libfm.a "${LIB_DIR}"
    mv -- fmzm.mod "${INC_DIR}" 2>/dev/null || true
    make -s clean
    print
fi

## ----------------------------------------------------------
##  S T A R T  T H E  B U I L D  O F  T H E  P R O G R A M S
## ----------------------------------------------------------

cd "${PROGRAMMING_DIR}/apophis" || exit 1

program_name="apophis"
if [[ ! -f "${BIN_DIR}/${program_name}${EXE}" ]]; then
    print -n -- "Building ${program_name:u} ... "
    rm -rf *.mod
    ${FC} -std=f2023 -O3 -Wall -Wno-unused-dummy-argument -I ../finclude ${program_name}.f90 -o ${program_name}${EXE} -L ../lib -lbasic_mods -lode_mods -lfortran-sdl2apps -lfortran-sdl2 ${LIBS[@]}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    print "done."
fi

## ----------------------------------------------------------

cd "${PROGRAMMING_DIR}/black_hole" || exit 1

program_name="black_hole"
if [[ ! -f "${BIN_DIR}/${program_name}${EXE}" ]]; then
    print -n -- "Building ${program_name:u} ... "
    rm -rf *.mod
    ${FC} -std=f2023 -O3 -Wall -Wno-unused-dummy-argument -I ../finclude ${program_name}.f90 -o ${program_name}${EXE} -L ../lib -lbasic_mods -lfortran-sdl2apps -lfortran-sdl2 ${LIBS[@]}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    print "done."
fi

## ----------------------------------------------------------

cd "${PROGRAMMING_DIR}/brkmtlsk" || exit 1

program_name="brooks_matelski"
if [[ ! -f "${BIN_DIR}/${program_name}${EXE}" ]]; then
    print -n -- "Building ${program_name:u} ... "
    rm -rf *.mod
    ## We do not need _all this_ but we use it to get a static build
    ${FC} -std=f2023 -O3 -Wall ${program_name}.f90 -o ${program_name}${EXE} ${LIBS[@]}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    print "done."
fi

program_name="brkmtlsk_calc"
if [[ ! -f "${BIN_DIR}/${program_name}${EXE}" ]]; then
    print -n -- "Building ${program_name:u} ... "
    rm -rf *.mod
    ## We do not need _all this_ but we use it to get a static build
    ${FC} -std=f2023 -O3 -Wall -I ../finclude ${program_name}.f90 -o ${program_name}${EXE} -L ../lib -lbasic_mods ${LIBS[@]}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    print "done."
fi

program_name="display_brkmtlsk"
if [[ ! -f "${BIN_DIR}/${program_name}${EXE}" ]]; then
    print -n -- "Building ${program_name:u} ... "
    rm -rf *.mod
    ${FC} -std=f2023 -O3 -Wall -I ../finclude ${program_name}.f90 -o ${program_name}${EXE} -L ../lib -lbasic_mods -lfortran-sdl2apps -lfortran-sdl2 ${LIBS[@]}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    print "done."
fi

## ----------------------------------------------------------

cd "${PROGRAMMING_DIR}/circle_3p" || exit 1

program_name="circle_3p"
if [[ ! -f "${BIN_DIR}/${program_name}${EXE}" ]]; then
    print -n -- "Building ${program_name:u} ... "
    rm -rf *.mod
    ${FC} -std=f2023 -O3 -Wall -I ../finclude ${program_name}.f90 -o ${program_name}${EXE} -L ../lib -lbasic_mods -lfortran-sdl2apps -lfortran-sdl2 ${LIBS[@]}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    print "done."
fi

## ----------------------------------------------------------

cd "${PROGRAMMING_DIR}/close_encounters" || exit 1

program_name="calc_orbits"
if [[ ! -f "${BIN_DIR}/${program_name}${EXE}" ]]; then
    print -n -- "Building ${program_name:u} ... "
    rm -rf *.mod
    ${FC} -std=f2023 -O3 -Wall -Wno-unused-dummy-argument -I ../finclude ${program_name}.f90 -o ${program_name}${EXE} -L ../lib -lbasic_mods -lode_mods ${LIBS[@]}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    print "done."
fi

program_name="process_orbits"
if [[ ! -f "${BIN_DIR}/${program_name}${EXE}" ]]; then
    print -n -- "Building ${program_name:u} ... "
    rm -rf *.mod
    ${FC} -std=f2023 -O3 -Wall -I ../finclude ${program_name}.f90 -o ${program_name}${EXE} -L ../lib -lbasic_mods ${LIBS[@]}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    print "done."
fi

program_name="test_jsunp"
if [[ ! -f "${BIN_DIR}/${program_name}${EXE}" ]]; then
    print -n -- "Building ${program_name:u} ... "
    rm -rf *.mod
    ${FC} -std=f2023 -O3 -Wall -Wno-unused-dummy-argument -I ../finclude ${program_name}.f90 -o ${program_name}${EXE} -L ../lib -lbasic_mods -lode_mods ${LIBS[@]}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    print "done."
fi

## ----------------------------------------------------------

cd "${PROGRAMMING_DIR}/complex-dynamics" || exit 1

program_name="complexplot"
if [[ ! -f "${BIN_DIR}/${program_name}${EXE}" ]]; then
    print -n -- "Building ${program_name:u} ... "
    rm -rf *.mod
    ${FC} -std=f2023 -O3 -Wall -I ../finclude ${program_name}.f90 -o ${program_name}${EXE} -L ../lib -lfortran-fparser -lbasic_mods -lfortran-sdl2apps -lfortran-sdl2 -lfpc++ -lstdc++ ${LIBS[@]}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    print "done."
fi

program_name="flux_stream"
if [[ ! -f "${BIN_DIR}/${program_name}${EXE}" ]]; then
    print -n -- "Building ${program_name:u} ... "
    rm -rf *.mod
    ${FC} -std=f2023 -O3 -Wall -I ../finclude ${program_name}.f90 -o ${program_name}${EXE} -L ../lib -lfortran-fparser -lbasic_mods -lfortran-sdl2apps -lfortran-sdl2 -lfpc++ -lstdc++ ${LIBS[@]}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    print "done."
fi

## ----------------------------------------------------------

cd "${PROGRAMMING_DIR}/double_pendulum" || exit 1

program_name="double_pendulum"
if [[ ! -f "${BIN_DIR}/${program_name}${EXE}" ]]; then
    print -n -- "Building ${program_name:u} ... "
    rm -rf *.mod
    ${FC} -std=f2023 -O3 -Wall -Wno-unused-dummy-argument -I ../finclude ${program_name}.f90 -o ${program_name}${EXE} -L ../lib -lbasic_mods -lode_mods -lfortran-sdl2apps -lfortran-sdl2 ${LIBS[@]}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    print "done."
fi

## ----------------------------------------------------------

cd "${PROGRAMMING_DIR}/ellipsoid_gravitation" || exit 1
program_name="ellipsoid_orbits"
if [[ ! -f "${BIN_DIR}/${program_name}${EXE}" ]]; then
    print -n -- "Building ${program_name:u} ... "
    rm -rf *.mod
    ${FC} -std=f2023 -O3 -Wall -I ../finclude ellipsoid_gravitation.f90 ${program_name}.f90 -o ${program_name}${EXE} -L ../lib -lbasic_mods -lfortran-sdl2apps -lfortran-sdl2 ${LIBS[@]}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    print "done."
fi

## ----------------------------------------------------------

cd "${PROGRAMMING_DIR}/forces" || exit 1

program_name="forcesfp"
if [[ ! -f "${BIN_DIR}/${program_name}${EXE}" ]]; then
    print -n -- "Building ${program_name:u} ... "
    rm -rf *.mod
    ${FC} -std=f2023 -O3 -Wall -I ../finclude ${program_name}.f90 -o ${program_name}${EXE} -L ../lib -lfortran-fparser -lbasic_mods -lfortran-sdl2apps -lfortran-sdl2 -lfpc++ -lstdc++ ${LIBS[@]}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    print "done."
fi

## ----------------------------------------------------------

cd "${PROGRAMMING_DIR}/foucault" || exit 1
program_name="foucault"
if [[ ! -f "${BIN_DIR}/${program_name}${EXE}" ]]; then
    print -n -- "Building ${program_name:u} ... "
    rm -rf *.mod
    ${FC} -std=f2023 -O3 -I ../finclude ${program_name}.f90 -o ${program_name}${EXE} -L ../lib -lbasic_mods -lfortran-sdl2apps -lfortran-sdl2 ${LIBS[@]}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    print "done."
fi

## ----------------------------------------------------------

cd "${PROGRAMMING_DIR}/heat1D_solver" || exit 1

program_name="heat1D_solver"
if [[ ! -f "${BIN_DIR}/${program_name}${EXE}" ]]; then
    print -n -- "Building ${program_name:u} ... "
    rm -rf *.mod
    ${FC} -std=f2023 -O3 -Wall -I ../finclude ${program_name}.f90 -o ${program_name}${EXE} -L ../lib -lfortran-fparser -lbasic_mods -lfortran-sdl2apps -lfortran-sdl2 -lfpc++ -lstdc++ ${LIBS[@]}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    print "done."
fi

## ----------------------------------------------------------

cd "${PROGRAMMING_DIR}/joke" || exit 1

program_name="bouncing"
if [[ ! -f "${BIN_DIR}/${program_name}${EXE}" ]]; then
    print -n -- "Building ${program_name:u} ... "
    rm -rf *.mod
    ${FC} -std=f2023 -O3 -Wall -I ../finclude ${program_name}.f90 -o ${program_name}${EXE} -L ../lib -lbasic_mods -lfortran-sdl2apps -lfortran-sdl2 ${LIBS[@]}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    print "done."
fi

program_name="colour_map"
if [[ ! -f "${BIN_DIR}/${program_name}${EXE}" ]]; then
    print -n -- "Building ${program_name:u} ... "
    rm -rf *.mod
    ${FC} -std=f2023 -O3 -Wall -I ../finclude ${program_name}.f90 -o ${program_name}${EXE} -L ../lib -lbasic_mods -lfortran-sdl2apps -lfortran-sdl2 ${LIBS[@]}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    print "done."
fi

program_name="display_ppm"
if [[ ! -f "${BIN_DIR}/${program_name}${EXE}" ]]; then
    print -n -- "Building ${program_name:u} ... "
    rm -rf *.mod
    ${FC} -std=f2023 -O3 -Wall -I ../finclude ${program_name}.f90 -o ${program_name}${EXE} -L ../lib -lfortran-sdl2apps -lfortran-sdl2 ${LIBS[@]}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    print "done."
fi

program_name="koch_snowflake"
if [[ ! -f "${BIN_DIR}/${program_name}${EXE}" ]]; then
    print -n -- "Building ${program_name:u} ... "
    rm -rf *.mod
    ${FC} -std=f2023 -O3 -Wall -I ../finclude ${program_name}.f90 -o ${program_name}${EXE} -L ../lib -lbasic_mods -lfortran-sdl2apps -lfortran-sdl2 ${LIBS[@]}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    print "done."
fi

program_name="plot_ellipse"
if [[ ! -f "${BIN_DIR}/${program_name}${EXE}" ]]; then
    print -n -- "Building ${program_name:u} ... "
    rm -rf *.mod
    ${FC} -std=f2023 -O3 -Wall -I ../finclude ${program_name}.f90 -o ${program_name}${EXE} -L ../lib -lbasic_mods -lfortran-sdl2apps -lfortran-sdl2 ${LIBS[@]}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    print "done."
fi

## ----------------------------------------------------------

cd "${PROGRAMMING_DIR}/lorenz_attractor" || exit 1

program_name="lorenz_attractor"
if [[ ! -f "${BIN_DIR}/${program_name}${EXE}" ]]; then
    print -n -- "Building ${program_name:u} ... "
    rm -rf *.mod
    ${FC} -std=f2023 -O3 -Wall -Wno-unused-dummy-argument -I ../finclude ${program_name}.f90 -o ${program_name}${EXE} -L ../lib -lbasic_mods -lode_mods -lfortran-sdl2apps -lfortran-sdl2 ${LIBS[@]}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    print "done."
fi

## ----------------------------------------------------------

cd "${PROGRAMMING_DIR}/mandelzoom" || exit 1

program_name="mandelzoom"
if [[ ! -f "${BIN_DIR}/${program_name}${EXE}" ]]; then
    print -n -- "Building ${program_name:u} ... "
    rm -rf *.mod
    ${FC} -std=f2023 -O3 -Wall -I ../finclude ${program_name}.f90 -o ${program_name}${EXE} -L ../lib -lbasic_mods -lfortran-sdl2apps -lfortran-sdl2 ${LIBS[@]}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    print "done."
fi

## ----------------------------------------------------------

cd "${PROGRAMMING_DIR}/maps_attractors" || exit 1

program_name="henon_map"
if [[ ! -f "${BIN_DIR}/${program_name}${EXE}" ]]; then
    print -n -- "Building ${program_name:u} ... "
    rm -rf *.mod
    ${FC} -std=f2023 -O3 -Wall -I ../finclude ${program_name}.f90 -o ${program_name}${EXE} -L ../lib -lbasic_mods -lfortran-sdl2apps -lfortran-sdl2 ${LIBS[@]}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    print "done."
fi

program_name="logistic_map"
if [[ ! -f "${BIN_DIR}/${program_name}${EXE}" ]]; then
    print -n -- "Building ${program_name:u} ... "
    rm -rf *.mod
    ${FC} -std=f2023 -O3 -Wall -I ../finclude ${program_name}.f90 -o ${program_name}${EXE} -L ../lib -lbasic_mods -lfortran-sdl2apps -lfortran-sdl2 ${LIBS[@]}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    print "done."
fi

## ----------------------------------------------------------

cd "${PROGRAMMING_DIR}/math_test" || exit 1

program_name="cbrt_test"
if [[ ! -f "${BIN_DIR}/${program_name}${EXE}" ]]; then
    print -n -- "Building ${program_name:u} ... "
    rm -rf *.mod
    ## We do not need _all this_ but we use it to get a static build
    ${FC} -std=f2023 -O3 -Wall ${program_name}.f90 -o ${program_name}${EXE} ${LIBS[@]}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    print "done."
fi

program_name="plnml_test"
if [[ ! -f "${BIN_DIR}/${program_name}${EXE}" ]]; then
    print -n -- "Building ${program_name:u} ... "
    rm -rf *.mod
    ## We do not need _all this_ but we use it to get a static build
    ${FC} -std=f2023 -O3 -Wall ${program_name}.f90 -o ${program_name}${EXE} ${LIBS[@]}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    print "done."
fi

program_name="solve_equation"
if [[ ! -f "${BIN_DIR}/${program_name}${EXE}" ]]; then
    print -n -- "Building ${program_name:u} ... "
    rm -rf *.mod
    ## We do not need _all this_ but we use it to get a static build
    ${FC} -std=f2023 -O3 -Wall -Wno-unused-function ${program_name}.f90 -o ${program_name}${EXE} ${LIBS[@]}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    print "done."
fi

## ----------------------------------------------------------

cd "${PROGRAMMING_DIR}/poisson2D_solver" || exit 1
program_name="poisson2D_solver"
if [[ ! -f "${BIN_DIR}/${program_name}${EXE}" ]]; then
    print -n -- "Building ${program_name:u} ... "
    rm -rf *.mod
    ${FC} -std=f2023 -O3 -Wall -I ../finclude ${program_name}.f90 -o ${program_name}${EXE} -L ../lib -lbasic_mods -lfortran-sdl2apps -lfortran-sdl2 ${LIBS[@]}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    print "done."
fi

## ----------------------------------------------------------

## Build Windows apps only on MINGW64/UCRT64
if [[ -n "${MINGW64_OS}" ]]; then
    cd "${PROGRAMMING_DIR}/poisson2D-win32" || exit 1

    program_name="poisson2D_win32"
    if [[ ! -f "${BIN_DIR}/${program_name}${EXE}" ]]; then
        print -n -- "Building ${program_name:u} ... "
        rm -rf {*.mod,*.res}
        ${RC} ${program_name}.rc -O coff -o ${program_name}.res
        ${FC} -std=f2023 -O3 -Wall -Wno-unused-dummy-argument -Wno-maybe-uninitialized -I ../finclude -static -mwindows ${program_name}.f90 -o ${program_name}${EXE} ${program_name}.res -L ../lib -lbasic_mods -lfortran-win32box -lfortran-win32app -lfortran-win32
        rm -rf {*.mod,*.res}
        mv ${program_name}${EXE} "${BIN_DIR}"
        print "done."
    fi
fi

## ----------------------------------------------------------

cd "${PROGRAMMING_DIR}/star_walk" || exit 1

program_name="star_walk"
if [[ ! -f "${BIN_DIR}/${program_name}${EXE}" ]]; then
    print -n -- "Building ${program_name:u} ... "
    rm -rf *.mod
    ${FC} -std=f2023 -O3 -Wall -I ../finclude ${program_name}.f90 -o ${program_name}${EXE} -L ../lib -lbasic_mods -lfortran-sdl2apps -lfortran-sdl2 ${LIBS[@]}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    print "done."
fi

## ----------------------------------------------------------

cd "${PROGRAMMING_DIR}/tfmajorana_mpc" || exit 1

program_name="tfmajorana_mpc"
if [[ ! -f "${BIN_DIR}/${program_name}${EXE}" ]]; then
    DARWIN_OS="$(uname -s | grep Darwin || true)"

    STATIC_OPT=""
    [[ "${DARWIN_OS}" != "Darwin" ]] && STATIC_OPT="-static"

    print -n -- "Building ${program_name:u} ... "
    rm -rf *.mod
    ${FC} -O3 ${STATIC_OPT} -I ../finclude ${program_name}.f90 -o ${program_name}${EXE} -L ../lib -lfm
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    print "done."
fi

## ----------------------------------------------------------

cd "${PROGRAMMING_DIR}/thomas-fermi" || exit 1

program_name="thomas_fermi"
if [[ ! -f "${BIN_DIR}/${program_name}${EXE}" ]]; then
    print -n -- "Building ${program_name:u} ... "
    rm -rf *.mod
    ${FC} -std=f2023 -O3 -Wall -I ../finclude ${program_name}.f90 -o ${program_name}${EXE} -L ../lib -lbasic_mods -lode_mods ${LIBS[@]}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    print "done."
fi

## ----------------------------------------------------------

## On GNU/Linux the linker 'ld' emits a warning which can be avoided
## adding '-z noexecstack' to the command line of FC
cd "${PROGRAMMING_DIR}/three_body_problem" || exit 1

program_name="three_body_problem"
if [[ ! -f "${BIN_DIR}/${program_name}${EXE}" ]]; then
    print -n -- "Building ${program_name:u} ... "
    rm -rf *.mod
    ${FC} -std=f2023 -O3 -Wall -Wno-unused-dummy-argument -I ../finclude ${program_name}.f90 -o ${program_name}${EXE} -L ../lib -lbasic_mods -lode_mods -lfortran-sdl2apps -lfortran-sdl2 ${LIBS[@]}
    rm -rf *.mod
    mv ${program_name}${EXE} "${BIN_DIR}"
    print "done."
fi
