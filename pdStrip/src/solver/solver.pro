TEMPLATE = app
CONFIG  += release
TARGET   = pdStrip

F90_SOURCES = pdstrip.f90

DESTDIR = ../../bin

QMAKE_LINK = gfortran

F90 = gfortran
ff90.output = ${QMAKE_FILE_BASE}.o
ff90.commands = $$F90 -c -g ${QMAKE_FILE_NAME} -o ${QMAKE_FILE_OUT}
ff90.input    = F90_SOURCES
QMAKE_EXTRA_COMPILERS += ff90
