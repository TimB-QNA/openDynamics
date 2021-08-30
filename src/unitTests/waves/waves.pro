TEMPLATE	= app
LANGUAGE	= C++

include(../../../config.pri)

QT += xml core

CONFIG  += qt warn_on release testcase

QMAKE_CXXFLAGS += -Wno-deprecated

HEADERS += ../../waves/odWaves.h \
           ../../waves/waveSpectrum.h \
           ../../waves/PMSpectrum.h \
           ../../odConstants.h
           
SOURCES += main.cpp \
           ../../waves/odWaves.cpp \
           ../../waves/waveSpectrum.cpp \
           ../../waves/PMSpectrum.cpp \
           ../../odConstants.cpp \
           ../../odObject.cpp

# The following files are referenced in code,
# but do not actually have any bearing on the mathematics under test.
HEADERS += ../../odPoint.h \
           ../../odMesh.h \
           ../../odPolygon.h \
           ../../odMatrix.h \
           ../../odRange.h
           
SOURCES += ../../odPoint.cpp \
           ../../odMesh.cpp \
           ../../odPolygon.cpp \
           ../../odMatrix.cpp \
           ../../odRange.cpp
           
LIBS += -lode

unix {
  UI_DIR = .ui
  MOC_DIR = .moc
  OBJECTS_DIR = .obj
  profile{
    QMAKE_CXXFLAGS += -fprofile-arcs -ftest-coverage
    QMAKE_LFLAGS   += -fprofile-arcs
  }
}

win32{
# Only needed for development purposes
# Probably handy to leave in though,
# as we tend to dump a lot of output to the console.
  CONFIG += console
  UI_DIR = win32/ui
  MOC_DIR = win32/moc
  OBJECTS_DIR = win32/obj
}
