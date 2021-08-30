DEFINES += TOLERANCE=1e-9 dDOUBLE VERSION=QString\\\(\\\"1.0-0\\\"\\\)

QMAKE_CXXFLAGS += -Wno-deprecated

CONFIG += qt warn_on

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
  TARGET = win32/VTKtest
}
