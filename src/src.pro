TEMPLATE	= app
LANGUAGE	= C++

QT += xml core

CONFIG  += qt warn_on debug

QMAKE_CXXFLAGS += -Wno-deprecated

DEFINES += TOLERANCE=1e-9 dDOUBLE VERSION=QString\\\(\\\"1.0-0\\\"\\\)

HEADERS += odWaves.h \
           odCable.h \
           odCurrent.h \
           odObject.h \
           odRange.h \
           odConstants.h \
           odSimCondition.h \
           odBody.h \
           odFoil.h \
           odFoilSection.h \
           odFoilPanel.h \
           odPoint.h \
           odMatrix.h \
           odMesh.h \
           odHull.h \
           odHullResistance.h \
           odHullResistance_DELFT.h \
           odPolygon.h \
           odMass.h \
           odWorld.h \
           odSpring.h \
           odSail.h \
           odTerrain.h \
           odPropeller.h \
           odPropellerModel.h \
           wageningenB.h

SOURCES += main.cpp \
           odWaves.cpp \
           odCable.cpp \
           odCurrent.cpp \
           odObject.cpp \
           odConstants.cpp \
           odSimCondition.cpp \
           odBody.cpp \
           odFoil.cpp \
           odFoilSection.cpp \
           odFoilPanel.cpp \
           odPoint.cpp \
           odMatrix.cpp \
           odMesh.cpp \
           odHull.cpp \
           odHullResistance.cpp \
           odHullResistance_DELFT.cpp \
           odPolygon.cpp \
           odMass.cpp \
           odWorld.cpp \
           odSpring.cpp \
           odSail.cpp \
           odTerrain.cpp \
           odPropeller.cpp \
           odPropellerModel.cpp \
           wageningenB.cpp

INCLUDEPATH += $$[QT_INSTALL_HEADERS]/openDynamics $$[QT_INSTALL_HEADERS]/OpenPilot /usr/include/vtk-5.8/ opennurbs

LIBS += -lode
#LIBS += -lodVTKWidget -lvtkCommon -lvtkWidgets -lvtkRendering -lvtkFiltering -lQVTK -lode -lqt4gnuplot opennurbs/libopenNURBS.a

DESTDIR = ../bin

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
