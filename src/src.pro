TEMPLATE	= app
LANGUAGE	= C++

include(../config.pri)

QT += xml core

CONFIG  += debug

HEADERS += waves/odWaves.h \
           waves/waveSpectrum.h \
           waves/PMSpectrum.h \
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
           waves/odWaves.cpp \
           waves/waveSpectrum.cpp \
           waves/PMSpectrum.cpp \
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
           odRange.cpp \
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
TARGET = openDynamics
