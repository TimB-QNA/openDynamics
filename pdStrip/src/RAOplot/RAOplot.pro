TEMPLATE = app
LANGUAGE = C++

CONFIG  += qt warn_on

SOURCES += main.cpp \
           ../lib/point.cpp \
           ../lib/section.cpp \
           ../lib/hullform.cpp \
           ../lib/panel.cpp

HEADERS += ../lib/point.h \
           ../lib/section.h \
           ../lib/hullform.h \
           ../lib/panel.h

DESTDIR = ../../bin

OBJECTS_DIR = .obj
