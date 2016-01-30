TEMPLATE = subdirs

CONFIG  += ordered

SUBDIRS  = geoConv \
           solver

binaries.path = /usr/bin
binaries.files = ../bin/*

INSTALLS = binaries
