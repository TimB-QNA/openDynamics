TEMPLATE = subdirs

CONFIG  += ordered

SUBDIRS  = geoConv \
           RAOplot \
           solver

binaries.path = /usr/bin
binaries.files = ../bin/*

INSTALLS = binaries
