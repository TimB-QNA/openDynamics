#ifndef HULLFORM_H
  #define HULLFORM_H

#include <stdio.h>

#include "section.h"
#include "panel.h"
#include <QString>
#include <vector>
using namespace std;

class hullform{
  public:
    double waterline;
    double immersedVolume;
    double weight;
    point min;
    point max;
    point volCentroid;
    point massCentroid;
    point gyRadius;
    
    point linearPos, linearVel, linearAcc;
    point angularPos, angularVel, angularAcc;
    
    vector<section> sect;
    vector<panel> pan;

    void clear();
    void cleanGeometry();
    void extents();
    void removeEmptySections();
    void trimAboveWaterline();
    void removeDuplicatePoints();
    void removeShortSections();
    void sort();
    void scale(double factor);
    void translate(double x, double y, double z);

    bool writeSectionDef(QString filename);
    bool readSectionDef(QString filename);
    bool readGhsDef(QString filename);
    bool writePointDef(QString filename,bool GNUPlot); // bool GNUPlot=false

    void generatePanels();
};

#endif
