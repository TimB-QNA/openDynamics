#ifndef PANEL_H
  #define PANEL_H

#include <stdio.h>

#include "point.h"
#include <vector>
using namespace std;

class panel{
  public:
    panel();
    double area();
    double areaXY();
    double calculatePrismVolume(double waterline);
    vector<panel> splitAtZ(double zSplit);

    point centroid();
    point normal();
    bool isDegenerate();
    void translate(double x, double y, double z);
    void pitch(double ang);

    point pt[3];
};

#endif
