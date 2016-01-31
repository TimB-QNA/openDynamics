#include "odPoint.h"
#include "odFoilSection.h"

#include <QtXml>
#include <vector>
using namespace std;

#ifndef ODFOILPANEL_H
  #define ODFOILPANEL_H

class odFoilPanel{
  public:
    odPoint force;
    odPoint moment;
    odPoint normal;
    odPoint aft;
    odPoint effectiveCentre;
    
    odFoilSection s[2];
    double area;
    void calculateArea();
    void calculateNormal();
    void calculateAft();
    void calculateEffectiveCentre();
    double chord();
    void rotate(odPoint stockOrigin, odPoint stockDelta, double stockAngle);
    void listPanelData();
};

#endif

