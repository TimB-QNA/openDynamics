#include "odPoint.h"
#include <vector>
#include <ode/ode.h>
using namespace std;

#ifndef ODPOLYGON_H
  #define ODPOLYGON_H
    
class odPolygon
{
  public:
    double pressure;   // Hydrostatic pressure
    odPoint PtangForce; // Tangential shear stress (Force/Area)
    odPoint PnormForce; // Panel Normal pressure (From skin friction solver)
    vector<odPoint> pt;
    odPoint velocity;
    
    odPolygon();
    void clear();
    double surfaceArea();
    double projectedAreaXY();
    odPoint centroid();
    odPoint normal();
    odPolygon transformed(dBodyID *odeBody);
    void translate(odPoint translation);
    
  private:
    odPoint norm;
    odPoint cent;
    
    double srfArea;
    double projectedXY;
};

#endif
