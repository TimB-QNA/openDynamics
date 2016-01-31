#ifndef ODCONSTANTS_H
  #define ODCONSTANTS_H

#include "odPoint.h"

class odConstants
{
  public:
    double pi;
    double g;
    double rhoWater;
    double rhoAir;
    double simTime;
    
    odPoint modelX;
    odPoint modelY;
    odPoint modelZ;
    odConstants();
};

#endif

