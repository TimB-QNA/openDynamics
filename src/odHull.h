#include "odObject.h"
#include "odMesh.h"
#include "odHullResistance.h"
#include <vector>
using namespace std;

#ifndef ODHULL_H
  #define ODHULL_H

class odHull : public odObject
{
  public:
#ifdef GRAPHICS
    void plot();
#endif
    void preSolveSetup();
    void run();
    void readFromXML(QDomNode root);  
    
  private:
    odHullResistance *resistance;
};

#endif

