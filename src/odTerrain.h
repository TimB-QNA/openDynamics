#ifndef ODTERRAIN_H
  #define ODTERRAIN_H
  
#include <ode/ode.h>
#include "odPoint.h"
#include <vector>

using namespace std;
  
class odTerrain
{
  public:    
    odTerrain();
    void createTerrain(dSpaceID space);
    void readFromXML(QDomNode root);
    
  private:
    odPoint normal;
    double offset;
};

#endif

