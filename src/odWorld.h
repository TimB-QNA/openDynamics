#ifndef ODWORLD_H
  #define ODWORLD_H
  
#include <ode/ode.h>
#include "odConstants.h"
#include "odBody.h"
#include "odWaves.h"
#include "odSpring.h"
#include "odCable.h"
#include "odCurrent.h"
#include "odTerrain.h"
#include <vector>

using namespace std;
  
class odWorld
{
  public:
    
    odConstants    constants;
    vector<odBody> vessel;
    vector<odSpring> springs;
    vector<odCable> cables;
    vector<odTerrain> terrain;
    odWaves        seastate;
    odCurrent      current;
    
    dWorldID world;
    dJointGroupID contactgroup;
    
    odWorld();
    void setupReferences();
    void run();
    void initialiseSolver();
    void advanceSolver();
    void readFromXML(QDomNode root);
    double currentTime();
    
  private:
    int tIndex;
    double simTime;
    double timeStep;
    double endTime;
    dSpaceID space;   
    odObject **objectList;
    
    void readSimulationXML(QDomNode root);
};

#endif

