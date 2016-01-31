#ifndef ODBODY_H
  #define ODBODY_H
  
#include <ode/ode.h>
#include "odConstants.h"
#include "odSimCondition.h"
#include "odObject.h"
#include "odMass.h"
#include <vector>
using namespace std;
  
class odBody
{
  public:
    double *simTime;
    dBodyID odeBody;
    dGeomID odeGeom; 
    dMass   odeMass;
    QString name;
    
    odConstants    *constants;
    odSimCondition simState;
    odSimCondition model;
    odMass mass;
    vector<odObject*> component;
    vector<odMesh> mesh;
    
    odBody();
    ~odBody();
//    void summate();
    void updateState();
    void addObject(odObject *obj);
    void setupBody(dWorldID *world);
    void readFromXML(QDomNode root);
    void preSolveSetup();
    void advanceSolver();
    void postAdvance();
    
    odPoint globalPosition(odPoint modelPos);
    
  private:
    int uid, tIndex;
    odPoint initialPosition;
    odPoint initialVelocity;
    odPoint initialAttitude;
    double angularDamping;
    double linearDamping;
    FILE *forcesFile, *positionFile;
    bool fixSurge;
    bool fixSway;
    bool fixHeave;
    bool fixRoll;
    bool fixPitch;
    bool fixYaw;
    
    bool lock;
    double lockEndTime;
      
    vector<double> x,y,z,roll,pitch,yaw;
    
    void readDynamicsDataFromXML(QDomNode root);
    void writeVTK();
};

#endif

