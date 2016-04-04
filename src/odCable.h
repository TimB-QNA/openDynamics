#include "odObject.h"
#include "odBody.h"
#include <QString>

#ifndef ODCABLE_H
  #define ODCABLE_H
  
class odCable : public odObject
{
  public:
    odCable();
    QString type();
    void preSolveSetup();
    void run();
    void postAdvance();
    void readFromXML(QDomNode root);
    void findBodies(vector<odBody> *vessel);
    void setupBody(dWorldID *world, dSpaceID space);
    void exportVTK(FILE *stream);
    void exportJointVTK(FILE *stream);
    
  private:
    int nSegments;
    double length, weight, diameter;
    double angularDamping, linearDamping;
   
    odPoint pos1, pos2, E1;
    odBody *body1, *body2;
    QString b1_name, b2_name;
    bool fb1, fb2, tensionOnly;
    
    dBodyID *element;
    dJointGroupID contactgroup;
    dSpaceID space;
    dJointID *joint;
    dJointID end1, end2;
    FILE *forcesFile, *positionFile;
    dJointFeedback *end1Feedback, *end2Feedback;
};

#endif


