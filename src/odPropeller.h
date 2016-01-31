#include "odObject.h"
#include "odPropellerModel.h"

#include <vector>
using namespace std;

#ifndef ODPROPELLER_H
  #define ODPROPELLER_H
    
class odPropeller : public odObject
{
  public:
    odPropeller();

#ifdef GRAPHICS
    void plot();
#endif

    void preSolveSetup();
    void run();
    void readFromXML(QDomNode root);
    
    double RPM;
    
  private:
    odPropellerModel *model;
    odPoint hubOrigin;
    odPoint thrustVector;
    
    odPropellerModel* createModel(QString modelType);
};

#endif

