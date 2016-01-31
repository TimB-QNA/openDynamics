#include "odObject.h"
#include "odBody.h"
#include <QString>

#ifndef ODSPRING_H
  #define ODSPRING_H
  
class odSpring : public odObject
{
  public:
    odSpring();
    QString type();
    void run();
    void readFromXML(QDomNode root);
    void attachBodies(vector<odBody> *vessel);
    void preSolveSetup();
    void exportVTK(FILE *stream);

  private:
    double k, preload, initialLength, tension;
    odPoint end1, end2;
    odPoint force1, force2;
    odBody *body1, *body2;
    QString b1_name, b2_name;
    bool fb1, fb2, tensionOnly;
    FILE *forcesFile, *positionFile;
};

#endif


