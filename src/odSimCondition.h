#include <QtXml>

#ifndef ODSIMCONDITION_H
  #define ODSIMCONDITION_H
#include "odPoint.h"
class odSimCondition
{
  public:
    double time;
    
    odPoint linear;
    odPoint rotational;
    
    odPoint linearVel;
    odPoint rotationalVel;
    
    odPoint linearAcc;
    odPoint rotationalAcc;
    
    void initialise();
    void readFromXML(QDomNode node);
};

#endif

