#include "odObject.h"
#include "odRange.h"
#include <vector>
using namespace std;

#ifndef ODCURRENT_H
  #define ODCURRENT_H

class odCurrent : public odObject
{
  public:    
    odCurrent();
    odPoint velocityInfluence(odPoint pos);
    void readFromXML(QDomNode root);
    
  private:
    double top;
    double bottom;
    odPoint velocity;

};

#endif

