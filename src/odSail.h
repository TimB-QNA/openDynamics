#include "odObject.h"
#include <vector>
using namespace std;

#ifndef ODSAIL_H
  #define ODSAIL_H
    
class odSail : public odObject
{
  public:
    odSail();
    void preSolveSetup();
    void run();
    void readFromXML(QDomNode root);
    
  private:
    int type;
    odPoint tack;
    odPoint clew;
    odPoint head;
    double roach;
    bool battens;
    odPolygon sailPolygon;
      
    double calcSailCl(double beta);
    double calcSailCd(double beta);
};

#endif

