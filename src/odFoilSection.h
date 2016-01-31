#include "odPoint.h"
#include <QtXml>
#include <vector>
using namespace std;

#ifndef ODFOILSECTION_H
  #define ODFOILSECTION_H

class odFoilSection
{
  public:
    vector<odPoint> point;
    odPoint LE;
    odPoint TE;
    
    void clear();
    void readFromXML(QDomNode node);
};

#endif

