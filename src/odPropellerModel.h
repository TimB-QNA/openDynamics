#include <QtXml>
#include "odPoint.h"

#ifndef ODPROPELLERMODEL_H
  #define ODPROPELLERMODEL_H
    
class odPropellerModel
{
  public:
    odPropellerModel();
#ifdef GRAPHICS
    virtual void plot();
#endif
    virtual void readFromXML(QDomNode root);
    virtual double calcT();
    virtual double calcQ();
    
    double V;   // Inflow speed (m/s)
    double rho; // Fluid density
    double n;   // Propeller rotaation rate RPS
    FILE *dataStream;
    QString type;
};

#endif

