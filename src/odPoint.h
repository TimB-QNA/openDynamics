/*
    Description: Provide 3D points for openDynamics
         Author: Tim Brocklehurst
        License: GPLv3
*/
#include <QtXml>

#ifndef ODPOINT_H
  #define ODPOINT_H

class odPoint
{
  public:
    odPoint();
    odPoint(double xd, double yd, double zd);
    
    void readFromXML(QDomElement element);
    
    odPoint operator*(double b);
    odPoint operator*(odPoint b);
    odPoint operator/(double b);
    odPoint operator+(odPoint b);
    odPoint operator-();
    odPoint operator-(odPoint b);
    void operator+=(odPoint b);
    void operator/=(double b);
    bool operator==(odPoint b);
    void operator*=(double b);
    
    double dotProduct(odPoint b);
    double dotProduct_natural(odPoint b);
    odPoint crossProduct(odPoint b);
    double mag();
    double mag(odPoint b);
    void display();
    odPoint rotate(odPoint angles);
    odPoint unitVector();
    
    double x, y, z;
    bool hasChanged;                               ///< Optional flag to set if this point/vector needs recalulating
};

#endif

 
