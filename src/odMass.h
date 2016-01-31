#include "odPoint.h"
#include "odMesh.h"
#include <QtXml>
#include <vector>
using namespace std;

#ifndef ODMASS_H
  #define ODMASS_H

/*!
 * Element of mass with applicable data
 */
class odMassElement
{
  public:
    double mass;        ///< Mass of this element
    double x;           ///< X centroid of this mass element
    double y;           ///< Y centroid of this mass element
    double z;           ///< Z centroid of this mass element
    
    QString meshRef;
    double density;
    double thickness;
    
    bool calcMassFromMeshProps;  ///< Flag to enable calculation of mass from mesh thicness and material density.
    bool hasChanged;         ///< Flag to allow automatic re-calculation
    
    odMassElement();
    void clear();
    void readFromXML(QDomElement element);
};

/*!
 * Hold masses for a vessel based on XML input.
 * Also provide useful output as required by ODE.
 */
class odMass
{
  public:
    double mass;
    odPoint centroid;
    double Ixx, Iyy, Izz, Ixy, Ixz, Iyz;
    
    vector<odMassElement> massElement;
    
    void readFromXML(QDomNode node);
    void calcProperties(vector<odMesh> *mesh);
    void report();
};

#endif

