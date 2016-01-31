#include "odObject.h"
#include "odMesh.h"
#include "odHullResistance.h"
#include <vector>
using namespace std;

#ifndef ODHULLRESISTANCE_DELFT_H
  #define ODHULLRESISTANCE_DELFT_H

typedef struct{
  float friction, upright, heeled, yawed, sideforce;
} res; 

class odHullResistance_DELFT : public odHullResistance
{
  public:
    odHullResistance_DELFT();
    void readFromXML(QDomNode root);  
    odPoint calculateForces();
    
  private:
    double correct_wsa();
    double h_heel95();
    double upright95();
//    double appendage_upright95();
//    double k_heel95();
    double induced95();
    double sideforce95();
    void   setup_coeff_gerritsma95();
    void   setup_coeff_keuning95();

    double Bwl,Cp,Cm,Displ,lcb,lcf,Awp,Sc, T, Tc;
    double a[9][11], froude[11], Tephi[4], b[6][4], c[6][11], d[4][11], lh[4], l[4][4]; 
};

#endif

