#include <QList>
#include <QtXml>
#include "../odPropellerModel.h"

class wageningenBcoeff{
  public:
    double C;
    int s, t, u, v;
};

class wageningenB : public odPropellerModel {
  public:
    wageningenB();
    void readFromXML(QDomNode root);
    double calcKt();
    double calcKq();
    double calcEta();
    double J();
    double calcT();
    double calcQ();
    
    int z; // Number of blades - Min 2 - Max 7
    
//    double V; // Ship speed (m/s)
//    double n; // RPS
    double D; // Diameter (m)
    double PD; // Pitch/Diameter ratio - Min 0.5 - Max 1.4
    double BAR; // Blade area ratio - referred to in paper as A_e/A_o - Typically 70% (0.7) - Min 0.3 - Max 1.05
//    double rho;
    
    QList<wageningenBcoeff> kTcoeffs;
    QList<wageningenBcoeff> kQcoeffs;
};
