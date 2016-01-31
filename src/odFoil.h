#include "odObject.h"
#include "odFoilSection.h"
#include "odFoilPanel.h"
#include <vector>
using namespace std;

#ifndef ODFOIL_H
  #define ODFOIL_H
    
class odFoil : public odObject
{
  public:
    
    int coefficientMode;                        ///< Mode 0 is polynomial. TODO mode 1 is a look-up table. 
    double stockAngle;                          ///< Control input angle of foil about the stock
    
    vector<double> alpha;                       ///< Steady state alpha values for lookup table
    vector<double> Cl;                          ///< Steady state Cl coefficients, or values for lookup table
    vector<double> Cm;                          ///< Steady state Cm coefficients, or values for lookup table
    vector<double> Cd;                          ///< Steady state Cd coefficients, or values for lookup table

    vector<odFoilSection> modelSection;        ///< Sections defining the foil in model axes.
                                               /// These will be copied to worldSection before forces are calculated.
    vector<odFoilPanel> panel;                 ///< Panels defining the foil. Each panel is built automatically from adjacent sections.

    odFoil();
#ifdef GRAPHICS
    void plot();
#endif
    void preSolveSetup();
    void run();
    void readFromXML(QDomNode root);
    
  private:
    double calculateCl(double a);
    double calculateCm(double a);
    double calculateCd(double a);
    
    QString type;
    bool hasStock;
    odPoint stockOrigin;
    odPoint stockDelta;
    
    double maxStockAngle;                     ///< Maximum control angle. Default 45 degrees.
};

#endif

