#include "../odObject.h"
#include "../odRange.h"
#include "waveSpectrum.h"
#include <vector>
using namespace std;

#ifndef ODWAVES_H
  #define ODWAVES_H

class odWaves : public odObject
{
  public:
    odWaves();
    void createSeaState();

    double elevationInfluence(double x, double y);
    odPoint velocityInfluence(odPoint pos);
#ifdef GRAPHICS
    void plot();
#endif
    void exportVTK(FILE *stream);
    void readFromXML(QDomNode root);
    
  private:
    waveSpectrum *spectrum;

    double waveHeading;
    vector<double> amplitude;
    vector<double> heading;
    double rampTime;
};

#endif

