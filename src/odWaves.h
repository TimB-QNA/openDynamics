#include "odObject.h"
#include "odRange.h"
#include <vector>
using namespace std;

#ifndef ODWAVES_H
  #define ODWAVES_H

class odWaves : public odObject
{
  public:
    odRange period;
    double sigHeight;
    double waveHeading;
    int spectrumType;
    int nFreqs;
    
    vector<double> frequency;
    vector<double> spectrum;
    vector<double> amplitude;
    vector<double> phase;
    vector<double> heading;

    odWaves();
    void createSeaState();
    void createPMspectrum();
    double elevationInfluence(double x, double y);
    odPoint velocityInfluence(odPoint pos);
#ifdef GRAPHICS
    void plot();
#endif
    void exportVTK(FILE *stream);
    void readFromXML(QDomNode root);
    
  private:
    odRange freq;

    double rampTime;
};

#endif

