#include <vector>
#include <QDomNode>
#include "../odRange.h"
#include "waveSpectrum.h"
using namespace std;

#ifndef PMSPECTRUM_H
  #define PMSPECTRUM_H

class PMSpectrum : public waveSpectrum
{
  public:
    PMSpectrum(odConstants *simConstants, QDomNode xmlNode); // Constructor should take everything required to produce your wave spectrum
       
    void readFromXML(QDomNode xmlNode);
    void createSpectrum();
    
  private:
    double sigHeight;
    odRange period;
    int nFreqs;
};

#endif

