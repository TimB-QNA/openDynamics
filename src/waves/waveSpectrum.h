#include <vector>
#include <QDomNode>
#include "../odRange.h"
#include "../odConstants.h"
using namespace std;

#ifndef WAVESPECTRUM_H
  #define WAVESPECTRUM_H

class waveSpectrumPoint
{
  public:
    double frequency;
    double magnitude;
    double phase;
};

class waveSpectrum
{
  public:
//    waveSpectrum(); // Constructor should take everything required to produce your wave spectrum (typically the XML data)
       
    vector<waveSpectrumPoint> spectrum;
    odRange freq;

    virtual void readFromXML(QDomNode xmlNode) = 0;
    virtual void createSpectrum() = 0;
    
    
    
  protected:
    odConstants *constants;
};

#endif

