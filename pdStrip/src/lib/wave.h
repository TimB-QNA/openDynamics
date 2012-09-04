#ifndef WAVE_H
  #define WAVE_H

#include "point.h"

using namespace std;

class wave{
  public:
    double z0;
    double wavPeriod;
  
    void initialise();
    double calculateZeta(double time, point pt);
    
  private:
    double wavOmega;
    double wavLambda;
    double wavK;
};

#endif
