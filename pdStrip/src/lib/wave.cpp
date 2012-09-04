#include "wave.h"
#include <stdio.h>
#include <math.h>

void wave::initialise(){
  double PI;
  PI=4.*atan(1.);
  wavOmega=(2.*PI)/wavPeriod;
  wavLambda=9.81/(2*PI)*pow(wavPeriod,2);
  wavK=(2.*PI)/wavLambda;
}

double wave::calculateZeta(double t, point pt){
  return z0*sin( wavOmega * t - wavK* pt.x);
}
