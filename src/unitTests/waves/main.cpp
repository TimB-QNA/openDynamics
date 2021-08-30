#include <stdio.h>
#include "../../odConstants.h"
#include "../../waves/PMSpectrum.h"

/// Test code to execute unit tests on the wave/seastate subsystem.
int main(){
  unsigned int i;
  FILE *output;
  QDomDocument xmlDescription;
  
  odConstants simConstants;
  
  xmlDescription.setContent(QString("<config> <height>5</height><period min=\"5\" max=\"20\"/><frequencies>30</frequencies> </config>"));
   
  PMSpectrum waveSpectrum(&simConstants, xmlDescription.firstChild().firstChild());
  waveSpectrum.createSpectrum();
  
  
  output=fopen("results/PM.dat","w");
  if (output==NULL){
    printf("Pierson-Moscowitz Unit Test: Failed to open output file\n");
    return 1;
  }  
  for (i=0;i<waveSpectrum.spectrum.size();i++){
    fprintf(output, "%g\t%g\n", waveSpectrum.spectrum[i].frequency, waveSpectrum.spectrum[i].magnitude);
  }
  fclose(output);
  
  system("gnuplot -c waves.gplt PM");
  return 0;
}
