#include "PMSpectrum.h"
#include "math.h"

PMSpectrum::PMSpectrum(odConstants *simConstants, QDomNode xmlNode){
  constants=simConstants;
  sigHeight=0.01;
  period.min=1;
  period.max=10;
  nFreqs=30;
  readFromXML(xmlNode);
}
    
void PMSpectrum::readFromXML(QDomNode xmlNode){
  QDomElement element;

  while (!xmlNode.isNull()){
    element=xmlNode.toElement();
    if (element.tagName().toLower()=="height") sigHeight=element.text().toDouble();
    if (element.tagName().toLower()=="period"){
      if (element.hasAttribute("min")) period.min=element.attribute("min").toDouble();
      if (element.hasAttribute("max")) period.max=element.attribute("max").toDouble();
    }
    if (element.tagName().toLower()=="frequencies") nFreqs=element.text().toInt();
    xmlNode=xmlNode.nextSibling();
  }
}

void PMSpectrum::createSpectrum(){
  double a, b;
  waveSpectrumPoint tws;
  int i;
  
  spectrum.clear();
  
  freq.min = (2.*constants->pi)/period.max;
  freq.max = (2.*constants->pi)/period.min;
  
//  printf("Sig Wave Height = %lf\n", sigHeight);
//  printf("Period Min/Max = %lf     %lf\n", period.min, period.max);
  
  for (i=0;i<nFreqs;i++){
    tws.frequency = freq.min + (double)i/(double)(nFreqs-1)*freq.range();
    
    a=8.1e-3 * pow(constants->g,2);
    b=0.0323*pow(constants->g / sigHeight,2);
    
    tws.magnitude = a * pow(tws.frequency,-5) * exp( -b * pow(tws.frequency,-4) );
    tws.phase     = (double)rand()/(double)RAND_MAX*2.*constants->pi;
    
//    printf("A / B = %lf     %lf\n", a, b);
//    printf("F / M = %lf     %lf\n", tws.frequency, tws.magnitude);
    spectrum.push_back(tws);
  }
}
