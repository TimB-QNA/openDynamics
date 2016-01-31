#include "wageningenB.h"
#include <math.h>

/*!
 * Set up coefficient sets for kT and kQ.
 * Coefficients are taken from "KT,KQ and Efficiency Curves for the Wageningen B-Series Propellers" by M.M. Bernitsas, D Ray & P. Kinley
 */
wageningenB::wageningenB(){
  wageningenBcoeff tmp;
  type="Wageningen-BSeries";
  // kT coefficients
  tmp.C= 0.00880496;   tmp.s=0; tmp.t=0; tmp.u=0; tmp.v=0; kTcoeffs.append(tmp);
  tmp.C=-0.204554;     tmp.s=1; tmp.t=0; tmp.u=0; tmp.v=0; kTcoeffs.append(tmp);
  tmp.C= 0.166351;     tmp.s=0; tmp.t=1; tmp.u=0; tmp.v=0; kTcoeffs.append(tmp);
  tmp.C= 0.158114;     tmp.s=0; tmp.t=2; tmp.u=0; tmp.v=0; kTcoeffs.append(tmp);
  tmp.C=-0.147581;     tmp.s=2; tmp.t=0; tmp.u=1; tmp.v=0; kTcoeffs.append(tmp);
  tmp.C=-0.481497;     tmp.s=1; tmp.t=1; tmp.u=1; tmp.v=0; kTcoeffs.append(tmp);
  tmp.C= 0.415437;     tmp.s=0; tmp.t=2; tmp.u=1; tmp.v=0; kTcoeffs.append(tmp);
  tmp.C= 0.0144043;    tmp.s=0; tmp.t=0; tmp.u=0; tmp.v=1; kTcoeffs.append(tmp);
  tmp.C=-0.0530054;    tmp.s=2; tmp.t=0; tmp.u=0; tmp.v=1; kTcoeffs.append(tmp);
  tmp.C= 0.0143481;    tmp.s=0; tmp.t=1; tmp.u=0; tmp.v=1; kTcoeffs.append(tmp);
  tmp.C= 0.0606826;    tmp.s=1; tmp.t=1; tmp.u=0; tmp.v=1; kTcoeffs.append(tmp);
  tmp.C=-0.0125894;    tmp.s=0; tmp.t=0; tmp.u=1; tmp.v=1; kTcoeffs.append(tmp);
  tmp.C= 0.0109689;    tmp.s=1; tmp.t=0; tmp.u=1; tmp.v=1; kTcoeffs.append(tmp);
  tmp.C=-0.133698;     tmp.s=0; tmp.t=3; tmp.u=0; tmp.v=0; kTcoeffs.append(tmp);
  tmp.C= 0.00638407;   tmp.s=0; tmp.t=6; tmp.u=0; tmp.v=0; kTcoeffs.append(tmp);
  tmp.C=-0.00132718;   tmp.s=2; tmp.t=6; tmp.u=0; tmp.v=0; kTcoeffs.append(tmp);
  tmp.C= 0.168496;     tmp.s=3; tmp.t=0; tmp.u=1; tmp.v=0; kTcoeffs.append(tmp);
  tmp.C=-0.0507214;    tmp.s=0; tmp.t=0; tmp.u=2; tmp.v=0; kTcoeffs.append(tmp);
  tmp.C= 0.0854559;    tmp.s=2; tmp.t=0; tmp.u=2; tmp.v=0; kTcoeffs.append(tmp);
  tmp.C=-0.0504475;    tmp.s=3; tmp.t=0; tmp.u=2; tmp.v=0; kTcoeffs.append(tmp);
  tmp.C= 0.010465;     tmp.s=1; tmp.t=6; tmp.u=2; tmp.v=0; kTcoeffs.append(tmp);
  tmp.C=-0.00648272;   tmp.s=2; tmp.t=6; tmp.u=2; tmp.v=0; kTcoeffs.append(tmp);
  tmp.C=-0.00841728;   tmp.s=0; tmp.t=3; tmp.u=0; tmp.v=1; kTcoeffs.append(tmp);
  tmp.C= 0.0168424;    tmp.s=1; tmp.t=3; tmp.u=0; tmp.v=1; kTcoeffs.append(tmp);
  tmp.C=-0.00102296;   tmp.s=3; tmp.t=3; tmp.u=0; tmp.v=1; kTcoeffs.append(tmp);
  tmp.C=-0.0317791;    tmp.s=0; tmp.t=3; tmp.u=1; tmp.v=1; kTcoeffs.append(tmp);
  tmp.C= 0.018604;     tmp.s=1; tmp.t=0; tmp.u=2; tmp.v=1; kTcoeffs.append(tmp);
  tmp.C=-0.00410798;   tmp.s=0; tmp.t=2; tmp.u=2; tmp.v=1; kTcoeffs.append(tmp);
  tmp.C=-0.000606848;  tmp.s=0; tmp.t=0; tmp.u=0; tmp.v=2; kTcoeffs.append(tmp);
  tmp.C=-0.0049819;    tmp.s=1; tmp.t=0; tmp.u=0; tmp.v=2; kTcoeffs.append(tmp);
  tmp.C= 0.0025983;    tmp.s=2; tmp.t=0; tmp.u=0; tmp.v=2; kTcoeffs.append(tmp);
  tmp.C=-0.000560528;  tmp.s=3; tmp.t=0; tmp.u=0; tmp.v=2; kTcoeffs.append(tmp);
  tmp.C=-0.00163652;   tmp.s=1; tmp.t=2; tmp.u=0; tmp.v=2; kTcoeffs.append(tmp);
  tmp.C=-0.000328787;  tmp.s=1; tmp.t=6; tmp.u=0; tmp.v=2; kTcoeffs.append(tmp);
  tmp.C= 0.000116502;  tmp.s=2; tmp.t=6; tmp.u=0; tmp.v=2; kTcoeffs.append(tmp);
  tmp.C= 0.000690904;  tmp.s=0; tmp.t=0; tmp.u=1; tmp.v=2; kTcoeffs.append(tmp);
  tmp.C= 0.00421749;   tmp.s=0; tmp.t=3; tmp.u=1; tmp.v=2; kTcoeffs.append(tmp);
  tmp.C= 0.0000565229; tmp.s=3; tmp.t=6; tmp.u=1; tmp.v=2; kTcoeffs.append(tmp);
  tmp.C=-0.00146564;   tmp.s=0; tmp.t=3; tmp.u=2; tmp.v=2; kTcoeffs.append(tmp);

  // kQ coefficients 
  tmp.C= 0.00379368;   tmp.s=0; tmp.t=0; tmp.u=0; tmp.v=0; kQcoeffs.append(tmp);
  tmp.C= 0.00886523;   tmp.s=2; tmp.t=0; tmp.u=0; tmp.v=0; kQcoeffs.append(tmp);
  tmp.C=-0.032241;     tmp.s=1; tmp.t=1; tmp.u=0; tmp.v=0; kQcoeffs.append(tmp);
  tmp.C= 0.00344778;   tmp.s=0; tmp.t=2; tmp.u=0; tmp.v=0; kQcoeffs.append(tmp);
  tmp.C=-0.0408811;    tmp.s=0; tmp.t=1; tmp.u=1; tmp.v=0; kQcoeffs.append(tmp);
  tmp.C=-0.108009;     tmp.s=1; tmp.t=1; tmp.u=1; tmp.v=0; kQcoeffs.append(tmp);
  tmp.C=-0.0885381;    tmp.s=2; tmp.t=1; tmp.u=1; tmp.v=0; kQcoeffs.append(tmp);
  tmp.C= 0.188561;     tmp.s=0; tmp.t=2; tmp.u=1; tmp.v=0; kQcoeffs.append(tmp);
  tmp.C=-0.00370871;   tmp.s=1; tmp.t=0; tmp.u=0; tmp.v=1; kQcoeffs.append(tmp);
  tmp.C= 0.00513696;   tmp.s=0; tmp.t=1; tmp.u=0; tmp.v=1; kQcoeffs.append(tmp);
  tmp.C= 0.0209449;    tmp.s=1; tmp.t=1; tmp.u=0; tmp.v=1; kQcoeffs.append(tmp);
  tmp.C= 0.00474319;   tmp.s=2; tmp.t=1; tmp.u=0; tmp.v=1; kQcoeffs.append(tmp);
  tmp.C=-0.00723408;   tmp.s=2; tmp.t=0; tmp.u=1; tmp.v=1; kQcoeffs.append(tmp);
  tmp.C= 0.00438388;   tmp.s=1; tmp.t=1; tmp.u=1; tmp.v=1; kQcoeffs.append(tmp);
  tmp.C=-0.0269403;    tmp.s=0; tmp.t=2; tmp.u=1; tmp.v=1; kQcoeffs.append(tmp);
  tmp.C= 0.0558082;    tmp.s=3; tmp.t=0; tmp.u=1; tmp.v=0; kQcoeffs.append(tmp);
  tmp.C= 0.0161886;    tmp.s=0; tmp.t=3; tmp.u=1; tmp.v=0; kQcoeffs.append(tmp);
  tmp.C= 0.00318086;   tmp.s=1; tmp.t=3; tmp.u=1; tmp.v=0; kQcoeffs.append(tmp);
  tmp.C= 0.015896;     tmp.s=0; tmp.t=0; tmp.u=2; tmp.v=0; kQcoeffs.append(tmp);
  tmp.C= 0.0471729;    tmp.s=1; tmp.t=0; tmp.u=2; tmp.v=0; kQcoeffs.append(tmp);
  tmp.C= 0.0196283;    tmp.s=3; tmp.t=0; tmp.u=2; tmp.v=0; kQcoeffs.append(tmp);
  tmp.C=-0.0502782;    tmp.s=0; tmp.t=1; tmp.u=2; tmp.v=0; kQcoeffs.append(tmp);
  tmp.C=-0.030055;     tmp.s=3; tmp.t=1; tmp.u=2; tmp.v=0; kQcoeffs.append(tmp);
  tmp.C= 0.0417122;    tmp.s=2; tmp.t=2; tmp.u=2; tmp.v=0; kQcoeffs.append(tmp);
  tmp.C=-0.0397722;    tmp.s=0; tmp.t=3; tmp.u=2; tmp.v=0; kQcoeffs.append(tmp);
  tmp.C=-0.00350024;   tmp.s=0; tmp.t=6; tmp.u=2; tmp.v=0; kQcoeffs.append(tmp);
  tmp.C=-0.0106854;    tmp.s=3; tmp.t=0; tmp.u=0; tmp.v=1; kQcoeffs.append(tmp);
  tmp.C= 0.0010903;    tmp.s=3; tmp.t=3; tmp.u=0; tmp.v=1; kQcoeffs.append(tmp);
  tmp.C=-0.000313912;  tmp.s=0; tmp.t=6; tmp.u=0; tmp.v=1; kQcoeffs.append(tmp);
  tmp.C= 0.0035985;    tmp.s=3; tmp.t=0; tmp.u=1; tmp.v=1; kQcoeffs.append(tmp);
  tmp.C=-0.00142121;   tmp.s=0; tmp.t=6; tmp.u=1; tmp.v=1; kQcoeffs.append(tmp);
  tmp.C=-0.00383637;   tmp.s=1; tmp.t=0; tmp.u=2; tmp.v=1; kQcoeffs.append(tmp);
  tmp.C= 0.0126803;    tmp.s=0; tmp.t=2; tmp.u=2; tmp.v=1; kQcoeffs.append(tmp);
  tmp.C=-0.00318278;   tmp.s=2; tmp.t=3; tmp.u=2; tmp.v=1; kQcoeffs.append(tmp);
  tmp.C= 0.00334268;   tmp.s=0; tmp.t=6; tmp.u=2; tmp.v=1; kQcoeffs.append(tmp);
  tmp.C=-0.00183491;   tmp.s=1; tmp.t=1; tmp.u=0; tmp.v=2; kQcoeffs.append(tmp);
  tmp.C= 0.000112451;  tmp.s=3; tmp.t=2; tmp.u=0; tmp.v=2; kQcoeffs.append(tmp);
  tmp.C=-0.0000297228; tmp.s=3; tmp.t=6; tmp.u=0; tmp.v=2; kQcoeffs.append(tmp);
  tmp.C= 0.000269551;  tmp.s=1; tmp.t=0; tmp.u=1; tmp.v=2; kQcoeffs.append(tmp);
  tmp.C= 0.00083265;   tmp.s=2; tmp.t=0; tmp.u=1; tmp.v=2; kQcoeffs.append(tmp);
  tmp.C= 0.00155334;   tmp.s=0; tmp.t=2; tmp.u=1; tmp.v=2; kQcoeffs.append(tmp);
  tmp.C= 0.000302683;  tmp.s=0; tmp.t=6; tmp.u=1; tmp.v=2; kQcoeffs.append(tmp);
  tmp.C=-0.0001843;    tmp.s=0; tmp.t=0; tmp.u=2; tmp.v=2; kQcoeffs.append(tmp);
  tmp.C=-0.000425399;  tmp.s=0; tmp.t=3; tmp.u=2; tmp.v=2; kQcoeffs.append(tmp);
  tmp.C= 0.0000869243; tmp.s=3; tmp.t=3; tmp.u=2; tmp.v=2; kQcoeffs.append(tmp);
  tmp.C=-0.0004659;    tmp.s=0; tmp.t=6; tmp.u=2; tmp.v=2; kQcoeffs.append(tmp);
  tmp.C= 0.0000554194; tmp.s=1; tmp.t=6; tmp.u=2; tmp.v=2; kQcoeffs.append(tmp);
}

void wageningenB::readFromXML(QDomNode root){
  QDomNode node, subNode;
  QDomElement element, subElement;

  node=root.firstChild();
  while (!node.isNull()){
    element=node.toElement();
    printf("Tag Name: %s\n",element.tagName().toLatin1().data());
    if (element.tagName().toLower()=="diameter") D   = element.text().toDouble();
    if (element.tagName().toLower()=="bar")      BAR = element.text().toDouble();
    if (element.tagName().toLower()=="pd")       PD  = element.text().toDouble();
    if (element.tagName().toLower()=="blades")   z   = element.text().toInt();
    node=node.nextSibling();
  }
}

double wageningenB::calcKt(){
  int i;
  double kT=0., j=J();
  
  for (i=0;i<kTcoeffs.count();i++){
    kT+=kTcoeffs[i].C*pow(j,kTcoeffs[i].s)*pow(PD,kTcoeffs[i].t)*pow(BAR,kTcoeffs[i].u)*pow(z,kTcoeffs[i].v);
  }
//  printf("V=%lf\tRPS=%lf\trho=%lf\tD=%lf\tJ=%lf\tkT=%lf\n",V,n,rho,D,j,kT);
  return kT;
}

double wageningenB::calcKq(){
  int i;
  double kQ=0., j=J();
  
  for (i=0;i<kQcoeffs.count();i++){
    kQ+=kQcoeffs[i].C*pow(j,kQcoeffs[i].s)*pow(PD,kQcoeffs[i].t)*pow(BAR,kQcoeffs[i].u)*pow(z,kQcoeffs[i].v);
  }
//  printf("J=%lf\tkQ=%lf\n",j,kQ);
  return kQ;
}

double wageningenB::calcEta(){
  double pi=4.*atan(1.);
  return J()/(2*pi)*calcKt()/calcKq();
}
  
double wageningenB::calcT(){
  return calcKt()*rho*pow(n,2)*pow(D,4);
}

double wageningenB::calcQ(){
  return calcKq()*rho*pow(n,2)*pow(D,5);
}

double wageningenB::J(){
  double Jcalc;
  if (n<1e-8) return 0;
  Jcalc=V/(n*D);
  if (Jcalc>2) Jcalc=2;
  return Jcalc;
}