#include "odHullResistance.h"

odHullResistance::odHullResistance(){
    g=9.81;
  rho=1025.;
   pi=4.*atan(1.);
   
// Set Defaults... based on Larsson's YD40
  Lwl=10.02;
}

void odHullResistance::readFromXML(QDomNode root){
  readCommonDataFromXML(root);
}

void odHullResistance::readCommonDataFromXML(QDomNode root){
  int i;
  QDomNode node;
  QDomElement element;
  
  node=root.firstChild();
  while (!node.isNull()){
    element=node.toElement();
    if (element.tagName().toLower()=="model") model=element.text().toInt();
    if (element.tagName().toLower()=="waterline") Lwl=element.text().toDouble();
    node=node.nextSibling();
  }
}

odPoint odHullResistance::calculateForces(){
  return odPoint();
}

void odHullResistance::setAttitude(odPoint A){
  attitude=A;  
}

void odHullResistance::setVelocity(odPoint V){
  velocity=V;
  Vf=sqrt(pow(velocity.x,2.) + pow(velocity.y,2.));
  Fn=Vf/sqrt(g*Lwl);
}

void odHullResistance::setConstants(double gravity, double density){
  g=gravity;
  rho=density;
}
    
void odHullResistance::setParametersFromBody(dBodyID *body){
  int i;
  const dReal *SpeedResult, *R;
  
  SpeedResult=dBodyGetLinearVel(*body);
  R=dBodyGetRotation(*body);
  velocity.x=-SpeedResult[0];
  velocity.y=-SpeedResult[1];
  velocity.z=-SpeedResult[2];
  
  attitude.x=asin(-R[8])*180./pi;          // Pitch
  attitude.y=-atan2(R[9], R[10])*180./pi;  // Roll
  attitude.z=atan2(R[4], R[0])*180./pi;    // Yaw
  
  Vf=sqrt(pow(velocity.x,2.) + pow(velocity.y,2.));
  Fn=Vf/sqrt(g*Lwl);
}

double odHullResistance::ittc57(double L, double A){
  double Rs;
  Rs=.5*rho*pow(Vf,2.)*A*ittc57_coeff(L);
  return Rs;
}

double odHullResistance::ittc57_coeff(double L){
  double Re, Cf, Rs;
  Re=(rho*Vf*L)/(0.001002);
  if (Re<1) return 0.001;
  Cf=.075/pow((log10(Re)-2.),2.);
  return Cf;
}