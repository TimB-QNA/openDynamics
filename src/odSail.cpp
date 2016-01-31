#include "odSail.h"

/*!
 * The default values here are for an NACA-0012 at Re=1000000
 */
odSail::odSail(){
  type=-1;
  roach=0;
  battens=false;
}

/*!
 * Reads a foil stanza from XML. The format for this is foil --> section --> point, leadingEdge, trailingEdge.
 * The LE and TE points are important, but the other "points" are used only for plotting. Currently, each section must have
 * the same number of points defined. The leading edge and trailing edge points must be defined. If LE and TE are defined
 * but no other points are, the LE & TE data is used for plotting.
 */
void odSail::readFromXML(QDomNode root){
  odPoint stockS, stockE;
  QDomNode node, subNode;
  QDomElement element, subElement;
  readCommonDataFromXML(root);
  
  node=root.firstChild();
  while (!node.isNull()){
    element=node.toElement();
    if (element.tagName().toLower()=="type"){
      if (element.text().toLower()==QString("spinnaker"))      type=0;
      if (element.text().toLower()==QString("jib"))            type=1;
      if (element.text().toLower()==QString("mainsail"))       type=2;
      if (element.text().toLower()==QString("mizzenstaysail")) type=3;
      if (element.text().toLower()==QString("mizzen"))         type=4;
    }
    if (element.tagName().toLower()=="tack")  tack.readFromXML(element);
    if (element.tagName().toLower()=="clew")  clew.readFromXML(element);
    if (element.tagName().toLower()=="head")  head.readFromXML(element);
    if (element.tagName().toLower()=="roach") roach=element.text().toDouble()/100.;
    if (element.tagName().toLower()=="battens" && element.text().toLower()==QString("true")) battens=true;
    node=node.nextSibling();
  }
}

void odSail::preSolveSetup(){
  openDataStream(QString("odFoil"));
  sailPolygon.pt.push_back(tack);
  sailPolygon.pt.push_back(clew);
  sailPolygon.pt.push_back(head);
}

/*!
 * This is the solver for this component. It calculates the forces on each "panel" of the foil
 * (between two sections) based on the local inflow velocity.
 */
void odSail::run(){
  double u, w, uwMag, alpha;
  double cl, cd, side, drive;
  double lift, drag;
  double pi=4.*atan(1.);
  odPoint fsVel;
  odPoint aft(1,0,0);
  odPoint normal=sailPolygon.normal();
  odPoint force;
  
  /*!
     * Calculate the effective angle of attack.
     * To do this we use the (local) free-stream velocity,
     * the panel normal vector, and the panel aft vector.
     * The dot-product of the velocity with the normal or aft vectors
     * gives the "u" and "v" velocity. The arc-tangent of these velocities
     * gives the undisturbed inflow angle.
     */
  
  fsVel=localVelocity(sailPolygon.centroid(), uid);
  u=aft.dotProduct_natural(fsVel);
  w=normal.dotProduct_natural(fsVel);
  uwMag=sqrt(u*u+w*w);
  alpha=atan2(w,u);
    
  lift=.5*density(sailPolygon.centroid())*pow(fsVel.mag(),2)*sailPolygon.surfaceArea()*calcSailCl(alpha);
  drag=.5*density(sailPolygon.centroid())*pow(fsVel.mag(),2)*sailPolygon.surfaceArea()*calcSailCd(alpha);
   
  side =lift*cos(alpha*pi/180.)+drag*sin(alpha*pi/180.);
  drive=lift*sin(alpha*pi/180.)-drag*cos(alpha*pi/180.);

  force=odPoint(1,0,0)*drive+normal*drive;
  
  dBodyAddRelForceAtRelPos(*odeBody, force.x, force.y, force.z, sailPolygon.centroid().x, sailPolygon.centroid().y, sailPolygon.centroid().z);
  fprintf(dataStream,"time %lf inflow u %lf w %lf alpha %lf force %lf\t%lf\t%lf moment %lf\t%lf\t%lf\n", *simTime, u, w, alpha, force.x, force.y, force.z);
}
  
double odSail::calcSailCl(double beta){
  double Cl=0;
  double pi=4.*atan(1.);
  double theta=fabs(beta*180./pi);
  
  if (type==0){
    Cl=1.33968+theta*0.013759+pow(theta,2)*-0.000284+pow(theta,3)*0.000001;
    if (theta<50 || theta>160) Cl=0; // from 50 to 160    
  }
  
  if (type==1){
    Cl=2.23765554+theta*-0.0359679+pow(theta,2)*0.00013126;
    if (theta<27 || theta>95) Cl=0; // from 27 to 95
    if (battens==true) Cl*=1.15;
  }
  
  if (type==2){
    Cl=1.41163606+theta*0.01047326+pow(theta,2)*-0.0002498+pow(theta,3)*0.00000082319; // from 0 to 180
    if (battens==true) Cl*=1.15;
  }
  
  if (type==3){
    Cl=-1.8193049+theta*0.0886991+pow(theta,2)*-0.0008584+pow(theta,3)*0.0000023433;
    if (theta<27 || theta>180) Cl=0; // from 27 to 180
    if (battens==true) Cl*=1.15;
  }
  
  if (type==4){
    Cl=0.8484628+theta*0.02755857+pow(theta,2)*-0.0004151+pow(theta,3)*0.0000013105;
    if (theta<0 || theta>160) Cl=0; // from 0 to 160
    if (battens==true) Cl*=1.15;
  }
  return Cl;
}

double odSail::calcSailCd(double beta){
  double Cd=0;
  double theta=fabs(beta);
  
  if (type==0){
    Cd=0.76041024+theta*-0.0617791+pow(theta,2)*0.0015308+pow(theta,3)*-0.00001128+pow(theta,4)*0.000000025945;
    if (theta<27 && theta>180) Cd=0; // from 27 to 180
  }
  
  if (type==1){
    Cd=-0.710149+theta*0.03876693+pow(theta,2)*-0.0004721+pow(theta,3)*0.0000015484;
    if (theta<27) Cd=0.02;
    if (theta>95) Cd=0; // from 27 to 95
  }
  
  if (type==2){
    Cd=-0.717772+theta*0.025938+pow(theta,2)*-0.000094;
    if (theta<29) Cd=0.02;
    if (theta>180) Cd=0;// from 29 to 180
  }

  if (type==3){
    Cd=1.53547531+theta*-0.1096438+pow(theta,2)*0.002406+pow(theta,3)*-0.00001786+pow(theta,4)*0.000000042319;
    if (theta<40 || theta>180) Cd=0; // from 40 to 180
  }
  
  if (type==4){
    Cd=-0.7206046+theta*0.02604925+pow(theta,2)*-0.00009726;
    if (theta<30 || theta>180) Cd=0; // from 30 to 180
  }
  
  return Cd;
}