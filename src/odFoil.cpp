#include "odFoil.h"
#ifdef GRAPHICS
  #include <vtkStructuredGrid.h>
  #include <vtkProperty.h>
  #include <vtkDataSetMapper.h>
#endif

/*!
 * The default values here are for an NACA-0012 at Re=1000000
 */
odFoil::odFoil(){
  coefficientMode=0;
  maxStockAngle=45;
  Cl.push_back(0.00506088); Cl.push_back(5.47194); Cl.push_back(16.0069); Cl.push_back(-65.2055);
  Cm.push_back(-0.0013418); Cm.push_back(0.373376); Cm.push_back(-7.79706); Cm.push_back(48.2481); Cm.push_back(-85.1436);
  Cd.push_back(0.0109772); Cd.push_back(-0.409762); Cd.push_back(8.65889); Cd.push_back(-53.1544); Cd.push_back(110.659);
}

void odFoil::preSolveSetup(){
  openDataStream(QString("odFoil"));
}

/*!
 * Reads a foil stanza from XML. The format for this is foil --> section --> point, leadingEdge, trailingEdge.
 * The LE and TE points are important, but the other "points" are used only for plotting. Currently, each section must have
 * the same number of points defined. The leading edge and trailing edge points must be defined. If LE and TE are defined
 * but no other points are, the LE & TE data is used for plotting.
 */
void odFoil::readFromXML(QDomNode root){
  int i;
  odPoint stockS, stockE;
  QDomNode node, subNode;
  QDomElement element, subElement;
  odFoilSection tmpSection;
  readCommonDataFromXML(root);
  
  node=root.firstChild();
  while (!node.isNull()){
    element=node.toElement();
    if (element.tagName().toLower()=="section"){
      tmpSection.clear();
      tmpSection.readFromXML(node.firstChild());
      modelSection.push_back(tmpSection);
    }
    if (element.tagName().toLower()=="stock"){
      subNode=element.firstChild();
      while (!subNode.isNull()){
        subElement=subNode.toElement();
        if (subElement.tagName().toLower()=="origin") stockOrigin.readFromXML(subElement);
        if (subElement.tagName().toLower()=="end")         stockE.readFromXML(subElement);      
        if (subElement.tagName().toLower()=="angle")       stockAngle=-subElement.text().toDouble();
        if (subElement.tagName().toLower()=="maxAngle") maxStockAngle=subElement.text().toDouble();
        subNode=subNode.nextSibling();
      }
      hasStock=true;
    }

    node=node.nextSibling();
  }
  stockDelta=stockE-stockOrigin;
//  stockDelta.display();
//  stockE.display();
//  stockOrigin.display();
  if ((int)modelSection.size()<2) printf("odFoil :: WARNING :: Fewer than two sections defined. Foil will not contribute.\n");
}

#ifdef GRAPHICS
/*!
 * For plotting purposes all sections must have the same number of points.
 */
void odFoil::plot(){
  int nPoints, i, j;
  odPoint pt;
  dVector3 result;
  vtkPoints *points;
  vector<odFoilSection> worldSection;        ///< Sections defining the foil in world axes; it is these sections
                                             /// that are translated and rotated before modelling forces and moments.
  worldSection=modelSection;
  
  for (i=0;i<(int)worldSection.size();i++){
    for (j=0;j<(int)worldSection[i].point.size();j++){
      pt=modelSection[i].point[j];
 //     dBodyVectorToWorld(*odeBody, pt.x, pt.y, pt.z, result);
 //     worldSection[i].point[j]=odPoint(result[0], result[1], result[2]);
      dBodyGetRelPointPos(*odeBody, pt.x, pt.y, pt.z, result);
      worldSection[i].point[j]=odPoint(result[0], result[1], result[2]);

    }
  }
      
  points = vtkPoints::New();
  for (i=0;i<(int)worldSection.size();i++){
    for (j=0;j<(int)worldSection[0].point.size();j++){
      points->InsertNextPoint( worldSection[i].point[j].x, worldSection[i].point[j].y, worldSection[i].point[j].z );
    }
  }
  vtkStructuredGrid *grid = vtkStructuredGrid::New();
  grid->SetDimensions(worldSection[0].point.size(), worldSection.size(), 1);
  grid->SetPoints(points);

  vtkDataSetMapper *mapper = vtkDataSetMapper::New();
  mapper->SetInputConnection(grid->GetProducerPort());
  actor->SetMapper(mapper);
  actor->GetProperty()->SetColor( 1.0, 0, 0 );
}
#endif

/*!
 * This is the solver for this component. It calculates the forces on each "panel" of the foil
 * (between two sections) based on the local inflow velocity.
 */
void odFoil::run(){
  int i, j;
  odPoint fsVel, force, moment;
  double pi=4.*atan(1.);
  double u, w, alpha, uwMag, dynamic;
  double cN, cT, cL, cD;
  odFoilPanel tmpPanel;
  odPoint modelForce, modelMoment;
  if ((int)modelSection.size()<2) return;

  // Limit angle
  if (stockAngle>maxStockAngle) stockAngle=maxStockAngle;

  if (panel.size()==0){
    for (i=0;i<(int)modelSection.size()-1;i++){
      panel.push_back(tmpPanel);
      panel.back().s[0]=modelSection[i];
      panel.back().s[1]=modelSection[i+1];
      panel.back().calculateArea();
      panel.back().calculateNormal();
      panel.back().calculateAft();
      panel.back().calculateEffectiveCentre();
    }
  }
  
//   modelForce =odPoint(0.,0.,0.);
//   modelMoment=odPoint(0.,0.,0.);

  for (i=0;i<(int)panel.size();i++){
    tmpPanel=panel[i];
    tmpPanel.rotate(stockOrigin, stockDelta, stockAngle);
    /*!
     * Calculate the effective angle of attack.
     * To do this we use the (local) free-stream velocity,
     * the panel normal vector, and the panel aft vector.
     * The dot-product of the velocity with the normal or aft vectors
     * gives the "u" and "v" velocity. The arc-tangent of these velocities
     * gives the undisturbed inflow angle.
     */

    fsVel=localVelocity(panel[i].effectiveCentre, uid);
//    printf("\nPanel %i : %s : %lf -\n",i,name.toAscii().data(),panel[i].area);
//    panel[i].effectiveCentre.display();
//    panel[i].normal.display();
//    panel[i].aft.display();
//    fsVel.display();
    u=panel[i].aft.dotProduct_natural(fsVel);
    w=panel[i].normal.dotProduct_natural(fsVel);
    uwMag=sqrt(u*u+w*w);
//    printf("u=%lf\tw=%lf\tuwMAG=%lf\n",u,w,uwMag);
    alpha=atan2(w,u)+stockAngle*pi/180.;
//    printf("Alpha %lf\n",alpha*180./pi);
    
     // Calculate forces and moments based on
     // Cl/Cm/Cd in the local co-ordinate system.
   
    dynamic=0.5*density(panel[i].effectiveCentre)*pow(uwMag,2)*panel[i].area;
//    printf("L=%lf\n",dynamic*calculateCl(alpha));
    cL = calculateCl(alpha);
 
    cD = calculateCd(alpha);
    cN = cD*sin(alpha) + cL*cos(alpha);
    cT = cL*sin(alpha) - cD*cos(alpha);
    
//    fprintf(dataStream,"cL=%lf\ncD=%lf\ncN=%lf\ncT=%lf\n",cL, cD, cN, cT);
//    printf("alpha=%lf\tcL=%lf\tcD=%lf\tcN = %lf\tcT = %lf\n",alpha*180./pi,cL,cD,cN,cT);
    
    panel[i].force=panel[i].aft*dynamic*-cT;
    panel[i].force+=panel[i].normal*dynamic*cN;
    panel[i].moment.y=dynamic*panel[i].chord()*calculateCm(alpha);
    
 //   printf("Panel Forces  "); panel[i].force.display();
//    panel[i].force=odPoint(1.,0.,0.);
    // Convert panel forces and moments to model co-ordinate system.
    
    force.x=constants->modelX.dotProduct_natural(panel[i].force);
    force.y=constants->modelY.dotProduct_natural(panel[i].force);
    force.z=constants->modelZ.dotProduct_natural(panel[i].force);
//    modelForce+=force;
    
    moment.x+=constants->modelX.dotProduct_natural(panel[i].moment);
    moment.y=constants->modelY.dotProduct_natural(panel[i].moment);
    moment.z+=constants->modelZ.dotProduct_natural(panel[i].moment);
    
//    modelMoment.x+=panel[i].force.z*panel[i].effectiveCentre.y - panel[i].force.y*panel[i].effectiveCentre.z;
//    moment.y+=panel[i].force.z*panel[i].effectiveCentre.x - panel[i].force.x*panel[i].effectiveCentre.z;
//    modelMoment.z+=panel[i].force.y*panel[i].effectiveCentre.x - panel[i].force.x*panel[i].effectiveCentre.y;
//    panel[i].effectiveCentre.display();
//    if (name=="Wing") fprintf(stderr,"Panel %i - Lift=%lf Centre=%lf\n",i,panel[i].force.z,panel[i].effectiveCentre.x);

    dBodyAddRelForceAtRelPos(*odeBody, force.x, force.y, force.z, panel[i].effectiveCentre.x, panel[i].effectiveCentre.y, panel[i].effectiveCentre.z);
    fprintf(dataStream,"panel_%i time %lf inflow u %lf w %lf alpha %lf force %lf\t%lf\t%lf moment %lf\t%lf\t%lf\n", i, *simTime, u, w, alpha, force.x, force.y, force.z, moment.x, moment.y, moment.z);
    dBodyAddRelTorque(*odeBody, moment.x, moment.y, moment.z);
    
  }
//  if (name=="Wing") printf("Wing : iForce = %lf\t%lf\t%lf\n", modelForce.x, modelForce.y, modelForce.z);
//  if (name=="Tail") printf("Tail : iForce = %lf\t%lf\t%lf\n", modelForce.x, modelForce.y, modelForce.z);

}
  
double odFoil::calculateCl(double a){
  int i;
  double coeff;
  bool inv=false;
  if (coefficientMode==0){
    if (fabs(a)>0.9) return 0.;
    if (fabs(a)<0.08) return a*8.*atan(1.);
    if (a<0){
      a=-a;
      inv=true;
    }
    if (a>0.349) a=0.174; // If angle of attack exceeds 20 degrees, use results for 10 degrees.
    coeff=0.;
    for (i=0;i<Cl.size();i++){
      coeff+=Cl[i]*pow(a,i);
    }
    if (inv) return -coeff;
    return coeff;
  }
  return 0.;
}

double odFoil::calculateCm(double a){
  int i;
  double coeff;
  bool inv=false;
  if (coefficientMode==0){
    if (a<0){
      a=-a;
      inv=true;
    }
    if (a>0.349) a=0.174; // If angle of attack exceeds 20 degrees, use results for 10 degrees.
    coeff=0.;
    for (i=0;i<Cm.size();i++){
      coeff+=Cm[i]*pow(a,i);
    }
    if (inv) return -coeff;
    return coeff;
  }
  return 0.;
}

double odFoil::calculateCd(double a){
  int i;
  double coeff;
  if (coefficientMode==0){
    if (a<0){
      a=-a;
    }
    if (a>0.349) a=0.349; // If angle of attack exceeds 20 degrees, use results for 10 degrees.
    if (a>0.7) return 0.2;
    coeff=0.;
    for (i=0;i<Cd.size();i++){
      coeff+=Cd[i]*pow(a,i);
    }
    return coeff;
  }
  return 0.;
}