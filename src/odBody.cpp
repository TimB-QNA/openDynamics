#include "odBody.h"
#include <math.h>
#include <stdio.h>

#include "odFoil.h"
#include "odHull.h"
#include "odSail.h"
#include "odPropeller.h"

odBody::odBody(){
  uid=rand();
  printf("Creating an odBody - %i\n",uid);
  tIndex=0;
  fixSurge=false;
  fixSway=false;
  fixHeave=false;
  fixRoll=false;
  fixPitch=false;
  fixYaw=false;
  lock=false;
  lockEndTime=-1;
}

odBody::~odBody(){
  printf("Destroying an odBody - %i\n",uid);  
}

void odBody::setupBody(dWorldID *world){
  unsigned int i, j;
  double pi=4.*atan(1.);
  dMatrix3 R;
  
  //Propogate mesh references...
  printf("Assigning meshes to components\n");
  printf("  %i meshes\n",mesh.size());
  printf("  %i components\n",component.size());
  for (i=0;i<component.size();i++){
    printf("Component mesh ref %s\n", component[i]->meshRef.toLatin1().data());
    for (j=0;j<mesh.size();j++){
//      printf("Current Mesh Ref %s\n", mesh[j].reference().toLatin1().data());
      if (component[i]->meshRef==mesh[j].reference()){
//	printf("Connecting mesh %i to component %i\n",j,i);
	component[i]->mesh=mesh[j];
      }
    }
  }
  
  odeBody = dBodyCreate(*world);

  dBodySetPosition(odeBody, initialPosition.x, initialPosition.y, initialPosition.z);
  dBodySetLinearVel(odeBody, initialVelocity.x, initialVelocity.y, initialVelocity.z);
  dRFromEulerAngles(R, initialAttitude.x*pi/180., initialAttitude.y*pi/180., initialAttitude.z*pi/180.); 
  dBodySetRotation(odeBody, R);       
  dBodySetDamping(odeBody, linearDamping, angularDamping);

  mass.calcProperties(&mesh);
  mass.report();
  
  for (i=0;i<mesh.size();i++) mesh[i].translate(-mass.centroid);
  mass.centroid=odPoint();
  
  dMassSetParameters(&odeMass, mass.mass, mass.centroid.x, mass.centroid.y, mass.centroid.z,
                     mass.Ixx, mass.Iyy, mass.Izz,
                     mass.Ixy, mass.Ixz, mass.Iyz);
  
  printf("Mass:\n");
  printf("  Total: %f\n",odeMass.mass);
  for (i=0;i<3;i++){
    printf("   C[%i]: %f\n",i, odeMass.c[i]);
  }
  for (i=0;i<9;i++){
    printf("   I[%i]: %f\n",i, odeMass.I[i]);
  }
  
//  l=5; b=1; d=1; vol=l*b*d;
//  dMassSetBoxTotal(&odeMass, mass.mass, l, b, d);
//  printf("Total Body mass = %lf\n",mass.mass);
  
  dBodySetMass(odeBody,&odeMass);
//  dMassTranslate(&body.m); //, body.mass.centroid.x, body.mass.centroid.y, body.mass.centroid.z);
//  dMassAdjust(&body.m, 1);

  
//  body.odeGeom = dCreateBox (0, .5, .5, .5);
//  dGeomSetBody(body.odeGeom,body.odeBody);
   
//  dBodySetPosition(body.odeBody, 0, 0, 0);
}

void odBody::updateState(){
  const dReal *P=dGeomGetPosition(odeGeom);
  const dReal *R=dGeomGetRotation(odeGeom);
  simState.linear.x=P[0];
  simState.linear.y=P[1];
  simState.linear.z=P[2];
  
//  simState.rotational.x=P[0];
//  simState.rotational.y=P[1];
//  simState.rotational.z=P[2];
}

void odBody::addObject(odObject *obj){
//  obj->component=component;
  obj->constants = constants;
  obj->odeBody   = &odeBody;
  component.push_back(obj);
}


void odBody::readFromXML(QDomNode root){
  QDomNode node;
  QDomElement element;
  odHull *tmpHull;
  odFoil *tmpFoil;
  odMesh tmpMesh;
  odSail *tmpSail;
  odPropeller *tmpProp;
  
  node=root.firstChild();
  while (!node.isNull()){
    element=node.toElement();
    
    if (element.tagName().toLower()=="hull"){
      tmpHull = new odHull();
      tmpHull->readFromXML(node);
      addObject(tmpHull);
    }
    
    if (element.tagName().toLower()=="foil"){
      tmpFoil = new odFoil();
      tmpFoil->readFromXML(node);
      addObject(tmpFoil);
    }
    
    if (element.tagName().toLower()=="sail"){
      tmpSail = new odSail();
      tmpSail->readFromXML(node);
      addObject(tmpSail);
    }
    
    if (element.tagName().toLower()=="mesh"){
      tmpMesh.readFromXML(node);
      mesh.push_back(tmpMesh);
    }
    
    if (element.tagName().toLower()=="propeller"){
      tmpProp = new odPropeller();
      tmpProp->readFromXML(node);
      addObject(tmpProp);
    }
    
    if (element.tagName().toLower()=="mass") mass.readFromXML(node);
    if (element.tagName().toLower()=="dynamics") readDynamicsDataFromXML(node);
    if (element.tagName().toLower()=="name")     name=element.text();
    
    
    node=node.nextSibling();
  } 
}

/*! \author Tim Brocklehurst
 * This routine reads data which should be included for every object.
 * At present this is
 * - Position
 * - Velocity
 * - Attitude
 * - Damping
 */
void odBody::readDynamicsDataFromXML(QDomNode root){
  int i;
  QDomElement element;
  QDomNode node;
  
  node=root.firstChild();  
  while (!node.isNull()){
    element=node.toElement();
    if (element.tagName().toLower()=="lock"){
      lock=true;
      lockEndTime=element.attribute("until").toDouble();
    }
    if (element.tagName().toLower()=="position") initialPosition.readFromXML(element);
    if (element.tagName().toLower()=="velocity") initialVelocity.readFromXML(element);
    if (element.tagName().toLower()=="attitude") initialAttitude.readFromXML(element);
    if (element.tagName().toLower()=="damping"){
      angularDamping=element.attribute("angular").toDouble();
      linearDamping=element.attribute("linear").toDouble();
    }
    if (element.tagName().toLower()=="fixsurge" && element.text().toLower()=="true") fixSurge=true;
    if (element.tagName().toLower()=="fixsway"  && element.text().toLower()=="true") fixSway=true;
    if (element.tagName().toLower()=="fixheave" && element.text().toLower()=="true") fixHeave=true;
    if (element.tagName().toLower()=="fixroll"  && element.text().toLower()=="true") fixRoll=true;
    if (element.tagName().toLower()=="fixpitch" && element.text().toLower()=="true") fixPitch=true;
    if (element.tagName().toLower()=="fixyaw"   && element.text().toLower()=="true") fixYaw=true;
    if (element.tagName().toLower()=="enableafter") lockEndTime=element.text().toDouble();
    node=node.nextSibling();
  }
}

void odBody::preSolveSetup(){
  int i, j;
  char fname[80];
  printf("Setup components\n");
  for (i=0;i<component.size();i++) component[i]->preSolveSetup();
  
  printf("Opening forces file\n");
  sprintf(fname,"output/Forces_%s.dat",name.toLatin1().data());
  forcesFile=fopen(fname,"w");
  fprintf(forcesFile,"Net forces and moments (T, Fx, Fy, Fz, Mx, My, Mz)\n");
  
  printf("Opening position file\n");
  sprintf(fname,"output/Position_%s.dat",name.toLatin1().data());
  positionFile=fopen(fname,"w");
  fprintf(positionFile,"Speed Position and attitude (T, Velx, Vely, Velz, Posx, Posy, Posz, Roll, Pitch, Yaw)\n");
  
  printf("Applying transforms\n");
  // Apply transform so we have some VTK data to write...
  for (i=0;i<component.size();i++){
    printf("Component %i\n", i);
    if (component[i]->mesh.polygon.size()!=0){
      if (component[i]->mesh.active.size()==0) component[i]->mesh.active.resize(component[i]->mesh.polygon.size());
      for (j=0;j<component[i]->mesh.polygon.size();j++){
        component[i]->mesh.active[j]=component[i]->mesh.polygon[j].transformed(&odeBody);
      }
    }
  }
  printf("Writing VTK for t=0\n");
  writeVTK();
}

void odBody::advanceSolver(){
  int i, j;
  double pi=4.*atan(1.);
  const dReal *SpeedResult, *R;
  dVector3 posResult;
  QStringList script;
  const dReal *force, *torque;
  double Fx, Fy, Fz, Mx, My, Mz;
  char fname[80];
  FILE *forces;
  
  SpeedResult=dBodyGetLinearVel(odeBody);
  dBodyGetRelPointPos(odeBody, 0, 0, 0, posResult);
  R=dBodyGetRotation(odeBody);

      x.push_back(posResult[0]);
      y.push_back(posResult[1]);
      z.push_back(posResult[2]);
   roll.push_back(-atan2(R[9], R[10])*180./pi);    //phi
  pitch.push_back(asin(R[8])*180./pi);             //theta
    yaw.push_back(atan2(R[4], R[0])*180./pi);      //gamma

  fprintf(positionFile,"%lf\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\n", *simTime,
         SpeedResult[0], SpeedResult[1], SpeedResult[2], x.back(), y.back(), z.back(),
         roll.back(), pitch.back(), yaw.back());
  fflush(positionFile);
  
  // Transform the polygon mesh...
  for (i=0;i<component.size();i++){
    if (component[i]->mesh.polygon.size()!=0){
      if (component[i]->mesh.active.size()==0) component[i]->mesh.active.resize(component[i]->mesh.polygon.size());
      for (j=0;j<component[i]->mesh.polygon.size();j++){
        component[i]->mesh.active[j]=component[i]->mesh.polygon[j].transformed(&odeBody);
      }
    }
  }
  
  // Execute the solvers
  for (i=0;i<component.size();i++) component[i]->run();
  
  force  = dBodyGetForce(odeBody);
  torque = dBodyGetTorque(odeBody);
  fprintf(forcesFile,"%lf\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\n", *simTime, force[0],   force[1],  force[2], torque[0], torque[1], torque[2]);
  fflush(forcesFile);
  
//  printf("Forces (%lf) x, y, z : %lf\t%lf\t%lf\n", *simTime, force[0],   force[1],  force[2]);
  // Fix the body in specified axes...
  Fx=force[0];  Fy=force[1];  Fz=force[2];
  Mx=torque[0]; My=torque[1]; Mz=torque[2];
  if (fixSurge) Fx=0;
  if (fixSway)  Fy=0;
  if (fixHeave) Fz=0;
  if (fixRoll)  Mx=0;
  if (fixPitch) My=0;
  if (fixYaw)   Mz=0;
  dBodySetForce(odeBody, Fx, Fy, Fz);
  dBodySetTorque(odeBody, Mx, My, Mz);
  
//  printf("Simulation Time = %lf\n", *simTime);
//  printf("Lock Time = %lf\n", lockEndTime);
  if (*simTime<lockEndTime){
//    printf("Body disabled\n");
    dBodySetKinematic(odeBody);
  }else{
    dBodySetDynamic(odeBody);
  }
}

void odBody::postAdvance(){
  int i;
  tIndex++;
  for (i=0;i<component.size();i++) component[i]->postAdvance();
   
  writeVTK();
}

odPoint odBody::globalPosition(odPoint modelPos){
  dVector3 posResult;
  dBodyGetRelPointPos(odeBody, modelPos.x, modelPos.y, modelPos.z, posResult);
  return odPoint(posResult[0], posResult[1], posResult[2]);
}

void odBody::writeVTK(){
  int i;
  FILE *vtkFile;
  char fname[80];
   
  for (i=0;i<component.size();i++){
    sprintf(fname,"output/VTK/%s_ts%i.%s",component[i]->name.toLatin1().data(), tIndex, component[i]->vtksuffix.toLatin1().data());
    vtkFile=fopen(fname, "w");
    component[i]->exportVTK(vtkFile);
    fclose(vtkFile);
  }
}
