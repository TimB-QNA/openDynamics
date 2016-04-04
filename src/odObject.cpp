#include "odObject.h"
#include "odBody.h"
#include <ode/ode.h>
#include <math.h>


odObject::odObject(){
  uid=0;
  dataStream=stderr;
  vtksuffix="vtu";
//  mesh=NULL;
}

void odObject::openDataStream(QString objectType){
  QString fileName=objectType+QString("_%1.txt").arg(uid);
  dataStream=fopen(fileName.toLatin1().data(),"w");
  if (dataStream=='\0'){
    dataStream=stderr;
    fprintf(dataStream,"Could not open - %s\n",fileName.toLatin1().data());
    return;
  }
  
  fprintf(dataStream,"# openDynamics %s\n",VERSION.toLatin1().data());
  fprintf(dataStream,"# Part Type - %s\n",objectType.toLatin1().data());
  fprintf(dataStream,"# UID       - %i\n",uid);
  fprintf(dataStream,"# Name      - %s\n",name.toLatin1().data());
  fprintf(dataStream,"# Mesh Ref  - %s\n",meshRef.toLatin1().data());
}

void odObject::postAdvance(){
  fflush(dataStream);
}

odObject::~odObject(){
  fclose(dataStream);  
}

/*
void odObject::initObject(vector<odObject*> solveComponent, odConstants *solveConstant, dBodyID *localBody){
  odObject tempObject;
  
  tempObject.odeBody=localBody;
  tempObject.constant=solveConstant;
  tempObject.otherObjects=solveComponent;
  
  initObject(&tempObject);
}
*/
/*
void odObject::initObject(odObject* currentObject){
  odeBody=currentObject->odeBody;
  constant=currentObject->constant;
  otherObjects=currentObject->otherObjects;
      
  objects = vtkCellArray::New();
  polygons = vtkPolyData::New();
  objectMapper = vtkPolyDataMapper::New();
  actor = vtkActor::New();
  objectMapper->SetInput(polygons);
  actor->SetMapper(objectMapper);
  
  modelX = odPoint(1.,0.,0.);
  modelY = odPoint(0.,1.,0.);
  modelZ = odPoint(0.,0.,1.);
}
*/

void odObject::plot(){
  
}

/*! \author Tim Brocklehurst
 * This routine reads data which should be included for every object.
 * At present this is
 * - Object Name
 * - Mesh
 */
void odObject::readCommonDataFromXML(QDomNode root){
  int i;
  QDomElement element;
  QDomNode node;
  
  node=root.firstChild();  
  while (!node.isNull()){
    element=node.toElement();
    if (element.tagName().toLower()=="name"){
      name=element.text();
    }
    if (element.tagName().toLower()=="mesh") meshRef=element.text();
    node=node.nextSibling();
  }
}

void odObject::readFromXML(QDomNode node){
  readCommonDataFromXML(node);
}

double odObject::elevationInfluence(double x, double y){
  return 0.;
}

double odObject::elevation(double x, double y){
  int i;
  double elevation=0.;
  for (i=0;i<nObjects;i++){
    elevation+=allObjects[i]->elevationInfluence(x, y);
  }
  return elevation;
}

/*! \author Tim Brocklehurst
 * Specify the influence of this object on the local velocity in
 * the model reference frame.
 */
odPoint odObject::velocityInfluence(odPoint pos){
  return odPoint(0.,0.,0.);
}

/*! \author Tim Brocklehurst
 * Calculate the local velocity at a point as seen from the model co-ordinate system.
 * Currently only the linear velocity is considered.
 */
odPoint odObject::localVelocity(odPoint pos, int fUid){
  int i;
  double u, v, w;
  odPoint locVel, dVec;
  dVector3 result;

  dBodyGetRelPointVel(*odeBody, pos.x, pos.y, pos.z, result);
  locVel=odPoint(-result[0], -result[1], -result[2]);
  
  for (i=0;i<(int)nObjects;i++){
    if (fUid!=allObjects[i]->uid) locVel+=allObjects[i]->velocityInfluence(pos);
  }
  locVel*=-1.;
  
  return locVel;
}

/*!
 * Allow other objects to setup data prior to solving
 */
void odObject::preSolveSetup(){
  openDataStream("odObject");
}

void odObject::advanceSolver(){
  run();
}
    
void odObject::run(){
  volumetricSolver();
}

double odObject::density(odPoint pos){
  if (pos.z>elevation(pos.x, pos.y)) return constants->rhoAir;
  return constants->rhoWater;
}

/*!
 * Volumetric solver based on the mesh referenced in this class
 */
void odObject::volumetricSolver(){
  int i;
  double fz, zeta, fzt=0.;
  odPoint M;
  
  // Volumetric solver
  for (i=0;i<mesh.active.size();i++){
    zeta=elevation(mesh.active[i].centroid().x, mesh.active[i].centroid().y);
    mesh.active[i].velocity=localVelocity(mesh.active[i].centroid(),0);
    
    if (mesh.active[i].centroid().z<zeta){
      mesh.active[i].pressure=(zeta-mesh.active[i].centroid().z)*density(mesh.active[i].centroid())*constants->g;
      fz=mesh.active[i].projectedAreaXY()*mesh.active[i].pressure;
      // Check whether there is a column of fluid above this cell, if so, consider it a downward force. 
      if (mesh.active[1].normal().z<0) fz=-fz;
      M.x += fz*mesh.active[i].centroid().x;
      M.y += fz*mesh.active[i].centroid().y;
      M.z += fz*mesh.active[i].centroid().z;
//      dBodyAddForceAtPos(*odeBody, 0, 0, fz, mesh.active[i].centroid().x, mesh.active[i].centroid().y, (mesh.active[i].centroid().z+zeta)/2.);
      dBodyAddForceAtRelPos(*odeBody, 0, 0, fz, mesh.polygon[i].centroid().x, mesh.polygon[i].centroid().y, (mesh.polygon[i].centroid().z+zeta)/2.);
      fzt+=fz;
    }else{
      mesh.active[i].pressure=0.;
    }
  }
  fprintf(dataStream,"volumetric time %lf forcez= %lf moment= %lf\t%lf\t%lf\n", *simTime, fzt, M.x, M.y, M.z);
}

void odObject::exportVTK(FILE *stream){
  mesh.exportVTK(stream);
}
