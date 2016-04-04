#include "odWorld.h"
#include "odTerrain.h"
#include <ode/ode.h>
#include <math.h>
#include <time.h>
#include <stdio.h>

void nearCallback (void *data, dGeomID o1, dGeomID o2){
  /* exit without doing anything if the two bodies are connected by a joint */
  int nContacts;
  dBodyID b1,b2;
  dContact contact;
  dVector3 P;
  odWorld *ref=reinterpret_cast<odWorld*>(data);
    
  b1 = dGeomGetBody(o1);
  b2 = dGeomGetBody(o2);
  if (b1 && b2 && dAreConnected (b1,b2)) return;
  
  contact.surface.mode = 0;
  contact.surface.mu = 0.1;
  contact.surface.mu2 = 0;
  
  dBodyGetRelPointPos(b1, 0, 0, 0, P);
//  printf("Body 1 Pos %lf\t%lf\t%lf\n", P[0], P[1], P[2]);  
  dBodyGetRelPointPos(b2, 0, 0, 0, P);
//  printf("Body 2 Pos %lf\t%lf\t%lf\n", P[0], P[1], P[2]);

//  nContacts=dCollide (o1, o2, 1, &contact.geom, sizeof(dContactGeom));
//  printf("Found %i contacts\n", nContacts);
//  if () {
//    printf("Creating joint\n");
//    dJointID c = dJointCreateContact (ref->world, ref->contactgroup, &contact);
//    printf("Attaching joint\n");
//    dJointAttach(c,b1,b2);
//  }
}

odWorld::odWorld(){
  tIndex=0;
  simTime=0.;
  timeStep=0.05;
}

double odWorld::currentTime(){
  return simTime;
}

void odWorld::setupReferences(){
  int i, j, k, l, comp, nObjects;
//  odObject **objectList;
  
  // Count the number of components, and provide each with a uid.
  // At the same time, set some useful pointers. 
  comp=2; // Waves and current will take some space...
  seastate.constants=&constants;
  seastate.timeStep=&timeStep;
  seastate.simTime=&simTime;
  
  for (i=0; i<vessel.size(); i++){
    vessel[i].simTime=&simTime;
    for (j=0; j<vessel[i].component.size(); j++){
      vessel[i].component[j]->constants=&constants;
      vessel[i].component[j]->odeBody=&vessel[i].odeBody;
      vessel[i].component[j]->timeStep=&timeStep;
      vessel[i].component[j]->simTime=&simTime;
      comp++;
    }
  }
  comp+=cables.size();
  
  // create the object list
  printf("Found %i components\n",comp);
  nObjects=comp;
  objectList = new odObject*[nObjects];
  
  // Populate the environmentals...
  objectList[0] = &seastate;
  objectList[1] = &current;
  
  // Fill the rest of the list...
  comp=2;
  for (i=0; i<vessel.size(); i++){
    for (j=0; j<vessel[i].component.size(); j++){
      objectList[comp]=vessel[i].component[j];
      comp++;
    }
  }
  for (i=0; i<cables.size(); i++){
    objectList[comp] = &cables[i];
    comp++;
  }
  
  // and propogate it suitably.
  seastate.allObjects = objectList;
  seastate.nObjects   = nObjects;
  for (i=0; i<vessel.size(); i++){
    for (j=0; j<vessel[i].component.size(); j++){
      vessel[i].component[j]->allObjects = objectList;
      vessel[i].component[j]->nObjects=nObjects;
    }
  }
  
  for (i=0; i<cables.size(); i++){
    cables[i].allObjects = objectList;
    cables[i].nObjects=nObjects;
  }
  
  // Propogate constants etc.
  for (i=0;i<nObjects;i++){
    objectList[i]->constants=&constants;
    objectList[i]->timeStep=&timeStep;
    objectList[i]->simTime=&simTime;
    objectList[i]->uid=i;
  }
}


void odWorld::readFromXML(QDomNode root){
  QDomNode node;
  QDomElement element;
  odBody tmpVessel;
  odSpring tmpSpring;
  odCable tmpCable;
  odTerrain tmpTerrain;
  
  node=root.firstChild();
  while (!node.isNull()){
    element=node.toElement();
    if (element.tagName().toLower()=="simulation") readSimulationXML(node);
    if (element.tagName().toLower()=="seastate") seastate.readFromXML(node);
    if (element.tagName().toLower()=="current") current.readFromXML(node);
    if (element.tagName().toLower()=="vessel"){
      tmpVessel.readFromXML(node);
      vessel.push_back(tmpVessel);
    }
    if (element.tagName().toLower()=="spring"){
      tmpSpring.readFromXML(node);
      springs.push_back(tmpSpring);
    }
    
    if (element.tagName().toLower()=="cable"){
      tmpCable.readFromXML(node);
      cables.push_back(tmpCable);
    }
    if (element.tagName().toLower()=="terrain"){
      tmpTerrain.readFromXML(node);
      terrain.push_back(tmpTerrain);
    }
    node=node.nextSibling();
  }
  printf("Found %i vessels\n",(int)vessel.size());
}

void odWorld::readSimulationXML(QDomNode root){
  QDomNode node;
  QDomElement element;
  odBody tmpVessel;
  odTerrain tmpTerrain;
  
  node=root.firstChild();
  while (!node.isNull()){
    element=node.toElement();
    if (element.tagName().toLower()=="timestep") timeStep=element.text().toDouble();
    if (element.tagName().toLower()=="endtime") endTime=element.text().toDouble();
    node=node.nextSibling();
  }
}

void odWorld::initialiseSolver(){
  int i;
  double l,b,d, vol;
  dMatrix3 R;
  FILE *vtkFile;
  char fname[80];
  
  world = dWorldCreate();
  dWorldSetERP(world, 1);
  dWorldSetQuickStepNumIterations(world, 10);
  dWorldSetGravity(world, 0, 0, -constants.g);
  
  printf("Creating contact group\n");
  space = dHashSpaceCreate(0);
  contactgroup = dJointGroupCreate(0);
  
  printf("Creating terrain\n");
  for (i=0;i<terrain.size();i++){
    terrain[i].createTerrain(space);
  }
  
  printf("We have %i vessels\n",(int)vessel.size());
  
  for (i=0; i<(int)vessel.size(); i++){
    vessel[i].setupBody(&world);
  }
  
  setupReferences();
  seastate.createSeaState();
  for (i=0; i<(int)springs.size(); i++){
    springs[i].attachBodies(&vessel);
//    springs[i].preSolveSetup();
  }
  for (i=0; i<(int)cables.size(); i++){
    cables[i].findBodies(&vessel);
    cables[i].setupBody(&world, space);
    cables[i].preSolveSetup();
  }  
  for (i=0; i<(int)vessel.size(); i++){
    vessel[i].preSolveSetup();
  }
  simTime=0;
  
  sprintf(fname,"waves_ts%i.vtu", 0);
  vtkFile=fopen(fname, "w");
  seastate.exportVTK(vtkFile);
  fclose(vtkFile);
    
  for (i=0;i<cables.size();i++){
    sprintf(fname,"cable_%i_ts%i.vtp", i, 0);
    vtkFile=fopen(fname, "w");
    cables[i].exportVTK(vtkFile);
    fclose(vtkFile);
  }
  
  for (i=0; i<(int)vessel.size(); i++){
    vessel[i].postAdvance();
  }
  constants.simTime=0;
  printf("Ready to solve\n");
}

void odWorld::run(){
  int i;
  clock_t start, end, diff;
  double secs;
  
  FILE *vtkFile;
  char fname[80];
  
  start=clock();
  for (i=0;simTime<=endTime;i++){
    constants.simTime=simTime;
    printf("Simulation time=%lf\n",simTime);
    advanceSolver();
  
    sprintf(fname,"waves_ts%i.vtu", tIndex);
    vtkFile=fopen(fname, "w");
    seastate.exportVTK(vtkFile);
    fclose(vtkFile);
    
    for (i=0;i<cables.size();i++){
      sprintf(fname,"cable_%i_ts%i.vtp", i, tIndex);
      vtkFile=fopen(fname, "w");
      cables[i].exportVTK(vtkFile);
      fclose(vtkFile);
      cables[i].postAdvance();
    }
    
    for (i=0; i<(int)vessel.size(); i++){
      vessel[i].postAdvance();
    }
    
  }
  end=clock();
  diff=end-start;
  secs=(double)diff/(double)CLOCKS_PER_SEC;
  fprintf(stderr,"Time taken %lf seconds for %lf sec of simulation. Speed %.3lfx real time\n", secs, endTime, endTime/secs);
}

void odWorld::advanceSolver(){
  int i;
  for (i=0; i<vessel.size(); i++){
    vessel[i].advanceSolver();
  }
  for (i=0; i<cables.size(); i++){
    cables[i].advanceSolver();
  }
  
  dSpaceCollide (space, this ,&nearCallback);
//  dWorldQuickStep(world, timeStep);
  dWorldStep(world, timeStep);
  /* remove all contact joints */
  dJointGroupEmpty (contactgroup);
    
  simTime+=timeStep;
  tIndex++;
}
