#include "odSolver.h"
#include <math.h>
#include <stdio.h>

void odSolver::initialise(){
  int i;
  simState.initialise();
  for (i=0;i<component.size();i++){
    delete component[i];
  }
  timeStep=0.01;
  model.linear=odPoint(0,0,0);
  model.linearVel=odPoint(3,0,0);
  model.rotational=odPoint(0,-.05,0);
  model.rotationalVel=odPoint(0,0,0);
}

void odSolver::advance(){
  int i;
  double mass=.02;
  double gyradius=0.15;
  odPoint grav=odPoint(0,0,-simConstants.g);
  odPoint CG=odPoint(.05,0,0);
  printf("Time = %lf s\n",simState.time);
  for (i=0;i<(int)component.size();i++){
    component[i]->run();
  }
  
  model.linearAcc=odPoint(0,0,0);
  for (i=0;i<(int)component.size();i++) model.linearAcc+=component[i]->modelForce;  
  simState.linearAcc=model.linearAcc.rotate(simState.rotational);
  simState.linearAcc+=grav*mass;
  printf("Net Force = "); simState.linearAcc.display();
  simState.linearAcc/=mass;
  simState.linear+=simState.linearVel*timeStep;
  simState.linearVel+=simState.linearAcc*timeStep;
  
//  printf("Position = "); model.linear.display();
//  printf("Velocity = "); simState.linearVel.display();
  
  model.rotationalAcc=odPoint(0,0,0);
  for (i=0;i<(int)component.size();i++) model.rotationalAcc+=component[i]->modelMoment;
//  model.rotationalAcc.x+=(mass*grav.z)*CG.rotate(simState.rotational).y;
  model.rotationalAcc.y+=(mass*grav.z)*CG.rotate(simState.rotational).x;
  simState.rotationalAcc=model.rotationalAcc.rotate(simState.rotational);
  
  printf("Net Moment = "); simState.rotationalAcc.display();

  simState.rotationalAcc/=pow(gyradius,2)*mass;
  simState.rotational+=simState.rotationalVel*timeStep;
  simState.rotationalVel+=simState.rotationalAcc*timeStep;

  simState.time+=timeStep;
  
//   printf("Rotation     = "); model.rotational.display();
//   printf("Rotation Vel = "); model.rotationalVel.display();
//   printf("Rotation Acc = "); model.rotationalAcc.display();
//  
//  printf("%lf\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\n",simState.time,
//         simState.linear.x, simState.linear.y, simState.linear.z,
//         model.rotationalAcc.x, model.rotationalAcc.y, model.rotationalAcc.z);
  
}