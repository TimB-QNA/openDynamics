#include "odCable.h"

odCable::odCable() : odObject(){
  nSegments=10; // reasonable default.
  vtksuffix="vtp";
}

QString odCable::type(){
  return QString("odCable");
}

void odCable::findBodies(vector<odBody> *vessel){
  int i;
  odPoint loc1, loc2, d;
  body1=NULL; body2=NULL;
  
  for (i=0;i<vessel->size();i++){
    if (vessel->at(i).name==b1_name){
      body1=&vessel->at(i);
      fb1=true;
    }
    if (vessel->at(i).name==b2_name){
      body2=&vessel->at(i);
      fb2=true;
    }
  }
}

void odCable::setupBody(dWorldID *world, dSpaceID space){
  int i;
  double pi=4.*atan(1.);
  double segLen, dL, x;
  double dd, s, rs;
  dVector3 OP;
  odPoint loc1, loc2, d, P, c;
  dMass m;
  dGeomID geometry[nSegments];
//  dQuaternion align;
  dReal align[4];
  
  printf("Setting up cable model\n");
  
  if (body1!=NULL){
    loc1=body1->globalPosition(pos1);
  }else{
    loc1=pos1;
  }
  if (body2!=NULL){
    loc2=body2->globalPosition(pos2);
  }else{
    loc2=pos2;
  }
  
  d=loc2-loc1;
  dL=d.mag()/(double)nSegments;
  segLen=length/(double)nSegments;
  printf("Distance = %lf m\n",d.mag());
  printf("Segment length = %lf m\n",segLen);
  
  printf("Allocating memory\n");
  element = new dBodyID[nSegments];
  joint = new dJointID[nSegments-1];
  
  printf("Creating elements\n");
  for (i=0;i<nSegments;i++){
    element[i] = dBodyCreate (*world);
//    dBodySetDamping(element[i], 0.1, 0.6);
    dMassSetBox (&m, weight*segLen, segLen, diameter, diameter);
    dMassAdjust (&m, weight*segLen);
    dBodySetMass (element[i], &m);
    x=((double)i+.5)*segLen;
    dBodySetPosition(element[i], x, 0, 0);
    geometry[i] = dCreateBox(space, length, diameter, diameter);
    dGeomSetBody(geometry[i], element[i]);
  }
  printf("Creating internal joints\n");
  for (i=0;i<nSegments-1;i++){
    joint[i] = dJointCreateBall(*world,0);
    dJointAttach (joint[i],element[i],element[i+1]);
    x = ((double)i+1.)*segLen;
    dJointSetBallAnchor (joint[i],x,0,0);
  }
/*
  c = d.crossProduct(odPoint(0,0,1)); 
  dd = d.dotProduct_natural(odPoint(0,0,1)); 
  s = sqrt((1.0 + dd)*2.0); 
  rs = 1.0/s;
  align[0]=s*0.5; align[1]=c.x*rs; align[2]=c.y*rs; align[3]=c.z*rs;

  printf("Moving segments to world positions...\n");
  for (i=0;i<nSegments;i++){
    P=loc1+d*((double)i)/(nSegments-1);
    if (i==0) P.x+=segLen/2.;
    if (i==nSegments-1) P.x-=segLen/2.;
    dBodySetPosition(element[i], P.x, P.y, P.z);
//    dBodySetQuaternion(element[i], align);
  }
*/  
  printf("Creating end joint 1\n");
  end1 = dJointCreateBall(*world,0);
  if (body1!=NULL){
    dJointAttach(end1, body1->odeBody, element[0]);
  }else{
    dJointAttach(end1, 0, element[0]);
  }
//  dJointSetBallAnchor (end1, loc1.x, loc1.y, loc1.z);
  dJointSetBallAnchor (end1, 0, 0, 0);
  end1Feedback = new dJointFeedback;
  dJointSetFeedback (end1, end1Feedback);
  
  printf("Creating end joint 2\n");  
  end2 = dJointCreateBall(*world,0);
  if (body2!=NULL){
    dJointAttach(end2, body2->odeBody, element[nSegments-1]);
  }else{
    dJointAttach(end2, 0, element[nSegments-1]);
  }
//  dJointSetBallAnchor (end2, loc2.x, loc2.y, loc2.z);
  dJointSetBallAnchor (end2, nSegments*segLen, 0, 0);
  end2Feedback = new dJointFeedback;
  dJointSetFeedback (end2, end1Feedback);
}


void odCable::preSolveSetup(){
  char fname[80];
  sprintf(fname,"Forces_%s.dat",name.toLatin1().data());
  forcesFile=fopen(fname,"w");
  fprintf(forcesFile,"# End forces (Time, F1x, F1y, F1z, F1tot, F2x, F2y, F2z, F2tot)\n");
  
//  printf("Creating position file for spring\n");
//  sprintf(fname,"Position_%s.dat",name.toLatin1().data());
//  positionFile=fopen(fname,"w");
//  fprintf(positionFile,"# Catenary Positions (Time, Px, Py, Pz)\n");
}

void odCable::run(){
  // The opendynamics solver handles connections by default (unless we wish to do something odd with them).
//  dBodyAddRelForceAtRelPos(element[nSegments-1], 50, 0, 50, 0, 0, 0);

}

void odCable::postAdvance(){
  odPoint F;
  end1Feedback=dJointGetFeedback(end1);
  F=odPoint(end1Feedback->f1[0], end1Feedback->f1[1], end1Feedback->f1[2]);
  fprintf(forcesFile,"%lf\t%lf\t%lf\t%lf\t", F.x, F.y, F.z, F.mag());
  end2Feedback=dJointGetFeedback(end2);
  F=odPoint(end2Feedback->f1[0], end2Feedback->f1[1], end2Feedback->f1[2]);
  fprintf(forcesFile,"%lf\t%lf\t%lf\t%lf\n", F.x, F.y, F.z, F.mag());
}

/*!
 * Specify body1 or body2 as blank to connect one end of the spring
 * to the "world"
 */
void odCable::readFromXML(QDomNode root){
  odPoint stockS, stockE;
  QDomNode node, subNode;
  QDomElement element, subElement;
  readCommonDataFromXML(root);
  
  tensionOnly=false;
  node=root.firstChild();
  while (!node.isNull()){
    element=node.toElement();
    if (element.tagName().toLower()=="body1")           b1_name=element.text();
    if (element.tagName().toLower()=="end1")            pos1.readFromXML(element);
    if (element.tagName().toLower()=="body2")           b2_name=element.text();
    if (element.tagName().toLower()=="end2")            pos2.readFromXML(element);
    if (element.tagName().toLower()=="length")          length=element.text().toDouble();
    if (element.tagName().toLower()=="segments")        nSegments=element.text().toInt();   // Must always be an even number
    if (element.tagName().toLower()=="weight")          weight=element.text().toDouble();   // Weight in Kg/m
    if (element.tagName().toLower()=="diameter")        diameter=element.text().toDouble();
    node=node.nextSibling();
  }
  
  /*
  <cable>
    <name>Example Chain</name>
    <type>chain</type>
    <end1 x="0" y="50" z="-20"/>
    <body1></body1>
    <end2 x="0" y="0" z="-1"/>
    <body2>Buoy</body2>
    <length>80</length>
    <segments>80</segments>
  </cable>
  */  
}

void odCable::exportJointVTK(FILE *stream){
  int i;
  dVector3 P, j0, j1;
  odPoint d;

  fprintf(stream,"<?xml version=\"1.0\"?>\n");
  
  fprintf(stream,"<VTKFile type=\"PolyData\" version=\"0.1\" byte_order=\"LittleEndian\">\n");

  fprintf(stream,"<PolyData>\n");
  fprintf(stream,"  <Piece NumberOfPoints=\"%i\" NumberOfLines=\"1\" >\n", nSegments+1);

  fprintf(stream,"    <Points>\n");
  fprintf(stream,"      <DataArray type=\"Float32\" NumberOfComponents=\"3\" format=\"ascii\">\n");
  dJointGetBallAnchor(end1, P);
  fprintf(stream,"%lf %lf %lf\n", P[0], P[1], P[2]);
  for (i=0;i<nSegments-1;i++){
    dJointGetBallAnchor (joint[i],P);
    fprintf(stream,"%lf %lf %lf\n", P[0], P[1], P[2]);
  }
  dJointGetBallAnchor(end2,P);
  fprintf(stream,"%lf %lf %lf\n", P[0], P[1], P[2]);
  fprintf(stream,"      </DataArray>\n");
  fprintf(stream,"    </Points>\n");
  
  fprintf(stream,"    <Lines>\n");
  fprintf(stream,"      <DataArray type=\"Int32\" Name=\"connectivity\" format=\"ascii\">\n");
  for (i=0;i<nSegments+1;i++){
    fprintf(stream,"%i\n",i);
  }
  fprintf(stream,"      </DataArray>\n");
  fprintf(stream,"      <DataArray type=\"Int32\" Name=\"offsets\" format=\"ascii\">\n");
    fprintf(stream,"%i\n", nSegments+1);
  fprintf(stream,"      </DataArray>\n");
  fprintf(stream,"    </Lines>\n");
/*  
  fprintf(stream,"    <PointData>\n");
  fprintf(stream,"      <DataArray type=\"Float32\" Name=\"Force\" NumberOfComponents=\"3\" format=\"ascii\">\n");
  fprintf(stream,"%lf %lf %lf ", force1.x, force1.y, force1.z);
  fprintf(stream,"%lf %lf %lf\n", force2.x, force2.y, force2.z);
  fprintf(stream,"      </DataArray>\n");
  fprintf(stream,"    </PointData>\n");
*/  
  fprintf(stream,"    <CellData>\n");
  fprintf(stream,"      <DataArray type=\"Float32\" Name=\"Element Length\" NumberOfComponents=\"1\" format=\"ascii\">\n");
  // First length from end1 to joint 0
  dJointGetBallAnchor(end1, j0);
  dJointGetBallAnchor(joint[0], j1);
  d=odPoint(j0[0], j0[1], j0[2])-odPoint(j1[0], j1[1], j1[2]);
  fprintf(stream,"%lf\n", d.mag());
  for (i=0;i<nSegments-2;i++){
    dJointGetBallAnchor(joint[i], j0);
    dJointGetBallAnchor(joint[i+1], j1);
    d=odPoint(j0[0], j0[1], j0[2])-odPoint(j1[0], j1[1], j1[2]);
    fprintf(stream,"%lf\n", d.mag());
  }
  fprintf(stream,"%lf\n", 0);
  fprintf(stream,"      </DataArray>\n");
  fprintf(stream,"    </CellData>\n");

  fprintf(stream,"  </Piece>\n");
  fprintf(stream,"</PolyData>\n");
  fprintf(stream,"</VTKFile>\n");
}

void odCable::exportVTK(FILE *stream){
  int i;
  double segLen=length/(double)nSegments;
  dVector3 P, j0, j1;
  odPoint d;

  fprintf(stream,"<?xml version=\"1.0\"?>\n");
  
  fprintf(stream,"<VTKFile type=\"PolyData\" version=\"0.1\" byte_order=\"LittleEndian\">\n");

  fprintf(stream,"<PolyData>\n");
  fprintf(stream,"  <Piece NumberOfPoints=\"%i\" NumberOfLines=\"%i\" >\n", nSegments*2, nSegments);

  fprintf(stream,"    <Points>\n");
  fprintf(stream,"      <DataArray type=\"Float32\" NumberOfComponents=\"3\" format=\"ascii\">\n");
  for (i=0;i<nSegments;i++){
    dBodyGetRelPointPos (element[i], -segLen/2., 0, 0, P);
    fprintf(stream,"%lf %lf %lf ", P[0], P[1], P[2]);
    dBodyGetRelPointPos (element[i], segLen/2., 0, 0, P);
    fprintf(stream,"%lf %lf %lf\n", P[0], P[1], P[2]);
  }
  fprintf(stream,"      </DataArray>\n");
  fprintf(stream,"    </Points>\n");
  
  fprintf(stream,"    <Lines>\n");
  fprintf(stream,"      <DataArray type=\"Int32\" Name=\"connectivity\" format=\"ascii\">\n");
  for (i=0;i<nSegments;i++){
    fprintf(stream,"%i %i\n",2*i, 2*i+1);
  }
  fprintf(stream,"      </DataArray>\n");
  fprintf(stream,"      <DataArray type=\"Int32\" Name=\"offsets\" format=\"ascii\">\n");
  for (i=0;i<nSegments;i++){
    fprintf(stream,"%i\n", 2*i+2);
  }
  fprintf(stream,"      </DataArray>\n");
  fprintf(stream,"    </Lines>\n");
/*  
  fprintf(stream,"    <PointData>\n");
  fprintf(stream,"      <DataArray type=\"Float32\" Name=\"Force\" NumberOfComponents=\"3\" format=\"ascii\">\n");
  fprintf(stream,"%lf %lf %lf ", force1.x, force1.y, force1.z);
  fprintf(stream,"%lf %lf %lf\n", force2.x, force2.y, force2.z);
  fprintf(stream,"      </DataArray>\n");
  fprintf(stream,"    </PointData>\n");
*/  
  fprintf(stream,"    <CellData>\n");
  fprintf(stream,"      <DataArray type=\"Float32\" Name=\"Element Length\" NumberOfComponents=\"1\" format=\"ascii\">\n");
  // First length from end1 to joint 0
  dJointGetBallAnchor(end1, j0);
  dJointGetBallAnchor(joint[0], j1);
  d=odPoint(j0[0], j0[1], j0[2])-odPoint(j1[0], j1[1], j1[2]);
  fprintf(stream,"%lf\n", d.mag());
  for (i=0;i<nSegments-2;i++){
    dJointGetBallAnchor(joint[i], j0);
    dJointGetBallAnchor(joint[i+1], j1);
    d=odPoint(j0[0], j0[1], j0[2])-odPoint(j1[0], j1[1], j1[2]);
    fprintf(stream,"%lf\n", d.mag());
  }
  fprintf(stream,"%lf\n", 0);
  fprintf(stream,"      </DataArray>\n");
  fprintf(stream,"    </CellData>\n");
  
  fprintf(stream,"    <CellData>\n");
  fprintf(stream,"      <DataArray type=\"Int32\" Name=\"Element ID\" NumberOfComponents=\"1\" format=\"ascii\">\n");
  for (i=0;i<nSegments;i++){
    fprintf(stream,"%i\n", i);
  }
  fprintf(stream,"%lf\n", 0);
  fprintf(stream,"      </DataArray>\n");
  fprintf(stream,"    </CellData>\n");
  
  fprintf(stream,"  </Piece>\n");
  fprintf(stream,"</PolyData>\n");
  fprintf(stream,"</VTKFile>\n");
}