#include "odSpring.h"

odSpring::odSpring() : odObject(){
  vtksuffix="vtp";
}

QString odSpring::type(){
  return QString("odSpring");
}

void odSpring::attachBodies(vector<odBody> *vessel){
  int i;
  odPoint loc1, loc2, d;
  fb1=false; fb2=false;
  
  for (i=0;i<vessel->size();i++){
    if (vessel->at(i).name==b1_name){
      body1=&vessel->at(i);
      vessel->at(i).component.push_back(this);
      fb1=true;
    }
    if (vessel->at(i).name==b2_name){
      body2=&vessel->at(i);
      vessel->at(i).component.push_back(this);
      fb2=true;
    }
  }
}

void odSpring::preSolveSetup(){
  char fname[80];
  odPoint loc1, loc2, d, F;

//  printf("Creating force file for spring\n");
  sprintf(fname,"Forces_%s.dat",name.toLatin1().data());
  forcesFile=fopen(fname,"w");
  fprintf(forcesFile,"# End forces (Time, Tension, F1x, F1y, F1z, F1tot, F2x, F2y, F2z, F2tot)\n");
  
//  printf("Creating position file for spring\n");
  sprintf(fname,"Position_%s.dat",name.toLatin1().data());
  positionFile=fopen(fname,"w");
  fprintf(positionFile,"# End positions (Time, P1x, P1y, P1z, P2x, P2y, P2z)\n");
  
  if (fb1){
    loc1=body1->globalPosition(end1);
  }else{
    loc1=end1;
  }
  if (fb2){
    loc2=body2->globalPosition(end2);
  }else{
    loc2=end2;
  }
  
  d=loc2-loc1;
  initialLength=d.mag();
  
//  printf("Initial spring length %lf m\n",initialLength);  
  // calculate effective initial length based on preloading
  initialLength-=preload/(k/initialLength);
//  printf("Corrected for pre-load %lf m\n",initialLength);

  F=d/d.mag()*preload;
  tension=preload;
  force1=-F;
  force2=F;
}

void odSpring::run(){
  double deltaLength;
  odPoint d, F, loc1, loc2;
  
  if (fb1){
    loc1=body1->globalPosition(end1);
  }else{
    loc1=end1;
  }
  if (fb2){
    loc2=body2->globalPosition(end2);
  }else{
    loc2=end2;
  }

  d=loc2-loc1;
  
  if (d.mag()<TOLERANCE) return;

  deltaLength=d.mag()-initialLength;
//  printf("deltaLength=%lf\n", deltaLength);
  F=d/d.mag()*(deltaLength*(k/initialLength));
  
  tension=deltaLength*k;
  
  if (tensionOnly && tension<0) F=odPoint();
  force1=F;
  force2=-F;

//  fprintf(forcesFile,"%lf\t",*simTime);
  fprintf(forcesFile,"%lf\t",tension);
  fprintf(forcesFile,"%lf\t%lf\t%lf\t%lf\t",F.x, F.y, F.z, F.mag());
  fprintf(forcesFile,"%lf\t%lf\t%lf\t%lf\n",-F.x, -F.y, -F.z, F.mag());

//  fprintf(positionFile,"%lf\t",*simTime);
  fprintf(positionFile,"%lf\t%lf\t%lf\t", loc1.x, loc1.y, loc1.z);
  fprintf(positionFile,"%lf\t%lf\t%lf\n", loc2.x, loc2.y, loc2.z);

  if (fb1){  
    dBodyAddForceAtRelPos(body1->odeBody, F.x, F.y, F.z, end1.x, end1.y, end1.z);
//    printf("Applying force %lf\t%lf\t%lf to body 1\n",F.x, F.y, F.z);
  }
  if (fb2){
    dBodyAddForceAtRelPos(body2->odeBody, -F.x, -F.y, -F.z, end2.x, end2.y, end2.z);
//    printf("Applying force %lf\t%lf\t%lf to body 2\n",-F.x, -F.y, -F.z);
  }
}

/*!
 * Specify body1 or body2 as blank to connect one end of the spring
 * to the "world"
 */
void odSpring::readFromXML(QDomNode root){
  odPoint stockS, stockE;
  QDomNode node, subNode;
  QDomElement element, subElement;
  readCommonDataFromXML(root);
  
  tensionOnly=false;
  node=root.firstChild();
  while (!node.isNull()){
    element=node.toElement();
    if (element.tagName().toLower()=="body1")           b1_name=element.text();
    if (element.tagName().toLower()=="end1")            end1.readFromXML(element);
    if (element.tagName().toLower()=="body2")           b2_name=element.text();
    if (element.tagName().toLower()=="end2")            end2.readFromXML(element);
    if (element.tagName().toLower()=="preload")         preload=element.text().toDouble();
    if (element.tagName().toLower()=="spring-constant") k=element.text().toDouble();
    if (element.tagName().toLower()=="tensiononly" && element.text().toLower()==QString("true")) tensionOnly=true;
    node=node.nextSibling();
  }
}

void odSpring::exportVTK(FILE *stream){
  odPoint d, F, loc1, loc2;
  
  if (fb1){
    loc1=body1->globalPosition(end1);
  }else{
    loc1=end1;
  }
  if (fb2){
    loc2=body2->globalPosition(end2);
  }else{
    loc2=end2;
  }
  
  fprintf(stream,"<?xml version=\"1.0\"?>\n");
  
  fprintf(stream,"<VTKFile type=\"PolyData\" version=\"0.1\" byte_order=\"LittleEndian\">\n");

  fprintf(stream,"<PolyData>\n");
  fprintf(stream,"  <Piece NumberOfPoints=\"%i\" NumberOfLines=\"%i\" >\n", 2, 1);

  fprintf(stream,"    <Points>\n");
  fprintf(stream,"      <DataArray type=\"Float32\" NumberOfComponents=\"3\" format=\"ascii\">\n");
  fprintf(stream,"%lf %lf %lf ", loc1.x, loc1.y, loc1.z);
  fprintf(stream,"%lf %lf %lf ", loc2.x, loc2.y, loc2.z);
  fprintf(stream,"\n");
  fprintf(stream,"      </DataArray>\n");
  fprintf(stream,"    </Points>\n");
  
  fprintf(stream,"    <Lines>\n");
  fprintf(stream,"      <DataArray type=\"Int32\" Name=\"connectivity\" format=\"ascii\">\n");
  fprintf(stream,"1 0\n");
  fprintf(stream,"      </DataArray>\n");
  fprintf(stream,"      <DataArray type=\"Int32\" Name=\"offsets\" format=\"ascii\">\n");
  fprintf(stream,"2\n");
  fprintf(stream,"      </DataArray>\n");
  fprintf(stream,"    </Lines>\n");
  
  fprintf(stream,"    <PointData>\n");
  fprintf(stream,"      <DataArray type=\"Float32\" Name=\"Force\" NumberOfComponents=\"3\" format=\"ascii\">\n");
  fprintf(stream,"%lf %lf %lf ", force1.x, force1.y, force1.z);
  fprintf(stream,"%lf %lf %lf\n", force2.x, force2.y, force2.z);
  fprintf(stream,"      </DataArray>\n");
  fprintf(stream,"    </PointData>\n");
  
  fprintf(stream,"    <CellData>\n");
  fprintf(stream,"      <DataArray type=\"Float32\" Name=\"Tension\" NumberOfComponents=\"1\" format=\"ascii\">\n");
  fprintf(stream,"%lf\n", tension);
  fprintf(stream,"      </DataArray>\n");
  fprintf(stream,"    </CellData>\n");
  
  fprintf(stream,"  </Piece>\n");
  fprintf(stream,"</PolyData>\n");
  fprintf(stream,"</VTKFile>\n");
}