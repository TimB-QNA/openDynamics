#include "odHull.h"
#include "odHullResistance_DELFT.h"
#ifdef GRAPHICS
  #include <vtkStructuredGrid.h>
  #include <vtkProperty.h>
  #include <vtkDataSetMapper.h>
#endif
void odHull::preSolveSetup(){
  openDataStream(QString("odHull"));
}

/*!
 * Reads a mesh stanza from XML. The following elements are supported in addition to the odObject elements:
 * - filename
 *  - filename for the class to load. At present only ASCII STL is supported.
 */
void odHull::readFromXML(QDomNode root){
  QDomNode node;
  QDomElement element;
  odHullResistance_DELFT *delftRes;
  
  readCommonDataFromXML(root);
  node=root.firstChild();
  while (!node.isNull()){
    element=node.toElement();
    if (element.tagName().toLower()=="resistance"){
      if (element.attribute("type").toLower()=="delft"){
        delftRes = new odHullResistance_DELFT();
        resistance=delftRes;
	resistance->readFromXML(node);
      }
    }
    node=node.nextSibling();
  }
}

#ifdef GRAPHICS
/*!
 * For plotting purposes all sections must have the same number of points.
 */
void odHull::plot(){
  
  int nPoints, i, j;
  dVector3 result;
  for (i=0;i<(int)mesh.size();i++) mesh[i].plot();
  if (mesh.size()>0) actor=mesh[0].actor;
}
#endif

/*!
 * This is the solver for this component.
 */
void odHull::run(){
  int i;
  double Csf;
  odPoint R, S, V, FR, Fs, Tf, Tm, Nf, Nm, Fp;
  odPoint vNorm, vTan, maxVel, BV;
  volumetricSolver();
  
  Csf=resistance->ittc57_coeff(mesh.range().x);
//  printf("Csf = %lf\n",Csf);
  
  dVector3 result;
  dBodyGetRelPointVel(*odeBody, 0, 0, 0, result);
  BV=odPoint(result[0], result[1], result[2]);
  printf("Body Velocity = %lf\t%lf\t%lf\n",BV.x, BV.y, BV.z);
  
  // Skin friction solver
  for (i=0;i<mesh.polygon.size();i++){
    V=-mesh.active[i].velocity;
    if (V.mag()>5) V=odPoint(0,0,0);
    vNorm = mesh.active[i].normal() * V.dotProduct_natural(mesh.active[i].normal());
    vTan = V-vNorm;
    
    if (V.mag()>maxVel.mag()) maxVel=V;
    
    if (vTan.mag()>TOLERANCE){
      Fs=vTan/vTan.mag() * 0.5*density(mesh.active[i].centroid())*pow(vTan.mag(),2)*Csf*mesh.active[i].surfaceArea();
    }else{
      Fs=odPoint(0,0,0);
    }
    
    Fp=mesh.active[i].normal() * 0.5*density(mesh.active[i].centroid())*pow(vNorm.mag(),2)*mesh.active[i].surfaceArea();
    mesh.active[i].PtangForce=Fs/mesh.active[i].surfaceArea();
    mesh.active[i].PnormForce=Fp/mesh.active[i].surfaceArea();
    if (i==-1){
      printf("      Panel Location=%lf\t%lf\t%lf\n", mesh.active[i].centroid().x, mesh.active[i].centroid().y, mesh.active[i].centroid().z);
      printf("        Surface Area=%lf\n",mesh.active[i].surfaceArea());
      printf("        Panel Normal=%lf\t%lf\t%lf\n", mesh.active[i].normal().x, mesh.active[i].normal().y, mesh.active[i].normal().z);
      printf("     Inflow Velocity=%lf\t%lf\t%lf\n", V.x, V.y, V.z);
      printf("     Normal Velocity=%lf\t%lf\t%lf\n", vNorm.x, vNorm.y, vNorm.z);
      printf(" Tangential Velocity=%lf\t%lf\t%lf\n", vTan.x, vTan.y, vTan.z);
      printf("        Normal Speed=%lf\n", vNorm.mag());
      printf("    Tangential Speed=%lf\n", vTan.mag());
      printf("Tangential Direction=%lf\t%lf\t%lf\n", (vTan/vTan.mag()).x, (vTan/vTan.mag()).y, (vTan/vTan.mag()).z);
      
      printf("    Tangential Force=%lf\t%lf\t%lf\n", Fs.x, Fs.y, Fs.z);
      printf("        Normal Force=%lf\t%lf\t%lf\n", Fp.x, Fp.y, Fp.z);
    }
    
//    dBodyAddForceAtRelPos(*odeBody, Fs.x+Fp.x, Fs.y+Fp.y, Fs.z+Fp.z, mesh.active[i].centroid().x, mesh.active[i].centroid().y, mesh.active[i].centroid().z);
//    if (i==100) fprintf(stderr,"Poly %i - Vel %lf\t%lf\t%lf\tVNormal  %lf\tForce %lf\t%lf\t%lf\n",i,V.x,V.y,V.z,vNorm,Fx,Fy,Fz);
    Tf+=Fs;
    Tm+=Fs*mesh.active[i].centroid();
    Nf+=Fp;
    Nm+=Fp*mesh.active[i].centroid();
  }
  fprintf(dataStream,"friction_tangential time %lf force= %lf\t%lf\t%lf moment= %lf\t%lf\t%lf\n", *simTime, Tf.x, Tf.y, Tf.z, Tm.x, Tm.y, Tm.z);
  fprintf(dataStream,"friction_normal     time %lf force= %lf\t%lf\t%lf moment= %lf\t%lf\t%lf\n", *simTime, Nf.x, Nf.y, Nf.z, Nm.x, Nm.y, Nm.z);
  fprintf(dataStream,"friction_total      time %lf force= %lf\t%lf\t%lf moment= %lf\t%lf\t%lf\n",
	             *simTime, (Tf+Nf).x, (Tf+Nf).y, (Tf+Nf).z, (Tm+Nm).x, (Tm+Nm).y, (Tm+Nm).z);
  printf("max Velocity = %lf m/s (%lf\t%lf\t%lf)\n",maxVel.mag(), maxVel.x, maxVel.y, maxVel.z);
  printf("friction_total      time %lf force= %lf\t%lf\t%lf moment= %lf\t%lf\t%lf\n",
	             *simTime, (Tf+Nf).x, (Tf+Nf).y, (Tf+Nf).z, (Tm+Nm).x, (Tm+Nm).y, (Tm+Nm).z);
  
  resistance->setParametersFromBody(odeBody);
  R=resistance->calculateForces();
//  printf("Residuary=%lf\t%lf\t%lf\n", R.x, R.y, R.z);
  dBodyAddRelForceAtRelPos(*odeBody, R.x, R.y, R.z, 0, 0, 0);
  
//  dBodyAddRelForceAtRelPos(*odeBody, 10000, 0, 0, 0, 0, 0);
}