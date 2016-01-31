#include "odPolygon.h"

odPolygon::odPolygon(){
  clear();
}

void odPolygon::clear(){
  pt.clear();
  srfArea=-1.;
  projectedXY=-1.;
  cent.hasChanged=true;
  norm.hasChanged=true;
}

/*!
 * Translate this polygon by the given amount.
 * This is used to move the mesh relative to the CG
 * when the case is set up. The centroid will change
 * by the translation, so this handled suitably.
 */
void odPolygon::translate(odPoint translation){
  int i;
  for (i=0;i<pt.size();i++) pt[i]+=translation;
  cent+=translation;
}

double odPolygon::surfaceArea(){
  odPoint d0, d1, d2;
  double st;
  
  if (srfArea<=0.){
    // We use Heron's formula here for both projected and total area
    d0=pt[0]-pt[1];
    d1=pt[1]-pt[2];
    d2=pt[2]-pt[0];

    st=.5*(d0.mag()+d1.mag()+d2.mag());
    srfArea=sqrt(st*(st-d0.mag())*(st-d1.mag())*(st-d2.mag()));
    if (srfArea!=srfArea){
//      printf("NAN detected at %lf\t%lf\t%lf area assumed 0\n", centroid().x, centroid().y, centroid().z);
      srfArea=0.;
    }

    
  }
  return srfArea;
}

double odPolygon::projectedAreaXY(){
  odPoint d0, d1, d2;
  double st;
  
  if (projectedXY<=0){
    // We use Heron's formula here for both projected and total area
    d0=pt[0]-pt[1]; d0.z=0.;
    d1=pt[1]-pt[2]; d1.z=0.;
    d2=pt[2]-pt[0]; d2.z=0.;

    st=.5*(d0.mag()+d1.mag()+d2.mag());
    projectedXY=sqrt(st*(st-d0.mag())*(st-d1.mag())*(st-d2.mag()));
    if (projectedXY!=projectedXY){
//      printf("NAN detected at %lf\t%lf\t%lf area assumed 0\n", centroid().x, centroid().y, centroid().z);
      projectedXY=0.;
    }
  }
  return projectedXY;
}

odPoint odPolygon::centroid(){
  int i;
  if (cent.hasChanged){
    cent=odPoint(0,0,0);
    for (i=0;i<pt.size();i++){
      cent+=pt[i];
    }
    cent/=(double)pt.size();
    cent.hasChanged=false;
  }
  return cent;
}

odPoint odPolygon::normal(){
  odPoint v1, v2, n;
  if (pt.size()<3) return odPoint();
  
  v1=pt[1]-pt[0];
  v2=pt[2]-pt[0];
  
  n=v1.crossProduct(v2);
  norm=n/n.mag();
  
  return norm;
}

/*!
 * Transform this polygon and return the output
 * as new polygon. In this transform we apply a
 * rigid transform, so whilst the following will
 * change:
 * - Projected area
 * - The centroid
 * - The normal vector
 * The following won't:
 * - Surface area
 */
odPolygon odPolygon::transformed(dBodyID *odeBody){
  int i;
  odPolygon outPoly;
  dVector3 result;
  
  for (i=0;i<pt.size();i++){
    dBodyGetRelPointPos(*odeBody, pt[i].x, pt[i].y, pt[i].z, result);
    outPoly.pt.push_back(odPoint(result[0], result[1], result[2]));
  }
  outPoly.srfArea=srfArea;
  
  outPoly.projectedAreaXY();
  outPoly.centroid();
  outPoly.normal();

  return outPoly;
}