#include "panel.h"
#include <math.h>
#include <stdio.h>

panel::panel(){
//  printf("Initialising Panel\n");
//  point blankPt;
//  pt.push_back(blankPt);
//  pt.push_back(blankPt);
//  pt.push_back(blankPt);
}

/*!
 * This routine calculates the surface area of the panel.
 * We use Heron's formula to calculate the panel's area based on it's side lengths.
 */
double panel::area(){
  int a, b, c, i;
  double f, dx,dy,dz, st, st0,st1,st2, sp, sp0,sp1,sp2;
  double area;
  vector<int> idx;
  panel tri[2];

  // We use Heron's formula here for both projected and total area
  dx=pt[0].x-pt[1].x; dy=pt[0].y-pt[1].y; dz=pt[0].z-pt[1].z;
  st0=sqrt(dx*dx+dy*dy+dz*dz); sp0=sqrt(dx*dx+dy*dy);

  dx=pt[1].x-pt[2].x; dy=pt[1].y-pt[2].y; dz=pt[1].z-pt[2].z;
  st1=sqrt(dx*dx+dy*dy+dz*dz); sp1=sqrt(dx*dx+dy*dy);

  dx=pt[2].x-pt[0].x; dy=pt[2].y-pt[0].y; dz=pt[2].z-pt[0].z;
  st2=sqrt(dx*dx+dy*dy+dz*dz); sp2=sqrt(dx*dx+dy*dy);

  st=.5*(st0+st1+st2); sp=.5*(sp0+sp1+sp2);
  area=sqrt(st*(st-st0)*(st-st1)*(st-st2));
  return area;
}

/*!
 * This routine calculates the area of the panel projected in the XY plane.
 * We use Heron's formula to calculate the panel's area based on it's side lengths.
 */
double panel::areaXY(){
  double sp, sp0,sp1,sp2;
  double area;
  vector<int> idx;
  panel tri[2];
  point d;

  d=pt[0]-pt[1]; sp0=sqrt(pow(d.x,2)+pow(d.y,2));
  d=pt[1]-pt[2]; sp1=sqrt(pow(d.x,2)+pow(d.y,2));
  d=pt[2]-pt[0]; sp2=sqrt(pow(d.x,2)+pow(d.y,2));

  sp=.5*(sp0+sp1+sp2);

  return sqrt(sp*(sp-sp0)*(sp-sp1)*(sp-sp2));
}

/*!
 * This routine calculates the volume of a triangular prism from the surface panel
 * to the waterline (which is the waterline at the XY centroid of the panel).
 * We use Heron's formula to calculate the panel's area based on it's side lengths.
 * This gives a very good general solution.
 *
 * Where a panel intersects the waterline, the immersed area of the panel is less than the total area. This
 * causes inaccuracy in the volume calculations, which can lead to oscillations which are artificially divergent.
 * To prevent this, any triangle which has a point above the waterline is split, then the volume is calculated on
 * the immersed section.
 */
double panel::calculatePrismVolume(double waterline){
  int a, b, c, i;
  double f, dx,dy,dz, st, st0,st1,st2, sp, sp0,sp1,sp2;
  vector<int> idx;
  vector<panel> tri;
  point cent;
  cent=centroid();
  if (cent.z>waterline) return 0.;
  return areaXY()*(waterline-cent.z);

//  if (pt[0].z<waterline && pt[1].z<waterline && pt[2].z<waterline){

//  }else{
//    tri=splitAtZ(waterline);
//    }

//  }
//  return volume;
}

/*!
 * Split a triangular panel at a prescribed Z.
 */
vector<panel> panel::splitAtZ(double zSplit){
  int a, b, c, i;
  double f, dx,dy,dz;
  vector<int> idx;
  vector<panel> result;
  panel tmpPanel;

  // Build immersed panels and calculate areas.
  for (i=0;i<3;i++){
    if (pt[i].z<zSplit) idx.push_back(i);
  }
  if (idx.size()==1){
    b=idx[0];
    if (b==0) { a=2; c=1; }
    if (b==1) { a=0; c=2; }
    if (b==2) { a=0; c=1; }
    dx=pt[b].x-pt[a].x; dy=pt[b].y-pt[a].y; dz=pt[b].z-pt[a].z;
    f=pt[b].z/dz;

    tmpPanel.pt[0]=pt[a];
    tmpPanel.pt[1]=pt[a]+point(dx*f,dy*f,dz*f);
    tmpPanel.pt[2]=pt[c];
    result.push_back(tmpPanel);

    dx=pt[b].x-pt[c].x; dy=pt[b].y-pt[c].y; dz=pt[b].z-pt[c].z;
    f=pt[b].z/dz;

    tmpPanel.pt[0]=pt[a];
    tmpPanel.pt[1]=pt[c]+point(dx*f,dy*f,dz*f);
    tmpPanel.pt[2]=pt[c];
    result.push_back(tmpPanel);
  }
  return result;
}

point panel::centroid(){
  point cent;
  cent.x=(pt[0].x +pt[1].x +pt[2].x)/3.;
  cent.y=(pt[0].y +pt[1].y +pt[2].y)/3.;
  cent.z=(pt[0].z +pt[1].z +pt[2].z)/3.;
  return cent;
}

point panel::normal(){
  point dp1, dp2, normal;

  dp1=pt[1]-pt[0];
  dp2=pt[1]-pt[2];

  normal=dp1.crossproduct(dp2);
  //  printf("Normal: %lf\t%lf\t%lf\n",normal.x, normal.y, normal.z);
  return normal;
}

void panel::translate(double x, double y, double z){
  int i;
  point m(x,y,z);

  for (i=0;i<3;i++){
   // printf("translating panel from %lf by %lf to ",pt[i].z,z);
    pt[i].x+=x;
    pt[i].y+=y;
    pt[i].z+=z;
 //   printf("%lf\n",pt[i].z);
  }
}

void panel::pitch(double ang){
  int i;
  double px, pz;

  for (i=0;i<3;i++){
    px=pt[i].x; pz=pt[i].z;
    pt[i].x=px*cos(ang)+pz*sin(ang);
    pt[i].z=-px*sin(ang)+pz*cos(ang);
  }
}

bool panel::isDegenerate(){
  if (pt[0]==pt[1] || pt[0]==pt[2] || pt[1]==pt[2]) return true;
  return false;
}