#include <math.h>
#include "point.h"

#ifndef TOLERANCE
 #define TOLERANCE 1e-6
#endif

point::point(){
  point(0,0,0, 0,0,0);
}

point::point(double lx, double ly, double lz){
  point(lx,ly,lz,0,0,0);
}

point::point(double lx, double ly, double lz, double vx, double vy, double vz){
  PI=4.*atan(1.); // about 3.141592654 to machine accuracy.
  x=lx; y=ly; z=lz;
  dx=vx; dy=vy; dz=vz;
}

point point::operator*(double b){
  point tmp;
  tmp.x=x*b;
  tmp.y=y*b;
  tmp.z=z*b;

  tmp.dx=dx*b;
  tmp.dy=dy*b;
  tmp.dz=dz*b;
  return tmp;
}

point point::operator/(double b){
  point tmp;
  tmp.x=x/b;
  tmp.y=y/b;
  tmp.z=z/b;

  tmp.dx=dx/b;
  tmp.dy=dy/b;
  tmp.dz=dz/b;
  return tmp;
}

point point::operator+(point b){
  point tmp;
  tmp.x=x+b.x;
  tmp.y=y+b.y;
  tmp.z=z+b.z;

  tmp.dx=dx+b.dx;
  tmp.dy=dy+b.dy;
  tmp.dz=dz+b.dz;
  return tmp;
}

point point::operator-(point b){
  point tmp;
  tmp.x=x-b.x;
  tmp.y=y-b.y;
  tmp.z=z-b.z;

  tmp.dx=dx-b.dx;
  tmp.dy=dy-b.dy;
  tmp.dz=dz-b.dz;
  return tmp;
}

void point::operator+=(point b){
   x+=b.x;   y+=b.y;   z+=b.z;
  dx+=b.dx; dy+=b.dy; dz+=b.dz;
}

void point::operator/=(double b){
   x/=b;  y/=b;  z/=b;
  dx/=b; dy/=b; dz/=b;
}

bool point::operator== (point b){
  if (fabs(x-b.x)<TOLERANCE &&
      fabs(y-b.y)<TOLERANCE &&
      fabs(z-b.z)<TOLERANCE) return true;
  return false;
}

void point::set(double lx, double ly, double lz){
  x=lx;
  y=ly;
  z=lz;
}

double point::dotproduct(point b){
  double angle=0.;
  angle=acos( dotproduct_natural(b) / (mag()*b.mag()) )*(180./PI);
  return angle;
}

double point::dotproduct_natural(point b){
  double dotprod;
  dotprod=dx*b.dx + dy*b.dy + dz*b.dz;
  return dotprod;
}

point point::crossproduct(point b){
  point c; // the output vector
  c.dx = dy*b.dz - dz*b.dy;
  c.dy = dz*b.dx - dx*b.dz;
  c.dz = dx*b.dy - dy*b.dx;
  return c;
}

double point::mag(){
  return sqrt(dx*dx+dy*dy+dz*dz);
}

double point::mag(point b){
  return b.mag();
}
