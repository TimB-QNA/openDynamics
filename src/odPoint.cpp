/*
    Description: Provide 3D odPoints for openDynamics
         Author: Tim Brocklehurst
        License: GPLv3
*/

#include "odPoint.h"
#include "odMatrix.h"
#include <stdio.h>

odPoint::odPoint(){
  x=0; y=0; z=0; hasChanged=true;
}

odPoint::odPoint(double xd, double yd, double zd){
  x=xd;
  y=yd;
  z=zd;
}

void odPoint::readFromXML(QDomElement element){
  if (element.hasAttribute("x")) x=element.attribute("x").toDouble();
  if (element.hasAttribute("y")) y=element.attribute("y").toDouble();
  if (element.hasAttribute("z")) z=element.attribute("z").toDouble();    
}

odPoint odPoint::operator*(double b){
  odPoint tmp;
  tmp.x=x*b;
  tmp.y=y*b;
  tmp.z=z*b;
  return tmp;
}

odPoint odPoint::operator*(odPoint b){
  odPoint tmp;
  tmp.x=x*b.x;
  tmp.y=y*b.y;
  tmp.z=z*b.z;
  return tmp;
}

odPoint odPoint::operator/(double b){
  odPoint tmp;
  tmp.x=x/b;
  tmp.y=y/b;
  tmp.z=z/b;
  return tmp;
}

odPoint odPoint::operator+(odPoint b){
  odPoint tmp;
  tmp.x=x+b.x;
  tmp.y=y+b.y;
  tmp.z=z+b.z;
  return tmp;
}

odPoint odPoint::operator-(){
  odPoint tmp;
  tmp.x=-x;
  tmp.y=-y;
  tmp.z=-z;
  return tmp;
}

odPoint odPoint::operator-(odPoint b){
  odPoint tmp;
  tmp.x=x-b.x;
  tmp.y=y-b.y;
  tmp.z=z-b.z;
  return tmp;
}

void odPoint::operator+=(odPoint b){
   x+=b.x;   y+=b.y;   z+=b.z;
}

void odPoint::operator/=(double b){
   x/=b;  y/=b;  z/=b;
}

bool odPoint::operator== (odPoint b){
  if (fabs(x-b.x)<TOLERANCE &&
      fabs(y-b.y)<TOLERANCE &&
      fabs(z-b.z)<TOLERANCE) return true;
  return false;
}

void odPoint::operator*=(double b){
   x*=b;  y*=b;  z*=b;
}

double odPoint::dotProduct(odPoint b){
  double angle=0.;
  angle=acos( dotProduct_natural(b) / (mag()*b.mag()) );
  return angle;
}

double odPoint::dotProduct_natural(odPoint b){
  double dotprod;
  dotprod=x*b.x + y*b.y + z*b.z;
  return dotprod;
}

odPoint odPoint::crossProduct(odPoint b){
  odPoint c; // the output vector
  c.x = y*b.z - z*b.y;
  c.y = z*b.x - x*b.z;
  c.z = x*b.y - y*b.x;
  return c;
}

double odPoint::mag(){
  return sqrt(x*x+y*y+z*z);
}

double odPoint::mag(odPoint b){
  return b.mag();
}

void odPoint::display(){
  printf("%lf\t%lf\t%lf == mag %lf\n",x,y,z,mag());
}

odPoint odPoint::rotate(odPoint angles){
  double pi=4.*atan(1.);
  double alpha=angles.x;
  double beta=-angles.y;
  double gamma=angles.z;
  odPoint result;
  
  odMatrix T(3,3);
  odMatrix in(3,1);
  odMatrix out(3,1);
  // Matrix from Wikipedia is incorrect
  // This matrix from http://www.mines.edu/~gmurray/ArbitraryAxisRotation/ArbitraryAxisRotation.html

  T.setvalue( 0,0, cos(beta)*cos(gamma) );
  T.setvalue( 0,1, cos(gamma)*sin(alpha)*sin(beta)-cos(alpha)*sin(gamma) );
  T.setvalue( 0,2, cos(alpha)*cos(gamma)*sin(beta)+sin(alpha)*sin(gamma) );

  T.setvalue( 1,0, cos(beta)*sin(gamma) );
  T.setvalue( 1,1, cos(alpha)*cos(gamma)+sin(alpha)*sin(beta)*sin(gamma) );
  T.setvalue( 1,2, -cos(gamma)*sin(alpha)+cos(alpha)*sin(beta)*sin(gamma) );

  T.setvalue( 2,0, -sin(beta) );
  T.setvalue( 2,1, cos(beta)*sin(alpha) );
  T.setvalue( 2,2, cos(alpha)*cos(beta) );

  in.setvalue(0,0,x);
  in.setvalue(1,0,y);
  in.setvalue(2,0,z);
  out.mult(T,in);
  result.x=out.getvalue(0,0);
  result.y=out.getvalue(1,0);
  result.z=out.getvalue(2,0);
  return result;
}

odPoint odPoint::unitVector(){
  if (mag()<1e-16) return *this;
  return *this/mag();
}