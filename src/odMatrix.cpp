#include "odMatrix.h"
#include "odPoint.h"
#include <vector>
using namespace std;
#include <math.h>
#include <stdio.h>

odMatrix::odMatrix(int i, int j){
  resize(i, j);
}

odMatrix::odMatrix(odPoint p){
  resize(3, 1);
  a[0]=p.x; a[1]=p.y; a[2]=p.z;
}

odMatrix odMatrix::operator+(odMatrix b){
  int i, j;
  odMatrix tmp(imax,jmax);
  
  for (i=0;i<imax;i++){
    for (j=0;j<jmax;j++){
      tmp.setvalue(i,j,getvalue(i, j));
    }
  }
  tmp.add(b);
  return tmp;
}

odMatrix odMatrix::operator*(double b){
  int i, j;
  odMatrix tmp(imax,jmax);
  
  for (i=0;i<imax;i++){
    for (j=0;j<jmax;j++){
      tmp.setvalue(i,j,getvalue(i, j));
    }
  }
  tmp.multscalar(b);
  return tmp;
}

odMatrix odMatrix::operator*(odMatrix b){
  int i, j;
  odMatrix tmp(imax,jmax);
  
  for (i=0;i<imax;i++){
    for (j=0;j<jmax;j++){
      tmp.setvalue(i,j,getvalue(i, j));
    }
  }
  tmp.mult(tmp,b);
  return tmp;
}

odPoint odMatrix::toOdPoint(){
  odPoint p;
  p.x=a[0]; p.y=a[1]; p.z=a[2];
  return p;
}

int odMatrix::ij2n(int i, int j) { return j*imax+i; }

double odMatrix::getvalue(int i, int j) { return a[ij2n(i,j)]; }

void odMatrix::setvalue(int i, int j, double d) { a[ij2n(i,j)]=d; }

void odMatrix::zero(){
  int i, j;
  for (i=0;i<imax;i++){
    for (j=0;j<jmax;j++){
      setvalue(i,j,0.);
    }
  }
}


void odMatrix::identity(){
  int i;
  zero();
  if (imax==jmax){
    for (i=0;i<imax;i++) setvalue(i,i,1.);
  }else{
    printf("Identity matrix must be square\n");
  }
}


void odMatrix::unity(){
int i, j;
  for (i=0;i<imax;i++){
    for (j=0;j<jmax;j++){
      setvalue(i,j,1.);
    }
  }
}


void odMatrix::resize(int i,int j){
  // This routine wipes existing data
  if (i<0) i=1;
  if (j<0) j=1;
  a.resize(i*j);
  imax=i; jmax=j;
}


void odMatrix::display(){
  int i, j;
  for (i=0;i<imax;i++){
    for (j=0;j<jmax;j++){
      printf("%8.5f  ",getvalue(i,j));
    }
    printf("\n");
  }
}


void odMatrix::mult(odMatrix a, odMatrix b){
  // Multiply Matrices a & b in a matrix sense
  int kmax, i, j, k;
  double s;
  
  if (a.jmax==b.imax){
    resize(a.imax,b.jmax);
    kmax=b.imax;
    for (i=0;i<imax;i++){
      for (j=0;j<jmax;j++){
        s=0.;
        for (k=0;k<kmax;k++) s+=a.getvalue(i,k)*b.getvalue(k,j);
        setvalue(i,j,s);
      }
    }
  }else{
    printf("Matrices cannot be multiplied\n");
  }
}


void odMatrix::multscalar(double s){
  int i, j;
  for (i=0;i<imax;i++){
    for (j=0;j<jmax;j++) setvalue(i,j,s*getvalue(i,j));
  }
}

void odMatrix::add(odMatrix b){
  int i, j;
  if (b.imax==imax && b.jmax==jmax){
    for (i=0;i<imax;i++){
      for (j=0;j<jmax;j++) setvalue(i,j,getvalue(i,j)+b.getvalue(i,j));
    }
  }
}

void odMatrix::subtract(odMatrix b){
  int i, j;
  if (b.imax==imax && b.jmax==jmax){
    for (i=0;i<imax;i++){
      for (j=0;j<jmax;j++) setvalue(i,j,getvalue(i,j)-b.getvalue(i,j));
    }
  }
}


void odMatrix::transpose(){
  int i, j;
  odMatrix t=odMatrix(jmax,imax);
    for (i=0;i<imax;i++){
      for (j=0;j<jmax;j++) t.setvalue(j,i,getvalue(i,j));
    }
    resize(jmax,imax);
    for (i=0;i<imax;i++){
      for (j=0;j<jmax;j++) setvalue(i,j,t.getvalue(i,j));
    }
}

bool odMatrix::symmetry(){
  int i, j;
  bool sym=true;
  if (imax==jmax){
    for (i=0;i<imax;i++){
      for (j=0;j<jmax;j++) if (getvalue(i,j)!=getvalue(j,i)) sym=false;
    }
  }else{
    sym=false;
  }
  return sym;
}

odMatrix odMatrix::Gaussian(odMatrix b){
// Use Gaussian Elimination method to solve simultaneous eqns.
  int i, j, k, nmat=imax;
  double m_ik;
  odMatrix temp=odMatrix(imax,jmax);
  temp=save(); // Preserve current values in matrix "temp"
  odMatrix x=odMatrix(b.imax,b.jmax); // Solution matrix
  if (b.jmax>b.imax) b.transpose();
  /*
  the Gauss elimination 
  */
  for (k=1; k<=nmat-1; ++k)
  {
    for (i=k+1; i<=nmat; ++i)
    {
      if (getvalue(k-1,k-1) != 0.0)
      {
        m_ik = getvalue(i-1,k-1) / getvalue(k-1,k-1);
        for (j=k; j<=nmat; ++j)
          setvalue(i-1,j-1, getvalue(i-1,j-1) - m_ik * getvalue(k-1,j-1));
        b.setvalue(i-1,0, b.getvalue(i-1,0) - m_ik * b.getvalue(k-1,0));
      }
      else
      {
        printf("Step %2d\n", k);
        printf("The (%2d,%2d)-element of the matrix is zero.\n", k, k);
      }
    }
  }
  // obtain the solution x by a backward substitution
  for (i=nmat; i>=1; --i)
  {
    x.setvalue(i-1,0, b.getvalue(i-1,0));
    for (j=nmat; j>=i+1; --j)
      x.setvalue(i-1,0,x.getvalue(i-1,0) - getvalue(i-1,j-1) * x.getvalue(j-1,0));
    x.setvalue(i-1,0,x.getvalue(i-1,0) / getvalue(i-1,i-1));
  }
  load(temp); // restore values from "temp"
  return x;
}

odMatrix odMatrix::save(){
  int i,j;
  odMatrix temp=odMatrix(imax,jmax);
  for (i=0;i<imax;i++){
    for (j=0;j<jmax;j++) temp.setvalue(i,j,getvalue(i,j));
  }
  return temp;
}

void odMatrix::load(odMatrix z){
  int i,j;
  resize(z.imax,z.jmax);
  for (i=0;i<imax;i++){
    for (j=0;j<jmax;j++) setvalue(i,j,z.getvalue(i,j));
  }
}
