#include "section.h"
#include <stdio.h>
#include <QString>
#include <QStringList>

void section::clear(){
  pt.clear();
}

/*!
 * Gaps are not handled.
 */
bool section::import_section(FILE *infile){
  int i, nPts;
  double xPos;
  point tmpPoint;
  char dat[4096];
  QString line;
  QStringList part, Ypart, Zpart;

  fgets(dat,4096,infile); line=dat;
  part=line.split(" ",QString::SkipEmptyParts);
  xPos=part[0].toDouble();
  nPts=part[1].toInt();

  for (i=0;Ypart.size()<nPts;i++){
    fgets(dat,4096,infile); line=dat;
    Ypart.append( line.split(" ",QString::SkipEmptyParts) );
  }

  for (i=0;Zpart.size()<nPts;i++){
    fgets(dat,4096,infile); line=dat;
    Zpart.append( line.split(" ",QString::SkipEmptyParts) );
  }

  for (i=0;i<nPts;i++){
    tmpPoint.x=xPos;
    tmpPoint.y=Ypart[i].toDouble();
    tmpPoint.z=Zpart[i].toDouble();
    pt.push_back(tmpPoint);
  }
  return true;
}

/*!
 * Import section from GHS File.
 */
bool section::import_GHS_section(FILE *infile){
  int i, nPts;
  double xPos;
  point tmpPoint;
  char dat[4096];
  QString line;
  QStringList part;

  fgets(dat,4096,infile);
  sscanf(dat,"%lf, %i", &xPos, &nPts);

  tmpPoint.x=-xPos;
  for (i=0;i<nPts;i++){
    fgets(dat,4096,infile);
    sscanf(dat,"%lf, %lf", &tmpPoint.y, &tmpPoint.z);
    pt.push_back(tmpPoint);
//    if (i<3) printf("X=%lf\tY=%lf\tZ=%lf\n",tmpPoint.x,tmpPoint.y,tmpPoint.z);
  }
//  printf("\n");
  return true;
}

void section::export_offsets(FILE *outfile){
  int i;
  for (i=0;i<(int)pt.size();i++) fprintf(outfile,"%lf\t%lf\t%lf\n",pt[i].x, pt[i].y, pt[i].z);
}

void section::export_section(FILE *outfile){
  int i, j;
  fprintf(outfile,"%lf\t%i\t0\n",pt[0].x, (int)pt.size());
  j=0;
  for (i=0;i<(int)pt.size();i++){
    fprintf(outfile,"%lf\t",pt[i].y);
    j++;
    if (j==8){
      fprintf(outfile,"\n");
      j=0;
    }
  }
  if (j!=0) fprintf(outfile,"\n");

  j=0;
  for (i=0;i<(int)pt.size();i++){
    fprintf(outfile,"%lf\t",pt[i].z);
    j++;
    if (j==8){
      fprintf(outfile,"\n");
      j=0;
    }
  }
  if (j!=0) fprintf(outfile,"\n");
}

void section::removeDuplicatePoints(){
  int i, j;
  for (i=0;i<(int)pt.size();i++){
    for (j=i+1;j<(int)pt.size();j++){
      if (pt[i]==pt[j]){
	pt.erase(pt.begin()+j);
	j--;
      }
    }
  }
}

/*!
 * Scale section by a given factor in X,Y & Z.
 */
void section::scale(double factor){
  int i;
  for (i=0;i<(int)pt.size();i++) pt[i]=pt[i]*factor;
}

/*!
 * Translate section by the given X,Y & Z.
 */
void section::translate(double x, double y, double z){
  int i;
  for (i=0;i<(int)pt.size();i++){
    pt[i].x+=x;
    pt[i].y+=y;
    pt[i].z+=z;
  }
}