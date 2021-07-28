#include "hullform.h"
#include <math.h>
#include <stdio.h>
#include <QString>

#ifndef TOLERANCE
 #define TOLERANCE 1e-10
#endif

/*!
 * Clears all data from hullform
 */
void hullform::clear(){
  waterline=0;
  sect.clear();
}

/*!
 * Cleans the contained geometry to remove empty sections.
 * In time this will also perform other geometry-cleaning tasks.
 */
void hullform::cleanGeometry(){
  int i, j;
  // Remove any points at y=0&z=0
  for (i=0;i<(int)sect.size();i++){
    for(j=0;j<(int)sect[i].pt.size();j++){
      if (fabs(sect[i].pt[j].y)<TOLERANCE &&
          fabs(sect[i].pt[j].z)<TOLERANCE) sect[i].pt.erase(sect[i].pt.begin()+j);
    }
  }
  removeDuplicatePoints();
  removeEmptySections();
  sort();
}

/*!
 * Calculate maximum and minimum extents of the hullform.
 */
void hullform::extents(){
  int i,j;

  min.x=10e9; min.y=10e9; min.z=10e9;
  max.x=-10e9; max.y=-10e9; max.z=-10e9;

  for (i=0;i<(int)sect.size();i++){
    for(j=0;j<(int)sect[i].pt.size();j++){
      if (sect[i].pt[j].x<min.x) min.x=sect[i].pt[j].x;
      if (sect[i].pt[j].y<min.y) min.y=sect[i].pt[j].y;
      if (sect[i].pt[j].z<min.z) min.z=sect[i].pt[j].z;

      if (sect[i].pt[j].x>max.x) max.x=sect[i].pt[j].x;
      if (sect[i].pt[j].y>max.y) max.y=sect[i].pt[j].y;
      if (sect[i].pt[j].z>max.z) max.z=sect[i].pt[j].z;
    }
  }

}

/*!
 * Trims sections leaving the part of the section below the waterline. It does not consider
 * waterline intersection (yet). Once it has trimmed the geometry, it removes any empty sections.
 */
void hullform::trimAboveWaterline(){
  int i, j;
  double f;
  vector<point> tmpPT;
  point d;

  removeEmptySections();

  for (i=0;i<(int)sect.size();i++){
    for (j=0;j<(int)sect[i].pt.size();j++){
      if (j<(int)sect[i].pt.size()-1){
        if ((sect[i].pt[j].z>waterline && sect[i].pt[j+1].z<waterline) ||
            (sect[i].pt[j].z<waterline && sect[i].pt[j+1].z>waterline)){
          d=sect[i].pt[j+1]-sect[i].pt[j];
          f=(sect[i].pt[j].z-waterline)/d.z;
          sect[i].pt[j]=sect[i].pt[j]-d*f;
        }
      }
      if (sect[i].pt[j].z>=waterline+TOLERANCE){
        sect[i].pt.erase(sect[i].pt.begin()+j);
        j--;        // Drop the counter back one to account for the deleted index, otherwise you only capture 50% of the deletions.
      }
    }

  }
  cleanGeometry();
}

/*!
 * Removes any sections with no points.
 */
void hullform::removeEmptySections(){
  int i;
  for (i=0;i<(int)sect.size();i++){
//    printf("Section %i - Pts %i\n", i, sect[i].pt.size());
    if ((int)sect[i].pt.size()==0){
      sect.erase(sect.begin()+i);
      i--;       // Drop the counter back one to account for the deleted index, otherwise you only capture 50% of the deletions.
    }
  }
}

/*!
 * Removes any sections with only 1 or 2 points.
 */
void hullform::removeShortSections(){
  int i;
  for (i=0;i<(int)sect.size();i++){
    if ((int)sect[i].pt.size()<5){
      sect.erase(sect.begin()+i);
      i--;       // Drop the counter back one to account for the deleted index, otherwise you only capture 50% of the deletions.
    }
  }
}

/*!
 * Removes any duplicate points in sections.
 */
void hullform::removeDuplicatePoints(){
  int i;
  for (i=0;i<(int)sect.size();i++) sect[i].removeDuplicatePoints();
}

/*!
 * Scale hullform by factor.
 */
void hullform::scale(double factor){
  int i;
  for (i=0;i<(int)sect.size();i++) sect[i].scale(factor);
}

/*!
 * Move hullform by X, Y, Z.
 */
void hullform::translate(double x, double y, double z){
  int i;
  for (i=0;i<(int)sect.size();i++) sect[i].translate(x,y,z);
  for (i=0;i<(int)pan.size();i++)  pan[i].translate(x,y,z);
}

/*!
 * Sorts points on sections for pdstrip.
 */
void hullform::sort(){
  int i, j;
  bool swap=true;
  point tmp;
  section sortedSection;

  for (i=0;i<(int)sect.size();i++){
    swap=true;
    while (swap){
      swap=false;
      for (j=0;j<(int)sect[i].pt.size()-1;j++){
        if (sect[i].pt[j].y>sect[i].pt[j+1].y){
          tmp=sect[i].pt[j+1];
          sect[i].pt[j+1]=sect[i].pt[j];
          sect[i].pt[j]=tmp;
          swap=true;
        }
      }
      // handle last point
      j=(int)sect[i].pt.size()-1;
      if (sect[i].pt[j].y<sect[i].pt[j-1].y){
          tmp=sect[i].pt[j-1];
          sect[i].pt[j-1]=sect[i].pt[j];
          sect[i].pt[j]=tmp;
          swap=true;
      }
    }

  }
}

/*!
 * Write section definition as used by pdstrip to a file.
 * pdstrip expects only the immersed sections of the hull.
 */
bool hullform::writeSectionDef(QString filename){
  int i;
  FILE *outputFile;
//  printf("Exporting sections into %s\n",filename.toLatin1().data());
//  printf("Establishing extents\n");
  extents();
//  printf("Opening file\n");
  outputFile=fopen(filename.toLatin1().data(),"w");
  if (outputFile==NULL) return false;
//  printf("Removing short sections\n");
  removeShortSections();
//  printf("Translating so baseline is at z=0\n");
  translate(0,0,-min.z);

  fprintf(outputFile,"%i T %lf\n",(int)sect.size(),waterline-min.z);
  for (i=(int)sect.size()-1;i>=0;i--){
//    printf("Section %i\n",i);
    sect[i].export_section(outputFile);
  }
  fclose(outputFile);
  return true;
}

/*!
 * Reads a section definition as used by pdstrip. This routine does not
 * handle objects with gaps.
 */
bool hullform::readSectionDef(QString filename){
  int i;
  char line[4096];
  FILE *inputFile;
  section tempSection;

  inputFile=fopen(filename.toLatin1().data(),"r");
  if (inputFile==NULL) return false;
  // First line contains symmetry data. Ignore for now.
  fgets(line,4096,inputFile);

  // Following lines are sections, so handle them...
  for (i=0;!feof(inputFile);i++){
    tempSection.clear();
    if (!tempSection.import_section(inputFile)) return false;
    sect.push_back(tempSection);
  }

  fclose(inputFile);
  removeDuplicatePoints();
  extents();
  return true;
}

/*!
 * Reads the first object in a general hydrostatics format file.
 */
bool hullform::readGhsDef(QString filename){
  int i, nSections;
  char line[4096];
  FILE *inputFile;
  section tempSection;

  inputFile=fopen(filename.toLatin1().data(),"r");
  if (inputFile==NULL) return false;
  // First three lines contain various non-section data.
  for (i=0;i<4;i++){
    fgets(line,4096,inputFile);
//    printf(line);
  }

  // Object name
  fgets(line,4096,inputFile);

  // Number of Sections
  fgets(line,4096,inputFile);
  sscanf(line,"%i", &nSections);
  // Following lines are sections, so handle them...
  for (i=0;i<nSections;i++){
    tempSection.clear();
    if (!tempSection.import_GHS_section(inputFile)) return false;
    sect.push_back(tempSection);
  }

  fclose(inputFile);
  removeDuplicatePoints();
  scale(0.3048);
  extents();
  return true;
}

/*!
 * Save hullform as a point definition. This is a list of XYZ points which constitute the hull.
 * Compatibility for GNUPlot (avoiding flybacks by separating the sections by a blank line)
 * is handled by setting the second parameter (bool GNUPlot) true.
 */
bool hullform::writePointDef(QString filename,bool GNUPlot=false){
  int i;
  FILE *outputFile;

  outputFile=fopen(filename.toLatin1().data(),"w");
  if (outputFile==NULL) return false;

  for (i=0;i<(int)sect.size();i++){
    sect[i].export_offsets(outputFile);
    if (GNUPlot) fprintf(outputFile,"\n");
  }

  if (GNUPlot){
    for (i=0;i<(int)sect.size();i++) fprintf(outputFile,"%lf\t%lf\t%lf\n", sect[i].pt[0].x, sect[i].pt[0].y, sect[i].pt[0].z);
    fprintf(outputFile,"\n");
    for (i=0;i<(int)sect.size();i++) fprintf(outputFile,"%lf\t%lf\t%lf\n", sect[i].pt[sect[i].pt.size()-1].x, sect[i].pt[sect[i].pt.size()-1].y, sect[i].pt[sect[i].pt.size()-1].z);
  }
  fclose(outputFile);
  return true;
}


void hullform::generatePanels(){
//  printf("Generating Panels\n");
  int i, j, k;
  panel tmpPan;

  for (i=0;i<(int)sect.size()-1;i++){
//    printf("Starting Section %i\n",i);
    for (j=0;j<(int)sect[i].pt.size();j++){

      if ((int)sect[i+1].pt.size()>j+1){
        tmpPan.pt[0]=sect[i].pt[j];
        tmpPan.pt[1]=sect[i+1].pt[j];
        tmpPan.pt[2]=sect[i+1].pt[j+1];
        pan.push_back(tmpPan);
//        printf("Creating Standard Panel");

        if (j<(int)sect[i].pt.size()-1){
          tmpPan.pt[0]=sect[i].pt[j];
          tmpPan.pt[1]=sect[i+1].pt[j+1];
          tmpPan.pt[2]=sect[i].pt[j+1];
          pan.push_back(tmpPan);
//          printf("Creating Incrementing Panel");
        }
      }

    }

    if (sect[i+1].pt.size()>sect[i].pt.size()){
      k=sect[i].pt.size()-1;
      for(j=k;j<(int)sect[i+1].pt.size();j++){
        tmpPan.pt[0]=sect[i].pt[k];
        tmpPan.pt[1]=sect[i+1].pt[j];
        tmpPan.pt[2]=sect[i].pt[j+1];
   //     pan.push_back(tmpPan);
      }
    }
  }

}
