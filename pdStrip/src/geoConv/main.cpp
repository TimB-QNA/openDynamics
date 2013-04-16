#include <QtCore>
#include <QString>
#include <stdio.h>
#include "../lib/hullform.h"
#include <vector>
using namespace std;

void header();
void help();

int main(int argc, char *argv[]){
  int i;
  QString input="input.geo", output="output.txt", iType="section", oType="point";
  bool GNUPlot;
  hullform hull, canoe;

  hull.waterline=0;

  header();
  if (argc==1) help();

  for (i=0;i<argc;i++){
    if (strcmp(argv[i],"-i")==0)     input=argv[i+1];
    if (strcmp(argv[i],"-o")==0)     output=argv[i+1];
    if (strcmp(argv[i],"-itype")==0) iType=QString(argv[i+1]).toLower();
    if (strcmp(argv[i],"-otype")==0) oType=QString(argv[i+1]).toLower();
    if (strcmp(argv[i],"-g")==0)     GNUPlot=true;
    if (strcmp(argv[i],"-wl")==0)    sscanf(argv[i+1],"%lf",&hull.waterline);
    if (strcmp(argv[i],"-h")==0)     help();
  }

  if (iType.toLower()=="section"){
    printf("Reading existing hullform from pdstrip sections file\n");
    if (!hull.readSectionDef(input)) return 1;
  }

  if (iType.toLower()=="ghs"){
    printf("Reading existing hullform from GHS file\n");
    if (!hull.readGhsDef(input)) return 1;
  }

  hull.trimAboveWaterline();
  hull.sort();

  if (oType.toLower()=="point"){
    printf("Writing hullform as point definition ");
    if (GNUPlot){
      printf("with GNUPlot compatibility");
    }
    printf("\n");
    if (!hull.writePointDef(output, GNUPlot)) return 2;
  }

  if (oType.toLower()=="section"){
    printf("Writing hullform as pdStrip section definition\n");
    if (!hull.writeSectionDef(output)) return 2;
  }

  return 0;
}


void header(){
  printf("openDynamics\n");
  printf("\n");
  printf("          Name: convert_QD\n");
  printf("          Type: Utility\n");
  printf("      Platform: Any g++/QT4\n");
  printf("     Author(s): Tim Brocklehurst (timb@engineering.selfip.org)\n");
  printf("  Last Compile: %s at %s\n",__DATE__,__TIME__);
  printf("    Purpose: Quick and Dirty conversion of offsets file into\n");
  printf("             a point-definition file for use in CAD.\n");
  printf("\n");
}

void help(){
  printf("  Switches:\n");
  printf("\t-i\tSpecify input file\n");
  printf("\t-o\tSpecify output file\n");
  printf("\t-g\tEnable GNUPlot compatibility for point definition output\n");
  printf("\t-wl\tSpecify the waterline location (default is 0m)\n");
  printf("\t-itype\tSpecify input file type - ghs, section*\n");
  printf("\t-otype\tSpecify output file type - point*, section\n");
  printf("\n");
  printf("\t* Default\n");
  printf("\n");
  printf("  Return Codes:\n");
  printf("\t0\tNo error\n");
  printf("\t1\tFailed to read input\n");
  printf("\t2\tFailed to write output\n");
}

