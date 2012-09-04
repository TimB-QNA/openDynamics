#ifndef SECTION_H
  #define SECTION_H

#include <stdio.h>

#include "point.h"
#include <vector>
using namespace std;

class section{
  public:
    void clear();
    bool import_section(FILE *infile);
    bool import_GHS_section(FILE *infile);
    void export_offsets(FILE *outfile);
    void export_section(FILE *outfile);
    void removeDuplicatePoints();
    void scale(double factor);
    void translate(double x, double y, double z);
    vector<point> pt;
};

#endif
