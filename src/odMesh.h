#include "odPoint.h"
#include "odPolygon.h"
#include <vector>
using namespace std;

#ifndef ODMESH_H
  #define ODMESH_H
    
class odMesh
{
  public:
    odMesh();
#ifdef GRAPHICS
    void plot();
#endif
    void run();
    void readFromXML(QDomNode root);
    void exportVTK(FILE *stream);
    void loadGeometry(); 
    void translate(odPoint translation);
    QString reference();
    double surfaceArea();
    odPoint minBound();
    odPoint maxBound();
    odPoint range();
    vector< odPolygon > polygon;
    vector< odPolygon > active;
    
  private:
    int nPoints;
    double totalSrf;
    QString refString;
    QString geometryFile;

#ifdef GRAPHICS
    vtkPoints *points;
    vector<vtkTriangle*> triangle;
#endif
    
    bool loadAsciiSTL(QString fileName);
    bool loadBinarySTL(QString fileName);
    bool loadRawTriangles(QString fileName);
};

#endif

