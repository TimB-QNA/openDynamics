#ifndef ODOBJECT_H
  #define ODOBJECT_H
  
#include <QtGui>
#include <QtXml>
#ifdef GRAPHICS
  #include <vtkPoints.h>
  #include <vtkTriangle.h>
  #include <vtkCellArray.h>
  #include <vtkPolyData.h>
  #include <vtkPolyDataMapper.h>
  #include <vtkActor.h>
#endif

#include "odConstants.h"
#include "odSimCondition.h"
#include "odPoint.h"
#include "odMesh.h"
#include <ode/ode.h>
#include <vector>
using namespace std;

class odObject
{
  public:
    odObject();
    ~odObject();
//    void initObject(vector<odObject*> solveComponent, odConstants *solveConstant, dBodyID *localBody);
//    void initObject(odObject* currentObject);
    
    virtual void plot();
    void readCommonDataFromXML(QDomNode root);
    virtual void readFromXML(QDomNode root);
    
    virtual double elevationInfluence(double x, double y);
    double elevation(double x, double y);
    
    virtual odPoint velocityInfluence(odPoint pos);
    odPoint localVelocity(odPoint pos, int fUid);
    
    virtual void advanceSolver();
    virtual void run();
    virtual void preSolveSetup();
    virtual void postAdvance();
    void openDataStream(QString objectType);
    double density(odPoint pos);
    void volumetricSolver();
    virtual void exportVTK(FILE *stream);
      
#ifdef GRAPHICS
    vtkCellArray *objects;
    vtkPolyData *polygons;
    vtkPolyDataMapper *objectMapper;
    vtkActor *actor;
#endif
    
    QString name;
    QString vtksuffix;
    FILE *dataStream;
    
    int nObjects;
    int uid;
    double *timeStep;
    double *simTime;
    odObject **allObjects;      // Array of pointers
    odConstants *constants;
    dBodyID *odeBody;
    odMesh mesh;
    QString meshRef;
};

#endif


