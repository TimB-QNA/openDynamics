#include <QtGui>
#ifdef GRAPHICS
  #include <odVTKWidget.h>
#endif
#include <ode/ode.h>
#include "odWorld.h"
#include <qt4gnuplot.h>
#include <vector>
using namespace std;

#ifndef sample_H
  #define sample_H

class mainWindow : public QMainWindow
{
   Q_OBJECT

  public:
    mainWindow(QWidget *parent=0);
    
    odWorld planet;
 
    void openProject();
    void openProject(QString fileName);
    /*
    void startSimulation();
    
  private:
    odVTKWidget *vtkView;
    qt4gnuplot *graph;
    QTabWidget *central;

    QMenu   *fileMenu;
    QAction *fileNewProject;
    QAction *fileOpenProject;
    QAction *fileExit;
    
    QMenu   *simMenu;
    QAction *simStart;
    QAction *simStop;
    
    odBody body;
    QTimer *simTimer;
  
    dWorldID world;
    dSpaceID Space;
    double simTime;
//    vector<double> x,y,z,roll,pitch,yaw;
    
  private slots:
    void advanceSim();
    void newProject();
    void openProject();
    
    void initialiseSimulation();
    */
};

#endif

