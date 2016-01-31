// Sample Gauges main program
// QT4 - Experimental stage!!

#include <QtGui>
#include <QtXml>
#include <math.h>
 
#include <ode/ode.h>
#include "mainWindow.h"

mainWindow::mainWindow(QWidget *parent)
                     : QMainWindow(parent)
{
  int i;
  /*
  setGeometry(400,300,640,480);

  central = new QTabWidget();  
  graph = new qt4gnuplot();
  central->addTab(graph,"Graphs");
  vtkView = new odVTKWidget(); 
  central->addTab(vtkView,"3D View");
  setCentralWidget(central); 

  fileMenu        = menuBar()->addMenu("&File");
  fileNewProject  = fileMenu->addAction("&New Project");
  fileOpenProject = fileMenu->addAction("&Open Project");
  fileExit        = fileMenu->addAction("E&xit");
  
  simMenu         = menuBar()->addMenu("&Simulate");
  simStart        = simMenu->addAction("&Start");
  simStop         = simMenu->addAction("S&top");
 
  simTimer = new QTimer();
  simTimer->setInterval(50);
  connect(simTimer, SIGNAL(timeout()), this, SLOT(advanceSim()));
  
  connect(fileNewProject , SIGNAL(activated()), this, SLOT(newProject()));    
  connect(fileOpenProject, SIGNAL(activated()), this, SLOT(openProject()));
  connect(fileExit       , SIGNAL(activated()), this, SLOT(close()));
  
  connect(simStart       , SIGNAL(activated()), simTimer, SLOT(start()));
  connect(simStop        , SIGNAL(activated()), simTimer, SLOT(stop()));
  printf("Setup done\n");
  newProject();
  */
  
  openProject("Example/example.odx");
  planet.initialiseSolver();
//  for (i=0;i<100;i++){
//    planet.advanceSolver();
//  }
}

void mainWindow::openProject(){
  QString fileName;

  fileName = QFileDialog::getOpenFileName(this, tr("Open Project File"),
                                          ".", // QString(getenv("HOME"))+QString("/.openpilot/"),
                                          tr("Open Dynamics Project (*.odx)"));
  if (!fileName.isEmpty()) openProject(fileName);
}

void mainWindow::openProject(QString fileName){
  int erl, erc, i;
  QDomElement root, e;
  QDomNode node, node2;
  QDomElement element, element2;
  QString errMsg;
  QFile file;

  // open XML project file
  file.setFileName(fileName);
  if (!file.open( QIODevice::ReadOnly )){
    printf("openDynamics :: Could not open XML file - %s\n",fileName.toAscii().data());
    return;
  }

  QDomDocument doc( "proFile" );
  if (!doc.setContent( &file, false, &errMsg, &erl, &erc)){
    printf("openDynamics :: Failed to set content\n");
    printf("openDynamics :: At line %i, Column %i\n", erl, erc);
    printf("openDynamics :: QDomDocument returned - %s\n",errMsg.toAscii().data());
    file.close();
    return;
  }
  file.close();

  root=doc.documentElement();
  if (root.tagName().toLower() != "opendynamics"){
    printf("openDynamics :: This is not an openDynamics XML file\n");
    return;
  }

  node=root.firstChild();
  while (!node.isNull()){
    element=node.toElement();
    if (element.tagName().toLower()=="world") planet.readFromXML(node);
    node=node.nextSibling();
  }
}