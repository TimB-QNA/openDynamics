// Sample Gauges main program
// QT4 - Experimental stage!!

#include "odWorld.h"
#include <time.h>
void openProject(QString fileName);

odWorld planet;
  
int main(int argc, char * argv[]){
  int i;
  QString fileName;
  clock_t start, end, diff;
  double secs;
  
  fileName="example.odx";  
  for (i=0;i<argc;i++){
    if (strcmp(argv[i],"-i")==0) fileName=argv[i+1];
  }

  openProject(fileName);
  planet.initialiseSolver();
  planet.run();
}

void openProject(QString fileName){
  int erl, erc, i;
  QDomElement root, e;
  QDomNode node, node2;
  QDomElement element, element2;
  QString errMsg;
  QFile file;

  // open XML project file
  file.setFileName(fileName);
  if (!file.open( QIODevice::ReadOnly )){
    printf("openDynamics :: Could not open XML file - %s\n",fileName.toLatin1().data());
    return;
  }

  QDomDocument doc( "proFile" );
  if (!doc.setContent( &file, false, &errMsg, &erl, &erc)){
    printf("openDynamics :: Failed to set content\n");
    printf("openDynamics :: At line %i, Column %i\n", erl, erc);
    printf("openDynamics :: QDomDocument returned - %s\n",errMsg.toLatin1().data());
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
