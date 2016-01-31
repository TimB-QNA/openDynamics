#include "odTerrain.h"
#include <ode/ode.h>
#include <math.h>
#include <time.h>
#include <stdio.h>

odTerrain::odTerrain(){
}

void odTerrain::createTerrain(dSpaceID space){
  dCreatePlane (space, normal.x, normal.y, normal.z, offset);
}


void odTerrain::readFromXML(QDomNode root){
  QDomNode node;
  QDomElement element;
  
  node=root.firstChild();
  while (!node.isNull()){
    element=node.toElement();
      
    if (element.tagName().toLower()=="normal")  normal.readFromXML(element);
    if (element.tagName().toLower()=="offset")  offset=element.text().toFloat();
    node=node.nextSibling();
  }
}