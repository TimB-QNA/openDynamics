#include "odFoilSection.h"

void odFoilSection::clear(){
  point.clear();
}

void odFoilSection::readFromXML(QDomNode node){
  QDomElement element;
  odPoint tmpPoint;
  
  while (!node.isNull()){
    element=node.toElement();
    if (element.tagName().toLower()=="point"){
      tmpPoint.readFromXML(element);
      point.push_back(tmpPoint);
    }
    if (element.tagName().toLower()=="leadingedge") LE.readFromXML(element);
    if (element.tagName().toLower()=="trailingedge") TE.readFromXML(element);
    node=node.nextSibling();
  }
  
  if ((int)point.size()<2){
    point.clear();
    point.push_back(TE); point.push_back(LE);
  } 
}
