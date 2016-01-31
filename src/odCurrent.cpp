#include "odCurrent.h"

odCurrent::odCurrent(){
  top=1e9;
  bottom=-1e9;
  velocity=odPoint(0,0,0);
}

void odCurrent::readFromXML(QDomNode root){
  QDomNode node;
  QDomElement element;
  node=root.firstChild();
  while (!node.isNull()){
    element=node.toElement();
    if (element.tagName().toLower()=="top")      top=element.text().toDouble();
    if (element.tagName().toLower()=="bottom")   bottom=element.text().toDouble();
    if (element.tagName().toLower()=="velocity") velocity.readFromXML(element);
    node=node.nextSibling();
  }
}

odPoint odCurrent::velocityInfluence(odPoint pos){
  if (pos.z>=bottom && pos.z<=top) return velocity;
  return odPoint();
}