#include "odSimCondition.h"

void odSimCondition::initialise(){
  time=0.;
}

void odSimCondition::readFromXML(QDomNode node){
  double pi=4.*atan(1.);
  QDomElement element;
  while (!node.isNull()){
    element=node.toElement();  
    if (element.tagName().toLower()=="position"){
      if (element.hasAttribute("x"))     linear.x     =element.attribute("x").toDouble();
      if (element.hasAttribute("y"))     linear.y     =element.attribute("y").toDouble();
      if (element.hasAttribute("z"))     linear.z     =element.attribute("z").toDouble();
      if (element.hasAttribute("roll"))  rotational.x =element.attribute("roll").toDouble()*pi/180.;
      if (element.hasAttribute("pitch")) rotational.y =element.attribute("pitch").toDouble()*pi/180.;
      if (element.hasAttribute("yaw"))   rotational.z =element.attribute("yaw").toDouble()*pi/180.;
    }
    if (element.tagName().toLower()=="velocity"){
      if (element.hasAttribute("x"))     linearVel.x     =element.attribute("x").toDouble();
      if (element.hasAttribute("y"))     linearVel.y     =element.attribute("y").toDouble();
      if (element.hasAttribute("z"))     linearVel.z     =element.attribute("z").toDouble();
      if (element.hasAttribute("roll"))  rotationalVel.x =element.attribute("roll").toDouble()*pi/180.;
      if (element.hasAttribute("pitch")) rotationalVel.y =element.attribute("pitch").toDouble()*pi/180.;
      if (element.hasAttribute("yaw"))   rotationalVel.z =element.attribute("yaw").toDouble()*pi/180.;
    }
    if (element.tagName().toLower()=="acceleration"){
      if (element.hasAttribute("x"))     linearAcc.x     =element.attribute("x").toDouble();
      if (element.hasAttribute("y"))     linearAcc.y     =element.attribute("y").toDouble();
      if (element.hasAttribute("z"))     linearAcc.z     =element.attribute("z").toDouble();
      if (element.hasAttribute("roll"))  rotationalAcc.x =element.attribute("roll").toDouble()*pi/180.;
      if (element.hasAttribute("pitch")) rotationalAcc.y =element.attribute("pitch").toDouble()*pi/180.;
      if (element.hasAttribute("yaw"))   rotationalAcc.z =element.attribute("yaw").toDouble()*pi/180.;
    }
    node=node.nextSibling();
  }
}