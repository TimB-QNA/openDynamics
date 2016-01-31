#include "odMass.h"
#include <math.h>

odMassElement::odMassElement(){
  clear();
}

void odMassElement::clear(){
  mass=0;
  x=0; y=0; z=0; hasChanged=true; calcMassFromMeshProps=false;
  meshRef="";
}

void odMassElement::readFromXML(QDomElement element){
  hasChanged=true;
  if (element.hasAttribute("x")) x=element.attribute("x").toDouble();
  if (element.hasAttribute("y")) y=element.attribute("y").toDouble();
  if (element.hasAttribute("z")) z=element.attribute("z").toDouble();
  if (element.hasAttribute("mass")) mass=element.attribute("mass").toDouble();
}


void odMass::readFromXML(QDomNode root){
  QDomElement element;
  QDomNode node;
  odMassElement tmpMassElement; tmpMassElement.hasChanged=true;
  
  node=root.firstChild();
  while (!node.isNull()){
    tmpMassElement.clear(); tmpMassElement.hasChanged=true;
    element=node.toElement();
    if (element.tagName().toLower()=="element"){
      tmpMassElement.readFromXML(element);
      massElement.push_back(tmpMassElement);
    }
    if (element.tagName().toLower()=="mesh"){
      if (element.text().isEmpty()){
	printf("No mesh defined. Ignoring\n");
	break;
      }
      if (!element.hasAttribute("mass") && !(element.hasAttribute("density") && element.hasAttribute("thickness"))){
	printf("You must specify either total mass for this mesh, or thickness and density. Ignoring element.\n");
	break;
      }
      if (!element.hasAttribute("mass")) tmpMassElement.calcMassFromMeshProps=true;
      tmpMassElement.meshRef=element.text();
      if (element.hasAttribute("density"))   tmpMassElement.density  =element.attribute("density").toDouble();
      if (element.hasAttribute("thickness")) tmpMassElement.thickness=element.attribute("thickness").toDouble();
      if (element.hasAttribute("mass"))      tmpMassElement.mass     =element.attribute("mass").toDouble();    
      massElement.push_back(tmpMassElement);
    }
    node=node.nextSibling();
  }
}

void odMass::calcProperties(vector<odMesh> *mesh){
  bool change=false;
  int i, j, meshIndex;
  double eMass;
  odPoint eCent;
  
  for (i=0;i<massElement.size();i++){
    if (massElement[i].hasChanged) change=true;
  }
  
  for (i=0;i<massElement.size();i++){
    // Find correct mesh
    if (!massElement[i].meshRef.isEmpty()){
      // Zero properties ready for calcs
      if (massElement[i].calcMassFromMeshProps){
        massElement[i].mass=0.;
        massElement[i].x=0.;
        massElement[i].y=0.;
        massElement[i].z=0.;
      }
      printf("Our Ref=%s\n",massElement[i].meshRef.toLatin1().data());
      meshIndex=-1;
      for (j=0;j<mesh->size();j++){
//	printf("Mesh Ref=%s\n",mesh->at(j).reference().toLatin1().data()); 
	if (mesh->at(j).reference()==massElement[i].meshRef){
	  meshIndex=j;
	  break;
	}
      }
      if (meshIndex==-1){
	printf("Could not find mesh %s - Ignoring\n", massElement[i].meshRef.toLatin1().data());
	massElement[i].meshRef="";
	continue;
      }
      
      for (j=0;j<mesh->at(meshIndex).polygon.size();j++){
	// Calculate mass from mesh if needed.
	if (massElement[i].calcMassFromMeshProps){
	  eMass=mesh->at(meshIndex).polygon[j].surfaceArea()*massElement[i].thickness*massElement[i].density;
	  massElement[i].mass+=eMass;
	}else{
          // Element mass...
	  eMass = massElement[i].mass * mesh->at(meshIndex).polygon[j].surfaceArea() / mesh->at(meshIndex).surfaceArea();
	}
	massElement[i].x+=mesh->at(meshIndex).polygon[j].centroid().x * eMass;
	massElement[i].y+=mesh->at(meshIndex).polygon[j].centroid().y * eMass;
	massElement[i].z+=mesh->at(meshIndex).polygon[j].centroid().z * eMass;
      }
      massElement[i].x/=massElement[i].mass;
      massElement[i].y/=massElement[i].mass;
      massElement[i].z/=massElement[i].mass;
    }
  }
  
  mass=0;
  centroid=odPoint();
  for (i=0;i<(int)massElement.size();i++){
    mass+=massElement[i].mass;
    centroid.x += massElement[i].mass*massElement[i].x;
    centroid.y += massElement[i].mass*massElement[i].y;
    centroid.z += massElement[i].mass*massElement[i].z;
    massElement[i].hasChanged=false;
  }
  centroid/=mass;
  
  // Now we know the centroid, we can calculate the inertia easily...
  Ixx=0.; Iyy=0.; Izz=0.; Ixy=0.; Ixz=0.; Iyz=0.;
  for (i=0;i<massElement.size();i++){
    
    // Find correct mesh
    if (massElement[i].meshRef.isEmpty()){
      eCent=odPoint(massElement[i].x, massElement[i].y, massElement[i].z)-centroid;
      Ixx+=massElement[i].mass*(pow(eCent.y,2.)+pow(eCent.z,2.));
      Iyy+=massElement[i].mass*(pow(eCent.x,2.)+pow(eCent.z,2.));
      Izz+=massElement[i].mass*(pow(eCent.x,2.)+pow(eCent.y,2.));
      
      Ixy+=massElement[i].mass*eCent.x*eCent.y;
      Ixz+=massElement[i].mass*eCent.x*eCent.z;
      Iyz+=massElement[i].mass*eCent.y*eCent.z;
    }else{
      meshIndex=-1;
      for (j=0;j<mesh->size();j++){
	if (mesh->at(j).reference()==massElement[i].meshRef){
	  meshIndex=j;
	  break;
	}
      }
      
      for (j=0;j<mesh->at(meshIndex).polygon.size();j++){
	// Calculate mass from mesh if needed.
	if (massElement[i].calcMassFromMeshProps){
	  eMass=mesh->at(meshIndex).polygon[j].surfaceArea()*massElement[i].thickness*massElement[i].density;
	}else{
          // Element mass...
	  eMass = massElement[i].mass * mesh->at(meshIndex).polygon[j].surfaceArea() / mesh->at(meshIndex).surfaceArea();
	}
	eCent=mesh->at(meshIndex).polygon[j].centroid()-centroid;
	Ixx+=eMass*(pow(eCent.y,2.)+pow(eCent.z,2.));
        Iyy+=eMass*(pow(eCent.x,2.)+pow(eCent.z,2.));
        Izz+=eMass*(pow(eCent.x,2.)+pow(eCent.y,2.));
      
        Ixy+=eMass*eCent.x*eCent.y;
        Ixz+=eMass*eCent.x*eCent.z;
        Iyz+=eMass*eCent.y*eCent.z;
      }
    }
  }
}

void odMass::report(){
  int i;
  printf("Mass Report:\n");
  printf("  Components = %i\n",massElement.size());
  for (i=0;i<(int)massElement.size();i++){
    printf("  Component %i - %s\n", i, massElement[i].meshRef.toLatin1().data());
    printf("        Mass = %lf Kg\n", massElement[i].mass);
    printf("    Centroid = %lf\t%lf\t%lf\t(x, y, z)\n", massElement[i].x, massElement[i].y, massElement[i].z);
  }
  printf("  Totals:\n");
  printf("    Mass     = %lf Kg\n", mass);
  printf("    Centroid = %lf\t%lf\t%lf\t(x, y, z)\n", centroid.x, centroid.y, centroid.z);
  printf("    Gyradius = %lf\t%lf\t%lf\t(Kxx, Kyy, Kzz)\n", sqrt(Ixx/mass), sqrt(Iyy/mass), sqrt(Izz/mass));
  printf("    Inertias = %lf\t%lf\t%lf\t(Ixy, Ixz, Iyz)\n", Ixy, Ixz, Iyz);
}
