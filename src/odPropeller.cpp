#include "odPropeller.h"
#include "wageningenB.h"

#ifdef GRAPHICS
  #include <vtkStructuredGrid.h>
  #include <vtkProperty.h>
  #include <vtkDataSetMapper.h>
#endif

#include <math.h>

/*!
 * Set up coefficient sets for kT and kQ.
 * Coefficients are taken from "KT,KQ and Efficiency Curves for the Wageningen B-Series Propellers" by M.M. Bernitsas, D Ray & P. Kinley
 */
odPropeller::odPropeller(){
  model = new odPropellerModel();
}

void odPropeller::preSolveSetup(){
  openDataStream(QString("odPropeller"));
  model->dataStream=dataStream;
}

void odPropeller::readFromXML(QDomNode root){
  QDomNode node, subNode;
  QDomElement element, subElement;
  readCommonDataFromXML(root);
  
  node=root.firstChild();
  while (!node.isNull()){
    element=node.toElement();
    if (element.tagName().toLower()=="location")       hubOrigin.readFromXML(element);
    if (element.tagName().toLower()=="thrustvector"){
      thrustVector.readFromXML(element);
      thrustVector=thrustVector.unitVector();
    }
    if (element.tagName().toLower()=="model"){
      delete model;
      model = createModel(element.attribute("type"));
      if (model->type.toLower()!="blank"){
        printf("Reading propeller model xml\n");
        model->readFromXML(element);
      }
    }
    if (element.tagName().toLower()=="rpm") RPM=element.text().toDouble();
    node=node.nextSibling();
  }
}


#ifdef GRAPHICS
/*!
 * For plotting purposes all sections must have the same number of points.
 */
void odPropeller::plot(){
  int nPoints, i, j;
  odPoint pt;
  dVector3 result;
  vtkPoints *points;
  vector<odFoilSection> worldSection;        ///< Sections defining the foil in world axes; it is these sections
                                             /// that are translated and rotated before modelling forces and moments.
  worldSection=modelSection;
  
  for (i=0;i<(int)worldSection.size();i++){
    for (j=0;j<(int)worldSection[i].point.size();j++){
      pt=modelSection[i].point[j];
 //     dBodyVectorToWorld(*odeBody, pt.x, pt.y, pt.z, result);
 //     worldSection[i].point[j]=odPoint(result[0], result[1], result[2]);
      dBodyGetRelPointPos(*odeBody, pt.x, pt.y, pt.z, result);
      worldSection[i].point[j]=odPoint(result[0], result[1], result[2]);

    }
  }
      
  points = vtkPoints::New();
  for (i=0;i<(int)worldSection.size();i++){
    for (j=0;j<(int)worldSection[0].point.size();j++){
      points->InsertNextPoint( worldSection[i].point[j].x, worldSection[i].point[j].y, worldSection[i].point[j].z );
    }
  }
  vtkStructuredGrid *grid = vtkStructuredGrid::New();
  grid->SetDimensions(worldSection[0].point.size(), worldSection.size(), 1);
  grid->SetPoints(points);

  vtkDataSetMapper *mapper = vtkDataSetMapper::New();
  mapper->SetInputConnection(grid->GetProducerPort());
  actor->SetMapper(mapper);
  actor->GetProperty()->SetColor( 1.0, 0, 0 );
}
#endif

/*!
 * This is the solver for this component. It calculates the forces on each "panel" of the foil
 * (between two sections) based on the local inflow velocity.
 */
void odPropeller::run(){
  odPoint fsVel;
  odPoint modelForce, modelMoment;

  fsVel=localVelocity(hubOrigin, uid);
  
  model->n=RPM/60.;
  model->V=fsVel.dotProduct_natural(thrustVector);
  model->rho=density(hubOrigin);
  
  modelForce =thrustVector*model->calcT();
  modelMoment=thrustVector*model->calcQ();
  
  dBodyAddRelForceAtRelPos(*odeBody, modelForce.x, modelForce.y, modelForce.z, hubOrigin.x, hubOrigin.y, hubOrigin.z);
  fprintf(dataStream,"time %lf inflow %lf %lf %lf model %lf %lf force %lf\t%lf\t%lf moment %lf\t%lf\t%lf\n", *simTime, fsVel.x, fsVel.y, fsVel.z,
                                                                                                       model->V, model->rho,
                                                                                                       modelForce.x, modelForce.y, modelForce.z,
                                                                                                       modelMoment.x, modelMoment.y, modelMoment.z);
  dBodyAddRelTorque(*odeBody, modelMoment.x, modelMoment.y, modelMoment.z);
}

odPropellerModel* odPropeller::createModel(QString modelType){
  if (modelType.toLower()=="wageningen-bseries"){
    return new wageningenB();
  }
  fprintf(dataStream,"Cannot find propeller model \"%s\"\n",modelType.toLatin1().data());
  return new odPropellerModel();
}
