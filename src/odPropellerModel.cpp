#include "odPropellerModel.h"

#ifdef GRAPHICS
  #include <vtkStructuredGrid.h>
  #include <vtkProperty.h>
  #include <vtkDataSetMapper.h>
#endif

odPropellerModel::odPropellerModel(){
  type="Blank";
}  

void odPropellerModel::readFromXML(QDomNode root){
  QDomNode node;
  QDomElement element;
  
  node=root.firstChild();
  while (!node.isNull()){
    element=node.toElement();
//    if (element.tagName().toLower()=="diameter")       diameter       = element.text().toDouble();
//    if (element.tagName().toLower()=="bladearearatio") bladeAreaRatio = element.text().toDouble();
//    if (element.tagName().toLower()=="pitchdiameter")  ptichDiameter  = element.text().toDouble();
//    if (element.tagName().toLower()=="blades")         blades         = element.text().toInt();
    node=node.nextSibling();
  }
}


#ifdef GRAPHICS
/*!
 * For plotting purposes all sections must have the same number of points.
 */
void odPropellerModel::plot(){
  /*
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
  */
}
#endif
    
double odPropellerModel::calcT(){
  return 0.0;
}

double odPropellerModel::calcQ(){
  return 0.0;
}
