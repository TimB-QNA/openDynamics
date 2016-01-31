#include <opennurbs.h>
#include <opennurbs_staticlib_linking_pragmas.h>
#include "openNURBSDriver.h"



openNURBSDriver::openNURBSDriver(){
  ON::Begin();
}

odPoint openNURBSDriver::min(){
  odPoint pt;
  pt.x=model.BoundingBox().Min().x;
  pt.y=model.BoundingBox().Min().y;
  pt.z=model.BoundingBox().Min().z;
  return pt;
}

odPoint openNURBSDriver::max(){
  odPoint pt;
  pt.x=model.BoundingBox().Max().x;
  pt.y=model.BoundingBox().Max().y;
  pt.z=model.BoundingBox().Max().z;
  return pt;
}
    
int openNURBSDriver::load(QString filename, bool debug){
  ON_TextLog error_log;
  FILE* archive_fp;
  
  error_log.SetIndentSize(2);
  archive_fp = ON::OpenFile( filename.toAscii().data(), "rb");
  
  // create achive object from file pointer
  ON_BinaryFile archive( ON::read3dm, archive_fp );
  // read the contents of the file into "model"
  bool rc = model.Read( archive, &error_log );
  // close the file
  ON::CloseFile( archive_fp );
    
  if (!rc){
    printf("Load Failed\n");
    return 1;
  }
  
//  model.IsValid(&error_log);
  return 0;
}

QString openNURBSDriver::convertString(ON_wString str){
  int i;
  QString out;
  for (i=0;i<str.Length();i++){
    out.append(str[i]);
  }
  return out;
}

void openNURBSDriver::paint(QPainter *painter, double x0, double y0, double scale){
  paintCurve( painter, x0, y0, scale);
}

void openNURBSDriver::paintCurve(QPainter *painter, double x0, double y0, double scale){
  int layerID, i, j, nPts=30;
  int lineWidth;
  double u, umin, umax;
  ON_Color lCol;
  ON_3dPoint ONPT;
  const ON_Curve* curve=0;
  QPen pen, blank;
  QPointF gpt[nPts];
  
  for(i=0;i<model.m_object_table.Count();i++){
    curve = ON_Curve::Cast(model.m_object_table[i].m_object);
    if (curve){
      layerID   = model.m_object_table[i].m_attributes.m_layer_index;
      lineWidth = model.m_layer_table[layerID].PlotWeight();
      lCol=model.m_layer_table[layerID].PlotColor();
      
      curve->GetDomain(&umin, &umax);
      for (j=0;j<nPts;j++){
        u=(double)j/(double)(nPts-1)*(umax-umin) + umin;
        ONPT=curve->PointAt(u);
        gpt[j].setX( (ONPT.x-x0)*scale );
        gpt[j].setY( (y0-ONPT.y)*scale );
      }
  
      pen.setWidth(lineWidth);
      pen.setColor(QColor(lCol.Red(), lCol.Green(), lCol.Blue(), 255-lCol.Alpha()));
      painter->setPen(pen);
      painter->drawPolyline(gpt,nPts);
      painter->setPen(blank);
    }
  }
}
