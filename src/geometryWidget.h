#ifndef GEOMETRYWIDGET_H
  #define GEOMETRYWIDGET_H
  
#include <QtGui>
#include <QtDesigner/QDesignerExportWidget>
#include <vector>
#include "openNURBSDriver.h" 
#include "odPoint.h"
using namespace std;

class QDESIGNER_WIDGET_EXPORT geometryWidget : public QWidget
{
   Q_OBJECT
   Q_PROPERTY(bool AntiAliased READ antialias WRITE setAntialias)

public:
  geometryWidget(QWidget *parent=0);
  bool loadGeometry(QString fname, bool debug=false);
  
public slots:
  void setAntialias(bool val);
  bool antialias();

private:
  void paintEvent(QPaintEvent *event);
  void paintCurve(QPainter *painter, int crv);
  
  openNURBSDriver onModel;
  
  bool SP_antialias;
  QString fileName;
  odPoint origin;
  double scale;
  odPoint min;
  odPoint max;
  int border;
};

#endif

