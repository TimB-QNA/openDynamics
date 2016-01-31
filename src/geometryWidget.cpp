/*
    Description: Provide Gauges for OpenPilot (Main Code)
         Author: Tim Brocklehurst
        Version: 0.2.0
           Date: 24 February 2008
        License: GPLv3

Changelog:
    09 December 2007 - v0.1.0 alpha:
        First Release
    24 February 2008 - v0.2.0
        Added two plates
        Set up foreground colour
*/

#include <QtGui>
#include <math.h>
#include "geometryWidget.h"
#include "openNURBSDriver.h" 

geometryWidget::geometryWidget(QWidget *parent) : QWidget(parent)
{
  setAntialias(false);
  border=2;
}


bool geometryWidget::loadGeometry(QString fname, bool debug){
  onModel.load(fname, debug);
  update();    
}

void geometryWidget::paintEvent(QPaintEvent *event){
  int i;
  double scX, scY;
 
  QPainter painter(this);
  painter.setRenderHint(QPainter::Antialiasing, antialias());
  scX=(double)(width()-border)/(max.x-min.x);
  scY=(double)(height()-border)/(max.y-min.y);
  scale=scY; if (scX<scY) scale=scX;
  origin=(min+max)/2.;
  origin.x-=(width()-border)/(2.*scale);
  origin.y+=(height()-border)/(2.*scale);
  onModel.paint(&painter, origin.x, origin.y, scale);
}

void geometryWidget::setAntialias(bool val){
  SP_antialias=val;
  update();
}

bool geometryWidget::antialias(){ return SP_antialias; }
