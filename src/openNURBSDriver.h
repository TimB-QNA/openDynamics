#ifndef ONDRIVER
  #define ONDRIVER
 
#include <QtCore>
#include <vector>
#include "odPoint.h"
#include "chanData.h"
#include <opennurbs.h>
#include <opennurbs_staticlib_linking_pragmas.h>

using namespace std;

class openNURBSDriver{
  public:
    QString view;
    
    openNURBSDriver();
    int load(QString filename, bool debug=false);
    QString convertString(ON_wString str);
    void paint(QPainter *painter, double x0, double y0, double scale);
    void paintCurve(QPainter *painter, double x0, double y0, double scale);
    odPoint min();
    odPoint max();
    
  private:
    ONX_Model model;
};
#endif
