#include "odMesh.h"
#ifdef GRAPHICS
  #include <vtkStructuredGrid.h>
  #include <vtkProperty.h>
  #include <vtkDataSetMapper.h>
#endif

odMesh::odMesh(){
  totalSrf=-1;  
}

void odMesh::translate(odPoint translation){
  int i;
  for (i=0;i<(int)polygon.size();i++) polygon[i].translate(translation);
}

double odMesh::surfaceArea(){
  int i;
  if (totalSrf>0) return totalSrf;
  
  totalSrf=0;
  for (i=0;i<(int)polygon.size();i++){
    totalSrf+=polygon[i].surfaceArea();
  }
  return totalSrf;
}
    
/*!
 * Reads a mesh stanza from XML. The following elements are supported in addition to the odObject elements:
 * - filename
 *  - filename for the class to load. At present only ASCII STL is supported.
 */
void odMesh::readFromXML(QDomNode root){
  int i;
  QDomNode node;
  QDomElement element;
  
  element=root.toElement();
  if (element.hasAttribute("reference")) refString=element.attribute("reference");
  
  node=root.firstChild();
  while (!node.isNull()){
    element=node.toElement();
    if (element.tagName().toLower()=="filename") geometryFile=element.text();
    node=node.nextSibling();
  }
  loadGeometry();
}


#ifdef GRAPHICS
/*!
 * For plotting purposes all sections must have the same number of points.
 */
void odMesh::plot(){
  int nPoints, i, j, triPt;
  dVector3 result;
  
  points = vtkPoints::New();
  triPt=0;
  for (i=0;i<(int)polygon.size();i++){
    triangle.push_back( vtkTriangle::New() );
    for (j=0;j<(int)polygon[i].pt.size();j++){
      dBodyGetRelPointPos(*odeBody, polygon[i].pt[j].x, polygon[i].pt[j].y, polygon[i].pt[j].z, result);
      points->InsertNextPoint(result[0], result[1], result[2]);
      triangle.back()->GetPointIds()->SetId ( j, triPt );
      triPt++;
    }
    objects->InsertNextCell(triangle.back());
  } 
  // Add the geometry and topology to the polydata
  polygons->SetPoints (points);
  polygons->SetPolys  (objects); 
  actor->GetProperty()->SetColor( 1.0, 1.0, 0 );
}
#endif

/*!
 * Calculate minimum x,y,z bound of mesh
 */
odPoint odMesh::minBound(){
  odPoint min(1e6,1e6,1e6);
  int i,j;
  
  for (i=0;i<(int)polygon.size();i++){
    for (j=0;j<(int)polygon[i].pt.size();j++){
      if (polygon[i].pt[j].x<min.x) min.x=polygon[i].pt[j].x;
      if (polygon[i].pt[j].y<min.y) min.y=polygon[i].pt[j].y;
      if (polygon[i].pt[j].z<min.z) min.z=polygon[i].pt[j].z;
    }
  }
  return min;
}

/*!
 * Calculate maximum x,y,z bound of mesh
 */
odPoint odMesh::maxBound(){
  odPoint max(-1e6,-1e6,-1e6);
  int i,j;
  
  for (i=0;i<(int)polygon.size();i++){
    for (j=0;j<(int)polygon[i].pt.size();j++){
      if (polygon[i].pt[j].x>max.x) max.x=polygon[i].pt[j].x;
      if (polygon[i].pt[j].y>max.y) max.y=polygon[i].pt[j].y;
      if (polygon[i].pt[j].z>max.z) max.z=polygon[i].pt[j].z;
    }
  }
  return max;
}

/*!
 * Calculate the x,y,z range between bounds (i.e. for a hull, length, beam and depth)
 */
odPoint odMesh::range(){
  return maxBound()-minBound();
}
    
    
/*!
 * This is the solver for this component. It calculates the forces on each "panel" of the foil
 * (between two sections) based on the local inflow velocity.
 */
void odMesh::run(){
  /*
  odPoint centroid, moment, relCent;
  odPolygon wrk;
  double pi=4.*atan(1.);
  double zeta, fz, fzt=0;
  dVector3 result;
  
  // Pre-calculate all areas...
  for (i=0;i<polygon.size();i++){
    polygon[i].surfaceArea();
  }

  for (i=0;i<polygon.size();i++){
    // calculate the working polygon
    wrk=polygon[i];
    for (j=0;j<polygon[i].pt.size();j++){
      dBodyGetRelPointPos(*odeBody, polygon[i].pt[j].x, polygon[i].pt[j].y, polygon[i].pt[j].z, result);
      wrk.pt[j]=odPoint(result[0], result[1], result[2]);
    }
    // Calculate volumetric properties below free surface.
    relCent=polygon[i].centroid();
    centroid=wrk.centroid();
    zeta=elevation(centroid.x, centroid.y);
    if (centroid.z<zeta){
      fz=wrk.projectedAreaXY()*(zeta-centroid.z)*density(centroid)*parentBody->simConstants.g;
      moment.x += fz*centroid.x;
      moment.y += fz*centroid.y;
      moment.z += fz*centroid.z;
      dBodyAddForceAtPos(*odeBody, 0, 0, fz, centroid.x, centroid.y, (centroid.z+zeta)/2.);
      fzt+=fz;
    }
  }
  moment/=fzt;
  printf("Z force = %lf N\tFCent %lf\t%lf\t%lf\n", fzt, moment.x, moment.y, moment.z);
  */
}

/*!
 * This routine attempts to load the geometry using one of the load*() routines.
 */
void odMesh::loadGeometry(){
  bool loadOk=false;
  int i;
  
  polygon.clear();  
  
  printf("odMesh :: Loading \"%s\"\n",geometryFile.toLatin1().data());
  if (loadAsciiSTL(geometryFile)) loadOk=true;
  
  if (!loadOk){
    polygon.clear();
    if (loadBinarySTL(geometryFile)) loadOk=true;
  }
  
  if (!loadOk){
    polygon.clear();
    if (loadRawTriangles(geometryFile)) loadOk=true;
  }
  
  if (loadOk){
    // Count the total number of points (yes, a lot of these are duplicate);
    nPoints=0;
    for (i=0;i<polygon.size();i++){
      nPoints+=polygon[i].pt.size();
    }
    // Pre-calculate all areas...
    for (i=0;i<polygon.size();i++){
      polygon[i].surfaceArea();
    }
  }else{
    printf("odMesh :: Could not load geometry - Unknown format or an error occurred\n"); 
  }
}

/*!
 * This routine loads the geometry as an ASCII STL file, or, if the format is not known, exits and returns false.
 */
bool odMesh::loadAsciiSTL(QString fileName){
  int i, j, nSolids=0;
  odPolygon pvr;   // Polygon vertex reference.
  char line[4096];
  QString scope;
  QStringList lineData;
  odPoint normal, tmpPt;
  FILE *inFile;
  
  printf("odMesh :: Attempting to load geometry as Ascii STL.\n");
  inFile=fopen(fileName.toLatin1().data(),"r");
  if (inFile==NULL){
    printf("odMesh :: Could not open '%s'.\n",fileName.toLatin1().data());
    return false;
  }
  printf("odMesh :: Individual solids will not be preserved.\n");
  printf("odMesh :: Facet normals will be ignored.\n");
  
  for (i=0;!feof(inFile);i++){
    line[0]='\0';
    fgets(line,4096,inFile);
//    printf("odMesh :: %s",line);
//    printf("odMesh :: Scope -- %s\n",scope.toLatin1().data());
    lineData=QString(line).trimmed().split(" ",QString::SkipEmptyParts);
    for (j=0;lineData.size()<2;j++) lineData.append(""); // Ensure that lineData contains at least two elements
//    printf("odMesh :: lineData[0] -- %s\n",lineData[0].toLatin1().data());
//    printf("odMesh :: lineData[1] -- %s\n",lineData[1].toLatin1().data());
    
    //Error checking section
    if (scope.contains("solid")){
      if (lineData[0]=="solid"){
        printf("odMesh :: Nested solids are not allowed at line %i. Stopping.\n",i);
        return false;
      }
    }
    if (scope.contains("facet")){
      if (lineData[0]=="facet"){
        printf("odMesh :: Nested facets are not allowed at line %i. Stopping.\n",i);
        return false;
      }
    }
    
    if (scope.contains("loop")){
      if (lineData[1]=="loop"){
        printf("odMesh :: Nested loops are not allowed at line %i. Stopping.\n",i);
        return false;
      }
      if (lineData[0]=="facet"){
        printf("odMesh :: Facet definition within loops are not allowed at line %i. Stopping.\n",i);
        return false;
      }
    }
    
    if (!scope.contains("solid")){
      if (lineData[0]=="facet"){
        printf("odMesh :: Facet data not allowed outside a solid definition at line %i. Stopping.\n",i);
        return false;
      }
      if (lineData[0]=="endsolid"){
        printf("odMesh :: \"end solid\" definition without prior solid definition at line %i. Stopping.\n",i);
        return false;
      }
    }
    
    if (!scope.contains("facet")){
      if (lineData[1]=="loop"){
        printf("odMesh :: Loop definition outside a facet at line %i. Stopping.\n",i);
        return false;
      }
    }
    
    //Read under correct conditions...
    if (!scope.contains("solid") && lineData[0]=="solid"){
      printf("odMesh :: Found solid - ");
      if (lineData.size()==1){
        printf("Solid has no name - Continuing\n");
      }else{
        printf("%s\n",lineData[1].toLatin1().data());
      }
      nSolids++;
      scope+="solid ";
    }

    if (scope.contains("solid")){
      if (lineData[0]=="endsolid"){
        scope.replace("solid ","");
        if (!scope.isEmpty()){
          printf("odMesh :: Scopes - %s - remain open at end solid at line %i. Continuing.",scope.toLatin1().data(),i);
          scope="";
        }
      }
      if (lineData[0]=="facet"){
        scope+="facet ";
        pvr.clear();
      }
    }
  
    if (scope.contains("solid") && scope.contains("facet")){
      if (lineData[1]=="normal"){
        normal.x=lineData[2].toDouble();
        normal.y=lineData[3].toDouble();
        normal.z=lineData[4].toDouble();
      }
      if (lineData[0]=="outer" && lineData[1]=="loop") scope+="loop ";
      if (lineData[0]=="endloop") scope.replace("loop ","");
      if (lineData[0]=="endfacet"){
        polygon.push_back(pvr);
        scope.replace("facet ","");
      }
    }
    
    if (scope.contains("solid") && scope.contains("facet") && scope.contains("loop")){
      if (lineData[0]=="vertex"){
        tmpPt.x=lineData[1].toDouble();
        tmpPt.y=lineData[2].toDouble();
        tmpPt.z=lineData[3].toDouble();
        pvr.pt.push_back(tmpPt);
      }
    }
  }
  
  if (polygon.size()==0){
    printf("odMesh :: Could not read any data.\n");
    return false;
  }
  printf("odMesh :: Geometry loaded sucessfully.\n");
  return true;
}

bool odMesh::loadBinarySTL(QString fileName){
  printf("odMesh :: Binary STL format not yet supported. Please use ASCII STL.\n");
  return false;
}

bool odMesh::loadRawTriangles(QString fileName){
  printf("odMesh :: Raw triangle format not yet supported. Please use ASCII STL.\n");
  return false;
}

QString odMesh::reference(){
  return refString;
}

/*!
 * Export mesh data as VTK for post-processing in paraview
 */
void odMesh::exportVTK(FILE *stream){
  int i, j, tt;
  fprintf(stream,"<?xml version=\"1.0\"?>\n");
  
  fprintf(stream,"<VTKFile type=\"UnstructuredGrid\" version=\"0.1\" byte_order=\"LittleEndian\">\n");
  fprintf(stream,"<UnstructuredGrid>\n");
  fprintf(stream,"  <Piece NumberOfPoints=\"%i\" NumberOfCells=\"%li\" >\n", nPoints, active.size());

  fprintf(stream,"    <Points>\n");
  fprintf(stream,"      <DataArray type=\"Float32\" NumberOfComponents=\"3\" format=\"ascii\">\n");
  for (i=0;i<active.size();i++){
    for (j=0;j<active[i].pt.size();j++){
      fprintf(stream,"%lf %lf %lf ", active[i].pt[j].x, active[i].pt[j].y, active[i].pt[j].z);
    }
    fprintf(stream,"\n");
  }
  fprintf(stream,"      </DataArray>\n");
  fprintf(stream,"    </Points>\n");
  
  fprintf(stream,"    <CellData Scalars=\"Pressure\">\n");
  fprintf(stream,"      <DataArray type=\"Float32\" Name=\"Pressure\" format=\"ascii\">\n");
  for (i=0;i<active.size();i++){
    fprintf(stream,"%lf ", active[i].pressure);
  }
  fprintf(stream,"\n");
  fprintf(stream,"      </DataArray>\n");
  
  fprintf(stream,"      <DataArray type=\"Float32\" Name=\"SkinFrictionTangential\" NumberOfComponents=\"3\" format=\"ascii\">\n");
  for (i=0;i<active.size();i++){
    fprintf(stream,"%lf %lf %lf ", active[i].PtangForce.x, active[i].PtangForce.y, active[i].PtangForce.z);
  }
  fprintf(stream,"\n");
  fprintf(stream,"      </DataArray>\n");
  fprintf(stream,"      <DataArray type=\"Float32\" Name=\"SkinFrictionNormal\" NumberOfComponents=\"3\" format=\"ascii\">\n");
  for (i=0;i<active.size();i++){
    fprintf(stream,"%lf %lf %lf ", active[i].PnormForce.x, active[i].PnormForce.y, active[i].PnormForce.z);
  }
  fprintf(stream,"\n");
  fprintf(stream,"      </DataArray>\n");  

  fprintf(stream,"      <DataArray type=\"Float32\" Name=\"Velocity\" NumberOfComponents=\"3\" format=\"ascii\">\n");
  for (i=0;i<active.size();i++){
    fprintf(stream,"%lf %lf %lf ", active[i].velocity.x, active[i].velocity.y, active[i].velocity.z);
  }
  fprintf(stream,"\n");
  fprintf(stream,"      </DataArray>\n");
  
  fprintf(stream,"      <DataArray type=\"Float32\" Name=\"Normals\" NumberOfComponents=\"3\" format=\"ascii\">\n");
  for (i=0;i<active.size();i++){
    fprintf(stream,"%lf %lf %lf ", active[i].normal().x, active[i].normal().y, active[i].normal().z);
  }
  fprintf(stream,"\n");
  fprintf(stream,"      </DataArray>\n");
  
  fprintf(stream,"    </CellData>\n");

  fprintf(stream,"    <Cells>\n");
  fprintf(stream,"      <DataArray type=\"Int32\" Name=\"connectivity\" format=\"ascii\">\n");
  
  tt=0;
  for (i=0;i<active.size();i++){
//    fprintf(stream,"%li ", active[i].pt.size());
    for (j=0;j<active[i].pt.size();j++){
      fprintf(stream,"%i ", tt);
      tt++;
    }
    fprintf(stream,"\n");
  }
  fprintf(stream,"      </DataArray>\n");
  fprintf(stream,"      <DataArray type=\"Int32\" Name=\"offsets\" format=\"ascii\">\n");
  tt=0;
  for (i=0;i<active.size();i++){
    tt+=active[i].pt.size();
    fprintf(stream,"%i ",tt);
  }
  fprintf(stream,"\n");
  fprintf(stream,"      </DataArray>\n");

  fprintf(stream,"      <DataArray type=\"UInt8\" Name=\"types\" format=\"ascii\">\n");
  for (i=0;i<active.size();i++){
    if (active[i].pt.size()==3) fprintf(stream,"5 ");
    if (active[i].pt.size()==4) fprintf(stream,"9 ");
    if (active[i].pt.size()>4)  fprintf(stream,"7 ");
  }
  fprintf(stream,"\n");
  fprintf(stream,"    </DataArray>\n");

  fprintf(stream,"    </Cells>\n");
  fprintf(stream,"  </Piece>\n");
  fprintf(stream,"</UnstructuredGrid>\n");
  fprintf(stream,"</VTKFile>\n");
}