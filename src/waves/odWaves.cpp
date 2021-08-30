#include "odWaves.h"
#include "PMSpectrum.h"

#ifdef GRAPHICS
  #include <vtkStructuredGrid.h>
  #include <vtkProperty.h>
  #include <vtkDataSetMapper.h>
#endif

odWaves::odWaves(){
  waveHeading=0.;
  name="Waves";
  spectrum=nullptr;
}

void odWaves::readFromXML(QDomNode root){
  QDomNode node;
  QDomElement element;
  node=root.firstChild();
  while (!node.isNull()){
    element=node.toElement();
    if (element.tagName().toLower()=="type"){
      if (element.text().toLower()=="pm") spectrum = new PMSpectrum(constants, root.firstChild());
    }
    if (element.tagName().toLower()=="heading")     waveHeading=element.text().toDouble()+90; // Add 90 to make wave heading 0 move toward +X
    if (element.tagName().toLower()=="ramptime")    rampTime=element.text().toDouble();
    node=node.nextSibling();
  }
}

void odWaves::createSeaState(){
  unsigned int i;
  double dOmega;
  
  amplitude.clear();
  heading.clear();

  if (spectrum==nullptr) return;
  
  spectrum->createSpectrum();
  
  dOmega=spectrum->freq.range()/(double)spectrum->spectrum.size();
  
//  printf("FreqMin = %lf\n", spectrum->freq.min);
//  printf("FreqMax = %lf\n", spectrum->freq.max);
//  printf("FreqRange = %lf\n", spectrum->freq.range());
//  printf("Spectrum size = %lf\n", (double)spectrum->spectrum.size());
//  printf("dOmega = %lf\n", dOmega);
  
  for (i=0;i<spectrum->spectrum.size();i++){
    amplitude.push_back(2.*spectrum->spectrum[i].magnitude*dOmega);
    heading.push_back(constants->pi/180. * waveHeading);
    
//    printf("F / M / A = %lf      %lf     %lf\n", spectrum->spectrum[i].frequency, spectrum->spectrum[i].magnitude,  amplitude[i]);
  }
}

double odWaves::elevationInfluence(double x, double y){
  unsigned int i;
  double zeta, lambda, k, period;
  if (spectrum==nullptr) return 0;
  
  zeta=0;
  for (i=0;i<spectrum->spectrum.size();i++){
    period=spectrum->spectrum[i].frequency/(2.*constants->pi);
    lambda=constants->g/(2.*constants->pi*pow(period,2));
    k=(2.*constants->pi)/lambda;
    zeta+=amplitude[i] * sin(spectrum->spectrum[i].frequency*(*simTime)-k*sin(heading[i])*x-k*cos(heading[i])*y+spectrum->spectrum[i].phase);
  }
  if (*simTime<rampTime) zeta*= *simTime/rampTime;
  zeta*=50;
  
  return zeta;
}

odPoint odWaves::velocityInfluence(odPoint pos){
  unsigned int i;
  double zeta_i, zeta, z, lambda, k, period, freq, phase;
  odPoint wV;
  if (spectrum==nullptr) return wV;
  
  zeta=odObject::elevation(pos.x, pos.y);
  if (pos.z>zeta) return wV;
  z=-pos.z;
  for (i=0;i<spectrum->spectrum.size();i++){
    freq=spectrum->spectrum[i].frequency;
    phase=spectrum->spectrum[i].phase;
    
    period=freq/(2.*constants->pi);
    lambda=constants->g/(2.*constants->pi*pow(period,2));
    k=(2.*constants->pi)/lambda;
    zeta_i=amplitude[i]*sin(freq*(*simTime)-k*sin(heading[i])*pos.x-k*cos(heading[i])*pos.y+phase);
    wV.x+=freq/(2.*constants->pi)*zeta*exp(k*z)*sin(freq*(*simTime)-k*sin(heading[i])*pos.x+phase);
    wV.y+=freq/(2.*constants->pi)*zeta*exp(k*z)*sin(freq*(*simTime)-k*cos(heading[i])*pos.y+phase);
    wV.z+=freq/(2.*constants->pi)*zeta*exp(k*z)*cos(freq*(*simTime)-k*sin(heading[i])*pos.x-k*cos(heading[i])*pos.y+phase);
  }
  if (*simTime<rampTime) wV*= *simTime/rampTime;
  return wV;
}

#ifdef GRAPHICS
void odWaves::plot(){
  int nPoints, i, j;
  double x, y;
  double extent_highRes=50, extent_lowRes=50;
  double lambda_min;
  vtkPoints *points;
  
  lambda_min=parentBody->simConstants.g/(2.*parentBody->simConstants.pi*pow(period.min,2));
  
  vtkStructuredGrid *grid = vtkStructuredGrid::New();
 
  points = vtkPoints::New();
  // Create High-res points
  nPoints=extent_highRes;
 
  for (j=0;j<nPoints;j++){
    for (i=0;i<nPoints;i++){  
      x=-extent_highRes + 2.*extent_highRes * (double)i/(double)(nPoints-1);
      y=-extent_highRes + 2.*extent_highRes * (double)j/(double)(nPoints-1);
      points->InsertNextPoint( x, y, elevation(x,y) );
    }
  }
  grid->SetDimensions(nPoints,nPoints,1);
  grid->SetPoints(points);
  
  vtkDataSetMapper *mapper = vtkDataSetMapper::New();
  mapper->SetInputConnection(grid->GetProducerPort());
  actor->SetMapper(mapper);
  actor->GetProperty()->SetColor( 0, 0, 1.0 );
}
#endif

/*!
 * Export mesh data as VTK for post-processing in paraview
 */
/*!
 * Export mesh data as VTK for post-processing in paraview
 */
void odWaves::exportVTK(FILE *stream){
  int i, j, off, imax=100, jmax=100;
  double x, y, extent=20;
  fprintf(stream,"<?xml version=\"1.0\"?>\n");
  
  fprintf(stream,"<VTKFile type=\"UnstructuredGrid\" version=\"0.1\" byte_order=\"LittleEndian\">\n");
  fprintf(stream,"<UnstructuredGrid>\n");
  fprintf(stream,"  <Piece NumberOfPoints=\"%i\" NumberOfCells=\"%i\" >\n", imax*jmax, (imax-1)*(jmax-1));
  
  fprintf(stream,"    <PointData Scalars=\"Elevation\">\n");
  fprintf(stream,"      <DataArray type=\"Float32\" Name=\"Elevation\" format=\"ascii\">\n");
  
  for (j=0;j<jmax;j++){
    for (i=0;i<imax;i++){  
      x=-extent + 2.*extent * (double)i/(double)(imax-1);
      y=-extent + 2.*extent * (double)j/(double)(jmax-1);
      fprintf(stream,"%lf ", elevation(x,y) );
    }
    fprintf(stream,"\n");
  }
  fprintf(stream,"      </DataArray>\n");
  fprintf(stream,"    </PointData>\n");
  
  fprintf(stream,"    <Points>\n");
  fprintf(stream,"      <DataArray type=\"Float32\" NumberOfComponents=\"3\" format=\"ascii\">\n");
  
  for (j=0;j<jmax;j++){
    for (i=0;i<imax;i++){  
      x=-extent + 2.*extent * (double)i/(double)(imax-1);
      y=-extent + 2.*extent * (double)j/(double)(jmax-1);
      fprintf(stream,"%lf %lf %lf ", x, y, elevation(x,y) );
    }
    fprintf(stream,"\n");
  }
  fprintf(stream,"      </DataArray>\n");
  fprintf(stream,"    </Points>\n");
  fprintf(stream,"    <Cells>\n");
  fprintf(stream,"      <DataArray type=\"Int32\" Name=\"connectivity\" format=\"ascii\">\n");
  
  for (j=0;j<jmax-1;j++){
    for (i=0;i<imax-1;i++){
      fprintf(stream,"%i %i %i %i ", j*imax+i, (j+1)*imax+i, (j+1)*imax+i+1, j*imax+i+1);
    }
    fprintf(stream,"\n");
  }
  fprintf(stream,"      </DataArray>\n");
  fprintf(stream,"      <DataArray type=\"Int32\" Name=\"offsets\" format=\"ascii\">\n");
  off=0;
  for (j=0;j<jmax-1;j++){
    for (i=0;i<imax-1;i++){
      off+=4;
      fprintf(stream,"%i ",off);
    }
    fprintf(stream,"\n");
  }
  fprintf(stream,"      </DataArray>\n");

  fprintf(stream,"      <DataArray type=\"UInt8\" Name=\"types\" format=\"ascii\">\n");
  for (j=0;j<jmax-1;j++){
    for (i=0;i<imax-1;i++){
      fprintf(stream,"9 ");
    }
    fprintf(stream,"\n");
  }
  fprintf(stream,"\n");
  fprintf(stream,"    </DataArray>\n");

  fprintf(stream,"    </Cells>\n");
  fprintf(stream,"  </Piece>\n");
  fprintf(stream,"</UnstructuredGrid>\n");
  fprintf(stream,"</VTKFile>\n");
}
