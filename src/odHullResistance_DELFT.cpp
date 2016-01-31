#include "odHullResistance_DELFT.h"

odHullResistance_DELFT::odHullResistance_DELFT(){
  odHullResistance();
  
  // Set Defaults... based on Larsson's YD40 upright with no yaw. at a Fn of .3
  Fn=.3; Lwl=10.02; Bwl=3.17;
  Cp=.56; Cm=.72; Displ=7.75;
  lcb=4.659; lcf=5.;
  Awp=15.; Sc=24.9;
  T=2.07; Tc=.57;
}

void odHullResistance_DELFT::readFromXML(QDomNode root){
  int i;
  QDomNode node;
  QDomElement element;
  
  readCommonDataFromXML(root);
  node=root.firstChild();
  while (!node.isNull()){
    element=node.toElement();
    if (element.tagName().toLower()=="beam") Bwl=element.text().toDouble();
    if (element.tagName().toLower()=="coefficient"){
      if (element.hasAttribute("prismatic")) Cp=element.attribute("prismatic").toDouble();
      if (element.hasAttribute("midship")) Cm=element.attribute("midship").toDouble();
    }
    if (element.tagName().toLower()=="volume") Displ=element.text().toDouble();
    if (element.tagName().toLower()=="lcb") lcb=element.text().toDouble();
    if (element.tagName().toLower()=="lcf") lcf=element.text().toDouble();
    if (element.tagName().toLower()=="waterplaneArea") Awp=element.text().toDouble();
    if (element.tagName().toLower()=="surfaceArea") Sc=element.text().toDouble();
    if (element.tagName().toLower()=="keelDraught") T=element.text().toDouble();
    if (element.tagName().toLower()=="canoeDraught") Tc=element.text().toDouble();
    
    node=node.nextSibling();
  }
}

odPoint odHullResistance_DELFT::calculateForces(){
  odPoint force;
  double rsHull;
  
  if (Fn<0.1) return odPoint();
    
  if (model==0){
    setup_coeff_gerritsma95();
/*
    fprintf(stderr,"ITTC    : %lf\n",ittc57(.7*Lwl, Sc));
    fprintf(stderr,"upright : %lf\n",upright95());
    fprintf(stderr,"heel    : %lf\n",h_heel95());
    fprintf(stderr,"induced : %lf\n",induced95()/2.);
*/
    force.x=(upright95()+h_heel95()+induced95()/2.)*1.1;
    force.y=sideforce95();
//    fprintf(stderr,"vel.x=%lf\t\tvel.y=%lf\n",velocity.x,velocity.y);
    if (velocity.x<0.) force.x*=-1.;
    if (velocity.y<0.) force.y*=-1.;
  }

//  if (model==1)    
//  fprintf(stderr,"Drag = %lf N\t\tSide = %lf N\n", force.x, force.y);
  return force;
}

double odHullResistance_DELFT::correct_wsa(){
  int i,hplus=7,hminus=0;
  double s[4][8], guide[8], s0, s1, s2, s3, frac, wa=0;
  guide[0]=0; guide[1]=5; guide[2]=10; guide[3]=15; guide[4]=20; guide[5]=25; guide[6]=30; guide[7]=35;
  s[0][0]=0; s[0][1]=-4.112; s[0][2]=-4.522; s[0][3]=-3.291; s[0][4]=1.85;  s[0][5]=6.51;   s[0][6]=12.334; s[0][7]=14.648;
  s[1][0]=0; s[1][1]=0.054;  s[1][2]=-.132;  s[1][3]=-.389;  s[1][4]=-1.2;  s[1][5]=-2.305; s[1][6]=-3.911; s[1][7]=-5.182;
  s[2][0]=0; s[2][1]=-.027;  s[2][2]=-.077;  s[2][3]=-.118;  s[2][4]=-.109; s[2][5]=-.066;  s[2][6]=.024;   s[2][7]=0.102;
  s[3][0]=0; s[3][1]=6.329;  s[3][2]=8.738;  s[3][3]=8.949;  s[3][4]=5.364; s[3][5]=3.443;  s[3][6]=1.767;  s[3][7]=3.497;

  // Use a linear interpolation here. Perhaps one day I'll use a spline interpolation...
  for (i=0;i<7;i++) if (attitude.y>=guide[i]) hminus=i;
  for (i=7;i>0;i--) if (attitude.y<guide[i]) hplus=i;
  if (guide[hminus]!=attitude.y){
  frac=(attitude.y-guide[hminus])/(guide[hplus]-guide[hminus]);
    s0=s[0][hminus]+frac*(s[0][hplus]-s[0][hminus]);
    s1=s[1][hminus]+frac*(s[1][hplus]-s[1][hminus]);
    s2=s[2][hminus]+frac*(s[2][hplus]-s[2][hminus]);
    s3=s[3][hminus]+frac*(s[3][hplus]-s[3][hminus]);
  } else {
    s0=s[0][hminus]; s1=s[1][hminus];
    s2=s[2][hminus]; s3=s[3][hminus];
  }
  wa=Sc*(1.+0.01*(s0 + s1*Bwl/Tc + s2*pow((Bwl/Tc),2.) + s3*Cm));
  return wa;
}

double odHullResistance_DELFT::h_heel95(){
  int i,hplus=7,hminus=0;
  double c0, c1, c2, c3, c4, c5, frac;
  double Rh, Rr;
  
  // Use a linear interpolation here. Perhaps one day I'll use a spline interpolation...
  for (i=0;i<10;i++) if (Fn>=froude[i])  hminus=i;
  for (i=10;i>0;i--) if (Fn<froude[i])  hplus=i;
  frac=(Fn-froude[hminus])/(froude[hplus]-froude[hminus]);
  c0=c[0][hminus]+frac*(c[0][hplus]-c[0][hminus]);
  c1=c[1][hminus]+frac*(c[1][hplus]-c[1][hminus]);
  c2=c[2][hminus]+frac*(c[2][hplus]-c[2][hminus]);
  c3=c[3][hminus]+frac*(c[3][hplus]-c[3][hminus]);
  c4=c[4][hminus]+frac*(c[4][hplus]-c[4][hminus]);
  c5=c[5][hminus]+frac*(c[5][hplus]-c[5][hminus]);
  
//  fprintf(stderr,"Fn=%lf frac=%lf c=%lf\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\n",Fn,frac,c0,c1,c2,c3,c4,c5);
  
  Rh=c0 + c1*Lwl/Bwl + c2*Bwl/Tc + c3*pow(Bwl/Tc,2.) + c4*lcb/Lwl + c5*pow(lcb/Lwl,2.);
  Rr=( Rh*Displ*(rho/1000.)*g ) *6.*pow((fabs(attitude.y)*pi/180.),1.7);

  return Rr;
}


double odHullResistance_DELFT::upright95()
{
  int i=0 ,fgreater=10,flesser=0;
  double a0,a1,a2,a3,a4,a5,a6,a7,a8,frac;
  double Rr;
  
  for (i=0;i<10;i++) if (Fn>=froude[i]) flesser=i;
  for (i=10;i>0;i--) if (Fn<froude[i]) fgreater=i;
  
  // Use a linear interpoltation here
  frac=(Fn-froude[flesser])/(froude[fgreater]-froude[flesser]);
  a0=a[0][flesser]+frac*(a[0][fgreater]-a[0][flesser]);
  a1=a[1][flesser]+frac*(a[1][fgreater]-a[1][flesser]);
  a2=a[2][flesser]+frac*(a[2][fgreater]-a[2][flesser]);
  a3=a[3][flesser]+frac*(a[3][fgreater]-a[3][flesser]);
  a4=a[4][flesser]+frac*(a[4][fgreater]-a[4][flesser]);
  a5=a[5][flesser]+frac*(a[5][fgreater]-a[5][flesser]);
  a6=a[6][flesser]+frac*(a[6][fgreater]-a[6][flesser]);
  a7=a[7][flesser]+frac*(a[7][fgreater]-a[7][flesser]);
  a8=a[8][flesser]+frac*(a[8][fgreater]-a[8][flesser]);
  
//  fprintf(stderr,"Fn=%lf frac=%lf a=%lf\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\t%lf\n",Fn,frac,a0,a1,a2,a3,a4,a5,a6,a7,a8);
  
  Rr=a0+( a1*lcb/Lwl + a2*Cp + a3*pow(Displ,2./3.)/Awp + a4*Bwl/Lwl + a5*pow(Displ,2./3.)/Sc + a6*lcb/lcf + a7*pow(lcb/Lwl,2.) +a8*pow(Cp,2.) ) * pow(Displ,1./3.)/Lwl;
  Rr=Rr * Displ * rho * g;
  return Rr;
}

/*
double odHullResistance_DELFT::appendage_upright95(){
  int i, fgreater=10, flesser=0;
  double d0, d1, d2, d3, frac, Rrk;
  for (i=0;i<=10;i++) if (Fn>=froude[i]) flesser=i;
  for (i=10;i>=0;i--) if (Fn<froude[i]) fgreater=i;
  if (froude[flesser]!=Fn){
    // Use a linear interpoltation here
    frac=(Fn-froude[flesser])/(froude[fgreater]-froude[flesser]);
    d0=d[0][flesser]+frac*(d[0][fgreater]-d[0][flesser]);
    d1=d[1][flesser]+frac*(d[1][fgreater]-d[1][flesser]);
    d2=d[2][flesser]+frac*(d[2][fgreater]-d[2][flesser]);
    d3=d[3][flesser]+frac*(d[3][fgreater]-d[3][flesser]);
  } else {
    d0=d[0][flesser];
    d1=d[1][flesser];
    d2=d[2][flesser];
    d3=d[3][flesser];
  }
  Rrk=(d0 + d1*T/Bwl + d2*(Tc+(T+Tc)/2.)/pow(k_vol,1./3.) + d3*(Displ/k_vol) ) * (k_vol*rho*g);
  return Rrk;
}

double odHullResistance_DELFT::k_heel95(){
  double Rr=0., Ch;
  if (attitude.y!=0){
    // We must be using gerritsma '95!!! Keuning '95 is for the bare-hull only!!!
    Ch=-3.5837*Tc/T - 0.0518*Bwl/Tc + 0.5958*Tc/T*Bwl/Tc + 0.2055*Lwl/pow(Displ,1./3.);
    Rr=(Ch*pow(Fn,2.)*attitude.y*PI/180.)*k_vol*rho*g;
  }
  return Rr;
}
*/

double odHullResistance_DELFT::induced95(){
  int i, pplus=3, pminus=0;
  double frac, Di=0, Te;
  double b0, b1, b2, b3, b4, b5;
  double taper=1; // Taper ratio (No appendages are defined here. They are handled separately)
  
    // Use a linear interpolation here too.
    for (i=0;i<3;i++) if (attitude.y>=Tephi[i]) pminus=i;
    for (i=3;i>0;i--) if (attitude.y<Tephi[i]) pplus=i;
    
    frac=(fabs(attitude.y)-Tephi[pminus])/(Tephi[pplus]-Tephi[pminus]);
    b0=b[0][pminus]+frac*(b[0][pplus]-b[0][pminus]);
    b1=b[1][pminus]+frac*(b[1][pplus]-b[1][pminus]);
    b2=b[2][pminus]+frac*(b[2][pplus]-b[2][pminus]);
    b3=b[3][pminus]+frac*(b[3][pplus]-b[3][pminus]);
    b4=b[4][pminus]+frac*(b[4][pplus]-b[4][pminus]);
    b5=b[5][pminus]+frac*(b[5][pplus]-b[5][pminus]);

    Te=( b0*(Tc/T) + b1*pow(Tc/T,2) + b2*(Bwl/Tc) + b3*taper ) * (b4 + b5*Fn)*T;
    Di=pow(sideforce95(),2)/(pi* pow(Te,2) *.5*rho*pow(Vf,2));
  return Di;
}


double odHullResistance_DELFT::sideforce95(){
  int pminus=0, pplus=3, i;
  double frac, l0, l1, l2, l3, Fh;
  for (i=0;i<3;i++) if (attitude.y>=lh[i]) pminus=i;
  for (i=3;i>0;i--) if (attitude.y<lh[i]) pplus=i;

  frac=(fabs(attitude.y)-lh[pminus])/(lh[pplus]-lh[pminus]);
  l0=l[0][pminus]+frac*(l[0][pplus]-l[0][pminus]);
  l1=l[1][pminus]+frac*(l[1][pplus]-l[1][pminus]);
  l2=l[2][pminus]+frac*(l[2][pplus]-l[2][pminus]);
  l3=l[3][pminus]+frac*(l[3][pplus]-l[3][pminus]);
  
  Fh=l0*pow(T,2.)/Sc + l1*pow(pow(T,2.)/Sc,2) + l2*Tc/T + l3*Tc/T*pow(T,2.)/Sc;
  Fh=Fh*(attitude.z*pi/180.)*Sc*.5*rho*pow(Vf,2.) / cos(attitude.y*pi/180.);
  Fh=Fh/2.;
  return Fh;
}

void odHullResistance_DELFT::setup_coeff_gerritsma95(){
  froude[0]=.10; froude[1]=.15; froude[2]=.20; froude[3]=.25; froude[4]=.30; froude[5]=.35; froude[6]=.40;
  froude[7]=.45; froude[8]=.50; froude[9]=.55; froude[10]=.6;

  a[0][0]=-.0014; a[1][0]=0.0403; a[2][0]=0.047;  a[3][0]=-.0227; a[4][0]=-.0119; a[5][0]=0.0061; a[6][0]=-.0086; a[7][0]=-.0307; a[8][0]=-0.0553;
  a[0][1]=0.0004; a[1][1]=-.1808; a[2][1]=0.1793; a[3][1]=-.0004; a[4][1]=0.0097; a[5][1]=0.0118; a[6][1]=-.0055; a[7][1]=0.1721; a[8][1]=-0.1728;
  a[0][2]=0.0014; a[1][2]=-.1071; a[2][2]=0.0637; a[3][2]=0.009;  a[4][2]=0.0153; a[5][2]=0.0011; a[6][2]=0.0012; a[7][2]=0.1021; a[8][2]=-0.0648;
  a[0][3]=0.0027; a[1][3]=0.0463; a[2][3]=-.1263; a[3][3]=0.015;  a[4][3]=0.0274; a[5][3]=-.0299; a[6][3]=0.011;  a[7][3]=-.0595; a[8][3]=0.122;
  a[0][4]=0.0056; a[1][4]=-.8005; a[2][4]=0.4891; a[3][4]=0.0269; a[4][4]=0.0519; a[5][4]=-.0313; a[6][4]=0.0292; a[7][4]=0.7314; a[8][4]=-0.3619;
  a[0][5]=0.0032; a[1][5]=-.1011; a[2][5]=-.0813; a[3][5]=-.0382; a[4][5]=0.032;  a[5][5]=-.1481; a[6][5]=0.0837; a[7][5]=0.0223; a[8][5]=0.1587;
  a[0][6]=-.0064; a[1][6]=2.3095; a[2][6]=-1.5152;a[3][6]=0.0751; a[4][6]=-.0858; a[5][6]=-.5349; a[6][6]=0.1715; a[7][6]=-2.455; a[8][6]=1.1865;
  a[0][7]=-.0171; a[1][7]=3.4017; a[2][7]=-1.9862;a[3][7]=0.3242; a[4][7]=-.145;  a[5][7]=-.8043; a[6][7]=0.2952; a[7][7]=-3.5284;a[8][7]=1.3575; 
  a[0][8]=-.0201; a[1][8]=7.1576; a[2][8]=-6.3304;a[3][8]=0.5829; a[4][8]=0.163;  a[5][8]=-.3966; a[6][8]=0.5023; a[7][8]=-7.1579;a[8][8]=5.2534;
  a[0][9]= .0495; a[1][9]=1.5618; a[2][9]=-6.0661;a[3][9]=0.8641; a[4][9]=1.1702; a[5][9]=1.761;  a[6][9]=0.9176; a[7][9]=-2.1191;a[8][9]=5.4281;
  a[0][10]=.0808; a[1][10]=-5.3233; a[2][10]=-1.1513; a[3][10]=.9663; a[4][10]=1.6084; a[5][10]=2.7459; a[6][10]=.8491; a[7][10]=4.7129; a[8][10]=1.1089;

  // For calculation of effective draught for induced drag
  Tephi[0]=0; Tephi[1]=10; Tephi[2]=20; Tephi[3]=30;
  b[0][0]=3.7455;  b[0][1]=4.4892;  b[0][2]=3.9592;  b[0][3]=3.4891;
  b[1][0]=-3.6246; b[1][1]=-4.8454;  b[1][2]=-3.9804;  b[1][3]=-2.9577;
  b[2][0]=0.0589;  b[2][1]=0.0294;  b[2][2]=0.0283;  b[2][3]=0.0250;
  b[3][0]=-0.0296; b[3][1]=-0.0075; b[3][2]=-0.0075; b[3][3]=-0.0272;
  b[4][0]=1.2306;  b[4][1]=1.4231;  b[4][2]=1.5450;  b[4][3]=1.4744;
  b[5][0]=-0.7256; b[5][1]=-1.2971; b[5][2]=-1.5622; b[5][3]=-1.2499;

  // The following is for Hull residuary resistance at 20 degrees heel
  // See Naval Architecture 2 notes, page 24.
  c[0][3]=-.0268; c[1][3]=-.00114; c[2][3]=-.0057; c[3][3]=0.0016; c[4][3]=-.0070; c[5][3]=-.0017;
  c[0][4]=0.6628; c[1][4]=-.0632; c[2][4]=-.00699; c[3][4]=0.0069; c[4][4]=0.0459; c[5][4]=-.0004;
  c[0][5]=1.6433; c[1][5]=-.2144; c[2][5]=-0.164; c[3][5]=0.0199; c[4][5]=-0.054; c[5][5]=-.0268;
  c[0][6]=-.8659; c[1][6]=-.0354; c[2][6]=0.2226; c[3][6]=0.0188; c[4][6]=-.5800; c[5][6]=-.1133;
  c[0][7]=3.2715; c[1][7]=0.1372; c[2][7]=0.5547; c[3][7]=0.0268; c[4][7]=-1.0064; c[5][7]=-.2026;
  c[0][8]=-.1976; c[1][8]=-.1480; c[2][8]=-.6593; c[3][8]=0.1862; c[4][8]=-.7489; c[5][8]=-.1648;
  c[0][9]=1.5873; c[1][9]=-.3749; c[2][9]=-.7105; c[3][9]=0.2146; c[4][9]=-.4818; c[5][9]=-.1174;

  // For the upright appendage resistance...
  d[0][0]=0; d[1][0]=0; d[2][0]=0; d[3][0]=0;
  d[0][1]=0; d[1][1]=0; d[2][1]=0; d[3][1]=0;
  d[0][2]=-.00104; d[1][2]=.00172;  d[2][2]=.00117;  d[3][2]=-.00008;
  d[0][3]=-.0055;  d[1][3]=.00597;  d[2][3]=.0039;   d[3][3]=-.00009;
  d[0][4]=-.0111;  d[1][4]=.01421;  d[2][4]=.00069;  d[3][4]=.00021;
  d[0][5]=-.00713; d[1][5]=.02632;  d[2][5]=-.00232; d[3][5]=.00039;
  d[0][6]=-.03581; d[1][6]=.08649;  d[2][6]=-.00999; d[3][6]=.00017;
  d[0][7]=-.0047;  d[1][7]=.11592;  d[2][7]=-.00064; d[3][7]=.00035;
  d[0][8]=.00553;  d[1][8]=.07371;  d[2][8]=0.05991; d[3][8]=-.00114;
  d[0][9]=.04822;  d[1][9]=.0066;   d[2][9]=0.07048; d[3][9]=-.00035;
  d[0][10]=.01021; d[1][10]=.14173; d[2][10]=.06409; d[3][10]=-.00192;

  // For calculation of side-force...
  lh[0]=0; lh[1]=10; lh[2]=20; lh[3]=30;
  l[0][0]=2.025;  l[0][1]=1.989;  l[0][2]=1.98;  l[0][3]=1.762;
  l[1][0]=9.551;  l[1][1]=6.729;  l[1][2]=0.633; l[1][3]=-4.957;
  l[2][0]=0.631;  l[2][1]=0.494;  l[2][2]=0.194; l[2][3]=-0.087;
  l[3][0]=-6.575; l[3][1]=-4.745; l[3][2]=-.792; l[3][3]=2.766;
}

void odHullResistance_DELFT::setup_coeff_keuning95(){
  froude[0]=.10; froude[1]=.15; froude[2]=.20; froude[3]=.25; froude[4]=.30; froude[5]=.35; froude[6]=.40;
  froude[7]=.45; froude[8]=.50; froude[9]=.55; froude[10]=.6;

  a[0][0]=-.00086; a[1][0]=-.08614;   a[2][0]=0.14825;   a[3][0]=-0.03150; a[4][0]=-.01166;  a[5][0]=0.04291;  a[6][0]=-0.01342; a[7][0]=0.09426;  a[8][0]=-.14215;
  a[0][1]=0.00078; a[1][1]=-.47227;   a[2][1]=0.43474;   a[3][1]=-0.01571; a[4][1]=0.00798;  a[5][1]=0.05920;  a[6][1]=-0.00851; a[7][1]=0.45002;  a[8][1]=-.39661;
  a[0][2]=.001840; a[1][2]=-.47484;   a[2][2]=0.39465;   a[3][2]=-0.02258; a[4][2]=0.01015;  a[5][2]=0.08595;  a[6][2]=-0.00521; a[7][2]=0.45274;  a[8][2]=-.35731;
  a[0][3]=0.00353; a[1][3]=-.35483;   a[2][3]=0.23978;   a[3][3]=-0.03606; a[4][3]=0.01942;  a[5][3]=0.10624;  a[6][3]=-0.00179; a[7][3]=0.31667;  a[8][3]=-.19911;
  a[0][4]=0.00511; a[1][4]=-1.07091;  a[2][4]=0.79081;   a[3][4]=-0.04614; a[4][4]=0.02809;  a[5][4]=0.10339;  a[6][4]=0.02247;  a[7][4]=0.97514;  a[8][4]=-.63631;
  a[0][5]=0.00228; a[1][5]=0.46080;   a[2][5]=-.53238;   a[3][5]=-0.11255; a[4][5]=0.01128;  a[5][5]=-.02888;  a[6][5]= 0.07961; a[7][5]=-.53566;  a[8][5]=0.54354;
  a[0][6]=-.00391; a[1][6]=3.33577;   a[2][6]=-2.71081;  a[3][6]=0.03992;  a[4][6]=-.06918;  a[5][6]=-.39580;  a[6][6]=0.24539;  a[7][6]=-3.52217; a[8][6]=2.20652;
  a[0][7]=-.01024; a[1][7]=2.16435;   a[2][7]=-1.18336;  a[3][7]= 0.21775; a[4][7]=-.13107;  a[5][7]=-.34443;  a[6][7]= 0.32340; a[7][7]=-2.42987; a[8][7]=0.63926;
  a[0][8]=-.02094; a[1][8]=7.77489;   a[2][8]=-7.0669;   a[3][8]=0.43727;  a[4][8]=0.11872;  a[5][8]=-.14469;  a[6][8]= 0.62896; a[7][8]=-7.90514; a[8][8]=5.81590;
  a[0][9]= .04623; a[1][9]=2.38461;   a[2][9]=-6.67163;  a[3][9]= 0.63617; a[4][9]=1.06325;  a[5][9]=2.09008;  a[6][9]= 0.96843; a[7][9]=-3.08749; a[8][9]=5.94214;
  a[0][10]=.07319; a[1][10]=-2.86817; a[2][10]=-3.16633; a[3][10]=0.70241; a[4][10]=1.49509; a[5][10]=3.00561; a[6][10]=0.88750; a[7][10]=2.25063; a[8][10]=2.88970;
} 