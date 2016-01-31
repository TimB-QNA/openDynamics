#include "odFoilPanel.h"
#include "odMatrix.h"
/*!
 * This routine calculates the surface area of the panel.
 * We use Heron's formula to calculate the panel's area based on it's side lengths.
 */
void odFoilPanel::calculateArea(){
  double st;
  odPoint d1, d2, d3;
  
  d1=s[0].LE-s[1].LE;
  d2=s[0].TE-s[0].LE;
  d3=s[0].TE-s[1].LE;
  
  st=.5*(d1.mag()+d2.mag()+d3.mag());
  area=sqrt(st*(st-d1.mag())*(st-d2.mag())*(st-d3.mag()));
  
  d1=s[1].LE-s[0].TE;
  d2=s[1].TE-s[1].LE;
  d3=s[1].TE-s[0].TE;
  st=.5*(d1.mag()+d2.mag()+d3.mag());
  area+=sqrt(st*(st-d1.mag())*(st-d2.mag())*(st-d3.mag()));
}

void odFoilPanel::calculateNormal(){
  double st;
  odPoint v1, v2, v3, v4, N1, N2;
  
  v1=s[0].LE-s[1].LE;
  v2=s[0].TE-s[0].LE;
  
  v3=s[0].LE-s[1].TE;
  v4=s[1].TE-s[1].LE;
  
  N1=v1.crossProduct(v2);
  N2=v3.crossProduct(v4);
  
  // Normalise vectors
  N1/=N1.mag();
  N2/=N2.mag();
  
  // Average normals for panel normals
  normal=(N1+N2)/2.;
}

void odFoilPanel::calculateAft(){
  double st;
  odPoint v1, v2;
  
  v1=s[0].TE-s[0].LE;
  v2=s[1].TE-s[1].LE;
  
  // Normalise vectors
  v1/=v1.mag();
  v2/=v2.mag();
  
  // Average aft vectors for nominal...
  aft=(v1+v2)/2.;
}

/*! \author Tim Brocklehurst
 * Rough and ready approximation of where the forces act on a panel.
 * The approximation assumes the point is at the quarter chord of the
 * panel, and at the mid span.
 */
void odFoilPanel::calculateEffectiveCentre(){
  double st;
  odPoint q1, q2;
  
  q1=s[0].LE + (s[0].TE-s[0].LE)*.25;
  q2=s[1].LE + (s[1].TE-s[1].LE)*.25;
  
  effectiveCentre = (q1+q2)/2.;
}

double odFoilPanel::chord(){
  odPoint c1, c2;
  
  c1=s[0].TE-s[0].LE;
  c2=s[1].TE-s[1].LE;
  
  return (c1.mag()+c2.mag())/2.;
}

/*
 http://steve.hollasch.net/cgindex/math/rotvec.html
gmt@reef.cis.ufl.edu (Gary McTaggart) writes:

    In a video game context, I have run into the problem of non-commutitivity of the 3d transformations used for rotation of an object. I want to be able to rotate an object with respect to itself and not the world. I got some decent results by using one vector to represent the direction that the "ship" is pointing with respect to the world and a scalar for rotation about that vector as an axis. When I want to rotate the object with respect to itself, I do the following: 

phillips@swanee.ee.uwa.oz.au (PHILLIPS C J) replies:

    You were off to a good start by using a vector for the ship 'forward' direction. I would strongly sugest that you store another vector for the ship's 'up' direction, and do all your rotations by vector arithmetic, and stay far far away from inverse trig functions. A 'sideways' vector for the ship may easily be found from the other two by a cross product. This gives you a 3d coordinate system aligned with the ship. Call these three vectors p,q,r. To perform a rotation in ship coordinates, (eg around the p axis) simply perform

            p' := p*cos(x)-q*sin(x)
            q' := p*sin(x)+q*cos(x)

    Similar eqns used for other rotations.

    As a general rule, I find vector arithmetic a hell of a lot saner to use than trig; It avoids most of the horrible singularities and multi-valued inverse problems that you get with atn's etc. 

I too have been working on a similar problem and after many hours I was finally guided to CG&A (FEB 1984 pg 31). In short, it talks about the rotation of a vector about another. To save you the trouble:

    let
        [v] = [vx, vy, vz]      the vector to be rotated.
        [l] = [lx, ly, lz]      the vector about rotation
              | 1  0  0|
        [i] = | 0  1  0|           the identity matrix        
              | 0  0  1|
              
              |   0  lz -ly |
        [L] = | -lz   0  lx |
              |  ly -lx   0 |

        d = sqrt(lx*lx + ly*ly + lz*lz)
        a                       the angle of rotation

    then

   matrix operations gives:

    [v] = [v]x{[i] + sin(a)/d*[L] + ((1 - cos(a))/(d*d)*([L]x[L]))} 

Therefore if you wish to rotated you ship left, you would rotated the forward dirction vector about the up vector. If you wish to pull up, you would rotate the direction vector and the up vector about their cross product (the normal). 
*/
void odFoilPanel::rotate(odPoint stockOrigin, odPoint stockDelta, double stockAngle){
  int i;
  double pi=4.*atan(1.), x0, y0, z0;
  double theta=stockAngle*pi/180;
  double d;
  odMatrix I(3,3), L(3,3);
  odPoint v, l, w;
//  listPanelData();
  
  for (i=0;i<2;i++){
    v=s[i].LE-stockOrigin;
    l.x=stockDelta.x;  l.y=stockDelta.y;  l.z=stockDelta.z;
    I.identity();
  
    L.setvalue(0,0, 0.0);  L.setvalue(0,1, l.z);  L.setvalue(0,2,-l.y);
    L.setvalue(1,0,-l.z);  L.setvalue(1,1, 0.0);  L.setvalue(1,2, l.x);
    L.setvalue(2,0, l.y);  L.setvalue(2,1,-l.x);  L.setvalue(2,2, 0.0);
    
    d=l.mag();
    w=((I+L*(sin(theta)/d) + (L*L)*((1-cos(theta))/pow(d,2)))*v).toOdPoint();
    s[i].LE=w+stockOrigin;
    
    v=s[i].TE-stockOrigin;
    l.x=stockDelta.x;  l.y=stockDelta.y;  l.z=stockDelta.z;
    I.identity();
  
    L.setvalue(0,0, 0.0);  L.setvalue(0,1, l.z);  L.setvalue(0,2,-l.y);
    L.setvalue(1,0,-l.z);  L.setvalue(1,1, 0.0);  L.setvalue(1,2, l.x);
    L.setvalue(2,0, l.y);  L.setvalue(2,1,-l.x);  L.setvalue(2,2, 0.0);
    
    d=l.mag();
    w=((I+L*(sin(theta)/d) + (L*L)*((1-cos(theta))/pow(d,2)))*v).toOdPoint();
    s[i].TE=w+stockOrigin;
  }  
 
//  calculateNormal();
//  calculateAft();
//  calculateEffectiveCentre();
//  listPanelData();
}

void odFoilPanel::listPanelData(){
  printf("odFoilPanel\n");
  printf("  Section 1\n");
  printf("    LE (xyz) -> %lf\t%lf\t%lf\n", s[0].LE.x, s[0].LE.y, s[0].LE.z);
  printf("    TE (xyz) -> %lf\t%lf\t%lf\n", s[0].TE.x, s[0].TE.y, s[0].TE.z);
  printf("  Section 2\n");
  printf("    LE (xyz) -> %lf\t%lf\t%lf\n", s[1].LE.x, s[1].LE.y, s[1].LE.z);
  printf("    TE (xyz) -> %lf\t%lf\t%lf\n", s[1].TE.x, s[1].TE.y, s[1].TE.z);
  printf("  Panel Area       = %lf\n", area);
  printf("  Panel Chord      = %lf\n", chord());
  printf("  Aft Vector       = %lf\t%lf\t%lf\n", aft.x, aft.y, aft.z);
  printf("  Normal Vector    = %lf\t%lf\t%lf\n", normal.x, normal.y, normal.z);
  printf("  Effective Centre = %lf\t%lf\t%lf\n", effectiveCentre.x, effectiveCentre.y, effectiveCentre.z);
}