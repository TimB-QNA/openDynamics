#include "odConstants.h"
#include <math.h>

odConstants::odConstants(){
  pi = 4.*atan(1.);
  g  = 9.81;
  rhoWater=1025.;
  rhoAir=1.2;
  modelX = odPoint(1.,0.,0.);
  modelY = odPoint(0.,1.,0.);
  modelZ = odPoint(0.,0.,1.);
}
