#include "odConstants.h"
#include "odSimCondition.h"
#include "odObject.h"
#include <vector>
using namespace std;

#ifndef ODSOLVER_H
  #define ODSOLVER_H

class odSolver
{
  public:
    double         timeStep;
    odConstants    simConstants;
    odSimCondition simState;
    odSimCondition model;
    vector<odObject*> component;
    
    void initialise();
    void advance();
};

#endif

