#include <ode/ode.h>
#include <QtXml>
#include "odPoint.h"
#ifndef ODHULLRESISTANCE_H
  #define ODHULLRESISTANCE_H

class odHullResistance
{
  public:
    int model;           /// \brief Resistance method to use.Data must be provided in the XML config file. Options are as follows:
                         /// -# is Delft series (Gerritsma 95).
                         /// -# is Delft series (Keuning 95).
    odHullResistance();
    virtual void readFromXML(QDomNode root);
    void readCommonDataFromXML(QDomNode root);
    void setAttitude(odPoint A);
    void setVelocity(odPoint V);
    void setConstants(double gravity, double density);
    virtual odPoint calculateForces();
    void setParametersFromBody(dBodyID *body);
    double ittc57_coeff(double L);
    
  protected:
    double g, rho, pi, Vf, Fn, Lwl;
    odPoint velocity;
    odPoint attitude;
    double ittc57(double L, double A);
};

#endif
