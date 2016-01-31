/*
    Description: Provide matrix manipulation for openDynamics
         Author: Tim Brocklehurst
        License: GPLv3
*/
#ifndef ODMATRIX_H
  #define ODMATRIX_H
  #include "odPoint.h"
  #include <vector>
  using namespace std;
// Note:
/*! \author Tim Brocklehurst
 * This is a matrix class which I wrote some time ago. It has found many uses
 * and has been reasonably well tested in those uses. This class does not provide
 * all the possible functionality, and some functionality it provides could be improved upon.
 * Indeed, it is quite likely that the whole class could be improved.
 * In this class, the following convention is used:
 * - "i" is thought of as the row number (ie. it runs top to bottom)
 * - "j" is thought of as the column number (ie. it runs left to right)
 */
class odMatrix
{
  public:
    odMatrix(int i, int j);
    odMatrix(odPoint p);
    int imax, jmax;
    double getvalue(int i, int j);
    void setvalue(int i, int j, double d);

    odMatrix operator+(odMatrix b);
    odMatrix operator*(double b);
    odMatrix operator*(odMatrix b);
    odPoint toOdPoint();
    
    void zero();
    void identity();
    void unity();
    void resize(int i, int j);
    void display();
    void mult(odMatrix a,odMatrix b);
    void multscalar(double s);
    void add(odMatrix b);
    void subtract(odMatrix b);
    void transpose();
    odMatrix Gaussian(odMatrix b);
    bool symmetry();
    
  private:
    int ij2n(int i, int j);
    int *p;
    vector<double> a;
    odMatrix save();
    void load(odMatrix z);
};

#endif
