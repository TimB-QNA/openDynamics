#ifndef POINT_H
  #define POINT_H

class point
{
  public:
    point();
    point(double lx, double ly, double lz);
    point(double lx, double ly, double lz, double vx, double vy, double vz);

    point operator*(double b);
    point operator/(double b);
    point operator+(point b);
    point operator-(point b);
    void operator+=(point b);
    void operator/=(double b);
    bool operator==(point b);

    void set(double lx, double ly, double lz);

    double dotproduct(point b);
    double dotproduct_natural(point b);
    point crossproduct(point b);
    double mag();
    double mag(point b);

    double x, y, z;
    double dx, dy, dz;

  private:
    double PI;
};
#endif
