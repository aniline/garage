#ifndef _LMAP_H
#define _LMAP_H 1

#include <utility>
#include <vector>
#include <functional>

// popf is population growth function popf(prev, r) gives the next
// population value given previous as prev and r as the rate.
extern void popfniter(std::function<double(double, double)> const& popf,
                      int niter,
                      double r,
                      double initial,
                      int scale_denominator,
                      std::vector<std::pair<double,int>> &outv,
                      unsigned int clip);

#endif
