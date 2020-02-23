#include <iostream>
#include <vector>
#include <algorithm>
#include <unordered_map>
#include <cmath>
#include <numeric>

#include "lmap.h"

const double err = 0.0001;
const double errinv = 1/err;

using namespace std;

double roundv(double v) {
  return round(v*errinv)/errinv;
}

bool cmp(pair<double,int> a, pair<double,int> b) {
  return (a.second > b.second);
}

void popfniter(function<double(double, double)> const& popf,
               int niter,
               double r,
               double initial,
               int scale_denominator,
               vector<pair<double,int>> &outv,
               unsigned int clip = 0) {
  int i=niter;
  double v = initial;
  unordered_map<double, int> m;
  
  while (i>0) {
    v = popf(v, r);
    double rv = roundv(v);
    m[rv] ++;
    i--;
  }

  vector <pair<double,int>> mv(m.begin(), m.end());
  outv.clear();
  outv.resize(m.size());
  transform(m.begin(), m.end(),
            outv.begin(),
            [scale_denominator, niter](pair<double, int> v) {
              return make_pair(v.first, (v.second*scale_denominator)/niter);
            });
  sort(outv.begin(), outv.end(), cmp);

  if ((clip != 0) && (outv.size() > clip)) {
    outv.erase(outv.begin()+clip, outv.end());
  }
  // for (auto i : outv) 
  //   cout << i.first << "("<<i.second<<") ";
  // cout << ::endl;
}

// int main () {
//   vector<pair<double, int>> yvals;
//   popfniter(200, 2, 0.5, 256, yvals);
//   // popfniter(200, 2.5, 0.5, 256, yvals);
//   // popfniter(400, 3.7, 0.5, 256, yvals);
//   popfniter(400, 4.5, 0.5, 256, yvals);
//   return 0;
// }
