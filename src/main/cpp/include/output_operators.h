#ifndef OUTPUT_OPERATORS_H
#define OUTPUT_OPERATORS_H

#include <iostream>
#include <thrust/device_vector.h>

namespace scalan_thrust {
  template <class T>
  std::ostream& operator << (std::ostream& out, const thrust::device_vector<T>& d_v) {
    thrust::copy(d_v.begin(), d_v.end(), std::ostream_iterator<T>(out, " "));
    return out;
  }

  template <class T> class parray;

  template <class T>
  std::ostream& operator << (std::ostream& out, const parray<T>& parr) {
    parr.print();
    return out;
  }
}

#endif OUTPUT_OPERATORS_H