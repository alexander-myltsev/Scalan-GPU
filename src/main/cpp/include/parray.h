#ifndef PARRAY_H
#define PARRAY_H

#include <thrust/device_vector.h>

namespace scalan_thrust {
  template <class T>
  class parray {
  public:
    virtual int length() const { return -1; } // TODO: Make it pure function
    virtual thrust::device_vector<T> const& data() const = 0;
    virtual void print() const { printf("Not implemented\n"); }
  };
}

#endif PARRAY_H