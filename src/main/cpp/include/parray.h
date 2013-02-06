#ifndef PARRAY_H
#define PARRAY_H

#include <thrust/device_vector.h>

namespace scalan_thrust {
  template <class T>
  class parray {
  public:
    virtual int length() const { return -1; } // TODO: Make it pure function
    //virtual thrust::device_vector<T> const& data() const = 0;
    virtual parray<T>& back_permute(const parray<int>& idxs) const = 0;
    virtual void print() const { printf("Not implemented\n"); }
    virtual bool equals(const parray<T>& that) const { return false; } // TODO: Make it pure function

    
    // TODO: Add monoid and zero element 
    // TODO: remove declarations at lines 7-8
    virtual parray<T>& scan() const = 0;
    virtual const T& sum() const = 0;
  };
}

#endif PARRAY_H