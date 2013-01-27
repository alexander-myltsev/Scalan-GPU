#ifndef BASE_ARRAY_H
#define BASE_ARRAY_H

#include <thrust/host_vector.h>
#include <thrust/device_vector.h>
#include <thrust/count.h>
#include <thrust/reduce.h>
#include <thrust/copy.h>
#include <thrust/functional.h>
#include <thrust/sort.h>

#include <cstdlib>
#include <algorithm>
#include <assert.h>

#include "monoid.h"
#include "pair.h"
#include "ref_counter.h"
#include "parray.h"

using thrust::device_vector;
using thrust::host_vector;
using thrust::reduce;

namespace scalan_thrust {
  template <class T> class nested_array;
  template <class T1, class T2> class pair_array;

  template <class T>
  class base_array : public parray<T>, public ref_counter {
  private:
    device_vector<T>* m_data;
    ref_counter* m_ref_counter;

  public:
    base_array() { }

    base_array(int size) { 
      m_data = new device_vector<T>(size); 
      m_ref_counter = new m_ref_counter(0); 
      m_ref_counter->grab();
    }

    base_array(int size, T t) { 
      m_data = new device_vector<T>(size, t); 
      m_ref_counter = new m_ref_counter(0); 
      m_ref_counter->grab();
    }

    base_array(const host_vector<T>& h_vec) { 
      m_data = new device_vector<T>(h_vec); 
      m_ref_counter = new m_ref_counter(0); 
      m_ref_counter->grab(); 
    }

    //base_array(device_vector<T>* d_vec) : m_data(d_vec) { 
    //  this->grab(); 
    //}

    base_array(base_array<T>* ptr) { 
      if (ptr != NULL) { 
        m_data = ptr->m_data;
        m_ref_counter = ptr->m_ref_counter;
        m_ref_counter->grab();
      }
    }

    ~base_array() { 
      if (!m_ref_counter->release()) {
        delete m_data;
        delete m_ref_counter;
      }
    }

    base_array<T> &operator=(const base_array<T>& ptr) {
      m_data = ptr.m_data;
      m_ref_counter = ptr.m_ref_counter;
      return *this; 
    }

    base_array(const base_array<T>& ba) : m_ref_counter(ba.m_ref_counter), m_data(ba.m_data) { 
      m_ref_counter.grab();
    }
    
    virtual device_vector<T> const& data() const { return *m_data; }
    virtual int length() const { return m_data->size(); }
    T get(int i) { return this[i]; }

    T sum(const monoid& m) const;

    base_array<T> back_permute(const base_array<int>& idxs) const;

    virtual void print() const;

    pair<base_array<T>, base_array<T> > flag_split(const base_array<bool>& flags);

    template <class B>
    base_array<T> expand_by(const nested_array<B>& nested_arr) const;

    base_array<T> write_pa(const pair_array<int, T>& vals);
  };
}

namespace scalan_thrust {
  template <class T>
  T base_array<T>::sum(const monoid& m) const  {
    // TODO: Reconsider it to: res = thrust::reduce(..., m.zero(), (m.binary_operation() == plus | minus | ...);
    T res;
    switch (m.op()) {
    case monoid::OP_PLUS:
      res = thrust::reduce(data().begin(), data().end(), m.zero(), thrust::plus<T>());
      break;
    case monoid::OP_MINUS:
      res = thrust::reduce(data().begin(), data().end(), m.zero(), thrust::minus<T>());
      break;
    case monoid::OP_MUL:
      res = thrust::reduce(data().begin(), data().end(), m.zero(), thrust::multiplies<T>());
      break;
    default:
      // TODO: Handle an error
      break;
    }
    return res;
  }

  template <class T>
  base_array<T> base_array<T>::back_permute(const base_array<int>& idxs) const { // NOTE: Can idxs be not base_array but PA?
    base_array<T> res(idxs.data().size());
    thrust::copy(
      thrust::make_permutation_iterator(data().begin(), idxs.data().begin()),
      thrust::make_permutation_iterator(data().end(), idxs.data().end()),
      res.m_data->begin()); // TODO: why can't write res.m_data->begin() ?
    return res;
  }

  template <class T>
  void base_array<T>::print() const {
    std::cout << "base_array: ";
    for (int i = 0; i < data().size(); i++) {
      std::cout << data()[i] << " ";
    }
    std::cout << std::endl;
  }

  template <class T>
  pair<base_array<T>, base_array<T> > base_array<T>::flag_split(const base_array<bool>& flags) {
    assert(this->length() == flags.length());

    device_vector<bool> flgs_dt = flags.data();
    device_vector<T> splitted_vals(data());
    thrust::sort_by_key(flgs_dt.begin(), flgs_dt.end(), splitted_vals.begin());
    int false_flags_count = thrust::count(flgs_dt.begin(), flgs_dt.end(), false);
    device_vector<T> vals_false(splitted_vals.begin(), splitted_vals.begin() + false_flags_count);
    device_vector<T> vals_true(splitted_vals.begin() + false_flags_count, splitted_vals.end());
    return pair<base_array<T>, base_array<T> >(*(new base_array<T>(vals_true)), *(new base_array<T>(vals_false))); // TODO: Is here a memory leak?
  }

  template <class T>
  template <class B>
  base_array<T> base_array<T>::expand_by(const nested_array<B>& nested_arr) const {
    device_vector<int> expanded_indxs(nested_arr.values().length());
    device_vector<T> expanded_values(nested_arr.values().length());
    expand(nested_arr.segments().data().begin(), nested_arr.segments().data().end(), expanded_indxs.begin());
    thrust::gather(expanded_indxs.begin(), expanded_indxs.end(), this->data().begin(), expanded_values.begin());
    return base_array<T>(expanded_values);
  }

  template <class T>
  struct write_pa_functor {
    __host__ __device__ 
      T operator()(thrust::tuple<T, T> t) {
        T x, y;
        thrust::tie(x, y) = t;
        return y == 0 ? x : y;
    }
  };

  template <class T>
  base_array<T> base_array<T>::write_pa(const pair_array<int, T>& vals) {
    assert(*thrust::max_element(vals.first().data().begin(), vals.first().data().end()) < this->length());

    device_vector<T> d_vals(vals.second().data());
    device_vector<int> d_idxs(vals.first().data());

    thrust::sort_by_key(d_idxs.begin(), d_idxs.end(), d_vals.begin());

    //std::cout << ">>>: "; thrust::copy(d_vals.begin(), d_vals.end(), std::ostream_iterator<T>(std::cout, " ")); std::cout << std::endl;
    //std::cout << ">>>: "; thrust::copy(d_idxs.begin(), d_idxs.end(), std::ostream_iterator<int>(std::cout, " ")); std::cout << std::endl;

    device_vector<T> d_vals_inplaces(this->length());
    thrust::scatter
      (d_vals.begin(), d_vals.end(),
      d_idxs.begin(),
      d_vals_inplaces.begin());
    //std::cout << ">>>: "; thrust::copy(d_vals_inplaces.begin(), d_vals_inplaces.end(), std::ostream_iterator<T>(std::cout, " ")); std::cout << std::endl;

    device_vector<T> res(this->data());
    thrust::transform
      (thrust::make_zip_iterator(thrust::make_tuple(res.begin(), d_vals_inplaces.begin())),
      thrust::make_zip_iterator(thrust::make_tuple(res.end(), d_vals_inplaces.end())),
      res.begin(),
      write_pa_functor<T>());

    base_array<T> a(res);
    //a.print();
    return a;
  }
}

#endif BASE_ARRAY_H