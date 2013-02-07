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
  template <class T> class base_array;

  base_array<float> sum_lifted(const nested_array<float>& na);
  parray<int>& series(int length);

  template <class T>
  class base_array : public parray<T> {
  private:
    ref_counter* m_ref_counter;
    device_vector<T>* m_data;
    int id;

    int next_id() { 
      static int id_count;
      return id_count++;
    }

  public:
    base_array() { }

    base_array(int size) : id(next_id()) { 
      m_data = new device_vector<T>(size); 
      m_ref_counter = new ref_counter(); 
      m_ref_counter->grab();
    }

    base_array(int size, T t) : id(next_id()) { 
      m_data = new device_vector<T>(size, t); 
      m_ref_counter = new ref_counter(); 
      m_ref_counter->grab();
    }

    base_array(const host_vector<T>& h_vec) : id(next_id()) { 
      m_data = new device_vector<T>(h_vec); 
      m_ref_counter = new ref_counter(); 
      m_ref_counter->grab(); 
    }

    base_array(base_array<T>* ptr) { 
      if (ptr != NULL) { 
        m_data = ptr->m_data;
        m_ref_counter = ptr->m_ref_counter;
        id = ptr->id;
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
      id = ptr.id;
      m_ref_counter->grab();
      return *this; 
    }

    base_array(const base_array<T>& ba) : m_ref_counter(ba.m_ref_counter), m_data(ba.m_data), id(ba.id) { 
      m_ref_counter->grab();
    }
    
    device_vector<T>& data() const { return *m_data; }

    virtual int length() const { return m_data->size(); }
    virtual T& get(int idx) const { 
      // TODO: This is a nasty hack to break "cannot convert from 'thrust::device_reference<T>' to 'int &'" compilation error
      static T v;
      v = (*m_data)[idx];
      return v;
    }

    T sum(const monoid& m) const;

    parray<T>& back_permute(const parray<int>& idxs) const;

    virtual void print() const;

    pair<base_array<T>, base_array<T> > flag_split(const base_array<bool>& flags);

    template <class B>
    base_array<T> expand_by(const nested_array<B>& nested_arr) const;

    base_array<T> write_pa(const pair_array<int, T>& vals);

    template <class T>
    struct tuple_eq_functor {
      __host__ __device__
      int operator()(thrust::tuple<T, T> t) {
          T x, y;
          thrust::tie(x, y) = t;
          return x == y;
      }
    };

    virtual bool equals(const parray<T>& that) const {
      if (length() != that.length())
        return false;
      const base_array<T>& that_ba = dynamic_cast<const base_array<T>&>(that);
      return thrust::transform_reduce(
              make_zip_iterator(thrust::make_tuple(m_data->begin(), that_ba.m_data->begin())),
              make_zip_iterator(thrust::make_tuple(m_data->end(), that_ba.m_data->end())),
              tuple_eq_functor<T>(),
              true,
              thrust::equal_to<T>());
    }

    bool contains(T v) const {
      return thrust::find(m_data->begin(), m_data->end(), v) != m_data->end();
    }

    parray<T>& scan() const {
      base_array<T> res(length());
      thrust::exclusive_scan(m_data->begin(), m_data->end(), res.m_data->begin());
      return res;
    }

    const T& sum() const {
      return thrust::reduce(m_data->begin(), m_data->end());
    }

    friend base_array<float> scalan_thrust::sum_lifted(const nested_array<float>& na);
    friend parray<int>& series(int length);
  };  
}

namespace scalan_thrust {
  template <class T>
  T base_array<T>::sum(const monoid& m) const  {
    // TODO: Reconsider it to: res = thrust::reduce(..., m.zero(), (m.binary_operation() == plus | minus | ...);
    T res;
    switch (m.op()) {
    case monoid::OP_PLUS:
      res = thrust::reduce(m_data->begin(), m_data->end(), m.zero(), thrust::plus<T>());
      break;
    case monoid::OP_MINUS:
      res = thrust::reduce(m_data->begin(), m_data->end(), m.zero(), thrust::minus<T>());
      break;
    case monoid::OP_MUL:
      res = thrust::reduce(m_data->begin(), m_data->end(), m.zero(), thrust::multiplies<T>());
      break;
    default:
      // TODO: Handle an error
      break;
    }
    return res;
  }

  template <class T>
  parray<T>& base_array<T>::back_permute(const parray<int>& idxs) const {
    const base_array<int>& idxs_ba = dynamic_cast<const base_array<int>&>(idxs);
    base_array<T>* res = new base_array<T>(idxs.length()); // TODO: Fix this memory leak
    thrust::copy(
      thrust::make_permutation_iterator(m_data->begin(), idxs_ba.data().begin()),
      thrust::make_permutation_iterator(m_data->end(), idxs_ba.data().end()),
      res->m_data->begin());
    return *res;
  }

  template <class T>
  void base_array<T>::print() const {
    std::cout << "[base_array: ";
    for (int i = 0; i < m_data->size(); i++) {
      std::cout << get(i) << " ";
    }
    std::cout << "] " << std::endl;
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
    base_array<T> nested_arr_segments_ba = dynamic_cast<base_array<T>&>(nested_arr.segments());
    expand(nested_arr_segments_ba.m_data->begin(), nested_arr_segments_ba.m_data->end(), expanded_indxs.begin());
    thrust::gather(expanded_indxs.begin(), expanded_indxs.end(), this->m_data->begin(), expanded_values.begin());
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
    //assert(*thrust::max_element(vals.first().m_data->begin(), vals.first().m_data->end()) < this->length()); // TODO: Fix this assert

    const base_array<T>& vals_snd_ba = dynamic_cast<const base_array<T>&>(vals.second());
    device_vector<T> d_vals(vals_snd_ba.data());

    const base_array<int>& vals_fst_ba = dynamic_cast<const base_array<int>&>(vals.first());
    device_vector<int> d_idxs(vals_fst_ba.data());

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