#ifndef PAIRARRAY_H
#define PAIRARRAY_H

#include <thrust/device_vector.h>

namespace scalan_thrust {
  // TODO: 
  // Someday make it like:
  // template <class PArr1, class PArr2>
  // class pair_array : public parray<pair<PArr1::elem_type, PArr2::elem_type> > {
  // private:
  // PArr1 m_a;
  template <class T1, class T2>
  class pair_array : public parray<pair<T1, T2>> {
  private:
    parray<T1>& m_a;
    parray<T2>& m_b;

    pair_array() : m_a(base_array<T1>()), m_b(base_array<T2>()) { }

  public:
    pair_array(parray<T1>& a, parray<T2>& b) : m_a(a), m_b(b) {
      assert(m_a.length() == m_b.length());
    }

    parray<T1> const& first() const { return m_a; }
    parray<T2> const& second() const { return m_b; }

    virtual parray<pair<T1, T2>>& back_permute(const parray<int>& idxs) const {
      return pair_array();
    }

    virtual int length() const { return m_a.length(); }

    virtual parray<pair<T1, T2>>& scan() const {
      return pair_array<T1, T2>();
    }

    virtual const pair<T1, T2>& sum() const {
      return pair<T1, T2>();
    }
  };
}

#endif