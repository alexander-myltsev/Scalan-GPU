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
  class pair_array : public parray<pair<T1, T2> > {
  private:
    base_array<T1> m_a;
    base_array<T2> m_b;
  public:
    pair_array(const base_array<T1>& a, const base_array<T2>& b) : m_a(a), m_b(b) { 
      assert(m_a.length() == m_b.length());
    }

    base_array<T1> const& first() const { return m_a; }
    base_array<T2> const& second() const { return m_b; }

    virtual int length() const { return m_a.length(); }
    virtual device_vector<pair<T1, T2>> const& data() const { return device_vector<pair<T1, T2>>(); } // TODO: Here should not be data call like this. Fix it.
  };
}

#endif