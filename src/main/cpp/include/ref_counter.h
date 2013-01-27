#ifndef REFCOUNTER_H
#define REFCOUNTER_H

#include <assert.h>

namespace scalan_thrust {
  class ref_counter {
  public:
    ref_counter() : m_ref_count(0) {}
    //ref_counter(int ref_count) : m_ref_count(ref_count) {}

    void grab() const { ++m_ref_count; }

    // returns 'true' if object has references
    bool release() const {
      assert(m_ref_count > 0);
      --m_ref_count;
      return m_ref_count > 0;      
    }

  private:
    mutable int m_ref_count;
  };
}

#endif