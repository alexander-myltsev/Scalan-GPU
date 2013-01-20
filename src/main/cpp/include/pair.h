#ifndef PAIR_H
#define PAIR_H

namespace scalan_thrust {

  template<class T1, class T2>
  class pair {
  private:
    T1 const * m_v1;
    T2 const * m_v2;
  public:
    pair(const T1& v1, const T2& v2) : m_v1(&v1), m_v2(&v2) { }
    pair() : m_v1(NULL), m_v2(NULL) { }

    const T1& fst() const { return *m_v1; }
    const T2& snd() const { return *m_v2; }

    ~pair() {
    }
  };

}

#endif PAIR_H