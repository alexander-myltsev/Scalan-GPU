#ifndef MONOID_H
#define MONOID_H

namespace scalan_thrust {
  /*
  // NOTE: This doesn't work. nvcc doesn't compile virtual functions.
  template <class Arg1, class Arg2, class Result>
  struct binary_function_callable : public thrust::binary_function<Arg1, Arg2, Result> {
  __host__ __device__ virtual Result operator() (Arg1 a, Arg2 b) const;
  };

  template<class T>
  struct plus : public binary_function_callable<T, T, T> {
  __host__ __device__ T operator() (const T& a, const T& b) const { return a + b; }
  };
  */

  class monoid {
  public:
    enum operation_t { OP_PLUS, OP_MINUS, OP_MUL };
  private:
    float m_zero;
    operation_t m_opname;
  public:
    monoid(float zero, operation_t opname) : m_zero(zero), m_opname(opname) { }

    float const& zero() const { return m_zero; }

    /*
    // TODO: thrust::plus() and thrust::minus() has no common ancetor with operator(). So, it is not possible to pass it to thrust::reduce() as last argument. How to implement it?
    thrust::binary_function<float, float, float>* op() {
    thrust::binary_function<float, float, float>* op_res = NULL;
    return op_res;
    }
    */

    operation_t const& op() const { return m_opname; }
  };
}

#endif MONOID_H