#include <thrust/host_vector.h>
#include <thrust/device_vector.h>
#include <thrust/generate.h>
#include <thrust/sort.h>
#include <thrust/copy.h>
#include <cstdlib>
#include <algorithm>

using thrust::host_vector;
using thrust::device_vector;
using thrust::tuple;

using std::cout;
using std::endl;
using std::string;

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

  template <class T>
  class unary_operation {
  public:
    virtual T operator()(T v) = 0;
  };

  class monoid {
  public:
    enum operation_t { OP_PLUS, OP_MINUS, OP_MUL };
  private:
    float m_zero;
    operation_t m_opname; // TODO: Should be enum
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

  template<class T1, class T2>
  class pair_element {
  private:
    T1 m_v1;
    T2 m_v2;
  public:
    pair_element(T1 v1, T2 v2) : m_v1(v1), m_v2(v2)  {  }

    T1 const& fst() const { return m_v1; }
    T2 const& snd() const { return m_v2; }
  };

  template <class T>
  class parray {
    //int 
  };

  template <class T>
  class nested_array {
  private:
    parray<T> m_values;
    host_vector<pair_element<int, int> > m_segments;
  public:
    host_vector<pair_element<int, int> > const& segments() const { return m_segments; }
    //base_array<T> map(const unary_operation<T>& op) {
    parray<T> map(const unary_operation<T>& op) {
      return m_values;
    }
  };

  template <class T>
  class base_array {
  private:
    device_vector<T> m_data;
  public:
    base_array() : m_data() { }
    base_array(const host_vector<T>& h_vec) : m_data(h_vec) { }

    T sum(const monoid& m) const {
      T res;
      switch (m.op()) {
      case monoid::OP_PLUS:
        res = thrust::reduce(m_data.begin(), m_data.end(), m.zero(), thrust::plus<T>());
        break;
      case monoid::OP_MINUS:
        res = thrust::reduce(m_data.begin(), m_data.end(), m.zero(), thrust::minus<T>());
        break;
      case monoid::OP_MUL:
        res = thrust::reduce(m_data.begin(), m_data.end(), m.zero(), thrust::multiplies<T>());
        break;
      default:
        // TODO: Handle an error
        break;
      }
      return res;
    }
  };
}

int simpleSum(const host_vector<int>& x5) {
  scalan_thrust::base_array<int> x6(x5);
  int x7 = x6.sum(scalan_thrust::monoid(0.0f, scalan_thrust::monoid::OP_PLUS));
  return x7;
}

using scalan_thrust::nested_array;
using scalan_thrust::base_array;
using scalan_thrust::pair_element;
using scalan_thrust::unary_operation;

class map_fun : public unary_operation<base_array<float> > {
private:
  nested_array<pair_element<int, float> > x;
  nested_array<float> y;
public:
  map_fun(const nested_array<pair_element<int, float> >& xx, const nested_array<float>& yy) : x(xx), y(yy) { }
  
  base_array<float> operator() (base_array<float> m_row) {
    /*
    base_array<pair_element<int, float> > narr_vals = x.values();
    base_array<int> narr_vals_fst = first_pa(narr_vals);
    base_array<int> repl = element_int<1>.replicate(narr_vals_fst.length());
    base_array<int> exp_binop = binop_array(OP_PLUS, repl, narr_vals_fst);
    base_array<int> back_perm = back_permute(m_row, exp_binop);
    base_array<float> narr_vals_snd = second_pa(narr_vals);
    base_array<float> parr = binop_array(OP_PLUS, back_perm, narr_vals_snd);
    base_array<pair_element<int, int> > segments = x.segments();
     nested_array<float> narr(parr, segments);
    base_array<float> sum_l = sum_lifted(narr);
    return sum_l;
    */
    return m_row;
  }
};

base_array<float> map_pa(const nested_array<float>& y, const map_fun& mf) {
  host_vector<pair_element<int, int> > segs = y.segments();
  y.map(mf);
  base_array<float> f;
  return f;
}

base_array<float> svmv(nested_array<pair_element<int, float> > x, nested_array<float> y) {
  base_array<float> r = map_pa(y, map_fun(x, y));
  return r;
}

int main() {
  host_vector<int> h_vec(10, 5);
  //thrust::generate(h_vec.begin(), h_vec.end(), rand);

  int r = simpleSum(h_vec);
  cout << r << endl;
}

/*
int main(void)
{
    // generate 32M random numbers on the host
    thrust::host_vector<int> h_vec(32 << 20);
    thrust::generate(h_vec.begin(), h_vec.end(), rand);

    // transfer data to the device
    thrust::device_vector<int> d_vec = h_vec;

    // sort data on the device (846M keys per second on GeForce GTX 480)
    thrust::sort(d_vec.begin(), d_vec.end());

    // transfer data back to host
    thrust::copy(d_vec.begin(), d_vec.end(), h_vec.begin());

    for (int i = 0; i < std::min(25, (int)h_vec.size()); ++i) {
      std::cout << i << ": " << h_vec[i] << std::endl;
    }

    return 0;
}
*/
