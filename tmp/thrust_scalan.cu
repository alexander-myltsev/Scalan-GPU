#include <thrust/host_vector.h>
#include <thrust/device_vector.h>
#include <thrust/generate.h>
#include <thrust/sort.h>
#include <thrust/copy.h>
#include <cstdlib>
#include <algorithm>

using thrust::host_vector;
using thrust::device_vector;

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

  class monoid {
  public:
    enum operation_t { OP_PLUS, OP_MINUS, OP_MUL };
  private:
    float m_zero;
    operation_t m_opname; // TODO: Must be enum
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

  class base_array {
  private:
    device_vector<float> m_data;
  public:
    base_array(const host_vector<float>& h_vec) : m_data(h_vec) { }

    float sum(const monoid& m) {
      float res;
      switch (m.op()) {
      case monoid::OP_PLUS:
        res = thrust::reduce(m_data.begin(), m_data.end(), m.zero(), thrust::plus<float>());
        break;
      case monoid::OP_MINUS:
        res = thrust::reduce(m_data.begin(), m_data.end(), m.zero(), thrust::minus<float>());
        break;
      case monoid::OP_MUL:
        res = thrust::reduce(m_data.begin(), m_data.end(), m.zero(), thrust::multiplies<float>());
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
  scalan_thrust::base_array x6(x5);
  int x7 = x6.sum(scalan_thrust::monoid(0.0f, scalan_thrust::monoid::OP_PLUS));
  return x7;
}

int main() {
  host_vector<int> h_vec(10, 2);
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
