// ----------------------------------------
// ----- Scalan-Thrust CUDA/C library -----
// ----------------------------------------
#include <thrust/host_vector.h>
#include <thrust/count.h>
#include <thrust/device_vector.h>
#include <thrust/iterator/permutation_iterator.h>
#include <thrust/iterator/counting_iterator.h>
#include <thrust/reduce.h>
#include <thrust/gather.h>
#include <thrust/scan.h>
#include <thrust/fill.h>
#include <thrust/copy.h>
#include <thrust/unique.h>
#include <thrust/functional.h>
#include <thrust/generate.h>
#include <thrust/sort.h>

#include <cstdlib>
#include <algorithm>
#include <assert.h>

using thrust::host_vector;
using thrust::device_vector;
using thrust::tuple;

using std::cout;
using std::endl;
using std::string;

#define DEBUG

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
  class pair {
  private:
    T1 m_v1;
    T2 m_v2;
  public:
    pair(T1 v1, T2 v2) : m_v1(v1), m_v2(v2)  {  }

    T1 fst() const { return m_v1; }
    T2 snd() const { return m_v2; }
  };

  template <class T>
  class parray {
  public:
    virtual int length() const { return -1; }; // TODO: Make it pure function
    virtual device_vector<T> const& data() const = 0;
  };  

  template <class T>
  class base_array : public parray<T> {
  private:
    device_vector<T> m_data;
    
  public:
    base_array() : m_data() { }
    base_array(int size) : m_data(size) { }
    base_array(const host_vector<T>& h_vec) : m_data(h_vec) { }
    base_array(const device_vector<T>& d_vec) : m_data(d_vec) { }
    
    virtual device_vector<T> const& data() const { return m_data; }
    virtual int length() const { return m_data.size(); }

    T sum(const monoid& m) const {
      // TODO: Reconsider it to: res = thrust::reduce(..., m.zero(), (m.binary_operation() == plus | minus | ...);
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
    
    base_array<T> back_permute(const base_array<int>& idxs) { // NOTE: Can idxs be not base_array but PA?
      device_vector<T> d_vec_res(idxs.data().size());
      thrust::copy(
        thrust::make_permutation_iterator(m_data.begin(), idxs.data().begin()),
        thrust::make_permutation_iterator(m_data.end(), idxs.data().end()),
        d_vec_res.begin());
      base_array<T> res(d_vec_res);
      return res;
    }
  };
  
  template <class T>
  base_array<T> binop_array(const base_array<T>& a, const base_array<T>& b) {
    assert(a.data().size() == b.data().size());
    device_vector<T> d_vec_res(a.data().size());
    thrust::transform(
      a.data().begin(), a.data().end(),
      b.data().begin(),
      d_vec_res.begin(),
      thrust::multiplies<T>()); // TODO: Implement BinOp<T>
    base_array<T> res(d_vec_res);
    return res;
  }
  
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
    virtual device_vector<pair<T1, T2>> const& data() const { return device_vector<pair<T1, T2>>(); } // TODO: There should not be data call like this. Fix it.
  };
  
  template <class T>
  class nested_array {
  private:
    parray<T>* m_values; // TODO: Make const and * combination as so: can change pointer but not values
    base_array<int> m_segments;
  public:
    nested_array() : m_values(), m_segments() { }    
    nested_array(parray<T>* values, const base_array<int>& segments) : m_segments(segments) { 
      m_values = values; // TODO: Why polymorphism doesn't work for 'm_values = (const parray<T>& values)'?
    }
  
    base_array<int> const& segments() const { return m_segments; }
    parray<T>& values() const { return *m_values; }
    virtual int length() const { return segments().length(); }
      
    //base_array<T> map(const unary_operation<T>& op) {
    //parray<T> map(const unary_operation<T>& op) {
    //  return m_values;
    //}
  };
  
  template <class T1, class T2>
  class nested_array <pair<T1, T2> > {
  private:
    pair_array<T1, T2>* m_values;
    base_array<int> m_segments;
  public:
    nested_array() : m_values(), m_segments() { }    
    nested_array(pair_array<T1, T2>* values, const base_array<int>& segments) : m_segments(segments) { 
      m_values = values; // TODO: Why polymorphism doesn't work for 'm_values = (const parray<T>& values)'?
    }
  
    base_array<int> const& segments() const { return m_segments; }
    pair_array<T1, T2>& values() const { return *m_values; }
    virtual int length() const { return segments().length(); }
  };
  
  template <typename InputIterator1,
            typename InputIterator2,
            typename OutputIterator>
  OutputIterator expand(InputIterator1 first1,
                        InputIterator1 last1,
                        InputIterator2 first2,
                        OutputIterator output)
  {
    // segs:   2 3 1 x
    // first1: |
    // last1:        |
    //         0 1 2 x
    // first2: |

    typedef typename thrust::iterator_difference<InputIterator1>::type difference_type; // int
    
    difference_type input_size  = thrust::distance(first1, last1); // 3
    difference_type output_size = thrust::reduce(first1, last1); // 2 + 3 + 1 = 6

    // scan the counts to obtain output offsets for each input element
    thrust::device_vector<difference_type> output_offsets(input_size, 0);
    thrust::exclusive_scan(first1, last1, output_offsets.begin());  // output_offsets: 0 2 5   
    
    #ifdef DEBUG
    std::cout << "expand::input: "; thrust::copy(first1, last1, std::ostream_iterator<int>(std::cout, " ")); std::cout << std::endl;
    #endif
    
    #ifdef DEBUG
    std::cout << "expand::output_offsets: "; thrust::copy(output_offsets.begin(), output_offsets.end(), std::ostream_iterator<int>(std::cout, " ")); std::cout << std::endl;
    #endif

    // scatter the nonzero counts into their corresponding output positions
    thrust::device_vector<difference_type> output_indices(output_size, 0);
    thrust::scatter_if
      (thrust::counting_iterator<difference_type>(0),
       thrust::counting_iterator<difference_type>(input_size),
       output_offsets.begin(),  // output_offsets: 0 2 5
       first1,                  // first1-last1:   2 3 1 // NOTE: if 0 then false and no value in output_indices
       output_indices.begin()); // output_indices: 0 0 1 0 0 2
      
    #ifdef DEBUG
    std::cout << "expand::output_indices: "; thrust::copy(output_indices.begin(), output_indices.end(), std::ostream_iterator<int>(std::cout, " ")); std::cout << std::endl;
    #endif

    // compute max-scan over the output indices, filling in the holes
    assert(output_indices.end() - output_indices.begin() == output_size);
    thrust::inclusive_scan
      (output_indices.begin(),
       output_indices.end(),
       output,
       thrust::maximum<difference_type>()); // output_indices: 0 0 1 1 1 2
    
    #ifdef DEBUG
    std::cout << "expand::output: "; thrust::copy(output, output + output_size, std::ostream_iterator<int>(std::cout, " ")); std::cout << std::endl;
    #endif

    // return output + output_size
    thrust::advance(output, output_size);
    return output;
  }
  
  struct is_positive
  {
    __host__ __device__
    bool operator() (long x)
    {
      return x > 0;
    }
  };
  
  base_array<float> sum_lifted(const nested_array<float>& na) {
    base_array<int> segs = na.segments();
    device_vector<int> segs_keys(na.values().length());
    expand(segs.data().begin(), segs.data().end(), 
           thrust::counting_iterator<int>(0), 
           segs_keys.begin());

	std::cout << "sum_lifted::seg_keys: "; thrust::copy(segs_keys.begin(), segs_keys.end(), std::ostream_iterator<int>(std::cout, " ")); std::cout << std::endl;

    int non_zero_values_count = thrust::count_if(segs.data().begin(), segs.data().end(), is_positive());
    device_vector<float> res_values(non_zero_values_count);
    device_vector<int> segs_d(segs.length());
    thrust::reduce_by_key(segs_keys.begin(), segs_keys.end(),
                          na.values().data().begin(), segs_d.begin(), res_values.begin());
    
    thrust::unique(segs_keys.begin(), segs_keys.end());

    //std::cout << ">>>: "; thrust::copy(v_star->data().begin(), v_star->data().end(), std::ostream_iterator<float>(std::cout, " ")); std::cout << std::endl;    
    
    device_vector<float> res(segs.length());
    thrust::scatter(res_values.begin(), res_values.end(), 
                    segs_keys.begin(), res.begin());
    return base_array<float>(res);
  }
}

using scalan_thrust::nested_array;
using scalan_thrust::base_array;
using scalan_thrust::parray;
using scalan_thrust::pair_array;
using scalan_thrust::pair;
using scalan_thrust::unary_operation;
using scalan_thrust::monoid;
using scalan_thrust::binop_array;
using scalan_thrust::sum_lifted;


// ----------------------------------------
base_array<float> fun(pair<nested_array<pair<int, float> >, base_array<float> > x14) {
base_array<float> x16 = x14.snd();
nested_array<pair<int, float> > x15 = x14.fst();
pair_array<int, float> x17 = x15.values();
base_array<int> x18 = x17.first();
base_array<float> x19 = x16.back_permute(x18);
base_array<float> x20 = x17.second();
base_array<float> x21 = binop_array(x19, x20);
base_array<int> x22 = x15.segments();
nested_array<float> x23 = nested_array<float>(&x21, x22);
base_array<float> x24 = sum_lifted(x23);
return x24;
}

