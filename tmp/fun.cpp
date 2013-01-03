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

//#define DEBUG

namespace scalan_thrust {
  template <class T> class nested_array;

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
    base_array(int size, T t) : m_data(size, t) { }
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

    void print() {
      std::cout << "base_array: ";
      for (int i = 0; i < m_data.size(); i++) {
        std::cout << m_data[i] << " ";
      }
      std::cout << std::endl;
    }

    T get(int i) {
      return this[i];
    }

    pair<base_array<T>, base_array<T> > flag_split(const base_array<bool>& flags) {
      assert(this->length() == flags.length());

      device_vector<bool> flgs_dt = flags.data();
      device_vector<T> splitted_vals(this->data());
      thrust::sort_by_key(flgs_dt.begin(), flgs_dt.end(), splitted_vals.begin());
      int false_flags_count = thrust::count(flgs_dt.begin(), flgs_dt.end(), false);
      device_vector<T> vals_false(splitted_vals.begin(), splitted_vals.begin() + false_flags_count);
      device_vector<T> vals_true(splitted_vals.begin() + false_flags_count, splitted_vals.end());
      return pair<base_array<T>, base_array<T> >(*(new base_array<T>(vals_true)), *(new base_array<T>(vals_false))); // TODO: Is here a memory leak?
    }

    template <class B>
    base_array<T> expand_by(nested_array<B> nested_arr) {
      device_vector<int> expanded_indxs(nested_arr.values().length());
      device_vector<T> expanded_values(nested_arr.values().length());
      expand(nested_arr.segments().data().begin(), nested_arr.segments().data().end(), expanded_indxs.begin());
      thrust::gather(expanded_indxs.begin(), expanded_indxs.end(), this->data().begin(), expanded_values.begin());
      return base_array<T>(expanded_values);
    }

  }; // class base_array<T>
  
  template <class T>
  base_array<T> binop_array(const base_array<T>& a, const base_array<T>& b) {
    assert(a.length() == b.length());
    device_vector<T> d_vec_res(a.data().size());
    thrust::transform(
      a.data().begin(), a.data().end(),
      b.data().begin(),
      d_vec_res.begin(),
      thrust::multiplies<T>()); // TODO: Implement BinOp<T>
    base_array<T> res(d_vec_res);
    return res;
  }

  template <class T>
  bool binop_array_equal(const base_array<T>& a, const base_array<T>& b) {
    assert(a.length() == b.length());
    return thrust::equal(a.data().begin(), a.data().end(), b.data().begin());
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
    virtual device_vector<pair<T1, T2>> const& data() const { return device_vector<pair<T1, T2>>(); } // TODO: Here should not be data call like this. Fix it.
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
  
  // expand([3,2,0,4]) -> [0,0,0,1,1,3,3,3,3]
  template <typename InputIterator1,
            typename OutputIterator>
  OutputIterator expand(InputIterator1 first1,
                        InputIterator1 last1,
                        OutputIterator output)
  {
    // segs:   2 3 1 x
    // first1: |
    // last1:        |

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
    expand(segs.data().begin(), segs.data().end(), segs_keys.begin());

  #ifdef DEBUG
	std::cout << "sum_lifted::seg_keys: "; thrust::copy(segs_keys.begin(), segs_keys.end(), std::ostream_iterator<int>(std::cout, " ")); std::cout << std::endl;
	#endif

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

// ----- tests -----

#define FLOAT_EQ(x, y) fabs((x) - (y)) < 0.001f
void test_sum() {
  host_vector<int> x5(10, 5);
  base_array<int> x6(x5);
  int x7 = x6.sum(monoid(0.f, monoid::OP_PLUS));
  assert(FLOAT_EQ(x7, 0.f + 5.f * 10.f));
}

void test_back_permute_1() {
  host_vector<float> x(9);
  for (int i = 0; i < x.size(); i++) 
    x[i] = ((float)i + 1) / 10.f;
  
  base_array<float> ba(x);
  
  #ifdef DEBUG
  std::cout << "test_back_permute::ba: "; thrust::copy(ba.data().begin(), ba.data().end(), std::ostream_iterator<float>(std::cout, " ")); std::cout << std::endl;
  #endif
  
  device_vector<int> idxs_data(4);
  idxs_data[0] = 2; idxs_data[1] = 1; idxs_data[2] = 4; idxs_data[3] = 8;
  base_array<int> idxs(idxs_data);
  
  base_array<float> permutation = ba.back_permute(idxs);
  // TODO: Fix floating number comparison
  assert(permutation.data().size() == idxs_data.size());
  assert(FLOAT_EQ(permutation.data()[0], ba.data()[2]));
  assert(FLOAT_EQ(permutation.data()[1], ba.data()[1]));
  assert(FLOAT_EQ(permutation.data()[2], ba.data()[4]));
  assert(FLOAT_EQ(permutation.data()[3], ba.data()[8]));
  
  #ifdef DEBUG
  std::cout << "test_back_permute::ba (should be same): "; thrust::copy(ba.data().begin(), ba.data().end(), std::ostream_iterator<float>(std::cout, " ")); std::cout << std::endl;
  std::cout << "test_back_permute::permutation: "; thrust::copy(permutation.data().begin(), permutation.data().end(), std::ostream_iterator<float>(std::cout, " ")); std::cout << std::endl;
  #endif
}

void test_back_permute_2() {
  host_vector<float> x(4);
  for (int i = 0; i < x.size(); i++) 
    x[i] = ((float)i + 1) / 10.f;
  
  base_array<float> ba(x);
  
  #ifdef DEBUG
  std::cout << "test_back_permute::ba: "; thrust::copy(ba.data().begin(), ba.data().end(), std::ostream_iterator<float>(std::cout, " ")); std::cout << std::endl;
  #endif
  
  device_vector<int> idxs_data(6);
  idxs_data[0] = 2; idxs_data[1] = 1; idxs_data[2] = 3; idxs_data[3] = 0; idxs_data[4] = 2; idxs_data[5] = 0; 
  base_array<int> idxs(idxs_data);
  
  base_array<float> permutation = ba.back_permute(idxs);
  // TODO: Fix floating number comparison
  assert(permutation.data().size() == idxs_data.size());
  assert(FLOAT_EQ(permutation.data()[0], ba.data()[2]));
  assert(FLOAT_EQ(permutation.data()[1], ba.data()[1]));
  assert(FLOAT_EQ(permutation.data()[2], ba.data()[3]));
  assert(FLOAT_EQ(permutation.data()[3], ba.data()[0]));
  assert(FLOAT_EQ(permutation.data()[4], ba.data()[2]));
  assert(FLOAT_EQ(permutation.data()[5], ba.data()[0]));
  
  #ifdef DEBUG
  std::cout << "test_back_permute::ba (should be same): "; thrust::copy(ba.data().begin(), ba.data().end(), std::ostream_iterator<float>(std::cout, " ")); std::cout << std::endl;
  std::cout << "test_back_permute::permutation: "; thrust::copy(permutation.data().begin(), permutation.data().end(), std::ostream_iterator<float>(std::cout, " ")); std::cout << std::endl;
  #endif
}

void test_binop_array() {
  host_vector<float> h_x(5), h_y(5);
  for (int i = 0; i < h_x.size(); i++) {
    h_x[i] = ((float)i + 1) / 10.f;
    h_y[i] = ((float)i + 10) / 10.f;
  }
  
  base_array<float> x(h_x), y(h_y);
  
  #ifdef DEBUG
  std::cout << "test_back_permute::x: "; thrust::copy(x.data().begin(), x.data().end(), std::ostream_iterator<float>(std::cout, " ")); std::cout << std::endl;
  std::cout << "test_back_permute::y: "; thrust::copy(y.data().begin(), y.data().end(), std::ostream_iterator<float>(std::cout, " ")); std::cout << std::endl;
  #endif
  
  base_array<float> res = binop_array<float>(x, y);
  
  #ifdef DEBUG
  std::cout << "test_binop_array::res: "; thrust::copy(res.data().begin(), res.data().end(), std::ostream_iterator<float>(std::cout, " ")); std::cout << std::endl;
  #endif
  
  assert(res.data().size() == x.data().size());  
  for (int i = 0; i < x.data().size(); i++) {
    assert(FLOAT_EQ(res.data()[i], x.data()[i] * y.data()[i]));
  }
}

void test_sum_lifted() {
  host_vector<float> vals_h(6);
  vals_h[0] = 1.f; vals_h[1] = 6.f; vals_h[2] = 3.f; vals_h[3] = 8.f; vals_h[4] = 15.f; vals_h[5] = 24.f;
  
  host_vector<int> segs_h(3);
  segs_h[0] = 2; segs_h[1] = 3; segs_h[2] = 1;

  base_array<float> vals(vals_h);
  base_array<int> segs(segs_h);
  nested_array<float> na(&vals, segs);
      
  base_array<float> res = sum_lifted(na);
  
  assert(res.length() == segs.length());
  assert(FLOAT_EQ(res.data()[0], 7.f));
  assert(FLOAT_EQ(res.data()[1], 26.f));
  assert(FLOAT_EQ(res.data()[2], 24.f));
  
  #ifdef DEBUG
  std::cout << "test_sum_lifted::res: "; thrust::copy(res.data().begin(), res.data().end(), std::ostream_iterator<float>(std::cout, " ")); std::cout << std::endl;
  #endif
}

void test_smvm() {
  // init
  host_vector<int> cols_h(6);
  cols_h[0] = 0; cols_h[1] = 2; cols_h[2] = 0; cols_h[3] = 1; cols_h[4] = 2; cols_h[5] = 3;
  
  host_vector<float> vals_h(6);
  vals_h[0] = 1.f; vals_h[1] = 2.f; vals_h[2] = 3.f; vals_h[3] = 4.f; vals_h[4] = 5.f; vals_h[5] = 6.f;
  
  host_vector<float> v_h(4);
  v_h[0] = 1.f; v_h[1] = 2.f; v_h[2] = 3.f; v_h[3] = 4.f;
  
  host_vector<int> segs_h(3);
  segs_h[0] = 2; segs_h[1] = 3; segs_h[2] = 1;

  base_array<float> vals(vals_h), v(v_h);
  base_array<int> cols(cols_h), segs(segs_h);
  pair_array<int, float> rows(cols, vals);
  nested_array<pair<int, float> > m(&rows, segs);
  
  // process
  base_array<float> bp = v.back_permute(m.values().first());
  base_array<float> ba = binop_array(bp, m.values().second());
  base_array<float> res = sum_lifted(nested_array<float>(&ba, m.segments()));
  
  #ifdef DEBUG
  std::cout << "test_smvm::res: "; thrust::copy(res.data().begin(), res.data().end(), std::ostream_iterator<float>(std::cout, " ")); std::cout << std::endl;
  #endif
  
  // verify
  assert(res.length() == segs.length());
  assert(FLOAT_EQ(res.data()[0], 7.f));
  assert(FLOAT_EQ(res.data()[1], 26.f));
  assert(FLOAT_EQ(res.data()[2], 24.f));
}

void test_flag_split() {
  // init
  device_vector<float> d_a(6);
  d_a[0] = 1.0f; d_a[1] = 2.0f; d_a[2] = 0.0f; d_a[3] = 3.0f; d_a[4] = 4.0f; d_a[5] = 5.0f;
  base_array<float> a(d_a);
  
  device_vector<bool> d_flags(6);
  d_flags[0] = d_flags[4] = true; d_flags[1] = d_flags[2] = d_flags[3] = d_flags[5] = false;
  base_array<bool> flags(d_flags); 

  // process
  pair<base_array<float>, base_array<float> > res = a.flag_split(flags);

  // verify
  assert(res.fst().length() + res.snd().length() == flags.length());
  assert(thrust::find(res.fst().data().begin(), res.fst().data().end(), 1.0f) != res.fst().data().end());
  assert(thrust::find(res.fst().data().begin(), res.fst().data().end(), 4.0f) != res.fst().data().end());

  assert(thrust::find(res.snd().data().begin(), res.snd().data().end(), 2.0f) != res.snd().data().end());
  assert(thrust::find(res.snd().data().begin(), res.snd().data().end(), 0.0f) != res.snd().data().end());
  assert(thrust::find(res.snd().data().begin(), res.snd().data().end(), 3.0f) != res.snd().data().end());
  assert(thrust::find(res.snd().data().begin(), res.snd().data().end(), 5.0f) != res.snd().data().end());

  //std::cout << "res.fst: "; thrust::copy(res.fst().data().begin(), res.fst().data().end(), std::ostream_iterator<float>(std::cout, " ")); std::cout << std::endl;
  //std::cout << "res.snd: "; thrust::copy(res.snd().data().begin(), res.snd().data().end(), std::ostream_iterator<float>(std::cout, " ")); std::cout << std::endl;

}

void test_base_array_expand_by() {
  device_vector<int> d_a(3);
  d_a[0] = 1; d_a[1] = 2; d_a[2] = 3;
  base_array<int> a(d_a);

  device_vector<int> d_segs(3);
  d_segs[0] = 1; d_segs[1] = 2; d_segs[2] = 1;
  device_vector<int> d_vals(4);
  d_vals[0] = 3; d_vals[1] = 4; d_vals[2] = 5; d_vals[3] = 6;
  nested_array<int> na(&base_array<int>(d_vals), base_array<int>(d_segs));

  base_array<int> r = a.expand_by(na);

  //std::cout << "r: "; thrust::copy(r.data().begin(), r.data().end(), std::ostream_iterator<int>(std::cout, " ")); std::cout << std::endl;

  assert(r.data()[0] == 1);
  assert(r.data()[1] == 2);
  assert(r.data()[2] == 2);
  assert(r.data()[3] == 3);
}

void tests() {
  std::cout << "--- test_sum ---" << std::endl;
  test_sum();
  std::cout << "--- test_back_permute_1 ---" << std::endl;
  test_back_permute_1();
  std::cout << "--- test_back_permute_2 ---" << std::endl;
  test_back_permute_2();
  std::cout << "--- test_binop_array ---" << std::endl;
  test_binop_array();
  std::cout << "--- test_sum_lifted ---" << std::endl;
  test_sum_lifted();
  std::cout << "--- test_smvm ---" << std::endl;
  test_smvm();
  std::cout << "--- test_flag_split ---" << std::endl;
  test_flag_split();
  std::cout << "--- test_base_array_expand_by --- " << std::endl;
  test_base_array_expand_by();
  printf("OK!");
}

// ----- tests -----

int main() {
  tests();
}

// ----------------------------------------

base_array<int> fun(const pair<pair<pair<nested_array<int>, base_array<int> >, base_array<int> >, int>& x10) {
// First(var_Sym(10): Tuple2[Tuple2[Tuple2[PArray[PArray[Int]], PArray[Int]], PArray[Int]], Int])
pair<pair<nested_array<int>, base_array<int>>, base_array<int>> x12 = x10.fst();
// First(Sym(12))
pair<nested_array<int>, base_array<int>> x14 = x12.fst();
// Second(Sym(14))
base_array<int> x17 = x14.snd();
// LengthPA(Sym(17))
int x18 = x17.length();
// Const(0)
// Equal(Sym(18),Sym(1))
bool x19 = x18 == x1;
// VarPA(Sym(17))
base_array<int> x21 = x17;
// Second(var_Sym(10): Tuple2[Tuple2[Tuple2[PArray[PArray[Int]], PArray[Int]], PArray[Int]], Int])
int x13 = x10.snd();
// ReplicatePA(Sym(18),Sym(13))
base_array<int> x20 = base_array<int>(x18, x13);
// ExpBinopArrayEquals(Sym(21),Sym(20))
base_array<bool> x22 = binop_array_equal(x21, x20);
// FlagSplit(Sym(22),Sym(22))
pair<base_array<float>, base_array<float> > x26 = x22.flag_split(x22);
// First(Sym(26))
base_array<bool> x27 = x26.fst();
// LengthPA(Sym(27))
int x29 = x27.length();
// Equal(Sym(29),Sym(1))
bool x30 = x29 == x1;
// Not(Sym(30))
bool x31 = !(x30);
// Or(Sym(19),Sym(31))
bool x32 = (x19||x31);
// Second(Sym(12))
base_array<int> x15 = x12.snd();
// VarPA(Sym(15))
base_array<int> x38 = x15;
// First(Sym(14))
nested_array<int> x16 = x14.fst();
// VarPA(Sym(16))
nested_array<int> x33 = x16;
// BackPermute(Sym(33),Sym(17))
nested_array<int> x34 = x33.back_permute(x17);
// NestedArrayValues(Sym(34))
nested_array<int> x35 = x34.values();
// BackPermute(Sym(38),Sym(35))
base_array<int> x39 = x38.back_permute(x35);
// LengthPA(Sym(39))
int x40 = x39.length();
// Const(-1)
// ReplicatePA(Sym(40),Sym(41))
base_array<int> x42 = base_array<int>(x40, x41);
// ExpBinopArrayEquals(Sym(39),Sym(42))
base_array<bool> x43 = binop_array_equal(x39, x42);
// FlagSplit(Sym(35),Sym(43))
pair<base_array<float>, base_array<float> > x44 = x35.flag_split(x43);
// First(Sym(44))
base_array<int> x45 = x44.fst();
// ExpandBy(Sym(21),Sym(34))
base_array<int> x36 = x21.expand_by(x34);
// FlagSplit(Sym(36),Sym(43))
pair<base_array<float>, base_array<float> > x47 = x36.flag_split(x43);
// First(Sym(47))
base_array<int> x48 = x47.fst();
// PairArray(Sym(45),Sym(48))
pair_array<int, int> x50(x45, x48);
// WritePA(Sym(38),Sym(50))
???
// IfArray(Sym(32),Sym(15),Sym(54),)
base_array<int> x62;
if (x32) x62 = x15 else x62 = x54;
return x62;
}
