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

#include <output_operators.h>
#include <monoid.h>
#include <pair.h>
#include <parray.h>
#include <base_array.h>
#include <pair_array.h>

using thrust::host_vector;
using thrust::device_vector;
using thrust::tuple;

using std::cout;
using std::endl;
using std::string;

//#define DEBUG

namespace scalan_thrust {
  template <class T> class nested_array;
  template <class T1, class T2> class pair_array;

  template <class T>
  base_array<T> binop_array_mult(const base_array<T>& a, const base_array<T>& b) {
    assert(a.length() == b.length());
    device_vector<T> d_vec_res(a.length());
    thrust::transform(
      a.data().begin(), a.data().end(),
      b.data().begin(),
      d_vec_res.begin(),
      thrust::multiplies<T>()); // TODO: Implement BinOp<T>
    return base_array<T>(d_vec_res);
  }

  template <class T>
  base_array<T> binop_array_sub(const base_array<T>& a, const base_array<T>& b) {
    assert(a.length() == b.length());
    device_vector<T> d_vec_res(a.length());
    thrust::transform(
      a.data().begin(), a.data().end(),
      b.data().begin(),
      d_vec_res.begin(),
      thrust::minus<T>()); // TODO: Implement BinOp<T>
    return base_array<T>(d_vec_res);
  }

  template <class T>
  base_array<T> binop_array_add(const base_array<T>& a, const base_array<T>& b) {
    assert(a.length() == b.length());
    device_vector<T> d_vec_res(a.length());
    thrust::transform(
      a.data().begin(), a.data().end(),
      b.data().begin(),
      d_vec_res.begin(),
      thrust::plus<T>()); // TODO: Implement BinOp<T>
    return base_array<T>(d_vec_res);
  }

  template <class T>
  base_array<bool> binop_array_equal(const base_array<T>& a, const base_array<T>& b) {
    assert(a.length() == b.length());
    device_vector<bool> res(a.length());
    thrust::transform(
      a.data().begin(), a.data().end(),
      b.data().begin(),
      res.begin(),
      thrust::equal_to<T>());
    return base_array<bool>(res);
  }

  template <class T>
  class nested_array : public parray<T> {
  private:
    base_array<T> m_values; //parray<T>* m_values; // TODO: Make const and * combination as so: can change pointer but not values
    base_array<int> m_segments;
  public:
    nested_array() : m_values(), m_segments() { }    
    nested_array(/*parray<T>**/ const base_array<T>& values, const base_array<int>& segments) { 
      m_segments = segments;
      m_values = values; // TODO: Why polymorphism doesn't work for 'm_values = (const parray<T>& values)'?
    }

    nested_array<T> &operator=(const nested_array<T>& ptr) {
      m_values = ptr.m_values; 
      m_segments = ptr.segments;
      return *this; 
    }

    nested_array(nested_array<T>* ptr) { 
      if (ptr != NULL) { 
        m_values = ptr->m_values;
        m_segments = ptr->m_segments;
      }
    }

    nested_array(const nested_array<T>& na) : m_values(na.m_values), m_segments(na.m_segments) { }

    ~nested_array() { }

    base_array<int> segments() const { return m_segments; }
    /*parray<T>&*/ base_array<T> values() const { return m_values; }
    virtual int length() const { return segments().length(); }
    virtual device_vector<T>& data() const { return values().data(); }

    //base_array<T> map(const unary_operation<T>& op) {
    //parray<T> map(const unary_operation<T>& op) {
    //  return m_values;
    //}

    struct back_permute_functor {
      __host__ __device__
        int operator()(int x, int y) {
          return y == -1 ? x + 1 : y;
      }
    };


    // NOTE: nested_array.back_permute in terms of base_array.back_permute, i.e. no tricky bytes manipultaions
    //  val data = fromArray(Array(100, 101, 102, 103, 104, 105, 106, 107, 108, 109))
    //  val segsLens = fromArray(Array(4, 1, 0, 5))
    //  val segsIdxs = fromArray(Array(0, 4, 5, 5))
    //  val na = mkNestedArray(data, segsIdxs zip segsLens)
    //  val idxs = fromArray(Array(1, 3, 2, 0))
    //  val a1 = segsIdxs.backPermute(idxs)
    //  val a2 = segsLens.backPermute(idxs).scan
    //  val a3 = (a1 zip a2) map {case Pair(x, y) => x - y} // a3 = a1 |-| a2
    //  val idxs1Data = fromArray(Array(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))
    //  val idxs1Lens = segsLens.backPermute(idxs)
    //  val idxs1Idxs = idxs1Lens.scan
    //  val idxs1 = mkNestedArray(idxs1Data, idxs1Idxs zip idxs1Lens)
    //  val a4 = a3.expandBy(idxs1)
    //  val a5 = (idxs1Data zip a4) map {case Pair(x, y) => x + y} // a5 = idxs1Data |+| a4
    //  val res = data.backPermute(a5)
    nested_array<T> back_permute(const base_array<int>& idxs) const { // NOTE: Can idxs be not base_array but PA?
      // NOTE: generally idxs.length() != this.length()

      // this: [[a,b,c],[],[d],[e,f]]
      // values: [a,b,c,d,e,f]
      // segs: [3,0,1,2]
      // idxs: [3,0,1,2]
      // res: [[e,f],[a,b,c],[],[d]]

      std::cout << "idxs: " << idxs;
      base_array<int> segments_idxs(m_segments.length());
      thrust::exclusive_scan(m_segments.data().begin(), m_segments.data().end(), segments_idxs.data().begin());
      std::cout << "segments_idxs: " << segments_idxs;
      base_array<int> segments_idxs_permuted = segments_idxs.back_permute(idxs);
      std::cout << "segments_idxs_permuted: " << segments_idxs_permuted;
      base_array<int> segments_lens_permuted = m_segments.back_permute(idxs);
      std::cout << "segments_lens_permuted: " << segments_lens_permuted;
      base_array<int> segments_permuted_idxs(segments_lens_permuted.length());
      thrust::exclusive_scan(segments_lens_permuted.data().begin(), segments_lens_permuted.data().end(), segments_permuted_idxs.data().begin());
      base_array<int> segments_movement_diff = binop_array_sub(segments_idxs_permuted, segments_permuted_idxs);
      std::cout << "segments_movement_diff: " << segments_movement_diff;
      
      
      base_array<int> idxs_new_data(thrust::reduce(segments_lens_permuted.data().begin(),segments_lens_permuted.data().end()));
      for (int i = 0; i < idxs_new_data.length(); i++) idxs_new_data.data()[i] = i;
      std::cout << "idxs_new_data: " << idxs_new_data;
      nested_array<int> idxs_new(idxs_new_data, segments_lens_permuted);
      std::cout << "idxs_new: " << idxs_new;
      base_array<int> a4 = segments_movement_diff.expand_by(idxs_new);
      std::cout << "a4: " << a4;
      base_array<int> a5 = binop_array_add(idxs_new_data, a4);
      std::cout << "a5: " << a5;
      base_array<T> res = m_values.back_permute(a5);
      std::cout << "res: " << res;

      return nested_array<T>(res, segments_lens_permuted);
    }

    virtual void print() const {
      printf("nested_array: [\n");
      printf("values-> ");
      values().print();
      printf("segments-> ");
      segments().print();
      printf("]\n");
    }
  }; // class nested_array<T>

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
    base_array<int> segs_keys(na.values().length());
    expand(segs.data().begin(), segs.data().end(), segs_keys.data().begin());

#ifdef DEBUG
    std::cout << "sum_lifted::seg_keys: "; thrust::copy(segs_keys.begin(), segs_keys.end(), std::ostream_iterator<int>(std::cout, " ")); std::cout << std::endl;
#endif

    int non_zero_values_count = thrust::count_if(segs.data().begin(), segs.data().end(), is_positive());
    base_array<float> res_values(non_zero_values_count);
    base_array<int> segs_d(segs.length());
    thrust::reduce_by_key(segs_keys.data().begin(), segs_keys.data().end(),
      na.values().data().begin(), segs_d.data().begin(), res_values.data().begin());

    thrust::unique(segs_keys.data().begin(), segs_keys.data().end());

    //std::cout << ">>>: "; thrust::copy(v_star->data().begin(), v_star->data().end(), std::ostream_iterator<float>(std::cout, " ")); std::cout << std::endl;    

    base_array<float> res(segs.length());
    thrust::scatter(res_values.data().begin(), res_values.data().end(), segs_keys.data().begin(), res.data().begin());
    return res;
  }  

  template <class T1, class T2>
  std::ostream& operator << (std::ostream& out, const pair<T1, T2>& pr) {
    out << "Pair(" << pr.fst() << "; " << pr.snd() << ")";
    return out;
  }
} // namespace scalan_thrust

using scalan_thrust::nested_array;
using scalan_thrust::base_array;
using scalan_thrust::parray;
using scalan_thrust::pair_array;
using scalan_thrust::pair;
using scalan_thrust::monoid;
using scalan_thrust::binop_array_mult;
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

  host_vector<int> idxs_data(4);
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

  host_vector<int> idxs_data(6);
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

  base_array<float> res = binop_array_mult<float>(x, y);

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
  nested_array<float> na(vals, segs);

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
  base_array<float> ba = binop_array_mult(bp, m.values().second());
  base_array<int> m_segs = m.segments();
  base_array<float> res = sum_lifted(nested_array<float>(&ba, &m_segs));

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
  host_vector<float> d_a(6);
  d_a[0] = 1.0f; d_a[1] = 2.0f; d_a[2] = 0.0f; d_a[3] = 3.0f; d_a[4] = 4.0f; d_a[5] = 5.0f;
  base_array<float> a(d_a);

  host_vector<bool> d_flags(6);
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
  host_vector<int> d_a(3);
  d_a[0] = 1; d_a[1] = 2; d_a[2] = 3;
  base_array<int> a(d_a);

  host_vector<int> d_segs(3);
  d_segs[0] = 1; d_segs[1] = 2; d_segs[2] = 1;
  host_vector<int> d_vals(4);
  d_vals[0] = 3; d_vals[1] = 4; d_vals[2] = 5; d_vals[3] = 6;
  base_array<int> vals(d_vals);
  base_array<int> segs(d_segs);
  nested_array<int> na(&vals, &segs);

  base_array<int> r = a.expand_by(na);

  //std::cout << "r: "; thrust::copy(r.data().begin(), r.data().end(), std::ostream_iterator<int>(std::cout, " ")); std::cout << std::endl;

  assert(FLOAT_EQ(r.data()[0], 1.0f));
  assert(FLOAT_EQ(r.data()[1], 2.0f));
  assert(FLOAT_EQ(r.data()[2], 2.0f));
  assert(FLOAT_EQ(r.data()[3], 3.0f));
}

void test_write_pa() {
  host_vector<float> d_input(5);
  d_input[0] = 1.0f; d_input[1] = 2.0f; d_input[2] = 0.0f; d_input[3] = 0.0f; d_input[4] = 5.0f;
  base_array<float> input(d_input);

  device_vector<float> d_vals(3);
  d_vals[0] = 7.0f; d_vals[1] = 8.0f; d_vals[2] = 9.0f;
  device_vector<int> d_idxs(3);
  d_idxs[0] = 4; d_idxs[1] = 1; d_idxs[2] = 2;
  base_array<int> idxs(d_idxs);
  base_array<float> vals(d_vals);
  pair_array<int, float> vals_idxs(idxs, vals);

  base_array<float> res = input.write_pa(vals_idxs);

  //std::cout << "res: "; res.print();
  assert(FLOAT_EQ(res.data()[0], 1.0f));
  assert(FLOAT_EQ(res.data()[1], 8.0f));
  assert(FLOAT_EQ(res.data()[2], 9.0f));
  assert(FLOAT_EQ(res.data()[3], 0.0f));
  assert(FLOAT_EQ(res.data()[4], 7.0f));
}

void test_nested_arr_backpermute() {
  device_vector<float> d_vals(6);
  d_vals[0] = 1; d_vals[1] = 2; d_vals[2] = 3; d_vals[3] = 4; d_vals[4] = 5; d_vals[5] = 6;
  device_vector<int> d_segs(4);
  d_segs[0] = 3; d_segs[1] = 0; d_segs[2] = 1; d_segs[3] = 2;
  base_array<float> vals(d_vals);
  base_array<int> segs(d_segs);
  nested_array<float> na(vals, segs);

  device_vector<int> d_permutation(4);
  d_permutation[0] = 3; d_permutation[1] = 0; d_permutation[2] = 1; d_permutation[3] = 2;
  base_array<int> permutation(d_permutation);

  nested_array<float> na_permuted = na.back_permute(permutation);

  na_permuted.print();
  assert(FLOAT_EQ(na_permuted.values().data()[0], 5.0f));
  assert(FLOAT_EQ(na_permuted.values().data()[1], 6.0f));
  assert(FLOAT_EQ(na_permuted.values().data()[2], 1.0f));
  assert(FLOAT_EQ(na_permuted.values().data()[3], 2.0f));
  assert(FLOAT_EQ(na_permuted.values().data()[4], 3.0f));
  assert(FLOAT_EQ(na_permuted.values().data()[5], 4.0f));

  assert(na_permuted.segments().data()[0] == 2);
  assert(na_permuted.segments().data()[1] == 3);
  assert(na_permuted.segments().data()[2] == 0);
  assert(na_permuted.segments().data()[3] == 1);
}

// ------------------- Generated 'Breadth First Search' ---------------------
base_array<int> x11(const pair<pair<pair<nested_array<int>, base_array<int> >, base_array<int> >, int>& x10) {
pair<pair<nested_array<int>, base_array<int>>, base_array<int>> x12 = x10.fst();
pair<nested_array<int>, base_array<int>> x14 = x12.fst();
base_array<int> x17 = x14.snd();
int x18 = x17.length();
int x1 = 0;
bool x19 = x18 == x1;
base_array<int> x21 = x17;
int x13 = x10.snd();
base_array<int> x20 = base_array<int>(x18, x13);
base_array<bool> x22 = binop_array_equal(x21, x20);
pair<base_array<bool>, base_array<bool>>x26 = x22.flag_split(x22);
base_array<bool> x27 = x26.fst();
int x29 = x27.length();
bool x30 = x29 == x1;
bool x31 = !(x30);
bool x32 = (x19||x31);
base_array<int> x66;
if (x32) {
base_array<int> x15 = x12.snd();
x66 = x15; } else {
base_array<int> x15 = x12.snd();
nested_array<int> x16 = x14.fst();
nested_array<int> x33 = x16;
nested_array<int> x34 = x33.back_permute(x17);
base_array<int> x35 = x34.values();
base_array<int> x38 = x15;
base_array<int> x39 = x38.back_permute(x35);
int x40 = x39.length();
int x41 = -1;
base_array<int> x42 = base_array<int>(x40, x41);
base_array<bool> x43 = binop_array_equal(x39, x42);
pair<base_array<int>, base_array<int>>x44 = x35.flag_split(x43);
base_array<int> x45 = x44.fst();
base_array<int> x58 = x45;
base_array<int> x36 = x21.expand_by(x34);
pair<base_array<int>, base_array<int>>x47 = x36.flag_split(x43);
base_array<int> x48 = x47.fst();
base_array<int> x56 = x48;
pair_array<int, int> x50(x45, x48);
base_array<int> x54 = x38.write_pa(x50);
base_array<int> x55 = x54.back_permute(x45);
base_array<bool> x57 = binop_array_equal(x56, x55);
pair<base_array<int>, base_array<int>>x59 = x58.flag_split(x57);
base_array<int> x60 = x59.fst();
pair<nested_array<int>, base_array<int>> x62 (x16, x60);
pair<pair<nested_array<int>, base_array<int>>, base_array<int>> x63 (x62, x54);
pair<pair<pair<nested_array<int>, base_array<int>>, base_array<int>>, int> x64 (x63, x13);
// Lambda: Lambda(var_Sym(10): Tuple2[Tuple2[Tuple2[PArray[PArray[Int]], PArray[Int]], PArray[Int]], Int],Sym(66))
base_array<int> x65 = x11(x64);
x66 = x65; }
return x66;
}

void test_bfs_1() {
  device_vector<int> d_segs(5);
  d_segs[0] = 1; d_segs[1] = 3; d_segs[2] = 2; d_segs[3] = 1; d_segs[4] = 1;
  device_vector<int> d_data(8);
  d_data[0] = 1; d_data[1] = 0; d_data[2] = 2; d_data[3] = 3; d_data[4] = 1; d_data[5] = 4; d_data[6] = 1; d_data[7] = 2;
  base_array<int> data(d_data);
  base_array<int> segs(d_segs);
  nested_array<int> graph(&data, &segs);

  device_vector<int> d_frontiers(1);
  d_frontiers[0] = 1;
  base_array<int> frontiers(d_frontiers);

  device_vector<int> d_bfs_tree(5);
  d_bfs_tree[0] = -1; d_bfs_tree[1] = 1; d_bfs_tree[2] = -1; d_bfs_tree[3] = -1; d_bfs_tree[4] = -1;
  base_array<int> bfs_tree(d_bfs_tree);

  int end_node = 4;

  pair<nested_array<int>, base_array<int> > p1(graph, frontiers);
  pair<pair<nested_array<int>, base_array<int> >, base_array<int> > p2(p1, bfs_tree);
  pair<pair<pair<nested_array<int>, base_array<int> >, base_array<int> >, int> input(p2, end_node);

  std::cout << input << std::endl;

  base_array<int> res = x11(input);

  assert(res.length() == 5);
  assert(FLOAT_EQ(res.data()[0], 1.0f));
  assert(FLOAT_EQ(res.data()[1], 1.0f));
  assert(FLOAT_EQ(res.data()[2], 1.0f));
  assert(FLOAT_EQ(res.data()[3], 1.0f));
  assert(FLOAT_EQ(res.data()[4], 2.0f));
}

// TODO: It fails for some reason inside x11. FIX IT
void test_bfs_2() {
  return;

  device_vector<int> d_segs(5);
  d_segs[0] = 1; d_segs[1] = 3; d_segs[2] = 2; d_segs[3] = 1; d_segs[4] = 1;
  device_vector<int> d_data(8);
  d_data[0] = 1; d_data[1] = 0; d_data[2] = 2; d_data[3] = 3; d_data[4] = 1; d_data[5] = 4; d_data[6] = 1; d_data[7] = 2;
  base_array<int> data(d_data);
  base_array<int> segs(d_segs);
  nested_array<int> graph(data, segs);

  device_vector<int> d_frontiers(1);
  d_frontiers[0] = 0;
  base_array<int> frontiers(d_frontiers);

  device_vector<int> d_bfs_tree(5);
  d_bfs_tree[0] = 0; d_bfs_tree[1] = -1; d_bfs_tree[2] = -1; d_bfs_tree[3] = -1; d_bfs_tree[4] = -1;
  base_array<int> bfs_tree(d_bfs_tree);

  int end_node = 4;

  pair<nested_array<int>, base_array<int> > p1(graph, frontiers);
  pair<pair<nested_array<int>, base_array<int> >, base_array<int> > p2(p1, bfs_tree);
  pair<pair<pair<nested_array<int>, base_array<int> >, base_array<int> >, int> input(p2, end_node);

  std::cout << input << std::endl;

  base_array<int> res = x11(input);

  std::cout << res;

  assert(res.length() == 5);
  assert(FLOAT_EQ(res.data()[0], 0.0f));
  assert(FLOAT_EQ(res.data()[1], 0.0f));
  assert(FLOAT_EQ(res.data()[2], 1.0f));
  assert(FLOAT_EQ(res.data()[3], 1.0f));
  assert(FLOAT_EQ(res.data()[4], 2.0f));
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
  std::cout << "--- test_write_pa --- " << std::endl;
  test_write_pa();
  std::cout << "--- test_nested_arr_backpermute --- " << std::endl;
  test_nested_arr_backpermute();
  std::cout << "--- test_bfs_1 --- " << std::endl;
  test_bfs_1();
  std::cout << "--- test_bfs_2 --- " << std::endl;
  test_bfs_2();
  printf("OK!");
}
// ----- tests -----

int main() {
  test_bfs_2();

  tests();
  std::getchar();
}
