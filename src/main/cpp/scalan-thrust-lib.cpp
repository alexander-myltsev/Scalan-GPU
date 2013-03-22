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
#include <nested_array.h>

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
  base_array<T> binop_array_mult(const parray<T>& a, const parray<T>& b) {
    assert(a.length() == b.length());
    
    const base_array<T>& a1 = dynamic_cast<const base_array<T>&>(a);
    const base_array<T>& b1 = dynamic_cast<const base_array<T>&>(b);
    base_array<T> res(a.length());
    thrust::transform(
      a1.data().begin(), a1.data().end(),
      b1.data().begin(),
      res.data().begin(),
      thrust::multiplies<T>()); // TODO: Implement BinOp<T>
    return res;
  }

  template <class T>
  base_array<T> binop_array_sub(const parray<T>& a, const parray<T>& b) {
    assert(a.length() == b.length());

    const base_array<T>& a1 = dynamic_cast<const base_array<T>&>(a);
    const base_array<T>& b1 = dynamic_cast<const base_array<T>&>(b);
    base_array<T> res(a.length());
    thrust::transform(
      a1.data().begin(), a1.data().end(),
      b1.data().begin(),
      res.data().begin(),
      thrust::minus<T>()); // TODO: Implement BinOp<T>
    return res;
  }

  template <class T>
  base_array<T> binop_array_add(const parray<T>& a, const parray<T>& b) {
    assert(a.length() == b.length());

    const base_array<T>& a1 = dynamic_cast<const base_array<T>&>(a);
    const base_array<T>& b1 = dynamic_cast<const base_array<T>&>(b);
    base_array<T> res(a.length());
    thrust::transform(
      a1.data().begin(), a1.data().end(),
      b1.data().begin(),
      res.data().begin(),
      thrust::plus<T>()); // TODO: Implement BinOp<T>
    return res;
  }

  template <class T>
  base_array<T> binop_array_equal(const parray<T>& a, const parray<T>& b) {
    assert(a.length() == b.length());

    const base_array<T>& a1 = dynamic_cast<const base_array<T>&>(a);
    const base_array<T>& b1 = dynamic_cast<const base_array<T>&>(b);
    base_array<T> res(a.length());
    thrust::transform(
      a1.data().begin(), a1.data().end(),
      b1.data().begin(),
      res.data().begin(),
      thrust::equal_to<T>()); // TODO: Implement BinOp<T>
    return res;
  }  

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

  parray<int>& series(int length) {
    base_array<int>* res = new base_array<int>(); // TODO: Fix this memory leak
    device_vector<int>* data = new device_vector<int>(length); // TODO: Rewrite it with counting iterator
    for (int i = 0; i < length; i++) (*data)[i] = i;
    res->m_data = data;
    res->m_ref_counter = new ref_counter();
    return *res;
  }

  base_array<float> sum_lifted(const nested_array<float>& na) {
    base_array<int> segs = dynamic_cast<base_array<int>&>(na.segments());
    base_array<int> segs_keys(na.values().length());
    expand(segs.m_data->begin(), segs.m_data->end(), segs_keys.m_data->begin());

#ifdef DEBUG
    std::cout << "sum_lifted::seg_keys: "; thrust::copy(segs_keys.begin(), segs_keys.end(), std::ostream_iterator<int>(std::cout, " ")); std::cout << std::endl;
#endif

    int non_zero_values_count = thrust::count_if(segs.m_data->begin(), segs.m_data->end(), is_positive());
    base_array<float> res_values(non_zero_values_count);
    base_array<int> segs_d(segs.length());
    base_array<float> na_vals_ba = dynamic_cast<base_array<float>&>(na.values());
    thrust::reduce_by_key(segs_keys.m_data->begin(), segs_keys.m_data->end(),
      na_vals_ba.m_data->begin(), segs_d.m_data->begin(), res_values.m_data->begin());

    thrust::unique(segs_keys.m_data->begin(), segs_keys.m_data->end());

    //std::cout << ">>>: "; thrust::copy(v_star->data().begin(), v_star->data().end(), std::ostream_iterator<float>(std::cout, " ")); std::cout << std::endl;    

    base_array<float> res(segs.length());
    thrust::scatter(res_values.m_data->begin(), res_values.m_data->end(), segs_keys.m_data->begin(), res.m_data->begin());
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

  parray<float>& permutation = ba.back_permute(idxs);

  assert(permutation.length() == idxs_data.size());
  assert(FLOAT_EQ(permutation.get(0), ba.get(2)));
  assert(FLOAT_EQ(permutation.get(1), ba.get(1)));
  assert(FLOAT_EQ(permutation.get(2), ba.get(4)));
  assert(FLOAT_EQ(permutation.get(3), ba.get(8)));

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

  parray<float>& permutation = ba.back_permute(idxs);
  // TODO: Fix floating number comparison
  assert(permutation.length() == idxs_data.size());
  assert(FLOAT_EQ(permutation.get(0), ba.get(2)));
  assert(FLOAT_EQ(permutation.get(1), ba.get(1)));
  assert(FLOAT_EQ(permutation.get(2), ba.get(3)));
  assert(FLOAT_EQ(permutation.get(3), ba.get(0)));
  assert(FLOAT_EQ(permutation.get(4), ba.get(2)));
  assert(FLOAT_EQ(permutation.get(5), ba.get(0)));

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

  assert(res.length() == x.length());  
  for (int i = 0; i < x.length(); i++) {
    float rv = res.get(i), xv = x.get(i), yv = y.get(i);
    assert(FLOAT_EQ(rv, xv * yv));
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
  assert(FLOAT_EQ(res.get(0), 7.f));
  assert(FLOAT_EQ(res.get(1), 26.f));
  assert(FLOAT_EQ(res.get(2), 24.f));

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
  parray<float>& bp = v.back_permute(m.values().first());
  base_array<float> ba = binop_array_mult(bp, m.values().second());
  nested_array<float> na(ba, m.segments());
  base_array<float> res = sum_lifted(na);

#ifdef DEBUG
  std::cout << "test_smvm::res: "; thrust::copy(res.data().begin(), res.data().end(), std::ostream_iterator<float>(std::cout, " ")); std::cout << std::endl;
#endif

  // verify
  assert(res.length() == segs.length());
  assert(FLOAT_EQ(res.get(0), 7.f));
  assert(FLOAT_EQ(res.get(1), 26.f));
  assert(FLOAT_EQ(res.get(2), 24.f));
}

void test_flag_split() {
  // init
  host_vector<float> d_a(6);
  d_a[0] = 1.f; d_a[1] = 2.f; d_a[2] = 0.f; d_a[3] = 3.f; d_a[4] = 4.f; d_a[5] = 5.f;
  base_array<float> a(d_a);

  host_vector<bool> d_flags(6);
  d_flags[0] = d_flags[4] = true; d_flags[1] = d_flags[2] = d_flags[3] = d_flags[5] = false;
  base_array<bool> flags(d_flags); 

  // process
  pair<base_array<float>, base_array<float> > res = a.flag_split(flags);

  // verify
  assert(res.fst().length() + res.snd().length() == flags.length());
  assert(res.fst().contains(1.f));
  assert(res.fst().contains(4.f));

  assert(res.snd().contains(2.f));
  assert(res.snd().contains(0.f));
  assert(res.snd().contains(3.f));
  assert(res.snd().contains(5.f));

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
  parray<int>& segs = base_array<int>(d_segs);
  nested_array<int> na(vals, segs);

  base_array<int> r = a.expand_by(na);

  //std::cout << "r: "; thrust::copy(r.data().begin(), r.data().end(), std::ostream_iterator<int>(std::cout, " ")); std::cout << std::endl;

  assert(FLOAT_EQ(r.get(0), 1.f));
  assert(FLOAT_EQ(r.get(1), 2.f));
  assert(FLOAT_EQ(r.get(2), 2.f));
  assert(FLOAT_EQ(r.get(3), 3.f));
}

void test_write_pa() {
  host_vector<float> d_input(5);
  d_input[0] = 1.f; d_input[1] = 2.f; d_input[2] = 0.f; d_input[3] = 0.f; d_input[4] = 5.f;
  base_array<float> input(d_input);

  device_vector<float> d_vals(3);
  d_vals[0] = 7.f; d_vals[1] = 8.f; d_vals[2] = 9.f;
  device_vector<int> d_idxs(3);
  d_idxs[0] = 4; d_idxs[1] = 1; d_idxs[2] = 2;
  base_array<int> idxs(d_idxs);
  base_array<float> vals(d_vals);
  pair_array<int, float> vals_idxs(idxs, vals);

  base_array<float> res = input.write_pa(vals_idxs);

  //std::cout << "res: "; res.print();
  assert(FLOAT_EQ(res.get(0), 1.f));
  assert(FLOAT_EQ(res.get(1), 8.f));
  assert(FLOAT_EQ(res.get(2), 9.f));
  assert(FLOAT_EQ(res.get(3), 0.f));
  assert(FLOAT_EQ(res.get(4), 7.f));
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

  parray<parray<float>>& na_permuted = na.back_permute(permutation);

  nested_array<float> na_cast = dynamic_cast<nested_array<float>&>(na_permuted);
  assert(FLOAT_EQ(na_cast.values().get(0), 5.f));
  assert(FLOAT_EQ(na_cast.values().get(1), 6.f));
  assert(FLOAT_EQ(na_cast.values().get(2), 1.f));
  assert(FLOAT_EQ(na_cast.values().get(3), 2.f));
  assert(FLOAT_EQ(na_cast.values().get(4), 3.f));
  assert(FLOAT_EQ(na_cast.values().get(5), 4.f));

  assert(na_cast.segments().get(0) == 2);
  assert(na_cast.segments().get(1) == 3);
  assert(na_cast.segments().get(2) == 0);
  assert(na_cast.segments().get(3) == 1);
}

// ------------------- Generated 'Breadth First Search' ---------------------
/*
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
parray<int>& x39 = x38.back_permute(x35);
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
  assert(FLOAT_EQ(res.get(0), 1.f));
  assert(FLOAT_EQ(res.get(1), 1.f));
  assert(FLOAT_EQ(res.get(2), 1.f));
  assert(FLOAT_EQ(res.get(3), 1.f));
  assert(FLOAT_EQ(res.get(4), 2.f));
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
  assert(FLOAT_EQ(res.get(0), 0.f));
  assert(FLOAT_EQ(res.get(1), 0.f));
  assert(FLOAT_EQ(res.get(2), 1.f));
  assert(FLOAT_EQ(res.get(3), 1.f));
  assert(FLOAT_EQ(res.get(4), 2.f));
}
*/

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
  //std::cout << "--- test_bfs_1 --- " << std::endl;
  //test_bfs_1();
  //std::cout << "--- test_bfs_2 --- " << std::endl;
  //test_bfs_2();
  printf("OK!");
}
// ----- tests -----

int main() {
  test_nested_arr_backpermute();

  tests();
  std::getchar();
}
