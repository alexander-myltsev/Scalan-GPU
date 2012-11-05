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

/*
template<class T>
base_array<T> tabulate(int lenght, unary_operation<int, T> f) {
    device_vector<T>* res = new device_vector<T>(length);
    thrust::transform(thrust::counting_iterator<int>(0), thrust::counting_iterator<int>(0) + length, res.begin(), f);
    return base_array<T>(res);
}

class x34 { // Lambda
private:
  pair_array<int, float> x27;
  base_array<float> x28;
public:
  x34(pair_array<int, float> x27, base_array<float> x28) : x27(x27), x28(x28) { // TODO: avoid copying. Pass pointers instead.
    // ...
  }

  __host__ __device__ float operator(int x33) {
    base_array<int> x29 = x27.first;
    base_array<float> x30 = x27.second;
    float x36 = x30.index(x33);
    int x35 = x29.index(x33);
    float x38 = x28.index(x35);
    float x39 = x38 * x36;
    return x39;
  }
};

float sparseVectorMul(thrust::tuple<pair_array<int, float>, base_array<float>> x26) {
    pair_array<int, float> s27 = x26.first;
    base_array<float> x28 = x26.second;
    base_array<int> x29 = x27.first;
    base_array<float> x41 = tabulate(x29.length, x34(x27, x28));
    float x42 = x41.sum(monoid(0.0f, OP_PLUS));
    return x42;
}
*/

int main() {
  device_vector<int> sparse_vec_idx(3);
  sparse_vec_idx[0] = 0;
  sparse_vec_idx[1] = 2;
  sparse_vec_idx[2] = 3;

  device_vector<int> sparse_vec_val(3);
  sparse_vec_val[0] = 10.0f;
  sparse_vec_val[1] = 30.0f;
  sparse_vec_val[2] = 40.0f;

  tuple< device_vector<int>, device_vector<float> > pair_array Array((0, 10f), (2, 30f), (3, 40f))


    val sparseVecPA = fromArray(sparseVec)
    val vec = Array(10f, 20f, 30f, 40f)
}
