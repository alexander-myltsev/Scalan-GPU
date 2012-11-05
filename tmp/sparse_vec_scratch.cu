template<class T>
base_array<T> tabulate(int lenght, operation<T> f) {
    device_vector<T>* res = new device_vector<T>(length);
    //thrust::transform(thrust::counting_iterator<int>(0), thrust::counting_iterator<int>(0) + length, res->begin(), f);
    thrust::transform(make_zip_iterator(, thrust::counting_iterator<int>(0) + length, res->begin(), f);
    return base_array<T>(res);
}

class x34 { // Lambda
private:
  //  pair_array<int, float> x27;
  //  base_array<float> x28;
public:
  /*
  x34(pair_array<int, float> x27, base_array<float> x28) : x27(x27), x28(x28) { // TODO: avoid copying. Pass pointers instead.
    // ...
  }
  */

  __host__ __device__ float operator(thrust::tuple<float, int, float> x_t) {
    float x36;
    int x35;
    float x38;
    thrust::tie(x36, x35, x38) = x_t;
    //base_array<int> x29 = x27.first;
    //base_array<float> x30 = x27.second;
    //float x36 = x30.index(x33);
    //int x35 = x29.index(x33);
    //float x38 = x28.index(x35);
    float x39 = x38 * x36;
    return x39;
  }
};

float sparseVectorMul(thrust::tuple<pair_array<int, float>, base_array<float>> x26) {
    pair_array<int, float> x27 = x26.first;
    base_array<float> x28 = x26.second;
    base_array<int> x29 = x27.first;
    base_array<float> x41 = tabulate(x29.length, x34);
    float x42 = x41.sum(monoid(0.0f, OP_PLUS));
    return x42;
}

// ----- 
/*
class f {
  __host__ __device__ operator(float value, float v_i) {
    
  }
};

float sparseVectorMul(thrust::tuple<pair_array<int, float>, base_array<float>> x26) {
    pair_array<int, float> x27 = x26.first;
    base_array<float> x28 = x26.second;
    base_array<int> x29 = x27.first;

    x27.map(f
}
*/
