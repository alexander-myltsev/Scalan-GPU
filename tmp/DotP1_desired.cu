  val dotP1 = (arr: Rep[Array[Int]]) => {
    val v = fromArray(arr)
    sum(v)
  }

class PArrayBase<T> {

}

class Functor<T> {
 T operator() {

 }
}

class Functor<A, T> {
 T operator(A a) {

 }
}

BaseElement<T> : ??? {
    PArrayBase<T> tabulate(int arr_length, Functor<int, T> fun) {
        for i = 0 .. arr_length - 1
    }

    PArrayBase<T> replicate(int count, T value) {
        размножает value количество раз = count (можно через tabulate, но неэффективно)
    }
}

template <class T>
class BaseArray<T> : PArrayBase<T> {
private:
    device_vector<T> _data;
public:
    BaseArray(host_vector<T> x) {
        _data = x;
    }

    T sum(.....

// Wrong because isn't Scala-like
    template <class Plus>
    T sum() {
        return thrust::reduce(_data.begin(), _data.end(), 0, thrust::plus<T>());
    }

    template <class Mul>
    T sum() {
        return thrust::reduce(_data.begin(), _data.end(), 0, thrust::plus<T>());
    }
}

int test(host_vector<int>* x5) {
BaseArray<int> x6(x5);
int x7 = x6.sum<Plus>();
return x7;
}
