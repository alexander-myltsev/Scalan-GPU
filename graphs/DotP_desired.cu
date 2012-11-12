  val dotP = (arrs: Rep[(Array[Float], Array[Float])]) => {
    val Pair(a, b) = arrs
    val v1 = fromArray(a)
    val v2 = fromArray(b)
    dotProduct(v1, v2)
  }

//;
int test(tuple<host_vector<float>*, host_vector<float>*> x18) {
  host_vector<float>* x19 = tuple::get<0>(x18);
  host_vector<float>* x20 = tuple::get<1>(x18);
  
  
      
