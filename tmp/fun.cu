/*****************************************
  Emitting Generated Code                  
*******************************************/

#include <thrust/device_vector.h>
#include <thrust/transform.h>
#include <thrust/sequence.h>
#include <thrust/copy.h>
#include <thrust/fill.h>
#include <thrust/replace.h>
#include <thrust/functional.h>
#include <iostream>

using namespace thrust;

int test(device_vector<int>* x5) {
int x7 = thrust::reduce(x5->begin(), x5->end(), 0, thrust::plus<int>());
return x7;
}
/*****************************************
  End of Generated Code                  
*******************************************/
