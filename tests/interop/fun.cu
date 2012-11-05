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
    
int fun(device_vector<float> v) {
    thrust::transform(v.begin(), v.end(), thrust::negate());
}
/*****************************************
  End of Generated Code                  
*******************************************/
