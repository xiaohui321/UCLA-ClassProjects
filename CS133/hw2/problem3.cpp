#include <stdlib.h>
#include <stdio.h>
#include <omp.h>

#define N 1000

int main(){
  char * result = malloc( N / 8);
  memeset(result,0,N);
  unsigned int p = 2;
  unsigned int i;
  omp_set_num_threads(32);
  
  while(p < N){
    #pragma omp parallel for
    for(i = p * p; i < N; i += p){
      result[i/8] ^= 1 << (i % 8);
    }
    for(i = p + 1; i < N; i++){
      if((result[i/8] >> (i % 8) & 1) == 0){
	p = i;
	break;
      }
    }
  }
  int count = 0;
  for(i = N - 1; count < 3 && i > 0; i--){
    if((result[i/8] >> (i % 8) & 1) == 0){
      count ++;
      cout << i << " ";
    }
  }  
  cout << endl;
  delete[] result;
  return 0;
}
