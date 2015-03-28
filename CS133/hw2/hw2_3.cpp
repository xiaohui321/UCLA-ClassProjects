#include <stdlib.h>
#include <stdio.h>
#include <omp.h>
#include <string.h>
#include <bitset>

#define N 1000000000

using namespace std;

int main(){
  bitset<N> result;

  unsigned int p = 2;
  unsigned int i;
  //  omp_set_num_threads(32);
  while(p < N){
    //#pragma omp parallel for
    for(i = p * p; i < N; i += p){
      result[i] = 1;
    }
    for(i = ++p; i < N; i++){
      if(result[i] == 0){
	p = i;
	break;
      }
    }
  }
  int count = 0;
  for(i = N - 1; count < 3 && i > 0; i--){
    if(result[i] == 0){
      count ++;
      printf("%d ",i);
    }
  }
  printf("\n");
  return 0;
}
