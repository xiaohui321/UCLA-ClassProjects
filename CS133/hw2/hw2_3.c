#include <stdlib.h>
#include <stdio.h>
#include <omp.h>
#include <string.h>
#include <time.h>
#include <math.h>

#define N 1000000000

int main(){
  char * result = malloc( N / 8 + 1);
  memset(result,0,N/8 + 1);
  
  unsigned int p = 2;
  unsigned int i;

  clock_t t1, t2;
  t1 = clock();
  omp_set_num_threads(32);
  while(p < sqrt(N)){
    #pragma omp parallel for
    for(i = p * p; i < N; i += p){
      result[i/8] |= (1 << (i % 8));
    }
    for(i = ++p; i < N; i++){
      if(((result[i/8] >> (i % 8)) & 1) == 0){
	p = i;
	break;
      }
    }
  }
  int count = 0;
  for(i = N - 1; count < 3 && i > 0; i--){
    if(((result[i/8] >> ( i % 8)) & 1) == 0){
      count ++;
      printf("%d ",i);
    }
  }
  printf("\n");
  
  t2 = clock();
  printf("%f ms\n",(double) (t2 - t1) / CLOCKS_PER_SEC * 1000);
  free(result);
  return 0;
}
