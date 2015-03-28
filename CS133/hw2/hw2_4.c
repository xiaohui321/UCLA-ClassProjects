#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>

#define N 10000000
#define M 64

int main(){
  double x[M];
  double y[M];
  double d[M];
  int p = 0;
  int c = 0;
  int r = 0;
  int count = 0;

  clock_t t1, t2;
  t1 = clock();
  
  #pragma omp parallel sections
  {
    
    #pragma omp section
    {
      #pragma omp task //consumer who calculates distance
      {
	while(c < N){
	  while((c < (r + M)) && p > c){
	    d[c % M] = sqrt((x[c % M] - 0.5)*(x[c % M] - 0.5) + (y[c % M] - 0.5)*(y[c % M] - 0.5));
	    c ++;
	  }
	}
      }

      #pragma omp task //consumer who find if the point in the circle
      {
	while(r < N){
	  while(r < c){
	    if(d[r++ % M] <= 0.5) count++;
	  }
	}

      }
      
      //producer for x and y
      while(p < N){
	while(p < (c + M)){
	  x[p % M] = (double) rand()/RAND_MAX;
	  y[p % M] = (double) rand()/RAND_MAX;
	  p ++;
	}
      }
    }
  }

  t2 = clock();
  printf("%f ms\n%.7f\n", (double) (t2-t1)/CLOCKS_PER_SEC * 1000, 4.0/N * count);
}
