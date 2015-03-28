#include "const.h"
#include <stdlib.h>
void mmul2(float A[ni][nk], float B[nk][nj], float C[ni][nj]){
  int BLOCK_SIZE = 64;
  int bi,bj,oi,oj,ok,i,j;
  float ** A2 = (float**) malloc(BLOCK_SIZE * sizeof(float*));
  float ** B2 = (float**) malloc(BLOCK_SIZE * sizeof(float*));
  
  omp_set_num_threads(4);
  #pragma omp parallel for
  for(i = 0; i < BLOCK_SIZE; i++){
    A2[i] = (float*) malloc(BLOCK_SIZE * sizeof(float));
    B2[i] = (float*) malloc(BLOCK_SIZE * sizeof(float));
  }

  for(bi = 0; bi < i/BLOCK_SIZE; bi++ ){
    for(bj = 0; bj < j/BLOCK_SIZE; bj++ ){
      for(i = nk * BLOCK_SIZE * bi, j = BLOCK_SIZE * bj;
	  i < nk * BLOCK_SIZE * (bi+1);
	  i+=BLOCK_SIZE, j+= BLOCK_SIZE * nj){
	
	//copy a block of matrix
#pragma omp parallel for shared(A,B,A2,B2,i,j,bi,bj) private(oi,oj) 
	for(oi = 0; oi<BLOCK_SIZE;oi++){
	  for(oj = 0; oj<BLOCK_SIZE;oj++){
	    A2[oi][oj] = A[oi][i + oj];
	    B2[oj][oi] = B[oi][j + oj];
	  }
	}
	
#pragma omp parallel for shared(A,B,A2,B2,i,j,bi,bj) private(oi,oj,ok) 
	for(oi = 0; oi<BLOCK_SIZE;oi++){
	  for(oj = 0; oj<BLOCK_SIZE;oj++){
	    float result = 0;
	    for(ok = 0; ok <BLOCK_SIZE; ok++){
	      result += A2[oi][ok] * B2[oj][ok];
	    }
	    C[BLOCK_SIZE * bi + oi][BLOCK_SIZE * bj + oj] += result;
	  }
	}
      }
    }
  }
}
