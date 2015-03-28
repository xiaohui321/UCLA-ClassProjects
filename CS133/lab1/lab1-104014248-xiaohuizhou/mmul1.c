#include "const.h"
#include <stdlib.h>
//my third approach
void mmul1(float A[ni][nk], float B[nk][nj], float C[ni][nj]){
  int i,j,k;
  omp_set_num_threads(32);
  float** B2 = (float**) malloc(nj * sizeof(float*));
  #pragma omp parallel for shared(B2) private(k)
  for(j = 0; j <nj;j++){
    B2[j] = (float*) malloc(nk * sizeof(float));
    for(k = 0; k < nk; k++){
      B2[j][k] = B[k][j];
    }
  }

#pragma omp parallel for shared(A,B2,C) private(i,j,k) schedule(guided)
  for(i = 0; i < ni; i++){
    for(j = 0; j < nj; j++){
      float result = 0;
      for(k = 0; k < nk; k++){
	result += A[i][k] * B2[j][k];
      }
      C[i][j] = result;
    }
  }

}

//my second approach
/*
void mmul1(float A[ni][nk], float B[nk][nj], float C[ni][nj]){
  int i,j,k;
  omp_set_num_threads(32);
#pragma omp parallel for shared(A,B,C) private(i,j,k) schedule(guided)
  for (i=0; i<ni; i++){
    for(k = 0; k<nk;k++){
      C[i][k] = 0;
    }
    for (k=0; k<nk; k++) {
      for (j=0; j<nj; j++) {
	C[i][j]  += A[i][k]*B[k][j];
      }
    }
  } 
}

*/
