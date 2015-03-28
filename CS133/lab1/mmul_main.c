#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <sys/time.h>
#include <omp.h>
#include "const.h"

void mmul1(float A[ni][nk], float B[nk][nj], float C[ni][nj]);
void mmul2(float A[ni][nk], float B[nk][nj], float C[ni][nj]);

float A[ni][nk], B[nk][nj], Cans[ni][nj], C[ni][nj];

void mmul0(float A[ni][nk], float B[nk][nj], float C[ni][nj]){
  int i, j, k;
  for (i=0; i<ni; i++) {
    for (j=0; j<nj; j++) {
      C[i][j] = 0;
      for (k=0; k<nk; k++) {
	C[i][j] += A[i][k]*B[k][j];
      }
    }
  }
}

float compute_diff(float C[ni][nj], float Cans[ni][nj])
{
  int cnt = 0;
  int i, j;
  float diff = 0.0;
  for (i=0; i<ni; i++) {
    for (j=0; j<nj; j++) {
      diff += (C[i][j]-Cans[i][j])*(C[i][j]-Cans[i][j]);
    }
  }
  return diff;
}

int main(int argc, char** argv) {

  // timer structs
  struct	timeval ts, te, td;
  float tser, tpar, diff;
  int i, j, k;

  printf("matrix size : %d x %d x %d\n", ni, nj, nk);
  srand(0);
	
  // initialization
  for (i=0; i<ni; i++) {
    for (k=0; k<nk; k++) {
      A[i][k] = (float)rand()/RAND_MAX;
    }
  }
  for (k=0; k<nk; k++) {
    for (j=0; j<nj; j++) {
      B[k][j] = (float)rand()/RAND_MAX;
    }
  }

  // serial computation
  gettimeofday(&ts, NULL);
  mmul0(A, B, Cans);
  gettimeofday(&te, NULL);
  timersub(&ts, &te, &td);
  tser = fabs(td.tv_sec+(float)td.tv_usec/1000000.0);

  // parallel computation
  gettimeofday(&ts, NULL);
  mmul2(A, B, C);
  gettimeofday(&te, NULL);
  timersub(&ts, &te, &td);
  tpar = fabs(td.tv_sec+(float)td.tv_usec/1000000.0);

  // compare results
  diff = compute_diff(C, Cans);

  printf("Performance : %.2f GFlop/s (%.1fX)\n", 2.0*ni*nj*nk/tpar/1000000000, tser/tpar );
  printf("Result Diff : %.3f\n", diff );

  return 0;
}
