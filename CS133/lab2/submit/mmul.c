#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


void mmul(float *A, float *B, float *C, int n){
  int rank,pnum,begin,end;
  int i,j,k,size;

  MPI_Comm_size(MPI_COMM_WORLD, &pnum);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  size = n / pnum; //the length of each processor responsible for
  begin = rank*size;

  float * bufferA;
  float * bufferB;
  float * bufferC;

  bufferA = malloc(sizeof(float) * size * n);
  bufferC = malloc(sizeof(float) * size * n);

  if(rank == 0) 
    bufferB = B;
  else
    bufferB = malloc(sizeof(float) * n * n);

  MPI_Scatter(A,n*size,MPI_FLOAT,bufferA,size*n,MPI_FLOAT,0,MPI_COMM_WORLD);
  MPI_Bcast(bufferB,n*n,MPI_FLOAT,0,MPI_COMM_WORLD);
  
  for(i = 0; i < size; i++){
    for(j = 0; j < n; j++){
        bufferC[i*n+j] = 0.0;
    }
  }
  
  for(i = 0; i < size; i++){
    for (k = 0; k < n; k++) {
      for (j = 0; j < n; j++) {
	bufferC[i*n + j] += bufferA[i*n + k] * bufferB[k*n + j];
      }
    }
  }

  MPI_Gather(bufferC,size*n,MPI_FLOAT,C,size*n,MPI_FLOAT,0,MPI_COMM_WORLD);
  
}
