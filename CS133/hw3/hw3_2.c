/******************************************************************************
 * FILE: hw3_2.c
 * CS 133 Winter 2015 
 * Homework 3
 * DESCRIPTION:
 *   Two implementations for a simplified version of MPI_Scatter
 *    
 * AUTHOR: Xiaohui, Zhou
 * LAST REVISED: 02/15/2015
 ******************************************************************************/
#include <mpi.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <omp.h>

#define N 204800

int MPI_Scatter_Hypercube (float *sendbuf, float *recvbuf, int size){
  int pid, pnum, d;
  int i,j;
  MPI_Status stat;
  
  MPI_Comm_size(MPI_COMM_WORLD, &pnum);
  MPI_Comm_rank(MPI_COMM_WORLD, &pid);

  if( pid != 0) 
    sendbuf = (float*) malloc( sizeof(float) * size * pnum);
  
  d = pnum / 2;
 
  while(d > 0){
    if(pid % (d*2)  == 0){
      MPI_Send(sendbuf + size * d,size * d, MPI_FLOAT,pid + d,0,MPI_COMM_WORLD);
    }else if(pid % d == 0){
      MPI_Recv(sendbuf,size * d, MPI_FLOAT,pid - d,0,MPI_COMM_WORLD, &stat);
    }
    d = d/2;
  }

  for(i = 0; i < size; i++)
    recvbuf[i]  = sendbuf[i];
}


int MPI_Scatter_Mesh (float *sendbuf, float *recvbuf, int size){ 
  int pid, pnum, d;
  int i,j;
  MPI_Status stat;
  
  MPI_Comm_size(MPI_COMM_WORLD, &pnum);
  MPI_Comm_rank(MPI_COMM_WORLD, &pid);

  if( pid != 0) 
    sendbuf = (float*) malloc( sizeof(float) * size * pnum);

  d = pnum / 2;
 
  while(d > 0){
    if(pid % (d*2)  == 0){
      MPI_Send(sendbuf + size * d,size * d, MPI_FLOAT,pid + d,0,MPI_COMM_WORLD);
    }else if(pid % d == 0){
      MPI_Recv(sendbuf,size * d, MPI_FLOAT,pid - d,0,MPI_COMM_WORLD, &stat);
    }
    d = d/2;
  }

  for(i = 0; i < size; i++)
    recvbuf[i]  = sendbuf[i];
}

int main(int argc, char** argv){
  int pnum, pid,size,i;
  double elapsed_time_built_in, elapsed_time_mesh, elapsed_time_hypercube;
  unsigned short seed[3];
  float *A, *B1, *B2, *B3, *B4;

  MPI_Init(&argc, &argv);
  MPI_Comm_size(MPI_COMM_WORLD, &pnum);
  MPI_Comm_rank(MPI_COMM_WORLD, &pid);

  size = N/pnum;
  B1 = (float*) malloc( sizeof(float) * size);
  B2 = (float*) malloc( sizeof(float) * size);
  B3 = (float*) malloc( sizeof(float) * size);
  B4 = (float*) malloc( sizeof(float) * size);

  if(pid == 0){
    A = (float*) malloc( sizeof(float) * N);
    seed[0] = 0; seed[1] = 1; seed[2] = 2;
    for(i = 0; i < N; i++){
      A[i] = (float)erand48(seed);
    }
  }

  //to make to result be more accurate, added this meaningless block
  MPI_Barrier(MPI_COMM_WORLD);
  MPI_Scatter(A,size,MPI_FLOAT,B4,size,MPI_FLOAT,0,MPI_COMM_WORLD);
  MPI_Barrier(MPI_COMM_WORLD);
  MPI_Scatter(A,size,MPI_FLOAT,B4,size,MPI_FLOAT,0,MPI_COMM_WORLD);
  MPI_Barrier(MPI_COMM_WORLD);
  
  
  elapsed_time_built_in = -1*MPI_Wtime();
  MPI_Scatter(A,size,MPI_FLOAT,B1,size,MPI_FLOAT,0,MPI_COMM_WORLD);
  MPI_Barrier(MPI_COMM_WORLD);
  elapsed_time_built_in += MPI_Wtime();

  elapsed_time_mesh = -1*MPI_Wtime();
  MPI_Scatter_Mesh(A,B2,size);
  MPI_Barrier(MPI_COMM_WORLD);
  elapsed_time_mesh += MPI_Wtime();

  elapsed_time_hypercube = -1*MPI_Wtime();
  MPI_Scatter_Hypercube(A,B3,size);
  MPI_Barrier(MPI_COMM_WORLD);
  elapsed_time_hypercube += MPI_Wtime();


  for(i = 0; i < size; i++){
    if(B1[i] != B2[i] || B1[i] != B3[i]){
      printf("wrong result\n");
      exit(1);
    }
  }

  if(pid == 0){
    printf("Built in  Method runtime : %.1f ns\n", 1000000000 * elapsed_time_built_in);
    printf("Mesh      Method runtime : %.1f ns\n", 1000000000 * elapsed_time_mesh);
    printf("Hypercube Method runtime : %.1f ns\n", 1000000000 * elapsed_time_hypercube);
    free(A);
  }

  free(B1);
  free(B2);
  free(B3);
  free(B4);
  MPI_Finalize();
}

