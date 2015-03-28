#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <omp.h>

#define A(i, k) A[i*n+k]
#define B(k, j) B[k*n+j]
#define C(i, j) C[i*n+j]
#define Cans(i, j) Cans[i*n+j]

void mmul(float *A, float *B, float *C, int n);

void mmul1(float *A, float *B, float *C, int n)
{
	int i, j, k;
#pragma omp parallel for private(j,k)
	for (i=0; i<n; i++) {
		for (j=0; j<n; j++)
			C(i,j) = 0;

		for (k=0; k<n; k++) {
			for (j=0; j<n; j++) {
				C(i,j) += A(i,k)*B(k,j);
			}
		}
	}
}

float compute_diff(float *C, float *Cans, int n)
{
	int cnt = 0;
	int i, j;
	float diff = 0.0;
	float power = 0.0;
#pragma omp parallel for private(j) reduction(+:diff,power)
	for (i=0; i<n; i++) {
		for (j=0; j<n; j++) {
			diff += (C(i,j)-Cans(i,j))*(C(i,j)-Cans(i,j));
			power += Cans(i,j)*Cans(i,j);
		}
	}
	return (diff/power);
}

int main( int argc, char** argv ) {

	int pnum, pid;
	double elapsed_time;
	float *A, *B, *C, *Cans;
	float diff;
	int n = 2048;
	int i, j, k;
	unsigned short seed[3];

	if( argc != 1 ){
		if( argc == 2 ){
			n = atoi(argv[1]);
		}
		else{
			printf("mmul [n]\n");
			exit(0);
		}
	}

	MPI_Init(&argc, &argv);
	MPI_Comm_size(MPI_COMM_WORLD, &pnum);
	MPI_Comm_rank(MPI_COMM_WORLD, &pid);
	//printf("Processor %d out of %d says hi!\n", pid, pnum);

	if( pid == 0 ){
		//printf("Intializing matrix size : %d x %d x %d\n", n, n, n);

		A = (float*)malloc( sizeof(float) * n * n );
		B = (float*)malloc( sizeof(float) * n * n );
		C = (float*)malloc( sizeof(float) * n * n );

		seed[0] = 0; seed[1] = 1; seed[2] = 2;

		for (i=0; i<n; i++) {
			for (k=0; k<n; k++) {
				A(i,k) = (float)erand48(seed);
			}
		}
		for (k=0; k<n; k++) {
			for (j=0; j<n; j++) {
				B(k,j) = (float)erand48(seed);
			}
		}
	}

	MPI_Barrier(MPI_COMM_WORLD);
	elapsed_time = -1*MPI_Wtime();
	
	//Please modify the content of this function
	mmul(A, B, C, n);

	MPI_Barrier(MPI_COMM_WORLD);
	elapsed_time += MPI_Wtime();

	if( pid == 0 ){
		printf("Elapsed Time : %f secs\n", elapsed_time);

		Cans = (float*)malloc( sizeof(float) * n * n );
		mmul1(A, B, Cans, n);
		diff = compute_diff(C, Cans, n);

	    printf("Performance  : %.2f GFlops\n", 2.0*n*n*n/elapsed_time/1000000000 );
		printf("Result Diff  : %.3f\%\n", diff*100 );

		free(A);
		free(B);
		free(C);
		free(Cans);
	}

	MPI_Finalize();

	return 0;	
}
