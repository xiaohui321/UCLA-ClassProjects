
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <unistd.h>
#include <assert.h>
#include <stdbool.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <CL/opencl.h>

#define A(i, j) A[(i)*n+(j)]

int load_file_to_memory(const char *filename, char **result) { 

	int size = 0;
	FILE *f = fopen(filename, "rb");
	if (f == NULL) 
	{ 
		*result = NULL;
		return -1; // -1 means file opening fail 
	} 
	fseek(f, 0, SEEK_END);
	size = ftell(f);
	fseek(f, 0, SEEK_SET);
	*result = (char *)malloc(size+1);
	if (size != fread(*result, sizeof(char), size, f)) 
	{ 
		free(*result);
		return -2; // -2 means file reading fail 
	} 
	fclose(f);
	(*result)[size] = 0;

	return size;
}

int main(int argc, char** argv) {

	int		n = 256;
	int		i, j;

    struct	timeval t1, t2, tr;

	if (argc>2)
		n = atoi(argv[2]);

	float *A = (float*)malloc(n*n*sizeof(float));
	float *x = (float*)malloc(n*sizeof(float));
	float *y = (float*)malloc(n*sizeof(float));
	float *y_cl = (float*)malloc(n*sizeof(float));

    gettimeofday(&t1, NULL);
	srand(tr.tv_sec);

	for (i=0; i<n; i++) {
		for (j=0; j<n; j++) {
			A(i, j) = (float)rand()/RAND_MAX;
		}
		x[i] = (float)rand()/RAND_MAX;
	}


	for (i=0; i<n; i++) {
		y[i] = 0;
		for (j=0; j<n; j++) {
			y[i] += A(i, j)*x[j];
		}
	}

	// OpenCL
	// Use this to check the output of each API call
    cl_int status;  
	cl_int numDevices = 1;
	
	// Connect to first platform
    cl_platform_id platform;
    status = clGetPlatformIDs(1, &platform, NULL);

	if (status != CL_SUCCESS) {
		printf("Error: Failed to find an OpenCL platform!\n");
		return -1;
	}
 
	char cBuffer[1024];
	clGetPlatformInfo(platform, CL_PLATFORM_VENDOR, sizeof(cBuffer), cBuffer, NULL);
	printf("CL_PLATFORM_VENDOR %s\n", cBuffer);

	clGetPlatformInfo(platform, CL_PLATFORM_NAME, sizeof(cBuffer), cBuffer, NULL);
	printf("CL_PLATFORM_NAME %s\n", cBuffer);

    cl_device_id device;
	status = clGetDeviceIDs(platform, CL_DEVICE_TYPE_ACCELERATOR, 1, &device, NULL);

	if (status != CL_SUCCESS) {
		printf("Error: Failed to create a device group!\n");
		return -1;
	}

	cl_long maxBufferSize = 0;
	status = clGetDeviceInfo(device, CL_DEVICE_MAX_MEM_ALLOC_SIZE, sizeof(cl_long), &maxBufferSize, NULL);
	printf("max buffer size: %lld\n", maxBufferSize);

    // Create a context and associate it with the devices
    cl_context context;
    context = clCreateContext(NULL, numDevices, &device, NULL, NULL, &status);
	

	if (status != CL_SUCCESS) {
		printf("Error in creating context, code %d\n", status);
		return -1;
	}
    // Create a command queue and associate it with the device 
    cl_command_queue cmdQueue;
    cmdQueue = clCreateCommandQueue(context, device, 0, &status);

	if (status != CL_SUCCESS) {
		printf("Error in creating command queue for a device, code %d\n", status);
		return -1;
	}
	/*
    // Create a program with source code
    cl_program program = clCreateProgramWithSource(context, 1, (const char**)&vmul_cl, NULL, &status);

    // Build (compile) the program for the device
    status = clBuildProgram(program, numDevices, devices, 
        NULL, NULL, NULL);
	*/

	// Load binary from disk
	unsigned char *kernelbinary;
	char *xclbin = argv[1];
	printf("loading %s\n", xclbin);
	int n_i = load_file_to_memory(xclbin, (char **) &kernelbinary);
	if (n_i < 0) {
		printf("ERROR: failed to load kernel from xclbin: %s\n", xclbin);
		return -1;
	}
	size_t n_bit = n_i;

	// Create the compute program from offline
	cl_program program = clCreateProgramWithBinary(context, 1, &device, &n_bit,
			(const unsigned char **) &kernelbinary, NULL, &status);
	if ((!program) || (status != CL_SUCCESS)) {
		printf("Error: Failed to create compute program from binary %d!\n", status);
		return -1;
	}

	// Build the program executable
	status = clBuildProgram(program, 0, NULL, NULL, NULL, NULL);

	if (status != CL_SUCCESS) {
		size_t len;
		char buffer[2048];

		printf("Error: Failed to build program executable!\n");
		clGetProgramBuildInfo(program, device, CL_PROGRAM_BUILD_LOG, sizeof(buffer), buffer, &len);
		printf("%s\n", buffer);
		return -1;
	}

	// Create the vector addition kernel
    cl_kernel kernel;
    kernel = clCreateKernel(program, "vmul", &status);

	// Create OpenCL buffers
    cl_mem bufA;
    cl_mem bufx;
    cl_mem bufy;

    bufA = clCreateBuffer(context, CL_MEM_READ_ONLY, n*n*sizeof(float), NULL, &status);
	if (status != CL_SUCCESS) {
		printf("clCreateBuffer 1 error: %d\n", status);
		return -1;
	}

    bufx = clCreateBuffer(context, CL_MEM_READ_ONLY, n*sizeof(float), NULL, &status);
    bufy = clCreateBuffer(context, CL_MEM_WRITE_ONLY, n*sizeof(float), NULL, &status);

    // Associate the input and output buffers with the kernel 
    status = clSetKernelArg(kernel, 0, sizeof(int), &n);
    status = clSetKernelArg(kernel, 1, sizeof(cl_mem), &bufA);
    status = clSetKernelArg(kernel, 2, sizeof(cl_mem), &bufx);
    status = clSetKernelArg(kernel, 3, sizeof(cl_mem), &bufy);

    status = clEnqueueWriteBuffer(cmdQueue, bufA, CL_FALSE, 0, n*n*sizeof(float), A, 0, NULL, NULL);
    status = clEnqueueWriteBuffer(cmdQueue, bufx, CL_FALSE, 0, n*sizeof(float), x, 0, NULL, NULL);

	size_t work_size[1] = {0};
	size_t group_size[1] = {1};

	work_size[0] = n;

    gettimeofday(&t1, NULL);
	// Execute the kernel for execution
    status = clEnqueueNDRangeKernel(cmdQueue, kernel, 1, NULL, work_size, group_size, 0, NULL, NULL);

	if (status != CL_SUCCESS) {
		printf("Error in clEnqueue, code %d\n", status);
		return -1;
	}

    // Read the device output buffer to the host output array
    clEnqueueReadBuffer(cmdQueue, bufy, CL_TRUE, 0, n*sizeof(float), y_cl, 0, NULL, NULL);

    gettimeofday(&t2, NULL);
    timersub(&t1, &t2, &tr);

	double diff = 0.0;

	for (i=0; i<n; i++) {
		if (i<8) {
			printf("%d: %f, %f\n", i, y[i], y_cl[i]);
		}
		diff += abs(y[i]-y_cl[i]); 	
	}

	printf("Diff = %f\n", diff);
    printf("Execute time: %.5f sec\n", fabs(tr.tv_sec+(double)tr.tv_usec/1000000.0));

	free(A);
	free(x);
	free(y);

	return 0;
}
