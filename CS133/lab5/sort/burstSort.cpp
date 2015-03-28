#include "burstSort.h"
#include "burstsort_cl.h"

BurstSort::BurstSort(int count){
	size = count;
	for(int i = 0; i < NODE_SIZE; i++){
		nodes[i].size = 0;
		nodes[i].used = 0;
	}
}

BurstSort::~BurstSort(){
	for(int i = 0; i < NODE_SIZE; i++){
		for(int j = 0; j < nodes[i].used;j++){
			free(nodes[i].entries[j]);
		}
		if(nodes[i].size != 0) free(nodes[i].entries);
	}
}

void BurstSort::insert(char* entry){
	int nodeNum = (entry[0] - ' ') * ALPHABET_SIZE + (entry[1] - ' ');
	if(nodes[nodeNum].size == 0){
		nodes[nodeNum].size = NODE_INITIAL_SIZE;
		nodes[nodeNum].entries = (char**) malloc(nodes[nodeNum].size * sizeof(char*));
	}else if(nodes[nodeNum].size == nodes[nodeNum].used){
		nodes[nodeNum].size += NODE_INCREMENT_SIZE;
		nodes[nodeNum].entries = (char**) realloc(nodes[nodeNum].entries,nodes[nodeNum].size * sizeof(char*));
	}

	nodes[nodeNum].entries[nodes[nodeNum].used] = (char*) malloc(sizeof(char) * (ENTRY_LENGTH + 1));
	strcpy(nodes[nodeNum].entries[nodes[nodeNum].used],entry);
	nodes[nodeNum].used ++;
}

void BurstSort::sortAndPrint(bool serial,std::ofstream& file){
	if(serial){
		for(int i = 0; i < NODE_SIZE; i++){
			quickSort(i,0,nodes[i].used -1);

		}
		for(int i = 0; i < NODE_SIZE; i++){
			for(int j = 0; j < nodes[i].used; j++){
				file << nodes[i].entries[j];
			}
		}
	}else{ 
		parallelSort(file);
	}
}

void BurstSort::getKey(char* key, char* line){
	for(int i = 0; i < KEY_LENGTH; i++){
		key[i] = line[i];
	}
	key[KEY_LENGTH] = '\0';
}

void BurstSort::quickSort(int nodeNum, int left, int right){
	if(right <= left) return;
	int i = left;
	int j = right;
	char* x = nodes[nodeNum].entries[(i+j)/2];
	char* tmp;
	do {
		while((std::strcmp(nodes[nodeNum].entries[i],x) < 0) && (i < right)) {
			i++;
		}
		while((std::strcmp(nodes[nodeNum].entries[j],x) > 0) && (j > left)) {
			j--;
		}
		if(i <= j) {
			tmp  = nodes[nodeNum].entries[i];
			nodes[nodeNum].entries[i] = nodes[nodeNum].entries[j];
			nodes[nodeNum].entries[j] = tmp;
			i++;
			j--;
		}
	} while(i <= j);

	if(left < j) {
		quickSort(nodeNum, left, j);
	}
	if(i < right) {
		quickSort(nodeNum, i, right);
	}
}

void BurstSort::copyString(char* a, char *b, int length){
	for(int i = 0; i < length; i++){
		b[i] = a[i];
	}
}

void BurstSort::swapEntry(int nodeNum, int a, int b){
	for(int i = 0; i < ENTRY_LENGTH; i++){
		char c = nodes[nodeNum].entries[a][i];
		nodes[nodeNum].entries[a][i] = nodes[nodeNum].entries[b][i];
		nodes[nodeNum].entries[b][i] = c;
	}
}

int BurstSort::compareKey(char* p1, char * p2){
	while (*p1 == *p2 ) {
		p1++;
		p2++;
	}

	return *p1 - *p2;
}


int BurstSort::load_file_to_memory(const char *filename, char **result) { 

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

void BurstSort::parallelSort(std::ofstream& file){
	char* buffer = NULL;
	char* tmp;
	int* posArray = NULL;
	int entryLength = KEY_LENGTH + sizeof(char*);
	buffer = (char*) malloc(sizeof(char) * size * entryLength);
	posArray = (int*) malloc(sizeof(int) * (NODE_SIZE + 1));
	int pos = 0;
	posArray[0] = 0;
	for(int i = 0; i < NODE_SIZE; i++){
		for(int j = 0; j < nodes[i].used; j++){
			memcpy(buffer + pos * entryLength, nodes[i].entries[j], KEY_LENGTH * sizeof(char));
			memcpy(buffer + pos * entryLength + KEY_LENGTH, &nodes[i].entries[j], sizeof(char*));
			pos += sizeof(char);
		}
		posArray[i+1] = pos;
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

	// Load binary from disk
	unsigned char *kernelbinary;
	char *xclbin = "sort_xiaohui.xclbin";
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
    kernel = clCreateKernel(program, "sort", &status);


	cl_mem clPosArray;
	cl_mem clBuffer;
	clBuffer = clCreateBuffer(context, CL_MEM_READ_WRITE, 
		sizeof(char) * size * entryLength, NULL, &status);

	clPosArray = clCreateBuffer(context, CL_MEM_READ_ONLY, 
		sizeof(int) * (NODE_SIZE + 1), NULL, &status);
	
	status = clEnqueueWriteBuffer(cmdQueue, clPosArray, CL_FALSE, 
		0, sizeof(int) * (NODE_SIZE + 1),posArray, 0, NULL, NULL);

	status = clEnqueueWriteBuffer(cmdQueue, clBuffer, CL_FALSE, 
		0, sizeof(char) * size * entryLength, buffer, 0, NULL, NULL);


    // Associate the input and output buffers with the kernel 
	status = clSetKernelArg(kernel, 0, sizeof(cl_mem), &clBuffer);

	status = clSetKernelArg(kernel, 1, sizeof(cl_mem), &clPosArray);

	int nodeSize = NODE_SIZE;
	status = clSetKernelArg(kernel, 2, sizeof(int), (void *)&nodeSize);

	status = clSetKernelArg(kernel, 3, sizeof(int), (void *)&entryLength);

	size_t globalWorkSize[1];   

	globalWorkSize[0] = NODE_SIZE;

    gettimeofday(&t1, NULL);
	// Execute the kernel for execution
    status = clEnqueueNDRangeKernel(cmdQueue, kernel, 1, NULL, globalWorkSize, NULL, 0, NULL, NULL);

	if (status != CL_SUCCESS) {
		printf("Error in clEnqueue, code %d\n", status);
		return -1;
	}


    // Read the device output buffer to the host output array
	clEnqueueReadBuffer(cmdQueue, clBuffer, CL_TRUE, 0, 
		sizeof(char) * size * entryLength, buffer, 0, NULL, NULL);

    // Free OpenCL resources
	clReleaseKernel(kernel);
	clReleaseProgram(program);
	clReleaseCommandQueue(cmdQueue);
	clReleaseMemObject(clBuffer);
	clReleaseMemObject(clPosArray);
	clReleaseContext(context);

    //print result
	for(int i = 0; i < size; i+= sizeof(char)){
		memcpy(&tmp,buffer + i * entryLength + KEY_LENGTH,sizeof(char*));
		file << tmp;
	}

    // Free host resources
	free(buffer);
	free(posArray);

	free(platforms);
	free(devices);

}
