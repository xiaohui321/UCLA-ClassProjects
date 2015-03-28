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

void BurstSort::parallelSort(std::ofstream& file){
	char* buffer = NULL;
	char* tmp;
	int* posArray = NULL;
	int entryLength = KEY_LENGTH + sizeof(char*);
	std::cout  << entryLength;
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
	// Use this to check the output of each API call
	cl_int status;  

    // Retrieve the number of platforms
	cl_uint numPlatforms = 0;
	status = clGetPlatformIDs(0, NULL, &numPlatforms);

    // Allocate enough space for each platform
	cl_platform_id *platforms = NULL;
	platforms = (cl_platform_id*)malloc(numPlatforms*sizeof(cl_platform_id));

    // Fill in the platforms
	status = clGetPlatformIDs(numPlatforms, platforms, NULL);

	// Find GPU
	int platform_index = -1;
	for (int i = 0; i < numPlatforms; i++){
		char vendor[128];
		clGetPlatformInfo (platforms[i], CL_PLATFORM_VENDOR, sizeof(vendor), vendor, NULL);
		char vendorF[7];
		memcpy((void*)vendorF, (void*)vendor, 6);
		vendorF[6] = '\0';
		std::cout << vendorF << std::endl;
		if (strcmp(vendorF, "NVIDIA") == 0){
			platform_index = i;
			break;
		}
	}
	if (platform_index == -1){
		printf("Didn't find CPU platform!\n");
		exit(1);
	}

    // Retrieve the number of devices
	cl_uint numDevices = 0;
	status = clGetDeviceIDs(platforms[platform_index], CL_DEVICE_TYPE_ALL, 0, 
		NULL, &numDevices);

	printf("#devices: %d, status %d\n", numDevices, status);
    // Allocate enough space for each device
	cl_device_id *devices;
	devices = (cl_device_id*)malloc(
		numDevices*sizeof(cl_device_id));

    // Fill in the devices 
	status = clGetDeviceIDs(platforms[platform_index], CL_DEVICE_TYPE_ALL,        
		numDevices, devices, NULL);

    // Create a context and associate it with the devices
	cl_context context;
	context = clCreateContext(NULL, numDevices, devices, NULL, 
		NULL, &status);

    // Create a command queue and associate it with the device 
	cl_command_queue cmdQueue;
	cmdQueue = clCreateCommandQueue(context, devices[0], 0, 
		&status);

	cl_mem clBuffer;
	clBuffer = clCreateBuffer(context, CL_MEM_READ_WRITE, 
		sizeof(char) * size * entryLength, NULL, &status);

	cl_mem clPosArray;
	clPosArray = clCreateBuffer(context, CL_MEM_READ_ONLY, 
		sizeof(int) * (NODE_SIZE + 1), NULL, &status);

	status = clEnqueueWriteBuffer(cmdQueue, clPosArray, CL_FALSE, 
		0, sizeof(int) * (NODE_SIZE + 1),posArray, 0, NULL, NULL);

	status = clEnqueueWriteBuffer(cmdQueue, clBuffer, CL_FALSE, 
		0, sizeof(char) * size * entryLength, buffer, 0, NULL, NULL);

    // Create a program with source code
	cl_program program = clCreateProgramWithSource(context, 1, 
		(const char**)&burstsort_cl, NULL, &status);

    // Build (compile) the program for the device
	status = clBuildProgram(program, numDevices, devices, 
		NULL, NULL, NULL);

	cl_kernel kernel;
	kernel = clCreateKernel(program, "burstsort", &status);

    // Associate the input and output buffers with the kernel 
	status = clSetKernelArg(kernel, 0, sizeof(cl_mem), &clBuffer);

	status = clSetKernelArg(kernel, 1, sizeof(cl_mem), &clPosArray);

	int nodeSize = NODE_SIZE;
	status = clSetKernelArg(kernel, 2, sizeof(int), (void *)&nodeSize);

	status = clSetKernelArg(kernel, 3, sizeof(int), (void *)&entryLength);

    // Define an index space (global work size) of work 
    // items for execution. A workgroup size (local work size) 
    // is not required, but can be used.
	size_t globalWorkSize[1];   

    // There are 'elements' work-items 
	globalWorkSize[0] = NODE_SIZE;

    // Execute the kernel for execution
	status = clEnqueueNDRangeKernel(cmdQueue, kernel, 1, NULL, 
		globalWorkSize, NULL, 0, NULL, NULL);

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
