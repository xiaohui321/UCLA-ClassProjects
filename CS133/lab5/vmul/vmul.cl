#define A(i, j) A[(i)*n+(j)]

__kernel __attribute__ ((reqd_work_group_size(1, 1, 1)))
void vmul(
		int	n,
		__global float* A,                        
        __global float* x,                        
        __global float* y)                        
{                                                   
                                                    
	if (n>1024) return;
	int gid = get_group_id(0);                      

	__local float l_A[1024];
	__local float l_x[1024];

	int i, j;

	i = gid;
	//y[i] = gid_1 + gid_2;

	event_t e_memcpy[2];
	async_work_group_copy(l_A, A+i*n, (size_t)n, e_memcpy[0]);
	async_work_group_copy(l_x, x, (size_t)n, e_memcpy[1]);
	wait_group_events(2, e_memcpy);

	float l_res = 0;

	__attribute__((xcl_pipeline_loop))
	for (j = 0; j<n; j++) { 

		l_res += l_A[j]*l_x[j];
	}

	y[i] = l_res;
}
