int compareEntry(__global char * buffer, int p1, char * key, int entryLength){
	int p = 2;
	while(p < 10 && buffer[p1 * entryLength + p] == key[p]){
		p++;
	}
	return p == 10 ? 0 : buffer[p1 * entryLength + p] - key[ p];
}

__kernel
void burstsort(__global char *buffer, __global int *clPosArray, int nodeSize,int entryLength){
	int left,right,top,i,j,k,p,z,middle;
	int stack[100];
	char c;
	char middleKey[11];
	middleKey[10] = '\0';
	int idx = get_global_id(0);
	
	left = clPosArray[idx];
	right = clPosArray[idx + 1] - 1;
	top = -1;
	stack[++top] = left;
	stack[++top] = right;

	while(top > 0){
		right = stack[top--];
		left = stack[top--];
		i = left;
		j = right;
		if(right <= left) continue;
		middle = ((left + right)/2) * entryLength;
		for(int z = 0; z < 10; z++)
			middleKey[z] = buffer[ middle + z];

		do{
			while((compareEntry(buffer,i,middleKey,entryLength) < 0) && (i < right)) {
				i++;
			}
			while((compareEntry(buffer,j,middleKey,entryLength) > 0) && (j > left)) {
				j--;
			}
			if(i <= j) {
				for(k = 0; k <entryLength; k++){
					c = buffer[i*entryLength + k];
					buffer[i*entryLength + k] = buffer[j*entryLength + k];
					buffer[j*entryLength + k] = c; 
				}
				i++;
				j--;
			}
		}while(i <= j);

		if(left < j){
			stack[++top] = left;
			stack[++top] = j;
		}

		if(right > i){
			stack[++top] = i;
			stack[++top] = right;
		}

	}
} 
