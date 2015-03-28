#include <fstream>
#include <iostream>
#include <cstddef>
#include <cmath>
#include <cstdlib>
#include <cstring>
#include <algorithm>
#include <CL/cl.h>

#define KEY_LENGTH 10
#define ENTRY_LENGTH 100
#define NUM_CHECKED_KEY 2
#define ALPHABET_SIZE 95
#define NODE_SIZE 9030  /* 95 ^ 2 = 9025*/
#define NODE_INCREMENT_SIZE 200
#define NODE_INITIAL_SIZE 1200

typedef struct Node{
	int size;
	int used;
	char** entries;
}Node;


class BurstSort{
public:
	BurstSort(int count);
	~BurstSort();
	void insert(char* entry);
	void sortAndPrint(bool serial, std::ofstream& file);
private:
	Node nodes[NODE_SIZE];
	int size;
	void getKey(char* key, char* line);
	void parallelSort(std::ofstream& file);
	void copyString(char* a, char *b, int length);
	void swapString(char* a, char *b, int length);
	void quickSort(int nodeNum, int left, int right);
	void swapEntry(int nodeNum, int a, int b);
	int compareKey(char * s1, char * s2);	
};
