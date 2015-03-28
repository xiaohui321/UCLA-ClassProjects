CC=g++

EXEDIR	= .

SORT_EXEC = sort

LINK	 = -O3 -g -lm -I$(OPENCL_ROOT)/include -L$(OPENCL_ROOT)/lib64 -lOpenCL

all: $(EXEDIR)/$(SORT_EXEC) 

$(EXEDIR)/$(SORT_EXEC): burstSort.h burstSort.cpp sort.cpp burstsort_cl.h
	$(CC) -o $(EXEDIR)/$(SORT_EXEC) burstSort.cpp sort.cpp $(LINK)

clean:
	rm -rf $(EXEDIR)/$(SORT_EXEC) *.o *~ *.gch 

runSerial:
	./sort  10000000 input.txt output.txt -serial

runParallel:
	./sort  10000000 input.txt output.txt

check:
	./64/valsort output.txt
