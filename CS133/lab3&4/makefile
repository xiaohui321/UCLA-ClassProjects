CC=g++

EXEDIR	= .

SORT_EXEC = sort

LINK	 = -O3 -lm -g -I$(OPENCL_ROOT)/include -L$(OPENCL_ROOT)/lib64 -lOpenCL

all: $(EXEDIR)/$(SORT_EXEC) 

$(EXEDIR)/$(SORT_EXEC): burstSort.h burstSort.cpp sort.cpp burstsort.cl burstsort_cl.h
	$(CC) -o $(EXEDIR)/$(SORT_EXEC) burstSort.cpp sort.cpp $(LINK)

burstsort_cl.h: burstsort.cl
	./cl_trans.sh burstsort.cl

clean:
	rm -rf $(EXEDIR)/$(SORT_EXEC) *.o *~ *.gch 

runSerial:
	./sort  10000000 input.txt /w/class.2/cs/cs133/xiaohuiz/output.txt -serial

runParallel:
	./sort  10000000 input.txt /w/class.2/cs/cs133/xiaohuiz/output.txt

check:
	./64/valsort /w/class.2/cs/cs133/xiaohuiz/output.txt
