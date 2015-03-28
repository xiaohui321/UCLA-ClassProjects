#!/bin/bash

rm mmul
mpicc -o mmul *.c -lmpi -lm -O3 -fopenmp

if [[ $? == "0" ]]; then
	mpirun -np 16 mmul 
fi

