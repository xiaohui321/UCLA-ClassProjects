#!/bin/bash

rm hw3_2
mpicc -o hw3_2 hw3_2.c -lmpi -lm -O3

if [[ $? == "0" ]]; then
	mpirun -np 16 hw3_2
fi

