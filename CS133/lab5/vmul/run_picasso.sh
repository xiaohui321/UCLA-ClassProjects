#!/bin/sh

$proj=`echo $1 | cut -d '.' f1`

if [[ $proj == "" ]]; then
	exit 1
fi

rm -rf $proj
picasso $proj.tcl
cp $proj/pkg/arm/zc706/bin/vmul1.xclbin ~/farmer_home/vmul
