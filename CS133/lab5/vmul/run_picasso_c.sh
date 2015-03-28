#!/bin/sh

rm -rf vmul_host
picasso run_picasso_c.tcl
cp vmul_host/pkg/arm/zc706/bin/vmul_host.exe ~/farmer_home/vmul
