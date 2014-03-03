#!/bin/bash

rm ./test/*.ppm
rm *.exe

g++ -o raytrace.exe raytrace.cpp vecm.h matm.h

./raytrace.exe ./test/tcb.txt
./raytrace.exe ./test/testAmbient.txt
./raytrace.exe ./test/testBackground.txt
./raytrace.exe ./test/testBehind.txt
./raytrace.exe ./test/testDiffuse.txt
./raytrace.exe ./test/testIllum.txt
./raytrace.exe ./test/testImgPlane.txt
./raytrace.exe ./test/testIntersection.txt
./raytrace.exe ./test/testParsing.txt
./raytrace.exe ./test/testReflection.txt
./raytrace.exe ./test/testSample.txt
./raytrace.exe ./test/testShadow.txt
./raytrace.exe ./test/testSpecular.txt

mv *.ppm ./test