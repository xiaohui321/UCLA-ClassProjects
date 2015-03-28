create_project -name mmult2 -platform zc706-linux-uart -force
add_host_src -filename "test-cl.c" -type source
compile_host -arch arm -cflags "-g -Wall -D FPGA_DEVICE"
build_sdimage
exit
