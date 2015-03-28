create_project -name vmul -platform zc706-linux-uart -force
add_host_src -filename "vmul_ocl.c" -type source
compile_host -arch arm -cflags "-g -Wall -D FPGA_DEVICE"
build_sdimage
exit
