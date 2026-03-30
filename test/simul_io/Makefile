# compile simul_io

intel:
	gcc -no-pie -m32 simul_io.c -o simul_io.intel.32.nopie.dynamic.sym 
	gcc -no-pie -m64 simul_io.c -o simul_io.intel.64.nopie.dynamic.sym 
	gcc -no-pie -m32 simul_io.c -o simul_io.intel.32.nopie.static.sym -static
	gcc -no-pie -m64 simul_io.c -o simul_io.intel.64.nopie.static.sym -static
	gcc -pie -m32 simul_io.c -o simul_io.intel.32.pie.static.sym -static
	gcc -pie -m64 simul_io.c -o simul_io.intel.64.pie.static.sym -static

arm.thumb:
	arm-linux-gnueabihf-gcc -no-pie simul_io.c -o simul_io.arm.thumb.nopie.dynamic.sym 
	arm-linux-gnueabihf-gcc -no-pie simul_io.c -o simul_io.arm.thumb.nopie.static.sym -static

arm.64:
	aarch64-linux-gnu-gcc -no-pie simul_io.c -o simul_io.arm.64.nopie.dynamic.sym 
	aarch64-linux-gnu-gcc -no-pie simul_io.c -o simul_io.arm.64.nopie.static.sym -static

clean:
	find . -maxdepth 1 -type f -executable -exec sh -c 'file -b "$$1" | grep -q "^ELF"' _ {} \; -delete
