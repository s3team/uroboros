Please use Uroboros (version 0.11) for the tests below. 

# Build

To build Uroboros, please run the building script

    ./build

# Coreutils:

Before the checking, please download Coreutils (version 8.15) online and make sure all the tests are passed (./configure; make; make check).

we provide a script (iter_test.py) to iterate the test cases. Please take a look, and it is very understandable.
    
    python iter_test.py >> dump.info 2>&1 

This script reads binary names from file "core_list", disassemble/reassemble each of them. Please double-check the following three binaries.

(1). make sure you correctly translate binary "[", since this unique name may cause some issue in different bashes (but works fine in the zsh I use).

(2). Binary "test" is renamed into "test_bin", since binary name "test" can cause some trouble to Uroboros. Please rename it back to "test" before checking. 

(3). When re-compile, binary "stdbuf" needs to use some linker scripts. We present steps to translate this binary below.

So what you need to do is:

    rm coreutils-815-bin-new/stdbuf
    mv coreutils-815-bin-new/test_bin  coreutils-815-bin-new/test
    cp coreutils-815-bin-new/*  /data/coreutils_8.15/src/
    cd /data/coreutils_8.15/  
    make check                     =====>  you should not see any fail/error at this step.


To test stdbuf, please leverage the linker scripts uploaded [here](https://www.dropbox.com/sh/hkmpj91dvt30mcy/AAC-P6rsf_gQh2g3stEgguBfa?dl=0) and follow the steps below:

for 32-bit stdbuf:

    python uroboros.py /data/ail_data/32-bit-temp/src/stdbuf
    readelf -SW /data/ail_data/32-bit-temp/src/stdbuf
    vim ld_stdbuf.sty

     In this step, please update the .plt section address in the
     linker script with the address read from the "readelf".

    gcc -T ld_stdbuf.sty final.s -m32     

for 64-bit stdbuf:

    python uroboros.py /data/ail_data/coreutils_bin_815/stdbuf
    readelf -SW /data/ail_data/32-bit-temp/src/stdbuf
    vim ld_stdbuf64.sty

      In this step, please update the .plt section address in the
      linker script with the address read from the "readelf".

    gcc -T ld_stdbuf64.sty final.s

Then you can put the reassembled binary into the Coreutils original folder and make check.


# SPEC 2006:

We use SPEC2006 version 1.0 for the experiments. SPEC2006 is a commercial software and we probably can not share with you the whole package. But we would like to provide the original binaries, the re-assembled binaries, as well as some assembly programs and linker scripts for your reference. You can find them [here](https://www.dropbox.com/sh/hkmpj91dvt30mcy/AAC-P6rsf_gQh2g3stEgguBfa?dl=0). Note that this is for *research purpose only*.


## commands for 32-bit spec programs:
    python uroboros.py /data/test_binaries/spec_2006_32_bit/bzip2_base.i386-m32-gcc42-nn
    python uroboros.py /data/test_binaries/spec_2006_32_bit/hmmer_base.i386-m32-gcc42-nn
    python uroboros.py /data/test_binaries/spec_2006_32_bit/perlbench_base.i386-m32-gcc42-nn
    python uroboros.py /data/test_binaries/spec_2006_32_bit/sjeng_base.i386-m32-gcc42-nn
    python uroboros.py /data/test_binaries/spec_2006_32_bit/mcf_base.i386-m32-gcc42-nn
    python uroboros.py /data/test_binaries/spec_2006_32_bit/libquantum_base.i386-m32-gcc42-nn
    python uroboros.py /data/test_binaries/spec_2006_32_bit/milc_base.i386-m32-gcc42-nn
    python uroboros.py /data/test_binaries/spec_2006_32_bit/lbm_base.i386-m32-gcc42-nn
    python uroboros.py /data/test_binaries/spec_2006_32_bit/sphinx_livepretend_base.i386-m32-gcc42-nn
    python uroboros.py /data/test_binaries/spec_2006_32_bit/h264ref_base.i386-m32-gcc42-nn -a 3
    python uroboros.py /data/test_binaries/spec_2006_32_bit/gcc_base.i386-m32-gcc42-nn -a 3

    python uroboros.py /data/test_binaries/spec_2006_32_bit/gobmk_base.i386-m32-gcc42-nn -a 2 -a 3

gobmk needs a linker script. Note that there are false positive reported in Wang et al., and we have provided a post-process script to fix them (gobmk_sub.py).

Since compiler would put some bytes at the beginning of the data sections (discussed in the last paragraph of Sec. 5.6 Wang et al.[1]). Before re-compiling, another task is to delete a few leading bytes in the {.rodata; .bss; .data} sections. We put the adjusted assembly program (final_gobmk32.s) in package SPEC2006_32_bit_translated.gz for your reference. Please "diff" to see how we delete a few leading bytes in the data sections.

After deleting the lead bytes, you can compile the assembly program by using:

    gcc -T ld_gobmk32.sty final.s -m32 -lm


## commands for 64-bit spec programs:

    python uroboros.py /data/test_binaries/spec_2006_64_bit/bzip2_base.amd64-m64-gcc42-nn
    python uroboros.py /data/test_binaries/spec_2006_64_bit/h264ref_base.amd64-m64-gcc42-nn
    python uroboros.py /data/test_binaries/spec_2006_64_bit/perlbench_base.amd64-m64-gcc42-nn -a 3
    python uroboros.py /data/test_binaries/spec_2006_64_bit/sjeng_base.amd64-m64-gcc42-nn
    python uroboros.py /data/test_binaries/spec_2006_64_bit/mcf_base.amd64-m64-gcc42-nn
    python uroboros.py /data/test_binaries/spec_2006_64_bit/libquantum_base.amd64-m64-gcc42-nn
    python uroboros.py /data/test_binaries/spec_2006_64_bit/sphinx_livepretend_base.amd64-m64-gcc42-nn
    python uroboros.py /data/test_binaries/spec_2006_64_bit/hmmer_base.amd64-m64-gcc42-nn
    python uroboros.py /data/test_binaries/spec_2006_64_bit/lbm_base.amd64-m64-gcc42-nn
    python uroboros.py /data/test_binaries/spec_2006_64_bit/milc_base.amd64-m64-gcc42-nn


    python uroboros.py /data/test_binaries/spec_2006_64_bit/gobmk_base.amd64-m64-gcc42-nn -a 2 -a 3
    python uroboros.py /data/test_binaries/spec_2006_64_bit/gcc_base.amd64-m64-gcc42-nn -a 2 -a 3


Same thing goes here, for gcc and gobmk, deleting several leading bytes. Please "diff" with the final_gobmk64.s and final_gcc64.s (they are in the SPEC2006_64_bit_translated.gz) to see how we delete a few leading bytes in the data sections.

the use linker scripts to compile:

    gcc -T ld_gcc64.sty final.s
    gcc -T ld_gobmk64.sty final.s -lm


## SPEC2006 Functionality Test:

We use the following commands.

    runspec --config=mytest.cfg --size={test; train; ref;} --noreportable --tune=base --iterations=1 BINARY_NAME


# Some other "real world" programs:

We use the following programs to test:

bc: 1.06 ; ctags: 5.8; gzip: 1.2.4; mongoose: 5.6; nweb: 23; oftpd: 0.3.7; thttpd: 2.26

commands:

    python uroboros.py /data/ail_project/AIL/thttpd-2.26/thttpd
    python uroboros.py /data/ail_project/AIL/nweb/nweb
    python uroboros.py /data/ail_project/AIL/oftpd-0.3.7/src/oftpd
    python uroboros.py /data/ail_project/AIL/mongoose/examples/web_server/web_server
    python uroboros.py /data/ail_project/AIL/bc-1.06/bc/bc
    python uroboros.py /data/ail_project/AIL/gzip/gzip124/gzip
    python uroboros.py /data/ail_project/AIL/ctags-5.8/ctags


[1] Reassembleable Disassembling. Shuai Wang, Pei Wang, and Dinghao Wu. In Proceedings of the 24rd USENIX Security Symposium (USENIX Security '15), 2015.

---
