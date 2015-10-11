import sys, os

fn = sys.argv[1]

os.system('file ' + fn + ' > elf.info')

def check_32():
    lines = []
    with open("elf.info") as f:
        lines = f.readlines()
    if "ELF 64-bit" in lines[0]:
        return False
    else:
        return True


def check_exe():
    lines = []
    with open("elf.info") as f:
        lines = f.readlines()
    if "LSB shared object" in lines[0]:
        return False
    else:
        return True

is_exe = check_exe()

if is_exe == False: # share library
    pass
else:
    is_32 = check_32()

    os.system('objdump -Dr -j .text '+fn + " > "+fn+".temp")

    lines = []

    with open(fn+".temp") as f:
        lines = f.readlines()

    ll = len(lines)
    main_symbol = ""
    has_found = False

    for i in range(ll):
        l = lines[i]
        # when not using O2 to compile the original binary, we will remove all the _start code,
        # including the routine attached on the original program. In that case, we can not discover the
        # main function
        if has_found == False and "<__libc_start_main@plt>" in l:
            if check_32() == True:
    	        main_symbol = lines[i-1].split()[-1]
                has_found = True
            else:
                main_symbol = lines[i-1].split()[-1].split(',')[0]
                has_found = True
    	#lines[i-1] = lines[i-1].replace(main_symbol, "main")
    	#main_symbol = main_symbol[1:].strip()
    	#print main_symbol

    ## Some of the PIC code/module rely on typical pattern to locate
    ## such as:

    ##	804c460: push   %ebx
    ##	804c461: call   804c452 <__i686.get_pc_thunk.bx>
    ##	804c466: add    $0x2b8e,%ebx
    ##	804c46c: sub    $0x18,%esp

    ## What we can do this pattern match `<__i686.get_pc_thunk.bx>` and calculate
    ## the address by plusing 0x2b8e and  0x804c466, which equals to the begin address of GOT.PLT table

    ## symbols can be leveraged in re-assemble are
    ##	_GLOBAL_OFFSET_TABLE_   ==    ** .got.plt **
    ##	....


    os.system('rm main.info')

    main_symbol =main_symbol.split('0x')[1]

    with open("main.info", 'w') as f:
        f.writelines('S_0x'+main_symbol.upper()+"\n")
