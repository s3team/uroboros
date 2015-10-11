import os
from sets import Set

def check_exe():
    lines = []
    with open("elf.info") as f:
        lines = f.readlines()
    if "LSB shared object" in lines[0]:
        return False
    else:
        return True

def check_32():
    lines = []
    with open("elf.info") as f:
        lines = f.readlines()
    if "32-bit" in lines[0]:
        return True
    else:
        return False



def reassemble():
    if check_32() == True:
        # 32-bit binary
        os.system('gcc final.s -lm -lrt -lpthread -lcrypt -m32 2> final.error')
    else:
        # 64-bit binary
        os.system('gcc final.s -lm -lrt -lpthread -lcrypt 2> final.error')


def parse_error():
    errors = []
    if os.path.isfile('final.error'):
        with open("final.error") as f:
            errors = f.readlines()
            addrs = []
        def help(l):
            if 'In function' in l:
                pass
            elif 'undefined reference' in l and 'S_0x' in l:
                addrs.append(l.split()[-1][1:-1])
        map(lambda l : help(l), errors)


        return Set(addrs)


def modify(errors):
    lines = []
    with open("final.s") as f:
        lines = f.readlines()
    def help(l):
        e = filter(lambda e : e in l, errors)
        if e != []:
            addr = e[0][2:]
            #print "undefined label : "+addr
            l = l.replace(e[0], addr)
        return l
    lines = map(lambda l : help(l), lines)
    with open("final.s", 'w') as f:
        f.writelines(lines)


def main():
    print "     modify final.s to adjust redundant symbols"
    #os.system('gcc final.s -lm 2> final.error')
    #os.system('gcc final.s -lm -m32 2> final.error')
    reassemble()
    errors = parse_error()
    modify(errors)
    print "     modify finished"

if __name__ == '__main__':
    main()
