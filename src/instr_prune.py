# this is part of the libc standard functions, usually attached at the beginning of the 
# .text sections
# functions belong to this set : _start; __do_global_dtors_aux; frame_dummy; 
		#............
		# push %ecx
		# push %esi
		# push main
		# call __libc_start_main  :   i - 12
		# BB_0:
		# S_0x8048330:
		# ....
		# S_0x80483AF : add $0x4,%esp
		# pop %ebx
		# pop %ebp
		# ret
		# nop;nop;nop;nop;nop;nop;nop;
		# nop;nop;nop;nop;nop;nop;nop;
		# BB_9:
		# S_0x80483C0:
		# push %ebp
		# mov %esp,%ebp
		# sub $0x18,%esp
		# mov 0x8049F24,%eax
		# test %eax,%eax
		# je S_0x80483E1
		# BB_10:
		# mov $0x0,%eax
		# test %eax,%eax
		# je S_0x80483E1
		# BB_11:
		# movl $0x8049F24,(%esp)         : --> .jcr  section beginning address
		# call *%eax
		# BB_12:

		# S_0x80483E1 : leave
		# ret
		# nop

# this is the part of the libc standard functions, usually attached at the end of the .text sections:

#functions belong to this set : __do_global_ctors_aux; __i686.get_pc_thunk.bx; __libc_csu_fini; __libc_csu_init;
# but actually function atexit can also contain pattern like `GLOBAL_OFFSET_TABLE`, so we might not be able to use it

		# __libc_csu_init:

		# S_0x8048490 : push %ebp
		# push %edi
		# push %esi
		# push %ebx
		# call __i686.get_pc_thunk.bx
		# BB_23:
		# add $_GLOBAL_OFFSET_TABLE_,%ebx  : --> identify this label , addr - 6   : NO!!!
		# sub $0x1C,%esp
		# .....
		# BB_30:
		# __libc_csu_fini:
		# S_0x8048500 : repz
		# ret
		# BB_31:
		# __i686.get_pc_thunk.bx:
		# mov (%esp),%ebx
		# ret
		# BB_32:
		# __do_global_ctors_aux:
		# push %ebp
		# mov %esp,%ebp
		# push %ebx
		# sub $0x4,%esp
		# mov 0x8049F14,%eax
		# cmp $0xFFFFFFFF,%eax
		# je S_0x8048534
		# BB_33:
		# mov $0x8049F14,%ebx   : --> identify .ctors section; this label, addr + 14
		# xchg %ax,%ax
		# BB_34:

		# S_0x8048528 : sub $0x4,%ebx
		# call *%eax
		# BB_35:
		# mov (%ebx),%eax
		# cmp $0xFFFFFFFF,%eax
		# jne S_0x8048528

		# S_0x8048534 : add $0x4,%esp
		# pop %ebx
		# pop %ebp
		# ret
		# nop
		# nop

import sys, os

fn = sys.argv[1]
c = sys.argv[2]  
# in the first round, we get the binary in test folder while 
# the next N-1 rounds we get the binary in the current folder

def dump_sections(fn):
	if c == 1:
		path = './test/'+fn 
	else:
		path = './' + fn
	os.system('readelf -S '+path+' > sec1.info')
	lines = []
	with open('sec1.info') as f:
		lines = f.readlines()
	ctors = 0
	jcr = 0 
	for l in lines:
		if '.jcr' in l:
			jcr = int(l.split()[3], 16)
			jcr = hex(jcr)[2:]
			jcr = '0x'+jcr.upper()
		elif '.ctors' in l:
			ctors = int(l.split()[3], 16)
			ctors = hex(ctors)[2:]
			ctors = '0x'+ctors.upper()

	return ctors, jcr

ctors, jcr = dump_sections(fn)

print '6: optimation --> reduce text segment size'

lines = []

with open('final.s') as f:
	lines = f.readlines()

lines.reverse() 

s1, e1, s2, e2 = 0, 0, 0, 0

found_sec_pattern = False
found_first_pattern = False
detect_sec_pattern = True
detect_first_pattern = True

for i in range(len(lines)):
	l = lines[i]
	if found_sec_pattern == False and detect_sec_pattern == True and ctors in l:
		# we are in the second zone
		s2 = i - 14
		found_sec_pattern = True 
	elif found_sec_pattern == True and detect_sec_pattern == True and '_GLOBAL_OFFSET_TABLE_' in l:
		# sometimes we just can not find nop above the matched function, let's just give it 
		detect_sec_pattern = False
		s2 = 0
	elif found_sec_pattern == True and detect_sec_pattern == True and 'nop' in l:
		e2 = i
		detect_sec_pattern = False
		print "     identify function __do_global_ctors_aux"
	elif found_first_pattern == False and detect_first_pattern == True and jcr in l:
		s1 = i - 5
		found_first_pattern = True 
	elif '__libc_start_main' in l and detect_first_pattern == True:
		detect_first_pattern = False
		e1 = i + 12
		print "     identify function _start; __do_global_dtors_aux; frame_dummy"


if s1 != 0 and e1 != 0:
	for i in range(s1, e1+1):
		lines[i] = ""

if s2 != 0 and e2 != 0:
	for i in range(s2, e2+1):
		lines[i] = ""

print "   remove these useless functions"

lines.reverse()

with open('final.s', 'w') as f:
	f.writelines(lines)

