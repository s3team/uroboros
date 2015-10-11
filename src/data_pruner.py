# this tool is used to make data section heuristic more accurate
# in data sections, we can find single refrence like this:
# ...
# .byte 0x80
# .byte 0x14
# .byte 0x24
# .long S_0x8124142
# .byte 0x00
# .byte 0x14
#
# In the above case, it is very likely S_0x8124142 is actually
# .byte 0x42
# .byte 0x41
# .byte 0x12
# .byte 0x08
#  We should refine this, according to to heriustic judgement
#
#
#
#     1. this address don't have a label ahead:
#			S_label : .long S_0x8124142 :  this is perhaps a global pointer
#
#     2. this address don't have neighbour label:
#           .long S_0x8148148
#			.long S_0x8148152
#
# This is obviously a ``Chicken-egg" problem, and
# theoritically, it can not be 100% percent accurate unless we do a iteration correctness.
#
# But it only has very low possiblity to fail
#

import sys, os


lines = []
with open('final_data.s') as f:
	lines = f.readlines()

secs1 = {}

secs = []
with open('sections.info') as f:
	secs = f.readlines()

for sec in secs:
	items = sec.split()
	n = items[0]
	n1 = int(items[1], 16)
	n2 = int(items[3], 16)
	secs1[n] = (n1, n1+n2)


secs = []
with open('text_sec.info') as f:
	secs = f.readlines()

for sec in secs:
	items = sec.split()
	n = items[0]
	n1 = int(items[1], 16)
	n2 = int(items[3], 16)
	secs1[n] = (n1, n1+n2)

def get_secs (l):
	global secs1
	ns = l.split('S_')[1][2:]
	ns = int(ns.strip(), 16)
	for k in secs1.keys():
		if ns >= secs1[k][0] and ns < secs1[k][1]:
			return k

	print "can not found corresponding sections!!"

last_sec = ""

lines = filter(lambda l : l.strip() != "", lines)

for i in range(1,len(lines)-1):
	l = lines[i]
	if "S_" in l and ".byte" not in l:
		if len(l.split('S_')) > 2: # judgement 1
			l1 = l.split(':')[1]
			last_sec = get_secs(l1)
			continue
		ns = l.split('S_')[1][2:]
		ns = ns.strip()
		if "S_" in lines[i-1] or "S_" in lines[i+1]: #judgement 2
			last_sec = get_secs(l)
			continue
		cur_sec = get_secs(l)
		if cur_sec == last_sec:
			last_sec = cur_sec
			continue
		# failed
		print ns
		assert(len(ns) == 7)
		n1 = ".byte 0x" + ns[5:7]
		n2 = ".byte 0x" + ns[3:5]
		n3 = ".byte 0x" + ns[1:3]
		n4 = ".byte 0x0" + ns[0]
		lines[i] = n1+"\n"+n2+"\n"+n3+"\n"+n4+"\n"


with open('final_data.s', 'w') as f:
	f.writelines(lines)
