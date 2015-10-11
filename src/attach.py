lines = []

with open('temp') as f:
	lines = f.readlines()


def help(l):
	l = l.strip()
	if l.endswith(':'):
		l = l[:-1]
	l = ".long " + l
	return l
lines = map(lambda l : help(l), lines)


print "        .section        .ctors,\"aw\",@progbits"
for l in lines:
	print l
