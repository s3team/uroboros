def check_32():
	lines = []
	with open("elf.info") as f:
		lines = f.readlines()
	if "32-bit" in lines[0]:
		return True
	else:
		return False

lines = []

with open("init_array.info") as f:
    lines = f.readlines()


start_index = 0

for i in range(len(lines)):
	l = lines[i]
	if "not found" in l:
		start_index = -1
	elif "Contents of section" in l:
		start_index = i+1
	else:
		continue

lines = lines[start_index:]

ctors = [] # C++ global ctors

def not_hex(d):
	try:
		int(d,16)
		return False
	except:
		return True

def parse(item):
	h1 = item[7:].upper()
	h2 = item[4:6].upper()
	h3 = item[2:4].upper()
	h4 = item[0:2].upper()
	addr = h1 + h2 + h3 + h4
	return "S_0x"+addr.lstrip('0')

def help(l):
	global ctors
	items = l.strip().split()
	addr = items[0]
	for item in items[1:]:
		if len(item) != 8 or not_hex(item):
			break
		else:
			label = parse(item)
			if len(label) == 4: continue
			ctors.append(label+"\n")
map(help, lines)

with open("init_array_new.info", 'w') as f:
    f.writelines(ctors)
