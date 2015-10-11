import re, sys

lines = []


fn = sys.argv[1]

with open(fn) as f:
	lines = f.readlines()

lines = lines[2:-2]

for i in range(len(lines)):
	l = lines[i]
	if ";" in l:
		lines[i] = l.split(';')[0]


text_b = 0
text_e = 0

with open('text_sec.info') as f:
	l = f.readlines()[0]
	text_b = int(l.split()[1], 16)
	text_e = int(l.split()[3], 16) + text_b

tbl_b = int(lines[0].split()[0],16)
tbl_e = int(lines[-1].split()[0],16)


text_labels = []

local_labels = []

def update_label1 (d):
	if d >= text_b and d < text_e:
		text_labels.append("S_0x"+hex(d)[2:].upper()+"\n")
		return True
	elif d >= tbl_b and d < tbl_e:
		local_labels.append(d)
		return True
	else:
		return False


def update_label2 (s):
	addr = s.split('_')[1]
	update_label1(int(addr,16))
	return "S_0x"+addr.upper()

# addr, type, cont
parsed_ls = []

def pat_match1(s):
	pat1 = r'[0-9A-F]{7}h'
	ms = re.search(pat1, s)
	if ms:
		msd = int(ms.group(0)[:-1], 16)
		if update_label1(msd):
			s1 = "S_0x"+hex(msd)[2:].upper()
			s = s.replace(ms.group(0),s1)
		return s
	else:
		pat1 = r'[0-9A-F]+h' # pure number
		for mp in re.findall(pat1, s):
		    s = s.replace(mp,"0x"+mp[:-1])
		return s

def pat_match2(s):
	pat2 = r'[a-z]+_[0-9A-F]+'
	ms = re.search(pat2, s)
	if ms:
		if "S_" not in ms.group(0):
			t = ms.group(0)
		elif "S_" not in ms.group(1):
			t = ms.group(1)
		else:
			print "failed : "+s
		s1 = update_label2(t)
		s = s.replace(t,s1)
	return s

def pat_match3(s):
	def help(t,n):
		s = ""
		for i in range(0,int(t,16)):
			s += n + ", "
		return s[:-2]
	pat1 = r'\d+ dup\(\d\)'
	for mp in re.findall(pat1, s):
		pat2 = r'(\d+) dup\((\d)\)'
		mp1 = re.search(pat2, mp)
		t = mp1.group(1)
		n = mp1.group(2)
		s = s.replace(mp,help(t,n))
	return s

def parse(s):
	if "@@CXXABI" in s:
		return s.split('@')[0]
	else:
		s1 = pat_match1(s)
		s2 = pat_match2(s1)
		s3 = pat_match3(s2)
		return s3

def typ_trans(t):
	if 'dd' in t:
		return ".long"
	elif 'db' in t:
		return ".byte"
	elif 'string' in t:
		return t
	else:
		print "unsupported type trans : " + t

for l in lines:
	has_off = False
	is_str = False
	if "offset" in l:
		has_off = True
	l = l.replace('offset','')
	if "'" in l:
		print l
		l = l.replace("'", '"')
		is_str = True
	items = l.strip().split()
	# only have address
	if len(items) == 1: 
		continue
	addr = int(items[0],16)
	if "dd" == items[1] or "db" == items[1]:
		label = ""
		if has_off == False:
			typ = items[1]
		else:
			typ = "dd"
		cont = parse(' '.join(items[2:]))
	else:
		label = ""
		if has_off == False:
			typ = items[2]
		else:
			typ = "dd"
		cont = parse(' '.join(items[3:]))
	if is_str == True:
		cont = cont.replace(',0','')
		typ = '.string'
	parsed_ls.append([addr,label,typ,cont])

for i in range(0,len(parsed_ls)):
	l = parsed_ls[i]
	if l[0] in local_labels:
		l[1] = "S_0x"+hex(l[0])[2:].upper()+": "
	l[2] = typ_trans(l[2])
	parsed_ls[i] = l

# print parsed_ls

with open(fn+'.data', 'w') as f:
	f.write('.section        .'+fn+',"aw",@progbits\n.align 4\n')
	f.writelines(map(lambda l : l[1]+" "+l[2]+" "+l[3]+"\n", parsed_ls))

with open(fn+'.info', 'w') as f:
	f.writelines(set(text_labels))
