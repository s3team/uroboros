import sys

lines = []

with open("final_data.s") as f:
    lines = f.readlines()

label, value, size, first_time, label_pos = sys.argv[1:]
size = int(size, 16)
first_time = int(first_time)

lines.append("\n")
if first_time: lines.append(".section .uroboros_plugin_data,\"aw\",@progbits\n")
 
if label_pos == 'before' : lines.append(label + " : \n")
for k in range(size):
    lines.append(".byte " + value + "\n")
if label_pos == 'after' : lines.append(label + " : \n")

with open('final_data.s', 'w') as f:
    f.writelines(lines)