# test
import os
import sys

with open("core_list") as f:
    lines = f.readlines()

# for l in lines[0:1]:
for l in lines:
    print '--------------', l.strip(), '------------------------'
    # os.system("rm " + l.strip())
    sys.stdout.flush()
    # os.system("rm " + l.strip())
    # os.system("python uroboros.py /data/ail_data/coreutils_bin_815/" + l.strip())
    os.system("python uroboros.py /data/ail_data/32-bit-temp/src/" + l.strip())
    # os.system("cp a.out /data/ail_data/coreutils_bin_815_new/" + l.strip())
    os.system("cp a.out /data/ail_data/coreutils_bin_815_new_32/" + l.strip())
    print '-----------------------------------------------------'
    sys.stdout.flush()
