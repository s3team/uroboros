#!/bin/bash

BINDIR=/data/coreutils/src
SRCDIR=/AIL/src

# run on coreutils binaries
pushd $SRCDIR

for i in $(find $BINDIR -executable -type f); do
    if [[ $i != *.out ]]; then
        python ./uroboros.py $i >$i.res 2>&1
        echo -e $i ":\t\t\t\t\t" $(tail -n1 $i.res)
        mv ./a.out $i.out
    fi
done

popd

# try to run all recompiled binaries
pushd $BINDIR

for i in $(ls *.out); do
    echo "$i..."
    ./$i --help >>$i.help
done

popd
