#!/bin/bash

BINDIR=${1}
SRCDIR=${2}

touch "failed.txt"

# run AIL on Coreutils binaries
pushd $SRCDIR

for i in $(find $BINDIR -executable -type f); do
    if [[ $i != *.out ]]; then
        python ${SRCDIR}/uroboros.py $i > ${SRCDIR}/$i.res 2>&1
        echo -e $i ":\t\t\t\t\t" $(tail -n1 ${SRCDIR}/$i.res)
        mv ${SRCDIR}/a.out ${SRCDIR}/$i.out
    fi
done

popd

# try to run all recompiled binaries
pushd $BINDIR

for i in $(ls *.out); do
    echo "$i..."
    ./$i --help
    if [ $? -eq 0 ]; then
    else
        echo "$i" >> "failed.txt"
    fi
done

popd

if [[ -s "failed.txt" ]]; then
    echo "failed on programs:"
    cat "failed.txt"
    exit 1
fi
