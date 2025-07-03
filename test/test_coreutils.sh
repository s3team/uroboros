#!/bin/bash

BINDIR=$(pwd)/test/coreutils-32bit/src
SRCDIR=$(pwd)/src

# Define a list of programs to skip (e.g., dcgen, program2, program3)
SKIP_PROGRAMS=("dcgen" "libstdbuf.so")

# Define words to look for in the output
STOP_WORDS="Segmentation|failed"  # Use pipe to separate words

# run on coreutils binaries
pushd $SRCDIR

for i in $(find $BINDIR -executable -type f); do
    if [[ $i != *.out ]]; then
        # Check if the program is in the skip list
        if [[ " ${SKIP_PROGRAMS[@]} " =~ " $(basename $i) " ]]; then
            echo "Skipping $i..."
            continue
        fi
        
        # Run the program with uroboros.py and check output
        python3 ./uroboros.py $i >$i.res 2>&1
        echo -e $i ":\t\t\t\t\t" $(tail -n1 $i.res)

        # Check for stop words in the output using grep
        if grep -iqE "$STOP_WORDS" "$i.res"; then
            echo "Found a error."
            exit 1  # Terminate the script if the word is found
        fi
        
        # Check if a.out exists before moving it
        if [[ ! -f ./a.out ]]; then
            echo "Error: a.out not found. Exiting the script."
            cat "$i.res"
            exit 1  # Exit if a.out doesn't exist
        fi
        
        mv ./a.out $i.out
    fi
done

popd

# try to run all recompiled binaries
pushd $BINDIR

for i in $(ls *.out); do
    
    echo "$i..."
    
    # Run the program and check if it exits successfully
    ./$i --help >>$i.help
    if grep -iqE "$STOP_WORDS" "$i.help"; then
            echo "Found a error."
            exit 1  # Terminate the script if the word is found
    fi
    
    # Pause the script for user input (optional)
done

popd

pushd $BINDIR

rm *.res
rm *.out
rm *.help

popd

BINDIR=$(pwd)/coreutils-64bit/src
SRCDIR=$(pwd)/src

pushd $SRCDIR

for i in $(find $BINDIR -executable -type f); do
    if [[ $i != *.out ]]; then
        # Check if the program is in the skip list
        if [[ " ${SKIP_PROGRAMS[@]} " =~ " $(basename $i) " ]]; then
            echo "Skipping $i..."
            continue
        fi
        
        # Run the program with uroboros.py and check output
        python3 ./uroboros.py $i >$i.res 2>&1
        echo -e $i ":\t\t\t\t\t" $(tail -n1 $i.res)

        # Check for stop words in the output using grep
        if grep -iqE "$STOP_WORDS" "$i.res"; then
            echo "Found a error."
            exit 1  # Terminate the script if the word is found
        fi
        
        # Check if a.out exists before moving it
        if [[ ! -f ./a.out ]]; then
            echo "Error: a.out not found. Exiting the script."
            cat "$i.res"
            exit 1  # Exit if a.out doesn't exist
        fi
        
        mv ./a.out $i.out
    fi
done

popd

# try to run all recompiled binaries
pushd $BINDIR

for i in $(ls *.out); do
    
    echo "$i..."
    
    # Run the program and check if it exits successfully
    ./$i --help >>$i.help
    if grep -iqE "$STOP_WORDS" "$i.help"; then
            echo "Found a error."
            exit 1  # Terminate the script if the word is found
    fi
    
    # Pause the script for user input (optional)
done

popd

pushd $BINDIR

rm *.res
rm *.out
rm *.help

popd

echo "testing success."
