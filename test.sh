#!/bin/bash
for i in supplied/lattests/{good,extensions/{struct,objects1,objects2}}/*.lat; do
    echo $i
    ./latc_x86 $i
    execPath="${i%/*}/a.out"
    if [ -r "${i%%.lat}.input" ]; then
        $execPath < ${i%%.lat}.input > tmp.out
    else
        $execPath > tmp.out
    fi
    diff -bB tmp.out ${i%%.lat}.output
done
