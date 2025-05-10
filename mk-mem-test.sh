#!/bin/zsh

if [ "$#" -ne 1 ]; then
    echo "Usage: $0 <arg>"
    exit 1
fi

make clean && make && leaks --atExit -- ./a.out "$1"
