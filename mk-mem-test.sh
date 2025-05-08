#!/bin/zsh

make clean && make && leaks --atExit -- ./a.out tests/readme_sample_code.sn
#make clean && make && leaks --atExit -- ./a.out tests/declarations_assignments.sn
#make clean && make && leaks --atExit -- ./a.out tests/control_flow.sn
