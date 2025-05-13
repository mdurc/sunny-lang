#!/bin/zsh

echo "Readme sample"
./a.out tests/readme_sample_code.sn > out.txt
diff out.txt tests/expected/rsc.txt

echo "\nDeclaration and assignments"
./a.out tests/declarations_assignments.sn > out.txt
diff out.txt tests/expected/da.txt

echo "\nContol flow"
./a.out tests/control_flow.sn > out.txt
diff out.txt tests/expected/cf.txt

echo "\nOrder of Operations"
./a.out tests/order_of_operations.sn > out.txt
diff out.txt tests/expected/order_ops.txt

echo "\nSymbol Table"
./a.out tests/symbol_table_tests.sn > out.txt
diff out.txt tests/expected/sym.txt
