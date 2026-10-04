set -v 
exec 2>&1
# convert BOZ (binary, octal, hexadecimal) values to base10
base zFF o777 U+FF b111111
# convert base2 values to base10
base 1010 101010 101010101010 -ibase 2
# convert base2 values to base10 in brief mode
base 10 1010 101010 10101010 1010101010 101010101010 -ibase 2 -brief
# convert base10 values to base2
base 2 42 2730 -obase 2
# convert base10 values to base3
base 10 20 50 -obase 3
# convert values of various explicit bases to base10
base 2:11 3:1212 4:123123
base "2#11 3#1212 4#123123"
# convert values of various explicit bases to base2 in brief mode
base 2:1111 3:10 4:10 8:10 16:10 --obase 2 --brief
# convert a value to multiple bases
base 16:FF -obase 2 8 16
base 16:FF -obase boz
