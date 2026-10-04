
[![fpm](https://img.shields.io/badge/fpm-install-blue?logo=fortran)](https://fpm.fortran-lang.org/registry/package/M_strings)

# base

## NAME

   base(1f) - [CONVERT] convert numbers between bases
   (LICENSE:PD)

## SYNOPSIS
   base values [ --ibase NN][--obase MM][--brief]|[[--help]|[--version]]

## DESCRIPTION

   base(1) converts a whole number in base IBASE into a number in
   base OBASE (IBASE and OBASE must be between 2 and 36).

   The letters A,B,...,Z represent 10,11,...,36 in a base > 10.

   The number is first converted from base IBASE to base 10 by
   DecodeBase(3f), then converted from base 10 to base OBASE by
   CodeBase(3f).

## OPTIONS

    values      space-delimited strings representing input values
    --ibase NN  default base for input values
                If the input values are whole numbers that are of the
                form NN:MMMM or NN#MMMM where NN is the base and MMMM the
                value the numbers are interpreted in the explicit base
                the numbers represent. Otherwise, they are interpreted
                as in base NN. NN defaults to 10
    --obase MM  bases for output values. The Default is 10. Values must
                be in the range from 2 to 36 or the keyword 'all' or 'boz'
    --brief     just show output value with no base designation
    --help      display this help and exit
    --version   output version information and exit

    Having a pound character (#) in an input line is problematic, as most
    shell programs and the M_kracken(3f) command line parser independently
    treat the character as beginning an in-line comment. To avoid using
    pound characters one may use the colon instead when using explicit
    base numbers in values.

## EXAMPLE

  Sample runs:

```text
# convert BOZ (binary, octal, hexadecimal) values to base10
base zFF o777 U+FF b111111
 > zFF            = 10#255
 > o777           = 10#511
 > U+FF           = 10#255
 > b111111        = 10#63

# convert base2 values to base10
base 1010 101010 101010101010 -ibase 2
 > 2#1010           = 10#10
 > 2#101010         = 10#42
 > 2#101010101010   = 10#2730

# convert base2 values to base10 in brief mode
base 10 1010 101010 10101010 1010101010 101010101010 -ibase 2 -brief
 > 2 10 42 170 682 2730

# convert base10 values to base2
base 2 42 2730 -obase 2
 > 10#2              = 2#10
 > 10#42             = 2#101010
 > 10#2730           = 2#101010101010

# convert base10 values to base3
base 10 20 50 -obase 3
 > 10#10             = 3#101
 > 10#20             = 3#202
 > 10#50             = 3#1212

# convert values of various explicit bases to base10
base 2:11 3:1212 4:123123
 > 2:11           = 10#3
 > 3:1212         = 10#50
 > 4:123123       = 10#1755
base "2#11 3#1212 4#123123"
 > 2#11           = 10#3
 > 3#1212         = 10#50
 > 4#123123       = 10#1755

# convert values of various explicit bases to base2 in brief mode
base 2:1111 3:10 4:10 8:10 16:10 --obase 2 --brief
 > 1111 11 100 1000 10000

# convert a value to multiple bases
base 16:FF -obase 2 8 16
 > 16:FF          = 2#11111111
 > 16:FF          = 8#377
 > 16:FF          = 16#FF
base 16:FF -obase boz
 > 16:FF          = 2#11111111
 > 16:FF          = 8#377
 > 16:FF          = 16#FF
```
## AUTHOR
   John S. Urban
## LICENSE
   Public Domain
