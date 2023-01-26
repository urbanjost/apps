NAME
====

tr-(1) - \[FUNIX:M\_strings\] translate or delete characters
(LICENSE:PD)

SYNOPSIS
========

tr- \[ **-o** SET1 \[ **-n** SET2 \]\]\|**-l**\| **-u** \|\[
**--version** \]\|\[ **--help** \]

DESCRIPTION
===========

Translate, squeeze, and/or delete characters from standard input,
writing to standard output.

OPTIONS
=======

****-o** SET1,**-n** SET2**

:   old set of characters and new set of characters to replace the old
    set.

    -   Each character in the input string that matches a character in
        the old set is replaced.

    -   If the new set is the empty set the matched characters are
        deleted.

    -   If the new set is shorter than the old set the last character in
        the new set is used to replace the remaining characters in the
        new set.

****-u****

:   convert to uppercase

****-l****

:   convert to lowercase

****--help****

:   display this help and exit

****--version****

:   output version information and exit

EXAMPLES
========

Sample usage

       #convert input to uppercase:
       tr- -u
       tr- -o 'abcdefghijklmnopqrstuvwxyz' -n 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'

       # out of  !"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~
       # just leave letters
       _ tr -o '!"#$%&'"'"'()*+,-./0123456789:;<=>?@[\]^_`{|}~'

AUTHOR
======

John S. Urban

LICENSE
=======

Public Domain
