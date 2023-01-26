NAME
====

seq- - \[FUNIX\] print a sequence of numbers (LICENSE:PD)

SYNOPSIS
========

seq- LAST \[OPTION\] \`\`\` seq- FIRST LAST \[OPTION\] \`\`\` seq- FIRST
INCREMENT LAST \[OPTION\] \`\`\`

DESCRIPTION
===========

Print numbers from FIRST to LAST, in steps of INCREMENT.

OPTIONS
=======

**FIRST,LAST,INCREMENT**

:   The values have the following restrictions

    -   If FIRST or INCREMENT is omitted, they will default to 1.

    -   The sequence ends when the sum of the current number and
        INCREMENT would become greater than LAST.

    -   FIRST, INCREMENT, and LAST are interpreted as floating point
        values.

    -   INCREMENT must be positive if FIRST is smaller than LAST or no
        output is produced.

    -   INCREMENT must be negative if FIRST is greater than LAST or no
        output is produced.

    -   INCREMENT must not be 0.

    -   none of FIRST, INCREMENT and LAST may be NaN.

****-f****

:   FORMAT The Fortran FORMAT used to print the values. Use a
    floating-point Fortran FORMAT for printing values unless FIRST,
    LAST, and INCREMENT are all whole numbers. Then the format must be
    suitable for printing one argument of type INTEGER. The format
    defaults to "(g0,/)"

****--help****

:   display this help and exit

****--version****

:   output version information and exit

EXAMPLE
=======

sample usage

       seq- 1 1 10 -f '(i0)'
       12345678910

       seq- 1 1 3
       1
       2
       3

       seq- 1 -1 -10 -f '(i0.3,":")'
       001:000:-001:-002:-003:-004:-005:-006:-007:-008:-009:-010:

AUTHOR
======

John S. Urban

LICENSE
=======

Public Domain
