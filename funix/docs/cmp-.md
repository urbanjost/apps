NAME
====

cmp-(1f) - \[FUNIX\] compare two files byte by byte (LICENSE:PD)

SYNOPSIS
========

cmp- file1 file2 \[ **-quiet**\] \[ **-show**\]\| \[ \[ **-help**\] \[
**-version**\] \]

DESCRIPTION
===========

The cmp-(1) utility compares two files byte by byte and returns true if
no differences are found, or false otherwise

OPTIONS
=======

The following options are supported:

**file1,file2**

:   names of files to compare

****-quiet****

:   suppress normal output

****-show****

:   show all bytes that are different until an end of file is
    encountered on both files

****-help****

:   Print description of this program.

****-version****

:   Print version information for this program

EXAMPLES
========

Given files "x1" and "x2" are identical, and file "x3" is the same
except one line is uppercase instead of lowercase \`\`\`

       $ cmp- x1 x2
         x1 x2  are identical

       $ cmp- x1 x3
         x1 x3 differ: byte 03814,  line 00086, ADE= 99 c miniscule c ADE= 67 C majuscule C
         STOP 3

       $ cmp- x1 x3 -show
         byte 03814,  line 00086, ADE= 99 c miniscule c              ADE= 67 C majuscule C
         byte 03815,  line 00086, ADE=111 o miniscule o              ADE= 79 O majuscule O
         byte 03816,  line 00086, ADE=110 n miniscule n              ADE= 78 N majuscule N
         byte 03817,  line 00086, ADE=116 t miniscule t              ADE= 84 T majuscule T
         byte 03818,  line 00086, ADE= 97 a miniscule a              ADE= 65 A majuscule A
         byte 03819,  line 00086, ADE=105 i miniscule i              ADE= 73 I majuscule I
         byte 03820,  line 00086, ADE=110 n miniscule n              ADE= 78 N majuscule N
         byte 03821,  line 00086, ADE=115 s miniscule s              ADE= 83 S majuscule S
         x1 x3  are different by 8 bytes

       $ cmp- x1 x3 -show -quiet
         3814            99    c  67    C
         3815           111    o  79    O
         3816           110    n  78    N
         3817           116    t  84    T
         3818            97    a  65    A
         3819           105    i  73    I
         3820           110    n  78    N
         3821           115    s  83    S

       $ cmp- x1 x2 -quiet
       $ echo $?
         0

       $ cmp- x1 x3 -quiet
         STOP 3
       $ echo $?
         3

EXIT STATUS
===========

The following exit values are returned:

> **0**
>
> :   no differences were found
>
> **1**
>
> :   differences were found
>
AUTHOR
======

John S. Urban

LICENSE
=======

Public Domain
