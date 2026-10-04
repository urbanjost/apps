NAME
====

digest(1f) - \[FUNIX\] compute SHA256 message digest (LICENSE:PD)

SYNOPSIS
========

digest FILE\`\`\`

DESCRIPTION
===========

digest(1f) prints SHA256 (256-bit) checksums. The sums are computed as
described in FIPS-180-2. For each regular file listed on the command
line a line is printed with the checksum, a byte count and the name of
the file.

It exercises the sha256(3f) routine. Note that it reads the files one at
a time into dynamically allocated memory.

OPTIONS
=======

****--auto\_test**,a**

:   run internal tests of routines in M\_sha3(3fm) module

EXAMPLES
========

Sample commands

        digest  *
         FF1A6FB532 ```. 22C3D6208360FF     1049831 c1-1-tirupathi.pdf
         F61B2FF27B ```. C3AB2CA72CA95B      109119 in-1
         B019112253 ```. 30B239057202EA        4591 newbugs.f90
         52E4A0D9AE ```. 4F299EDBC3C8C9        4505 record.sh

AUTHOR
======

John S. Urban

LICENSE
=======

Public Domain
