NAME
====

hasher(1f) - \[M\_hashkeys\] exercise the string hash methods in the
M\_hashkey(3fm) module (LICENSE:PD)

SYNOPSIS
========

hasher \[ input\_files \[ **-hash** hashname\] \]\|\[ **-string**
string\_value\]\|**--help**\|**--version**

DESCRIPTION
===========

hasher(1f) does a byte by byte hash of a file or a hash of a string
using the procedures available in the M\_hashkey(3fm) module. It is up
to the user to determine if the method is suitable for a specific use.

OPTIONS
=======

**input\_files**

:   files to generate a hash for

**hash**

:   name of hash algorithm. Currently allowed values are:

<!-- -->

                      djb2 (default)   calls djb2_hash(3f)
                      sdbm             calls sdbm_hash(3f)
                      crc32            calls cfc32_hash(3f)

****--help****

:   display this help and exit

****--version****

:   output version information and exit

AUTHOR
======

John S. Urban

LICENSE
=======

Public Domain
