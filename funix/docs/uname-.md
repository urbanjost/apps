NAME
====

uname-(1f) - \[FUNIX\] print system information (LICENSE:PD)

SYNOPSIS
========

uname- \[OPTION\]\`\`\`

DESCRIPTION
===========

Print certain system information. With no OPTION, print all information,
one value per line.

****-a**, **--all****

:   print all information, in the following order:

****-s**, **--kernel-name****

:   print the kernel name

****-n**, **--nodename****

:   print the network node hostname

****-r**, **--kernel-release****

:   print the kernel release

****-v**, **--kernel-version****

:   print the kernel version

****-m**, **--machine****

:   print the machine hardware name

****--help****

:   display this help and exit

****--version****

:   output version information and exit

EXAMPLE
=======

Sample usage:

          >uname-
          >kernel-name    : CYGWIN_NT-10.0
          >nodename       : buzz
          >kernel-release : 2.6.0(0.304/5/3)
          >kernel-version : 2016-08-31 14:32
          >machine        : x86_64

          >uname- --all
          >CYGWIN_NT-10.0 buzz 2.6.0(0.304/5/3) 2016-08-31 14:32 x86_64

          >uname- -machine
          >x86_64

AUTHOR
======

John S. Urban

LICENSE
=======

Public Domain
