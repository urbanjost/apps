NAME
====

kill-(1f) - \[FUNIX\] send signals to processes (LICENSE:PD)

SYNTAX
======

kill- PIDs \[ **-s** signal\_number\] \[ **--help**\|**--version**\]

DESCRIPTION
===========

Calls system\_kill(3f), which calls kill(3c) to send signals to
processes.

OPTIONS
=======

**PIDs**

:   PID numbers to send signal to

****-s****

:   signal number to send to the processes

****--help****

:   display this help and exit

****--version****

:   output version information and exit

EXAMPLE
=======

Sample execution:

        > $ kill- 60476 234234 OTHER -s 9
        > *a2d* - cannot produce number from string [OTHER]
        > *a2d* - [Bad value during integer read]
        > *kill*: SIGNAL=9 PID=60476 successfully sent
        > *kill*: process not found
        > *kill*: PID value of 0 is not supported

AUTHOR
======

John S. Urban

LICENSE
=======

Public Domain
