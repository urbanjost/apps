NAME
====

tty-(1f) - \[FUNIX:FILESYSTEM\] print information about the
file/terminal connected to standard input (LICENSE:PD)

SYNOPSIS
========

tty- \[OPTION\]\`\`\`

DESCRIPTION
===========

Call INQUIRE(3f) and print information about the file name of the
terminal connected to standard input. The results are dependent on the
programming environment used, as some of the behavior is system
dependent.

OPTIONS
=======

****--help****

:   display this help and exit

****--version****

:   output version information and exit

EXAMPLE
=======

The standard leaves quite a bit as system dependent. It is always
interesting. For example, on CygWin using gfortran V5.4

If I just type in the command at a prompt the INPUT\_UNIT of 5 is
returned as unit 0. But if I call the tty- from within vim(1) it is
returned as 5.

            tty-
            > for INPUT_UNIT=5
            >====================================================
            >*print_inquire* checking file:/dev/pty10
            >*print_inquire* file exists
            >*print_inquire* using unit number  0
            >*print_inquire* access type SEQUENTIAL,FORMATTED

Even though the file is not a terminal but exists I get a name of
"stdin". Other compilers return a null filename, others return the name
of the redirected file. What if there was actually a file called
"stdin"?

            tty- </dev/null
            > for INPUT_UNIT=5
            >====================================================
            >*print_inquire* checking file:stdin
            >*print_inquire* file does not exist

Try these:

            tty- # a simple call from the keyboard
            tty- </tmp/notthere # a file assumed to not exist
            tty- </dev/null   # a special file that exists
            tty- </etc/passwd # some non-terminal file that exists
            tty- < `tty`      # reading the pathname of your terminal
            tty- < /dev/pty08 # some TTY not assigned to your ID

REPORTING BUGS
==============

Report \_yes bugs to \<http://www.urbanjost.altervista.org/index.html\>

SEE ALSO
========

isatty(3c),tty(1), ttyname(3)

AUTHOR
======

John S. Urban

LICENSE
=======

Public Domain
