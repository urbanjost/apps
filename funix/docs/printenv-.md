NAME
====

printenv-(1f) - \[FUNIX:ENVIRONMENT\] print all or part of environment
in formats readable by various shells (LICENSE:PD)

SYNOPSIS
========

printenv- \[variable\`\`\`\] \[ **-C**\|**-B**\] printenv- \[
**--help**\|**--version**\]

DESCRIPTION
===========

If no arguments are given, printenv-(1f) prints the entire environment.
If one or more variable names are given, it prints the value of each one
that is set.

OPTIONS
=======

**variable(s)**

:   if variable names are given, print the value for each one that is
    set.

****-C****

:   print output in a form that can be sourced into a C shell (eg. as a
    setenv(1) command).

****-B****

:   print output in a form that can be sourced into a Bourne shell.

****--help****

:   display this help and exit

****--version****

:   output version information and exit

USAGE
=====

Example commands:

          printenv-                       # print entire environment
          printenv- HOME TMP LOGNAME USER # print selected variables
          printenv- USER -C               # print as a C-shell setenv(1) command

EXIT STATUS
===========

The exit status is:

> **0**
>
> :   if all variables specified were found
>
> **1**
>
> :   otherwise
>
SEE ALSO
========

env(1), printenv(1)

AUTHOR
======

John S. Urban

LICENSE
=======

Public Domain
