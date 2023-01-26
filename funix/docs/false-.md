NAME
====

**false-**(1f) - \[FUNIX\] do nothing, unsuccessfully (LICENSE:PD)

SYNOPSIS
========

false- *value* \[ **--verbose**\]\[ **--help**\| **--version**\]

DESCRIPTION
===========

Exit with a status code indicating failure.

OPTIONS
=======

**number**

:   optional number of 1 to 32, which will be used to generate the exit
    status code if supported.

****--help****

:   display this help and exit

****--version****

:   output version information and exit

****--verbose****

:   display ASCII graphic of cockroach

EXAMPLE
=======

Bash example:

             false- || echo Should print this

             if false-
             then
                echo command got zero exit $?
             else
                echo command got non-zero exit $?
             fi

Expected output::

ERROR STOP
----------

Should print this

ERROR STOP
----------

command got non-zero exit 1

SEE ALSO
========

**\_true**(1f)
