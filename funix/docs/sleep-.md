NAME
====

sleep-(1f) - \[FUNIX:TIME\] pause for specified duration (LICENSE:PD)

SYNOPSIS
========

sleep- \[dd-hh:mm:ss\[.xxx\]\|xxx.yyy\[s\|m\|h\|d\]\] \[
**-countdown**\|**-countup** **-count**\]\|**--help**\|**--version**

DESCRIPTION
===========

Given a duration in the form dd-hh:mm:ss.xxx where dd is days, hh hours,
mm minutes and ss.xxx seconds pause for the specified amount of time.

Alternatively, the time may be specified by a number immediately
followed by a unit letter; where s is seconds, m is minutes, h is hours
and d is days.

If the suffix r is used, a random time between zero and the specified
number of seconds is used. This is useful for spreading out cron(1)
tasks in a HPC cluster.

Given multiple arguments, pause for the time specified by the sum of the
values.

OPTIONS
=======

**dd-hh:mm:ss**

:   Given a string representing a duration of time in the following
    forms:

<!-- -->

                      dd-hh:mm:ss[.xx]
                         hh:mm:ss[.xx]
                            mm:ss[.xx]
                               ss[.xx]

or

**xx\[.yy\]SUFFIX**

:   where Suffix may be s for seconds, m for minutes, h for hours, or d
    for days.

****-countdown****

:   sleep in one-second intervals and count down to end of sleep command

****-countup****

:   sleep in one-second intervals and count up till end of command

****-count** COUNT**

:   how many seconds between counts when **-countdown** and **-countup**
    are specified. Values other than one may trim the total sleep time
    by up to COUNT seconds.

****--help****

:   display this help and exit

****--version****

:   output version information and exit

EXAMPLE
=======

usage:

       sleep- 0.10     # pause one tenth of a second
       sleep- 3m 10s   # pause three minutes and 10 seconds
       sleep- 1:00:00  # pause for one hour

AUTHOR
======

John S. Urban

LICENSE
=======

Public Domain
