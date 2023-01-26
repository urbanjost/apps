NAME
====

stat-(1f) - \[FUNIX:FILESYSTEM\] list file properties (LICENSE:PD)

SYNOPSIS
========

stat- pathnames\|**--version**\|**--help**

DESCRIPTION
===========

Given pathnames list properties

OPTIONS
=======

**pathnames**

:   pathnames to display properties of.

****--help****

:   display command help and exit

****--version****

:   output version information and exit

EXAMPLES
========

Sample command lines \`\`\`

       stat- stat-.ff

Results

       Pathname:                    stat-.ff
       Residence:                   Inode:18295873486224096  Device ID(hex/decimal):3E6BE045h/1047257157d  Device where located:0
       Size:                        File size(bytes):4267  No. of blocks allocated:8  Preferred block size(bytes):65536
       File mode octal/decimal/str: 100744o/33252d/-rwxr--r-- ---
       Number of links:             1
       Owner's uid/username:        197609/JSU
       Owner's gid/group:           197121/None
       Last access time:            1507483493 2017-10-08 13:24:53
       Last modification time:      1507483493 2017-10-08 13:24:53
       Last status change time:     1507483494 2017-10-08 13:24:54

AUTHOR
======

John S. Urban

LICENSE
=======

Public Domain
