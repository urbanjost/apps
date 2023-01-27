NAME
====

sha3(1) - \[FUNIX:M\_strings\] generate SHA-{224,256,384,512} digest
values for specified files (LICENSE:PD)

SYNOPSIS
========

sha3 \[-**-bits** \[224\|256\|384\|512\] \[-**-auto\_test**\] FILE\`\`\`

DESCRIPTION
===========

Example of using M\_sha3(3fm) module. Calculates SHA digest values for
specified files.

NOT CURRENTLY WORKING
---------------------

OPTIONS
=======

****--bits**,b**

:   NNN where NNN is the digest value size in bits.

<!-- -->

                 +224   calculate SHA-224 digest values for specified files
                 +256   calculate SHA-256 digest values for specified files
                        (default)
                 +384   calculate SHA-384 digest values for specified files
                 +512   calculate SHA-512 digest values for specified files

****--auto\_test**,a**

:   run internal tests of routines in M\_sha3(3fm) module

<!-- -->

       FILE(S)          names of files to generate a hash for.

EXAMPLES
========

Sample usage

          sha3 *
          sha3 --bits 512 *
