NAME
====

dw(1f) - \[TEXT\] find duplicate words (LICENSE:PD)

SYNOPSIS
========

dw \< infile \>outfile

DESCRIPTION
===========

dw(1) is a handy tool for finding a common typographical error in
documentation.

dw(1) filters its standard input, printing on standard output the
duplicated word and the line it occurs on when a word appears twice in
succession. Each duplicate word is prefixed by its line number.

A reasonable guess is made as to what constitutes a word. Letter case is
ignored.

OPTIONS
=======

****--help****

:   display this help and exit

****--usage****

:   display state of command options and exit

****--version****

:   output version information and exit

EXAMPLE
=======

Sample run:

       dw <<\end_of_data
          In the the house there is a a cat.
          The cat in the house
          house is black.  There is is a dog dog in the
          house that does not like cats.  Cats are feline
          creatures.
       end_of_data

Sample output:

       000001 the::      In the the house there is a a cat.
       000001 a::      In the the house there is a a cat.
       000003 house::      house is black.  There is is a dog dog in the
       000003 is::      house is black.  There is is a dog dog in the
       000003 dog::      house is black.  There is is a dog dog in the
       000004 cats::      house that does not like cats.  Cats are feline
