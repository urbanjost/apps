#!/bin/bash
# create file with printable ASCII characters, as 
# file contains single quotes, grave, ctrl-M, ctrl-I
# and significant blanks on the ends of lines
(
echo -e '" use ,a and ,b to mark a region, then'
echo -e '" ,c ,d ,m for copy, delete, move'
echo -e '" ,u uppercase; ,l lowercase; ,w write to file, o overwrite to file'
echo -e '" ,p lineup; t trim; E expand; F filter'
echo -e 'map , #'
echo -e 'map #K mcHmdMme'
echo -e '" mark "above" and "below"'
echo -e 'map #a mamb'
echo -e 'map #b mb'
echo -e 'map #c #K:\047a;\047bco\047c\r'
echo -e 'map #d #K\047a"Dd\047b\047e\0140c'
echo -e 'map #m #K:\047a;\047bm\047c\r'
echo -e '" uppercase'
echo -e 'map #u #K:\047a,\047bs/.*/\\U&/\r'
echo -e '" lowercase'
echo -e 'map #l #K:\047a,\047bs/.*/\\L&/\r'
echo -e '" write to file'
echo -e 'map #w :\047a;\047bw '
echo -e 'map #o :\047a;\047bw! '
echo -e '" remove trailing whitespace'
echo -e 'map #t #K:\047a,\047bs/[ \t][ \t]*$//g\r'
echo -e '" expand tabs with expand(1) command'
echo -e 'map #E #K\047a!\047bexpand\r'
echo -e '" line up on ::'
echo -e 'map #p #K\047a!\047blu\r'
echo -e '" filter through command that is specified'
echo -e 'map #f #K\047a!\047b'
) > exrc_regions
export EXINIT='so exrc_regions'
vi ${*:-in1}
