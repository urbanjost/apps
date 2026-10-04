### [fpm-tools](https://github.com/search?q="fpm-tools"%20in:topic%20language:fortran)

NOTE: This repository requires being built with fpm ( [Fortran Package Manager](https://github.com/fortran-lang/fpm) )
      Alternatively, it may be found as a part of the GPF( [General Purpose Fortran](https://github.com/urbanjost/general-purpose-fortran) )

## Name

### hashkeys - a collection of hash key generators using the M_hashkeys repository

man-pages are incorporated in the distribution that have been rendered

## Descriptions


as HTML ...

+ [digest](https://urbanjost.github.io/apps/hashkeys/digest.1.html)
  compute SHA256 message digest (LICENSE:PD)
+ [hasher](https://urbanjost.github.io/apps/hashkeys/hasher.1.html) 
  exercise the string hash methods in the M_hashkey(3fm) module to generate file checksums 
+ [sha3](https://urbanjost.github.io/apps/hashkeys/sha3.1.html)
  generate SHA-{224, 256, 384, 512} digest values for specified files 

as Markdown ...

+ [digest](https://github.com/urbanjost/apps/hashkeys/blob/main/docs/digest.md)
  compute SHA256 message digest (LICENSE:PD)
+ [hasher](https://github.com/urbanjost/apps/hashkeys/blob/main/docs/hasher.md)
  exercise the string hash methods in the M_hashkey(3fm) module to generate file checksums 
+ [sha3](https://github.com/urbanjost/apps/hashkeys/blob/main/docs/sha3.md)
  generate SHA-{224, 256, 384, 512} digest values for specified files 

<!--
### Markdown IO
NO
+ [digest](https://urbanjost.github.io/apps/hashkeys/digest.md)
+ [hasher](https://urbanjost.github.io/apps/hashkeys/hasher.md)
+ [sha3](https://urbanjost.github.io/apps/hashkeys/sha3.md)
### Markdown site
NO
+ [digest](https://github.com/urbanjost/docs/digest.md)
+ [hasher](https://github.com/urbanjost/docs/hasher.md)
+ [sha3](https://github.com/urbanjost/docs/sha3.md)
### Markdown
YES
-->

## Installing

Build using fpm(1):

```bash
git clone https://github.com/urbanjost/hashkeys
cd hashkeys
fpm install
```
