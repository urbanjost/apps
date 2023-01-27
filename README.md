# fpm tools and utilities

A collection of small fpm(1) projects that use the GPF (General Purpose
Fortran) modules to build utility programs.

These are generally miscellaneous small utility programs that are low activity
that are unlikely to garner enough traffic to justify a separate repository.

There is no simple hierarchy in github repositories that I found useful and the
projects are relatively small so it is reasonable to create just one git(1) 
project.

Each directory is an independent fpm(1) project that builds an executable.

Note most if not all of these programs are also part of the General
Purpose Fortran project. These are alternative small fpm(1) projects.

Most require being built in a POSIX environment.
+ [**compute**](https://github.com/urbanjost/apps/blob/main/compute/README.md) - 
  evaluate FORTRAN77-like mathematical expressions in line mode

+ [**f90split**](https://github.com/urbanjost/apps/blob/main/f90split/README.md) - 
  Michel Olagnons's version that splits Fortran source file into individual files

+ [**fcmd**](https://github.com/urbanjost/apps/blob/main/fcmd/README.md) - 
  find the pathname of commands and optionally perform commands on them

+ [**flower**](https://github.com/urbanjost/apps/blob/main/flower/README.md) - 
  change case of free-format Fortran file; or remove code; or remove comments

+ [**funix**](https://github.com/urbanjost/apps/blob/main/funix/README.md) - 
  a collection of example programs mostly calling the M_system POSIX interface
  that resemble (sometimes) Unix-like commands. Shows how to use fpm(1) to
  call file-system-related packages.

+ [**hashkeys**](https://github.com/urbanjost/apps/blob/main/hashkeys/README.md) - 
  checksums of files

+ [**la**](https://github.com/urbanjost/apps/blob/main/la/README.md) - 
  interpret matrix expressions using a shell-like interface

+ [**lsup**](https://github.com/urbanjost/apps/blob/main/lsup/README.md) - 
  list permissions of pathname and directories in pathname

+ [**numdiff**](https://github.com/urbanjost/apps/blob/main/numdiff/README.md) -
  Compare numeric differences in a file

+ [**sub**](https://github.com/urbanjost/apps/blob/main/sub/README.md) -
  replace fixed strings in a file

+ [**xpand**](https://github.com/urbanjost/apps/blob/main/xpand/README.md) -
  expand tabs,remove trailing white space and adjacent blank lines somewhat like expand(1)

### Building ...

For example, to build flower(1)
flower(1) using fpm(1):
```bash
git clone https://github.com/urbanjost/apps
cd apps/flower
fpm install
flower --help
```
# Others

Other programs in their own repositories ...

+ [**prep**](https://github.com/urbanjost/prep) - Fortran code preprocessor
+ [**fpm-gdb**](https://github.com/urbanjost/fpm-gdb) - plug-in to call gdb(1) from fpm(1) on Linux
+ [**fpm-search**](https://github.com/urbanjost/fpm-search) - search through fpm(1) packages
