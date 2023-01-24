# fpm tools and utilities

A collection of fpm(1) projects that use the GPF (General Purpose Fortran) modules

There is no simple hierarchy in github repositories, so this is one github repository
that contains a number of directories that are independent fpm(1) projects that build
executable program(s).

Most require being built in a POSIX environment.

+ [**f90split**](https://github.com/urbanjost/apps/blob/main/f90split/README.md) - 
  Michel Olagnons's version that splits Fortran source file into individual files

+ [**fcmd**](https://github.com/urbanjost/apps/blob/main/fcmd/README.md) - 
  find the pathname of commands and optionally perform commands on them.

+ [**flower**](https://github.com/urbanjost/apps/blob/main/flower/README.md) - 
  change case of free-format Fortran file; or remove code; or remove comments

+ [**lsup**](https://github.com/urbanjost/apps/blob/main/lsup/README.md) - 
  list permissions of pathname and directories in pathname

+ [**numdiff**] (https://github.com/urbanjost/apps/blob/main/numdiff/README.md) -
  Compare numeric differences in a file

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
