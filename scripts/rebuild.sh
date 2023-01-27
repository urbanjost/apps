#!/bin/bash
set -x
EXE(){
   set -x
   EXENAME=$1
   #  generate man page and install
   read VER VERSION OTHER <<< $($EXENAME --version|grep VERSION:|tail -n 1)
   mkdir -p $HOME/.local/man/man1/ man/man1
   $EXENAME --help|
      txt2man -t $EXENAME -r "$EXENAME-${VERSION}" -s 1 -v "fpm Fortran tools" >man/man1/$EXENAME.1
   # nroff -man man/man1/$EXENAME.1|less -r
   cp man/man1/$EXENAME.1 $HOME/.local/man/man1/
   # generate markdown help text
   pandoc --from=man --to=markdown_mmd --output=docs/$EXENAME.md <man/man1/$EXENAME.1
   #export GITHUB=FALSE
   #export DEMO_OUTDIR=../../example
   #export DEMO_SUBDIR=FALSE
   man2html man/man1/$EXENAME.1 > docs/$EXENAME.1.html
   gzip -f man/man1/$EXENAME.1
}
NAMES=${*:-'f90split fcmd flower lsup numdiff xpand compute la sub funix hashkeys'}
export NAME
for NAME in $NAMES
do
   exec 2>&1
   # preprocess Fortran source
   export UFPP_DOCUMENT_DIR=$(pwd)
   case $NAME in
   funix|hashkeys)
    for SRC in source/$NAME/*.[fF][fF]
    do
       prep F90 TESTPRG90 --noenv --comment doxygen --verbose -i $SRC -o $NAME/app/$NAME.f90
    done
    ;;
   *)
    prep F90 TESTPRG90 --noenv --comment doxygen --verbose -i source/$NAME.[fF][fF] -o $NAME/app/$NAME.f90
    ;;
   esac
   (
      exec 2>&1
      set -x
      # position at top of fpm project
      cd $(dirname $0)/../$NAME
      # build and install
      fpm install
      # generate documentation with ford(1)
      mkdir -p docs/
      ford ford.md
   case $NAME in
   funix|hashkeys)
      fpm run '*' --runner|while read LONGNAME
      do
         EXE $(basename $LONGNAME)
      done
   ;;
   *) EXE $NAME
   ;;
   esac
   )
done | tee rebuild.log
exit
