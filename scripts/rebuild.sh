#!/bin/bash
exec 2>&1
set -x
EXE(){
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
NAMES=${*:-$(cd source;echo *)}
export LNAME NAME
export UFPP_DOCUMENT_DIR=$(pwd)
for LNAME in $NAMES
do
   # preprocess Fortran source
   NAME=$( basename "$LNAME" .ff )
   NAME=$( basename "$NAME" .FF )
   #-----------------------------------------------
   case $LONGNAME in
   #-----------------------------------------------
   *.ff|*.FF)
    prep F90 TESTPRG90 --noenv --comment doxygen --verbose -i source/$NAME.[fF][fF] -o $NAME/app/$NAME.f90
    ;;
   #-----------------------------------------------
   *) # assumed to be a directory
    for SRC in source/$NAME/*.[fF][fF]
    do
       [ -r "$SCR" ] || continue
       SHORTSRC=$( basename "$SRC" .ff )
       SHORTSRC=$( basename "$SHORTSRC" .FF )
       prep F90 TESTPRG90 --noenv --comment doxygen --verbose -i $SRC -o $NAME/app/$SHORTSRC.f90
    done
    ;;
   #-----------------------------------------------
   esac
   #-----------------------------------------------
   (
      # position at top of fpm project
      cd $(dirname $0)/../$NAME
      # build and install
      fpm build
      fpm install
      # generate documentation with ford(1)
      mkdir -p docs/
      ford ford.md
      # build and get list of names
      case $NAME in
      *.ff|*.FF) EXE $NAME;;
      *)
         fpm build
         fpm run '*' --runner|while read LONGNAME
         do
	    [ -x "$LONGNAME" ] || continue
            EXE $(basename "$LONGNAME")
         done
      ;;
      esac
   )
   #-----------------------------------------------
done | tee rebuild.log
exit
