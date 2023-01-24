#!/bin/bash
export NAME
for NAME in f90split fcmd flower lsup numdiff xpand
do
   exec 2>&1
   # preprocess Fortran source
   export UFPP_DOCUMENT_DIR=$(pwd)
   prep F90 TESTPRG90 --noenv --comment doxygen --verbose -i source/$NAME.[fF][fF] -o $NAME/app/$NAME.f90
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
      #  generate man page and install
      read VER VERSION OTHER <<< $($NAME --version|grep VERSION:|tail -n 1)
      mkdir -p $HOME/.local/man/man1/ man/man1
      $NAME --help|
         txt2man -t $NAME -r "$NAME-${VERSION}" -s 1 -v "fpm Fortran tools" >man/man1/$NAME.1
      # nroff -man man/man1/$NAME.1|less -r
      cp man/man1/$NAME.1 $HOME/.local/man/man1/
      # generate markdown help text
      pandoc --from=man --to=markdown_mmd --output=docs/$NAME.md <man/man1/$NAME.1
      #export GITHUB=FALSE
      #export DEMO_OUTDIR=../../example
      #export DEMO_SUBDIR=FALSE
      man2html man/man1/$NAME.1 > docs/flower.1.html
      gzip -f man/man1/$NAME.1
   )
done | tee rebuild.log
exit
