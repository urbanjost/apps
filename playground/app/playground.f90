program playground
use, intrinsic :: iso_fortran_env, only : &
 & stderr=>ERROR_UNIT,&
 & stdin=>INPUT_UNIT,&
 & stdout=>OUTPUT_UNIT
use M_io, only : filebyte, basename
use M_cli2, only : set_args, get_args, names=>unnamed
implicit none
character(len=1),allocatable :: text(:) ! array to hold file in memory
character(len=:),allocatable :: help_text(:)
character(len=:),allocatable :: version_text(:)
integer :: i
   call setup()
   call set_args('',help_text,version_text )
   call begin()
   do i=1,size(names)
      call filebyte(names(i),text) ! allocate character array and copy file into it
      if(.not.allocated(text))then
         write(stderr,*)'*playground* failed to load file '//trim(names(i))
         cycle
      endif
      call instance(names(i))
   enddo
   call finish()

contains

subroutine begin()
integer :: i
character(len=:),allocatable :: start(:)
   start=[ CHARACTER(LEN=128) :: &
   '<!DOCTYPE html>',&
   '<html xmlns="http://www.w3.org/1999/xhtml">',&
   '<head>                                     ',&
   '  <meta name="generator" content="playground" />',&
   '  <title>playground</title>',&
   '</head>',&
   '<body>',&
   '']
   write(stdout,'(a)')(trim(start(i)),i=1,size(start))
end subroutine begin

subroutine tostream()
! The characters allowed in a URI are either reserved or unreserved
! (or a percent character as part of a percent-encoding). Reserved
! characters are those characters that sometimes have special meaning,
! while unreserved characters have no such meaning. Using percent-encoding,
! characters which otherwise would not be allowed are represented using
! allowed characters. The sets of reserved and unreserved characters and
! the circumstances under which certain reserved characters have special
! meaning have changed slightly with each revision of specifications that
! govern URIs and URI schemes.

! According to RFC 3986, the characters in a URL have to be taken from
! a defined set of unreserved and reserved ASCII characters. Any other
! characters are not allowed in a URL.

! The unreserved characters can be encoded, but should not be encoded. The
! unreserved characters are:

! ABCDEFGHIJKLMNOPQRSTUVWXYZ abcdefghijklmnopqrstuvwxyz 0123456789 - _ . ~

! The reserved characters have to be encoded only under certain
! circumstances. The reserved characters are:

! * ' ( ) ; : @ & = + $ , / ? % # [ ]

integer :: i
   do i=1,size(text)
      select case(text(i))
      case('a':'z','A':'Z','0':'9','-','_','.','~')
         write(stdout,'(a)',advance='no')text(i)
      case default
         write(stdout,'(*(a,z2.2))',advance='no')'%',text(i)
      end select
   enddo
   write(stdout,'(*(g0))')'"'
end subroutine tostream

subroutine instance(name)
character(len=*),intent(in) :: name
character(len=:),allocatable :: shortname
character(len=:),allocatable :: play(:)
integer :: i
   shortname=basename(name)//'      '
   if(index(shortname,'demo_').eq.1)shortname=shortname(6:)
   shortname=trim(shortname)
   ! write percent-encrypted copy of code as a URI
   write(stdout,'(*(g0))',advance='no')'  <a href="https://play.fortran-lang.org/?code='
   call tostream()
   play=[ CHARACTER(LEN=128) :: &
   '  target="_blank" title="Open in Fortran Playground">               ',&
   '  <img src="https://raw.githubusercontent.com/fortran-lang/playground/main/frontend/src/fortran-logo.png"',&
   '  alt="Fortran logo" class="align-text-bottom" height="15.5" /> '//shortname//'',&
   '  </a>                                                                                                     ',&
   '  <xmp>']
   write(stdout,'(a)')(trim(play(i)),i=1,size(play))
   ! now write file to stdout as-is
   write(stdout,'(*(a:))',advance='no')text
   deallocate(text)  ! release memory
   write(*,'(*(a:))')'  </xmp>'
end subroutine instance

subroutine finish()
character(len=:),allocatable :: footer(:)
integer :: i
   footer=[ CHARACTER(LEN=128) :: &
   '</body>',&
   '</html>',&
   '']
   write(stdout,'(a)')(trim(footer(i)),i=1,size(footer))
end subroutine finish

subroutine setup()
help_text=[ CHARACTER(LEN=128) :: &
'NAME',&
'playground(1f) - convert Fortran file to an HTML document that uploads',&
'                 the code to "Fortran Playground"                     ',&
'                                                                      ',&
'SYNOPSIS                                                              ',&
'    playground [ --help| --version] *.[fF]90                          ',&
'                                                                      ',&
'DESCRIPTION                                                           ',&
'                                                                      ',&
'create an HTML document from Fortran source files that includes a     ',&
'click-able download to the "Fortran Playground".                      ',&
'                                                                      ',&
'URL encoding, officially known as percent-encoding, is a method to    ',&
'encode arbitrary data in a Uniform Resource Identifier (URI) using only',&
'a limited subset of ASCII characters, replacing them with one or more  ',&
'character triplets that consist of the percent character and a two-digit',&
'hexadecimal value.                                                      ',&
'                                                                        ',&
'OPTIONS                                                                 ',&
'    --help         display this help and exit                           ',&
'    --version      output version information and exit                  ',&
'    filename(s)    Fortran source files                                 ',&
'                                                                        ',&
'EXAMPLES                                                                ',&
'  Sample commands                                                       ',&
'                                                                        ',&
'   > $  playground hello.f90 > playground.html                          ',&
'   > <!DOCTYPE html>                                                    ',&
'   > <html xmlns="http://www.w3.org/1999/xhtml">                        ',&
'   > <head>                                                             ',&
'   >   <meta name="generator" content="playground" />                   ',&
'   >   <title>playground</title>                                        ',&
'   > </head>                                                            ',&
'   > <body>                                                             ',&
'   >                                                                    ',&
'   >   <a href="https://play.fortran-lang.org/?code=                    ',&
'   >   program%20hello%5Fworld                                          ',&
'   >   %0A%20%20%20write%28%2A%2C%2A%29%27Hello%20World%21%27           ',&
'   >   %0Aend%20program%20hello%5Fworld                                 ',&
'   >   %0A"                                                             ',&
'   >   target="_blank" title="Open in Fortran Playground">              ',&
'   >   <img src="https://raw.githubusercontent.com/fortran-lang/        ',&
'   >   playground/main/frontend/src/fortran-logo.png"                   ',&
'   >   alt="Fortran logo" class="align-text-bottom" height="15.5" /> hello',&
'   >   </a>                                                               ',&
'   >   <xmp>                                                              ',&
'   > program hello_world                                                  ',&
'   >    write(*,*)''Hello World!''                                        ',&
'   > end program hello_world                                              ',&
'   >   </xmp>                                                             ',&
'   > </body>                                                              ',&
'   > </html>                                                              ',&
'                                                                          ',&
'']
version_text=[ CHARACTER(LEN=128) :: &
'PRODUCT:        GPF (General Purpose Fortran) utilities and examples',&
'PROGRAM:        playground(1)                                       ',&
'DESCRIPTION:    create an HTML document from Fortran sources files  ',&
'VERSION:        1.0, 2023-05-20                                     ',&
'AUTHOR:         John S. Urban                                       ',&
'HOME PAGE:      http://www.urbanjost.altervista.org/index.html      ',&
'']
end subroutine setup
end program playground
