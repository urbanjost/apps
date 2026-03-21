program codepoints
! @(#) take command line argument utf-8 text and generate Fortran statement that represents the string
use, intrinsic :: iso_fortran_env, only : stdout => output_unit
use M_unicode,                     only : unicode_type, assignment(=), len
implicit none
character(len=*),parameter   :: form= '("char(int(z''",z0,"''),kind=ucs4)":,"// &")'
character(len=*),parameter   :: form_M= '("int(z''",z0,"'')":,", &")'
character(len=*),parameter   :: form_zhtml= '(*(:"&#x",z0,";"))'
character(len=*),parameter   :: form_html= '(*(:"&#",i0,";"))'
character(len=*),parameter   :: form_codepoint= '(*(:"",i0,:","))'
character(len=*),parameter   :: g= '(*(g0))'
integer                      :: i
integer,allocatable          :: codes(:)
character(len=:),allocatable :: command_line
type(unicode_type)           :: ustr
   command_line=getargs()          ! get string containing all command arguments as CHARACTER bytes
   ustr=command_line               ! convert bytes to internal Fortran Unicode representation

   !open (stdout, encoding='UTF-8')

   ! write the command line out as a Fortran variable expression using the CHAR() function

   write(stdout,g) 'program fortran_unicode'
   write(stdout,g) 'use,intrinsic :: iso_fortran_env, only: stdout => output_unit'
   write(stdout,g) 'implicit none'
   write(stdout,g) "integer,parameter :: ucs4 = selected_char_kind('ISO_10646')"
   write(stdout,g) 'character(kind=ucs4,len=:),allocatable :: ustr2'
   write(stdout,g) '! ISO-10646 ENCODING:',command_line
   write(stdout,g) 'character(len=*,kind=ucs4),parameter :: ustr= &'
   write(stdout,form)(ustr%codepoint(i,i),i=1,len(ustr))
   write(stdout,g) 'integer :: i'
   write(stdout,g) "   open(stdout,encoding='utf-8')"
   write(stdout,g) '   write(stdout,*)ustr'
   write(stdout,g) '   ! can do internal writes, substrings, character intrinsics ...'
   write(stdout,g) '   allocate(character(len=len(ustr),kind=ucs4) :: ustr2)'
   write(stdout,g) "   write(ustr2,'(*(a))')[(ustr(i:i),i=len(ustr),1,-1)]"
   write(stdout,g) '   write(stdout,*)ustr2'
   write(stdout,g) 'end program fortran_unicode'

   write(stdout,g)

   write(stdout,g) 'program fortran_M_unicode'
   write(stdout,g) 'use,intrinsic :: iso_fortran_env, only: stdout => output_unit'
   write(stdout,g) 'use M_unicode, only: unicode_type, assignment(=), len'
   write(stdout,g) 'implicit none'
   write(stdout,g) 'type(unicode_type) :: ustr'
   write(stdout,g) 'type(unicode_type) :: ustr2'
   write(stdout,g) '! GLYPH UNICODE ENCODING:',command_line
   write(stdout,g) 'integer,parameter :: codes(*)= [&'
   write(stdout,form_M,advance='no')ustr%codepoint()
   write(stdout,g) ']'
   write(stdout,g) 'integer :: i'
   write(stdout,g) 'integer :: iostat'
   write(stdout,g) "   open(stdout,encoding='utf-8',iostat=iostat)"
   write(stdout,g) '   ustr = codes'
   write(stdout,g) '   write(stdout,*)ustr%character()'
   write(stdout,g) '   ! can do substrings, character intrinsics ...'
   write(stdout,g) '   ustr2 = ustr%character(len(ustr),1,-1)'
   write(stdout,g) '   write(stdout,*)ustr2%character()'
   write(stdout,g,advance='no') '   ustr=['
   write(stdout,form_codepoint,advance='no')ustr%codepoint()
   write(stdout,g) ']'
   write(stdout,g) '   write(stdout,*)ustr%character()'
   write(stdout,g) '   ustr="',ustr%character(),'"'
   write(stdout,g) '   write(stdout,*)ustr%character()'
   write(stdout,g) 'end program fortran_M_unicode'

   write(stdout,g)

   write(stdout,g) '#include <iostream>'
   write(stdout,g) '#include <string>'
   write(stdout,g) 'int main() {'
   write(stdout,g,advance='no') '   std::u32string mystring = U"'
   codes=ustr%codepoint()
   do i=1,size(codes)
      if(codes(i).lt.127)then
         write(stdout,g,advance='no') achar(codes(i))
      else
         write(stdout,'(''U\'',z8.8)',advance='no') codes(i)
      endif
   enddo
   write(stdout,g) '";'
   write(stdout,g) '   // Outputting these directly might depend on console/terminal support'
   write(stdout,g) "   // and may require conversion to the system's preferred encoding."
   write(stdout,g) '   return 0;'
   write(stdout,g) '}'

   write(stdout,g)

   write(stdout,g) "<!-- HTML Entities for ",ustr%character(),"-->"
   write(stdout,g) '<pre>'
   write(stdout,g)ustr%character()
   write(stdout,form_zhtml)ustr%codepoint() ! &#x4EBA;
   write(stdout,form_html)ustr%codepoint()  ! &#20154;
   write(stdout,g) '</pre>'
   write(stdout,g)
contains

function getargs() result(command_line)
integer                                :: length
character(len=:),allocatable           :: command_line
   call get_command(length=length)                 ! get command line length
   allocate(character(len=length) :: command_line) ! allocate string big enough to hold command line
   call get_command(command=command_line)          ! get command line as a string
   call get_command_argument(0,length=length)      ! remove argument 0
   command_line=adjustl(command_line(length+2:))
end function getargs

end program codepoints
!
! An example run; using the famous Confucian expression
! "己所不欲，勿施於人" (jǐ suǒ bù yù, wù shī yú rén) or
! "What you do not want done to yourself, do not do to others":
!
!    codepoints "己所不欲，勿施於人"
!
! Odd that hexadecimal values can be used directly and much more compactly in a DATA statement
! but multiple BOZ values in INT() are not allowed, perhaps because DATA assumes the values are
! for the type being initialized; and no syntax to allow DATA to initialize a parameter (?)
! integer,save :: codes(9)
! data codes/ z'5DF1',z'6240',z'4E0D',z'6B32',z'FF0C',z'52FF',z'65BD',z'65BC',z'4EBA'/

