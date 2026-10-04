> program fortran_unicode use,intrinsic :: iso_fortran_env, only: stdout
> =\> output_unit implicit none integer,parameter :: ucs4 =
> **selected_char_kind**('ISO_10646')
> **character**(kind=ucs4,len=:),allocatable :: ustr2 ! ISO-10646
> ENCODING:**--help** **character**(len=\*,kind=ucs4),parameter :: ustr=
> & **char**(int(z'2D'),kind=ucs4)// & **char**(int(z'2D'),kind=ucs4)//
> & **char**(int(z'68'),kind=ucs4)// & **char**(int(z'65'),kind=ucs4)//
> & **char**(int(z'6C'),kind=ucs4)// & **char**(int(z'70'),kind=ucs4)
> integer :: i **open**(stdout,encoding='utf-8')
> **write**(stdout,\*)ustr ! can do internal writes, substrings,
> character intrinsics ...
> **allocate**(**character**(len=len(ustr),kind=ucs4) :: ustr2)
> **write**(ustr2,'(\*(a))')\[(ustr(i:i),i=len(ustr),1,**-1**)\]
> **write**(stdout,\*)ustr2 end program fortran_unicode
>
> program fortran_M_unicode use,intrinsic :: iso_fortran_env, only:
> stdout =\> output_unit use M_unicode, only: unicode_type,
> **assignment**(=), len implicit none **type**(unicode_type) :: ustr
> **type**(unicode_type) :: ustr2 ! GLYPH UNICODE ENCODING:**--help**
> integer,parameter :: **codes**(\*)= \[& **int**(z'2D'), &
> **int**(z'2D'), & **int**(z'68'), & **int**(z'65'), & **int**(z'6C'),
> & **int**(z'70')\] integer :: i integer :: iostat
> **open**(stdout,encoding='utf-8',iostat=iostat) ustr = codes
> **write**(stdout,\*)ustr%**character**() ! can do substrings,
> character intrinsics ... ustr2 =
> **ustr%character**(len(ustr),1,**-1**)
> **write**(stdout,\*)ustr2%**character**()
> ustr=\[45,45,104,101,108,112\]
> **write**(stdout,\*)**ustr%character**() ustr="**--help**"
> **write**(stdout,\*)**ustr%character**() end program fortran_M_unicode
>
> \#include \<iostream\> \#include \<string\> int **main**() {
> std::u32string mystring = U"**--help**"; // Outputting these directly
> might depend on console/terminal support // and may require conversion
> to the system's preferred encoding. return 0; }
>
> \<!-- HTML Entities for **--help**--\> \<pre\> **--help**
> &#x2D;&#x2D;&#x68;&#x65;&#x6C;&#x70;
> &#45;&#45;&#104;&#101;&#108;&#112; \</pre\>
