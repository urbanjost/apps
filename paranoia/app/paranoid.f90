! NAME                                                                          
!     paranoid(1f) - [DEVELOPER] call doubleprecision and real versions         
!     of paranoia(3f)                                                           
!     (LICENSE:PD)                                                              
! SYNOPSIS                                                                      
!     paranoid                                                                  
! DESCRIPTION                                                                   
!    This program and the routines it calls can be used to test various         
!    Fortran compiler options.                                                  
!                                                                               
!    The paranoid(1f) command is strictly for use by developers. This           
!    program and the sparanoi(3f) and dparanoi(3f) procedures all need          
!    recompiled with the compiler options being tested. Then the program        
!    is run and the resulting tests and their output are examined.              
!                                                                               
!    The results require interpretation and an understanding of program         
!    internals.                                                                 
!                                                                               
!    Because programs are often built with a variety of compilers and           
!    compiler options on a number of different platforms it is prudent to       
!    select options that choose operations that meet the double precision       
!    specification defined in the IEEE 754-1985 standard when available;        
!    but "failure" of the strict testing performed does not imply a flaw        
!    in the program.                                                            
!                                                                               
!    This permits developers to verify that the compiler and loader options     
!    selected while building a program and the system hardware currently        
!    being used reasonably perform floating point operations.                   
!                                                                               
! LICENSE                                                                       
!    Public Domain                                                              
! 
!@(#)PROGRAM:        paranoid(1)
!@(#)DESCRIPTION:    call doubleprecision and real versions of paranoia(3f)
!@(#)VERSION:        1.0, 20150508
!@(#)COMPILED:       2022-02-12 12:26:55 UTC-300
!
program test_paranoia
implicit none
integer,parameter            :: is=100
!-----------------------------------------------------------------------------------------------------------------------------------
   print '(a)', repeat('=',80)                           ! print break line
   call print_version()
   print '(a)', repeat('=',80)                           ! print break line
   write(*,"('*paranoia* single precision test')")
   call sparanoia()
   print '(a)', repeat('=',80)                           ! print break line
   write(*,"('*paranoia* double precision test')")
   call dparanoia()

contains

subroutine print_version()
use,intrinsic :: iso_fortran_env, only : compiler_version, compiler_options
use,intrinsic :: iso_fortran_env, only : stdin=>input_unit, stdout=>output_unit, stderr=>error_unit

character(len=:),allocatable :: options(:)
integer           :: i
integer           :: io=stdout
   write(io,'("=================:: ")')
   !  COMPILER-RELATED COMPILATION INFORMATION
   write(io,'("Compiler version :: ",a)') COMPILER_VERSION()
                                                      ! (hopefully) pretty-print compiler options
   call split(COMPILER_OPTIONS(),options)             ! parse compiler options on ' ' (likely delimiter)
   write(io,'("Compiler options ::")',advance='no')   ! only start new lines when a - begins the option
   do i=1,size(options)
      if(options(i)(1:1).eq.'-'.and.i.ne.1)then
         write(io,'(a)')
         write(io,'("                 :: ",a)',advance='no') trim(options(i))
      else
         write(io,'(" ",a)',advance='no') trim(options(i))
      endif
   enddo
   write(io,'(/,80("="))')
end subroutine print_version

subroutine split(input_line,array,delimiters)
!-----------------------------------------------------------------------------------------------------------------------------------

!$@(#) M_strings::split(3f): parse string on delimiter characters and store tokens into an allocatable array

!  John S. Urban
!-----------------------------------------------------------------------------------------------------------------------------------
intrinsic index, min, present, len
!-----------------------------------------------------------------------------------------------------------------------------------
!  given a line of structure " par1 par2 par3 ... parn " store each par(n) into a separate variable in array.
!    o by default adjacent delimiters in the input string do not create an empty string in the output array
!    o no quoting of delimiters is supported
character(len=*),intent(in)              :: input_line  ! input string to tokenize
character(len=*),optional,intent(in)     :: delimiters  ! list of delimiter characters
character(len=:),allocatable,intent(out) :: array(:)    ! output array of tokens
!-----------------------------------------------------------------------------------------------------------------------------------
integer                       :: n                      ! max number of strings INPUT_LINE could split into if all delimiter
integer,allocatable           :: ibegin(:)              ! positions in input string where tokens start
integer,allocatable           :: iterm(:)               ! positions in input string where tokens end
character(len=:),allocatable  :: dlim                   ! string containing delimiter characters
character(len=:),allocatable  :: ordr                   ! string containing order keyword
character(len=:),allocatable  :: nlls                   ! string containing nulls keyword
integer                       :: ii,iiii                ! loop parameters used to control print order
integer                       :: icount                 ! number of tokens found
integer                       :: lgth                   ! length of input string with trailing spaces trimmed
integer                       :: i10,i20,i30            ! loop counters
integer                       :: icol                   ! pointer into input string as it is being parsed
integer                       :: idlim                  ! number of delimiter characters
integer                       :: ifound                 ! where next delimiter character is found in remaining input string data
integer                       :: inotnull               ! count strings not composed of delimiters
integer                       :: ireturn                ! number of tokens returned
integer                       :: imax                   ! length of longest token
!-----------------------------------------------------------------------------------------------------------------------------------
   ! decide on value for optional DELIMITERS parameter
   if (present(delimiters)) then                                     ! optional delimiter list was present
      if(delimiters.ne.'')then                                       ! if DELIMITERS was specified and not null use it
         dlim=delimiters
      else                                                           ! DELIMITERS was specified on call as empty string
         dlim=' '//char(9)//char(10)//char(11)//char(12)//char(13)//char(0) ! use default delimiter when not specified
      endif
   else                                                              ! no delimiter value was specified
      dlim=' '//char(9)//char(10)//char(11)//char(12)//char(13)//char(0)    ! use default delimiter when not specified
   endif
   idlim=len(dlim)                                                   ! dlim a lot of blanks on some machines if dlim is a big string
!-----------------------------------------------------------------------------------------------------------------------------------
   ordr='sequential' ! decide on value for optional ORDER parameter
   nlls='ignore'     ! optional parameter
!-----------------------------------------------------------------------------------------------------------------------------------
   n=len(input_line)+1                        ! max number of strings INPUT_LINE could split into if all delimiter
   if(allocated(ibegin))deallocate(ibegin)    !x! intel compiler says allocated already ?
   if(allocated(iterm))deallocate(iterm)      !x! intel compiler says allocated already ?
   allocate(ibegin(n))                        ! allocate enough space to hold starting location of tokens if string all tokens
   allocate(iterm(n))                         ! allocate enough space to hold ending location of tokens if string all tokens
   ibegin(:)=1
   iterm(:)=1
!-----------------------------------------------------------------------------------------------------------------------------------
   lgth=len(input_line)                                           ! lgth is the column position of the last non-blank character
   icount=0                                                       ! how many tokens found
   inotnull=0                                                     ! how many tokens found not composed of delimiters
   imax=0                                                         ! length of longest token found
!-----------------------------------------------------------------------------------------------------------------------------------
   if(lgth.gt.0)then                                              ! there is at least one non-delimiter in INPUT_LINE if get here
      icol=1                                                      ! initialize pointer into input line
      INFINITE: do i30=1,lgth,1                                   ! store into each array element
         ibegin(i30)=icol                                         ! assume start new token on the character
         if(index(dlim(1:idlim),input_line(icol:icol)).eq.0)then  ! if current character is not a delimiter
            iterm(i30)=lgth                                       ! initially assume no more tokens
            do i10=1,idlim                                        ! search for next delimiter
               ifound=index(input_line(ibegin(i30):lgth),dlim(i10:i10))
               IF(ifound.gt.0)then
                  iterm(i30)=min(iterm(i30),ifound+ibegin(i30)-2)
               endif
            enddo
            icol=iterm(i30)+2                                     ! next place to look as found end of this token
            inotnull=inotnull+1                                   ! increment count of number of tokens not composed of delimiters
         else                                                     ! character is a delimiter for a null string
            iterm(i30)=icol-1                                     ! record assumed end of string. Will be less than beginning
            icol=icol+1                                           ! advance pointer into input string
         endif
         imax=max(imax,iterm(i30)-ibegin(i30)+1)
         icount=i30                                               ! increment count of number of tokens found
         if(icol.gt.lgth)then                                     ! no text left
            exit INFINITE
         endif
      enddo INFINITE
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   select case (trim(adjustl(nlls)))
   case ('ignore','','ignoreend')
      ireturn=inotnull
   case default
      ireturn=icount
   end select
   allocate(character(len=imax) :: array(ireturn))                ! allocate the array to return
   !allocate(array(ireturn))                                       ! allocate the array to return
!-----------------------------------------------------------------------------------------------------------------------------------
   select case (trim(adjustl(ordr)))                              ! decide which order to store tokens
   case ('reverse','right') ; ii=ireturn ; iiii=-1                ! last to first
   case default             ; ii=1       ; iiii=1                 ! first to last
   end select
!-----------------------------------------------------------------------------------------------------------------------------------
   do i20=1,icount                                                ! fill the array with the tokens that were found
      if(iterm(i20).lt.ibegin(i20))then
         select case (trim(adjustl(nlls)))
         case ('ignore','','ignoreend')
         case default
            array(ii)=' '
            ii=ii+iiii
         end select
      else
         array(ii)=input_line(ibegin(i20):iterm(i20))
         ii=ii+iiii
      endif
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
   end subroutine split

end program test_paranoia
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
