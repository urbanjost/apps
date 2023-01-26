program word_counts   ! process file text -> output file words,counts
!(LICENSE:PD)
implicit none         ! e.g.  wc.exe bible.txt >counts.txt
real :: times(4)
integer,parameter :: hashbits=17, maxw=2**hashbits, wlen=30
integer,parameter :: ia = ichar('a'), iz = ichar('z'), iblank=ichar(' '), iquote=ichar("'")
integer(1),allocatable :: text(:)
character(256)    :: code = "                                       '                         abcdefghijklmnopqrstuvwxyz&
   &abcdefghijklmnopqrstuvwxyz     " // &
      "  " // char(130) // "       " // char(138)
integer(1)        :: acode(0:255) ; equivalence (acode,code)
character(wlen)   :: word, fname*80
integer(1)        :: aword(wlen)  ; equivalence (aword,word)
integer           :: ich, i, k, n, nchars, nc=0, collisions=0, total=0, unique=0, odd, even
type word_count
      union
      map ; character(wlen) :: word   ; end map   ! CVF, Intel, etc? extension
      map ; integer(1) :: aword(wlen) ; end map
      end union
      integer :: k, count
end type
type (word_count) :: wc(maxw)
   wc%word = ' ' ; wc%k = 0 ; wc%count = 0
! ---------------------
   call cpu_time(times(1))           ! get benchmark start time
   call getarg(1,fname)              ! get input file name
   if (fname == ' ') then
      write (*,*) 'usage:  e.g. >wc.exe  bible.txt >bible.out' ; stop
   endif

   open (1,file=fname,form='binary',position='append')  ! to end of file
   INQUIRE(FILE=filename, SIZE=nchars)
   allocate ( text(nchars) )
   rewind(1) ; read(1) text          ! input file -> text
   call cpu_time(times(2))           ! get benchmark time 2
! ---------------------
   text = acode(text)    ! upper->lower, non-alpha->blank, except '
   ! and 2 extended ascii chars (130), (138)

   do ; nc = nc+1 ; if (nc > nchars) exit
      ich = text(nc) ; if (ich < ia.or.ich > iz) cycle   ! skip until alpha start
      k = 1 ; aword(1) = ich                      ! found start new word
      odd = ich ; even = 1                        ! init. hash with 1st char
      do ; nc = nc+1
         ich = text(nc) ; if (ich == iblank) exit ! blank is end word
         if (ich == iquote) cycle                 ! delete ' from word
         k = k+1 ; aword(k) = ich                 ! accumulate word
         if (iand(k,1) == 0) then
            even = ieor(ishftc(even,5,hashbits),ich)  ! accum even hash
         else
            odd  = ieor(ishftc(odd, 5,hashbits),ich)  ! accum odd  hash
         endif
      enddo
      n = ieor(ishft(odd*even,-hashbits-2),odd*even)  ! hash product pieces
      n = iand(n,maxw-1)                              ! positive index
      do ; n = n+1 ; if (n > maxw-1) n = 1            ! reset index
         if (wc(n)%count == 0) then
            wc(n)%word(1:k) = word(1:k) ; wc(n)%k = k ; wc(n)%count = 1 ; exit  ! initial entry
         else if (word(1:k) == wc(n)%word(1:wc(n)%k)) then
            wc(n)%count = wc(n)%count+1 ; exit        ! count occurances
         else
            collisions = collisions+1
         endif
      enddo
   enddo
! ---------------------
   call cpu_time(times(3))              ! get process words,counts time
   n = 0
   do i = 1,maxw                        ! make entries contiguous from wc(1:
      if (wc(i)%count == 0) cycle
      n = n+1 ; wc(n) = wc(i)
      total = total + wc(i)%count ; unique = unique+1
   enddo
   call qsort(0,n-1)                    ! quicksort wc(1:n) entries
   write (*,'(i5,2x,a)') (wc(i)%count, wc(i)%word(1:wc(i)%k), i=1,n)
   write (*,*) 'file processed = ',trim(fname)
   write (*,*) 'total words  =',total
   write (*,*) 'unique words =',unique
   write (*,*) 'collisions   =',collisions
! ---------------------
   call cpu_time(times(4))              ! get benchmark stop time
   write (*,91) times(2)-times(1),' Sec  input'
   write (*,91) times(3)-times(2),' Sec  process'
   write (*,91) times(4)-times(3),' Sec  output'
   write (*,91) times(4)-times(1),' Sec  total  2.8ghz pentium4'
91 format (f0.3,a)
   stop
contains
! -----------------------------------
   recursive subroutine qsort(l,r)
      integer :: l, r, i,j
      i = l ; j = r ; word = wc((l+r)/2+1)%word
      do while (i <= j)
         do while (wc(i+1)%word < word .and. i < r)
            i = i+1
         enddo
         do while (word < wc(j+1)%word .and. j > l)
            j = j-1
         enddo
         if (i <= j) then
            wc([i+1,j+1]) = wc([j+1,i+1])   ! swap words,counts
            i = i+1
            j = j-1
         endif
      enddo
      if (l < j) call qsort(l, j)
      if (i < r) call qsort(i, r)
   end subroutine qsort
end program

! example bible.txt file -> words counts processing
! 8177  a
!  319  aaron
!   ..........
!    5  zurishaddai
!    1  zuzims
! bible.txt
! total words  =      789781
! unique words =       12691
! collisions   =        4486
! 0.016 Sec  input
! 0.188 Sec  process
! 0.063 Sec  output
! 0.266 Sec  total  2.8ghz pentium4
