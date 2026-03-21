program sortup
!@(#) change to lowercase and left-justify and then sort text
use M_slurp, only: readstdin, writeup
use M_aux,   only: cqsort, tolower
implicit none
character(len=:),allocatable :: text(:)
   call readstdin(text)
   text(:)=tolower(text)
   text(:)=adjustl(text)
   text(:)=cqsort(text)
   call writeup(text)
end program sortup
