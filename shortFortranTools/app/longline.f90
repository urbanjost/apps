program findll
!@(#) find long lines
use M_slurp, only: readstdin
use M_aux,   only: argn
implicit none
integer                      :: linenumber, columns, iostat, limit
character(len=:),allocatable :: text(:)
   limit=argn(1,0)
   call readstdin(text)
   do linenumber=1,size(text)
      columns=len_trim(text(linenumber))
      if(columns.gt.limit)then
         write(*,'(i0.5," : ",i0.4," : ",a)')linenumber,columns,text(linenumber)(:columns)
      endif
   enddo
end program findll
