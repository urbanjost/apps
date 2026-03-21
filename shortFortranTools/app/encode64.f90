program demo_base64
! base64-encode data to RFC-4648 
use,intrinsic :: iso_fortran_env, only: stdout=>OUTPUT_UNIT, int32
use M_slurp, only: file_to_byte
use M_aux,   only: stretch
implicit none
integer(kind=int32)          :: i, column, sz, pad
character(len=1),allocatable :: bytes(:)
character(len=4)             :: chunk
integer,parameter            :: rfc4648_linelength=76
character(len=1),parameter   :: rfc4648_padding='='
   ! allocate array and copy file into it and pad with two characters at end
   call file_to_byte(bytes) 
   sz=size(bytes)
   bytes=[bytes,achar(0),achar(0)]
   pad=3-mod(sz,3) ! figure out how many characters at end are pad characters
   column=0
   ! take sets of 6 bits from integer and place into every 8 bits
   do i=1,sz,3 ! place three bytes and zero into 32bit integer
      chunk=three2four(bytes(i:i+2))
      if(i.gt.sz-3)then ! if at end put any pad characters in place
         if(pad.gt.0.and.pad.lt.3)then
            chunk(5-pad:)=repeat(rfc4648_padding,pad)
         endif
      endif
      if(column.ge.rfc4648_linelength)then
         write(stdout,'(a)')
         column=0
      endif
      write(stdout,'(a)',advance='no')chunk
      column=column+4
   enddo
   if(column.ne.0)write(stdout,'(a)')
contains
function three2four(tri) result(quad)
character(len=1),intent(in) :: tri(3)
character(len=4)            :: quad
integer(kind=int32)         :: i32, i, j, k, iout(4)
character(len=*),parameter  :: rfc4648_alphabet='ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'
   i32 = transfer([(tri(j),j=3,1,-1),achar(0)], i32 )
   iout = 0
   do j=0,3
      call  mvbits(i32, (j)*6, 6, iout(4-j), 0) ! Note the bits are numbered 0 to BIT_SIZE(I)-1, from right to left.
      k=4-j
      quad(k:k)=rfc4648_alphabet(iout(k)+1:iout(k)+1)
   enddo
end function three2four
end program demo_base64
