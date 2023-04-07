module m_minipre

   use m_calculator, only: inum0
   implicit none
   private
   integer, parameter   ::  max_nest_level = 20
   logical, save        :: condop(0:max_nest_level)
   data condop/.true., max_nest_level*.false./
   integer             :: ierr_logic = 0
   logical             :: dc
   integer, public, save, protected :: nest_level = 0
   logical, public, save, protected :: write = .true.

   public  :: cond
   public  :: journal80

   private :: if
   private :: evalit
   private :: else
   private :: elseif
   private :: endif
contains

   subroutine cond(verb, line, ierr)
      character(len=*), intent(in)       :: verb
      character(len=*), intent(in)       :: line
      integer, intent(out)               :: ierr
      logical, save                   :: eb = .false.
      integer, save                   :: noelse = 0

      ierr_logic = 0

      select case (verb)
      case ('if'); call if(line, noelse, eb)
      case ('elseif'); call elseif(line, noelse, eb)
      case ('else'); call else(noelse, eb)
      case ('endif'); call endif(noelse, eb)
      case default
         call journal80('*logic* FATAL - UNKNOWN DIRECTIVE '//trim(verb))
      end select
      ierr = ierr_logic
   end subroutine cond

   subroutine if(line, noelse, eb)
      character(len=*)               :: line
      integer                        :: noelse
      logical                        :: eb

      noelse = 0
      write = .false.
      nest_level = nest_level + 1
      if (nest_level .gt. max_nest_level) then
         writeit: block
            character(len=255) :: msgline
            write (msgline, '(*(g0))') '*logic* ABORT - "IF" BLOCK NESTING TOO DEEP:', nest_level
            call journal80(msgline)
         end block writeit
         ierr_logic = -30
         nest_level = 0
         write = .true.
         return
      end if
      call evalit(line)
      if (.not. dc .or. .not. condop(nest_level - 1) .or. eb) then
         return
      end if
      condop(nest_level) = .true.
      write = condop(nest_level)
   end subroutine if

   subroutine evalit(line)
      character(len=*), intent(in):: line
      integer                    :: ival
      integer                    :: status
      ival = inum0(line, ierr=status)
      if (ival .eq. 0) then
         dc = .true.
      else
         dc = .false.
      end if
      if (status .ne. 0) then
         dc = .false.
      end if
   end subroutine evalit

   subroutine else(noelse, eb)
      integer                    :: noelse
      logical                    :: eb

      if (noelse .eq. 1 .or. nest_level .eq. 0) then
         call journal80('*logic* FATAL - MISPLACED "ELSE" DIRECTIVE.')
         ierr_logic = -10
         nest_level = 0
         write = .true.
         return
      end if
      noelse = 1
      if (.not. condop(nest_level - 1)) return
      eb = .false.
      if (condop(nest_level)) then
         eb = .true.
         write = .false.
      else
         condop(nest_level) = .true.
         write = .true.
      end if
   end subroutine else

   subroutine elseif(line, noelse, eb)
      logical                    :: eb
      integer                    :: noelse
      character(len=*)           :: line

      if (noelse .eq. 1 .or. nest_level .eq. 0) then
         call journal80('*logic* FATAL - MISPLACED "ELSEIF" DIRECTIVE.')
         ierr_logic = -20
         nest_level = 0
         write = .true.
         return
      end if
      if (.not. condop(nest_level - 1)) return
      eb = .false.
      if (condop(nest_level)) then
         eb = .true.
         write = .false.
      else
         nest_level = nest_level - 1
         call if(line, noelse, eb)
      end if
   end subroutine elseif

   subroutine endif(noelse, eb)
      logical                    :: eb
      integer                    :: noelse

      if (noelse .eq. 0) then
         call else(noelse, eb)
      end if

      nest_level = nest_level - 1
      if (nest_level .lt. 0) then
         call journal80('*logic* FATAL - MISPLACED "ENDIF" DIRECTIVE.')
         ierr_logic = -40
         nest_level = 0
         write = .true.
         return
      end if
      noelse = 0
      eb = .not. condop(nest_level + 1)
      write = .not. eb
      condop(nest_level + 1) = .false.
      if (nest_level .eq. 0) then
         write = .true.
         eb = .false.
      end if
   end subroutine endif

   subroutine journal80(message)
      character(len=*), intent(in) :: message(*)
      integer, parameter:: LINE_WIDTH = 80
      integer :: i
      call innerjournal(message, len(message))
   contains
      subroutine innerjournal(charr, ilen)
         character(len=1),intent(in) :: charr(ilen)
         integer,intent(in) :: ilen
            write (*, '(80(a))') (charr(i), i=1, ilen)
      end subroutine innerjournal
   end subroutine journal80

end module m_minipre

program demo_cond
   use M_strings, only: lower, delim, v2s ! convert character case; split string
   use m_minipre, only: cond, journal80
   use m_minipre, only: write             ! flag whether current data lines should be written
   use m_minipre, only: nest_level        ! nesting level for #IF/#ELSEIF/#ELSE/#ENDIF
   use M_calculator, only: rnum0
   implicit none
   character(len=1)    :: prefix              ! directive prefix character
   character(len=1024) :: line                ! input line
   integer, parameter  :: max_words = 2       ! maximum number of words allowed on line
   character(len=1024) :: array(max_words)    ! working copy of input line
   ! location where words start and end
   integer             :: ibegin(max_words), iterm(max_words)
   integer             :: icount, ierr_logic, ilen, ios
   real                :: value
   !----------------------------------------------------------------------------
   PREFIX = '#'              ! for the example, assume direct lines use a # prefix
   write (*, *) 'find conditional lines #if #else #elseif #endif #define'
   !----------------------------------------------------------------------------
   READLINE: do                                   ! read loop to read input file
      read (*, '(a)', iostat=ios) line
      if (ios .ne. 0) then
         if (nest_level .ne. 0) then ! check to make sure all if blocks are closed
            call journal80('*logic* error - #IF BLOCK NOT CLOSED WHEN READING FILE FINISHED.')
         end if
         stop
      end if
      ! although delim(3f) can do more
      ! just parsing the first word out and finding where second word starts
      ! make sure array is initialized for when
      ! icount(number of words on line) is zero
      array = ' '
      call delim(lower(line), array, max_words, icount, ibegin, iterm, ilen, ' ')
      select case (array(1))
         ! find conditional lines
      case ('#if', '#else', '#elseif', '#endif')
         ! process conditional directive
         call cond(trim(array(1) (2:)), line(iterm(1) + 1:), ierr_logic)
      case ('#define')
         ! evaluate expression
         value = rnum0(line(iterm(1) + 1:))
      case default
         ! find input lines you want to use, skip others
         if (write) then
            ! for example, if last conditional was true then write line
            write (*, '(a)') trim(line)
            ! write data line
         end if
      end select
   end do READLINE
end program demo_cond
