! $Id: FileUtilities.f90 2327 2016-08-16 07:53:09Z ian $
! ff08 source code copyright 2012 M.E.G.M.S.  See LICENCE.txt for licence.
!> @file
!! Defines the FileUtilities module.


!*******************************************************************************
!!
!> Utilities for working with input files.
!!
!! (By "input file" we mean something that specifies program options, as 
!! opposed to source files.)

MODULE FileUtilities
  
  IMPLICIT NONE
  
  PRIVATE
  
  !-----------------------------------------------------------------------------
  ! Expose module procedures and interfaces
  
  PUBLIC :: AddListFile
  PUBLIC :: AddFromConsole
  PUBLIC :: GetLine
  PUBLIC :: GetLines
  
  !> Add a list of records read from a file.
  INTERFACE AddListFile
    MODULE PROCEDURE AddListFile_
  END INTERFACE AddListFile
  
  !> Add a list of records read from the console.
  INTERFACE AddFromConsole
    MODULE PROCEDURE AddFromConsole_
  END INTERFACE AddFromConsole
  
  !> Get a line (a record) from a sequential or stream file.
  INTERFACE GetLine
    MODULE PROCEDURE GetLine_
    MODULE PROCEDURE GetLine_msg
  END INTERFACE GetLine
  
  !> Get lines (lots of records) from a sequentual or stream file.
  INTERFACE GetLines
    MODULE PROCEDURE GetLines_msg
  END INTERFACE GetLines
  
  !-----------------------------------------------------------------------------
  
  !> Format specification for a file open error.
  CHARACTER(*), PARAMETER :: fmt_file_err = "(A,': ',A)"
  
CONTAINS
  
  !*****************************************************************************
  !!
  !> Reads records from a file and adds them to a list.
  !!
  !! @param[in,out] list              The list to be added to.
  !!
  !! @param[in]     list_file         The name of the list file (file to be 
  !! read).
  !!
  !! @param[in]     err_unit          Unit number connected for formatted 
  !! output for error messages.
  !!
  !! @param[out]    stop_code         Error code - non-zero on error.
  
  SUBROUTINE AddListFile_(list, list_file, err_unit, stop_code)
    
    USE Strings
    USE UnitUtilities
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(String), INTENT(INOUT), ALLOCATABLE :: list(:)
    CHARACTER(*), INTENT(IN) :: list_file
    INTEGER, INTENT(IN) :: err_unit
    INTEGER, INTENT(OUT) :: stop_code
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: read_unit      ! Unit for reading the file.
    INTEGER :: stat           ! Error code.
    
    ! The records from the file.
    TYPE(String), ALLOCATABLE :: file_list(:)
    
    ! IO error message
    CHARACTER(1024) :: iomsg
    
    !***************************************************************************
    
    CALL GetUnit(read_unit)
    OPEN( UNIT=read_unit, FILE=list_file, ACTION='READ', STATUS='OLD',  &
        POSITION='REWIND', IOSTAT=stat, IOMSG=iomsg )
    IF (stat /= 0) THEN
      WRITE (err_unit, fmt_file_err) list_file,  &
          'Error opening file - ' // TRIM(iomsg)
      CALL ReleaseUnit(read_unit)
      stop_code = 3
      RETURN
    END IF
    
    CALL GetLines(read_unit, file_list, stat, iomsg)
    IF (stat /= 0) THEN
      WRITE (err_unit, fmt_file_err) list_file,  &
          'Error reading file - ' // TRIM(iomsg)
      CLOSE(read_unit)
      CALL ReleaseUnit(read_unit)
      stop_code = 3
      RETURN
    END IF
    
    CALL Append(list, file_list)
    
    CLOSE(read_unit)
    CALL ReleaseUnit(read_unit)
    
    stop_code = 0
    
  END SUBROUTINE AddListFile_
  
  
  !*****************************************************************************
  !!
  !> Reads records from the input unit (the console) and adds them to a list.
  !!
  !! @param[in,out] file_list         The list of records.
  !!
  !! @param[in]     err_unit          Logical unit connected for formatted 
  !! output for error messages.
  !!
  !! @param[out]    stop_code         Error code - non-zero on error.
  
  SUBROUTINE AddFromConsole_(file_list, err_unit, stop_code)
    
    USE Strings
    
    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INPUT_UNIT
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(String), INTENT(INOUT), ALLOCATABLE :: file_list(:)
    INTEGER, INTENT(IN) :: err_unit
    INTEGER, INTENT(OUT) :: stop_code
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: stat           ! Error code
    
    ! A line in the input (which is a file spec or similar).
    CHARACTER(:), ALLOCATABLE :: file
    
    !***************************************************************************
    
    DO
      CALL GetLine(INPUT_UNIT, file, stat)
      IF (stat > 0) THEN
        WRITE (err_unit, "(A)") 'Error reading from console.'
        stop_code = 3
        RETURN
      END IF
      IF (LEN_TRIM(file) == 0) EXIT
      CALL Append(file_list, file)
      IF (stat /= 0) EXIT     ! IOSTAT_END terminates this.
    END DO
    
    stop_code = 0
   
  END SUBROUTINE AddFromConsole_
  
  
  !*****************************************************************************
  !!
  !> Reads a complete line (end-of-record terminated) from a file.
  !!
  !! @param[in]     unit              Logical unit connected for formatted 
  !! input to the file.
  !!
  !! @param[out]    line              The line read.
  !!
  !! @param[out]    stat              Error code, positive on error, 
  !! IOSTAT_END (which is negative) on end of file.
  
  SUBROUTINE GetLine_(unit, line, stat)
    
    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: IOSTAT_EOR
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    INTEGER, INTENT(IN) :: unit
    CHARACTER(:), INTENT(OUT), ALLOCATABLE :: line
    INTEGER, INTENT(OUT) :: stat
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Buffer to read the line (or partial line).
    CHARACTER(256) :: buffer
    
    INTEGER :: size           ! Number of characters read from the file.
    
    !***************************************************************************
    
    line = ''
    DO
      READ (unit, "(A)", ADVANCE='NO', IOSTAT=stat, SIZE=size) buffer
      IF (stat > 0) RETURN      ! Some sort of error.
      line = line // buffer(:size)
      IF (stat < 0) THEN
        IF (stat == IOSTAT_EOR) stat = 0
        RETURN
      END IF
    END DO
    
  END SUBROUTINE GetLine_
  
  
  !*****************************************************************************
  !!
  !> Reads a complete line (end-of-record terminated) from a file.  This 
  !! variant allows an error message to be returned.
  !!
  !! @param[in]     unit              Logical unit connected for formatted 
  !! input to the file.
  !!
  !! @param[out]    line              The line read.
  !!
  !! @param[out]    stat              Error code, positive on error, 
  !! IOSTAT_END (which is negative) on end of file.
  !!
  !! @param[out]    iomsg             Error message - only defined if 
  !! iostat is non-zero.
  
  SUBROUTINE GetLine_msg(unit, line, stat, iomsg)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    INTEGER, INTENT(IN) :: unit
    CHARACTER(:), INTENT(OUT), ALLOCATABLE :: line
    INTEGER, INTENT(OUT) :: stat
    CHARACTER(*), INTENT(OUT) :: iomsg
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Buffer to read the line (or partial line).
    CHARACTER(256) :: buffer
    
    INTEGER :: size           ! Number of characters read from the file.
    
    !***************************************************************************
    
    line = ''
    DO
      READ (unit, "(A)", ADVANCE='NO', IOSTAT=stat, IOMSG=iomsg, SIZE=size)  &
          buffer
      IF (IS_IOSTAT_EOR(stat)) THEN
        line = line // buffer(:size)
        stat = 0
        EXIT
      ELSE IF (stat == 0) THEN
        line = line // buffer
      ELSE
        EXIT
      END IF
    END DO
    
  END SUBROUTINE GetLine_msg
  
  
  !*****************************************************************************
  !!
  !> Reads a series of records from a file, through to the end of file.
  !!
  !! @param[in]     unit              Logical unit connected for formatted 
  !! input to the file.
  !!
  !! @param[out]    lines             The records (lines) read from the file.
  !!
  !! @param[out]    stat              Error code, positive on error, zero on 
  !! end of file.
  !!
  !! @param[out]    iomsg             Error message - only defined if 
  !! iostat is non-zero.
  !!
  !! One element in @a lines is created for each record read from @a unit.
  
  SUBROUTINE GetLines_msg(unit, lines, stat, iomsg)
    
    USE Strings
    
    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: IOSTAT_EOR
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    INTEGER, INTENT(IN) :: unit
    TYPE(String), INTENT(OUT), ALLOCATABLE :: lines(:)
    INTEGER, INTENT(OUT) :: stat
    CHARACTER(*), INTENT(OUT) :: iomsg
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    CHARACTER(:), ALLOCATABLE :: line
    
    !***************************************************************************
    
    ALLOCATE(lines(0))
    
    DO
      CALL GetLine(unit, line, stat, iomsg)
      IF (stat > 0) RETURN      ! Error
      IF (stat /= 0) THEN       ! End of file
        stat = 0
        EXIT
      END IF
      ! Add the line to our list of lines.
      CALL Append(lines, line)
    END DO
    
  END SUBROUTINE GetLines_msg
  
END MODULE FileUtilities
