! $Id: SourceFiles.f90 1311 2014-09-09 03:59:39Z ian $
! ff08 source code copyright 2012 M.E.G.M.S.  See LICENCE.txt for licence.
!> @file
!! Defines the SourceFiles module.


!*******************************************************************************
!!
!> Types and procedures for manipulating source files.
!!
!! A SourceFile is an extension of the Source type.  Use a SourceFile for 
!! sources of source that are files (in a strict sense formatted sequential 
!! files).

MODULE SourceFiles
  
  USE CompilerKinds
  USE Sources
  
  IMPLICIT NONE
  
  PRIVATE
  
  !-----------------------------------------------------------------------------
  ! Expose module procedures and interfaces
  
  PUBLIC :: OpenSourceFile
  
  !-----------------------------------------------------------------------------
  
  !> Represents a source of source that comes from a file.
  TYPE, PUBLIC, EXTENDS(Source) :: SourceFile
    PRIVATE   ! Can't be constructed outside of this module.
    !> Unit to use to read the file.  Set to zero if the file is not open.
    INTEGER :: unit
  CONTAINS
    ! Deferred bindings from parent type.
    PROCEDURE :: ReadLine => sourcefile_ReadLine
    PROCEDURE :: Reset => sourcefile_Reset
  END TYPE SourceFile
  
  !-----------------------------------------------------------------------------
  
  !> Open a source file.
  INTERFACE OpenSourceFile
    MODULE PROCEDURE OpenSourceFile_
  END INTERFACE OpenSourceFile
  
  !-----------------------------------------------------------------------------
  
  !> Size of the buffer used to read in a line from the source file.
  INTEGER, PARAMETER :: buffer_size = 132
  
  !> Component for underlying Fortran processor reporting errors.
  CHARACTER(*), PARAMETER :: comp = 'Fortran runtime'
  
CONTAINS
  
  !*****************************************************************************
  !!
  !> Opens a source file and adds it to the global list of sources.
  !!
  !! @param[in]     name              Name of the file to open.
  !!
  !! @param[out]    source_file       A pointer to an object that describes 
  !! the source file.
  !!
  !! @param[out]    err               Optional error object.  If not present 
  !! the the source file is not immediately opened.  In that case any errors 
  !! associated with opening the file will be reported instead when 
  !! SourceFile%ReadLine is called.
  !!
  !! @param[in]     parent_file       Optional parent file.
  
  SUBROUTINE OpenSourceFile_(name, source_file, err)
    
    USE Errors
    USE UnitUtilities
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CHARACTER(*,KIND=fnck), INTENT(IN) ::name
    TYPE(SourceFile), INTENT(OUT), POINTER :: source_file
    TYPE(Error), INTENT(OUT), OPTIONAL :: err
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: stat           ! Error code from OPEN
    
    ! The new source object prior to addition to the global list.
    CLASS(Source), ALLOCATABLE :: new_source_file
    
    ! The new source object, after addition to the global list.
    CLASS(Source), POINTER :: list_ptr
    
    ! Error message reported by the underlying processor.
    CHARACTER(256) :: iomsg
    
    !***************************************************************************
    
    ! Create the source object and add it to the persistent list.
    ALLOCATE(new_source_file, SOURCE=SourceFile(NAME=name, LINE=0, UNIT=0))
    CALL AddSource(new_source_file, list_ptr)
    
    ! ifort 13.0 crash here due to polymorphic component/move_alloc bug.
    SELECT TYPE (list_ptr)
    TYPE IS (SourceFile)
      source_file => list_ptr
      IF (PRESENT(err)) THEN
        ! Logic in ReadLine means that we could leave the unit zero and 
        ! not open the file, but it is convenient to get any errors about 
        ! the file not existing, not being readable, etc, at the time the 
        ! source object is created.
        CALL GetUnit(source_file%unit)
        OPEN( UNIT=source_file%unit, FILE=name, STATUS='OLD',  &
            ACTION='READ', POSITION='REWIND', IOSTAT=stat, IOMSG=iomsg )
        IF (stat /= 0) THEN
          err = ConstructError( CODE=stat, COMPONENT=comp,  &
              MSG='Error opening file "' // name // '": ' // TRIM(iomsg) )
          CALL ReleaseUnit(source_file%unit)
          source_file%unit = 0
        END IF
      END IF
      
    CLASS DEFAULT
      STOP 'ptr argument of AddSource came back with the wrong dynamic type'
      
    END SELECT
    
  END SUBROUTINE OpenSourceFile_
  
  
  !*****************************************************************************
  !!
  !> Read a line from a source file.
  !!
  !! @param[in,out] src               The source file object.
  !!
  !! @param[out]    line              The line of source.
  !!
  !! @param[out]    err               Error object.
  !!
  !! Errors returned from this procedure do not reference any particular 
  !! component.
  
  SUBROUTINE sourcefile_ReadLine(src, line, err)
    
    USE Errors
    USE UnitUtilities
    
    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: IOSTAT_EOR, IOSTAT_END
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(SourceFile), INTENT(INOUT) :: src
    CHARACTER(:,KIND=scck), INTENT(OUT), ALLOCATABLE :: line
    TYPE(Error), INTENT(OUT) :: err
    
    !---------------------------------------------------------------------------
    ! Local constants
    
    ! Format specification for reading text from the file.
    CHARACTER(*), PARAMETER :: fmt = "(A)"
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Buffer to read into.
    CHARACTER(buffer_size,KIND=scck) :: buffer
    
    INTEGER :: stat           ! IOSTAT code.
    INTEGER :: sz             ! Number of characters read.
    
    ! Error message reported by the underlying processor.
    CHARACTER(256) :: iomsg
    
    !***************************************************************************
    
    line = scck_''
    
    ! If the source file was previously reset then we may need to (re)open 
    ! the file.
    IF (src%unit == 0) THEN
      CALL GetUnit(src%unit)
      OPEN( UNIT=src%unit, FILE=src%name, STATUS='OLD',  &
          ACTION='READ', POSITION='REWIND', IOSTAT=stat, IOMSG=iomsg )
      IF (stat /= 0) THEN
        err = ConstructError( CODE=stat, COMPONENT=comp,  &
            MSG='Error opening file "' // src%name // '": ' // TRIM(iomsg) )
        CALL ReleaseUnit(src%unit)
        src%unit = 0
        RETURN
      END IF
    END IF
    
    src%line = src%line + 1
    
    ! Do a series of non-advancing reads until we've got a complete line.
    DO
      READ (src%unit, fmt, IOSTAT=stat, IOMSG=iomsg, ADVANCE='NO', SIZE=sz)  &
          buffer
      
      IF (stat == 0) THEN
        ! We got a partial chunk of the line.
        line = line // buffer
      ELSE IF (stat == IOSTAT_EOR) THEN
        ! We got the remainder of the line.  If this is the first time through 
        ! the whole line fitted in the buffer.
        line = line // buffer(:sz)
        EXIT
      ELSE IF (stat == IOSTAT_END) THEN
        IF (LEN(line) == 0) THEN
          ! Nothing read - normal end of file.
          DEALLOCATE(line)
          RETURN
        ELSE
          ! ? Error?
          ! line = line // buffer
          EXIT
        END IF
      ELSE
        err = ConstructError( CODE=stat, COMPONENT=comp,  &
            MSG='Error reading file "' // src%name // '": ' // TRIM(iomsg) )
        RETURN
      END IF
    END DO
    
  END SUBROUTINE sourcefile_ReadLine
  
  
  !*****************************************************************************
  !!
  !> Implementation of SourceFile%Reset - reset the source file to its 
  !! initial state.
  !!
  !! @param[in,out] src               The source file object.
  !!
  !! Unlike the real initial state of the SourceFile object, we close 
  !! the logical unit to conserve resources.  The next time that ReadLine 
  !! is called it will be opened again.  The idea behind this was to 
  !! conserve the number of logical units that might be in use at any one 
  !! time.
  
  SUBROUTINE sourcefile_Reset(src)
    
    USE UnitUtilities
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(SourceFile), INTENT(INOUT) :: src
    
    !***************************************************************************
    
    IF (src%unit == 0) RETURN
    
    CLOSE(src%unit)
    CALL ReleaseUnit(src%unit)
    src%unit = 0
    
    src%line = 0
    
  END SUBROUTINE sourcefile_Reset
  
END MODULE SourceFiles
