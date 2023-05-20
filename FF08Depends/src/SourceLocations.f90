! $Id: SourceLocations.f90 1543 2015-02-19 02:16:17Z ian $
! ff08 source code copyright 2012 M.E.G.M.S.  See LICENCE.txt for licence.
!> @file
!! Defines the SourceLocations module.


!*******************************************************************************
!!
!> Provides the SourceLocation type for dealing with positions in source 
!! files.
!!
!! The extension of ErrorLocation defined by this module can be use with the 
!! persistent (once created they never die) Source objects created by the 
!! processor.  Because those Source objects are guaranteed to be persistent 
!! until near the very end of the host program, we can reference them with 
!! a pointer object rather than storing the filename.
!!
!! We anticipated that there might be a large number of SourceLocation objects 
!! created (one for each token), so the idea was to reduce the memory 
!! footprint of each object.

MODULE SourceLocations
  
  USE ErrorLocations
  USE Sources
  
  IMPLICIT NONE
  
  PRIVATE
  
  !-----------------------------------------------------------------------------
  
  PUBLIC :: SourceLocation
  ! This is a temporary name for the overloaded structure constructor, 
  ! to accomodate issues that ifort 15.0.2 has when using this language 
  ! feature.  When overloaded structure constructors work reliably, it 
  ! can be removed and the generic interface renamed to be the same as 
  ! the type.
  PUBLIC :: SourceLocationIfortBug
  
  !-----------------------------------------------------------------------------
  
  !> Describes a location in a source of source.
  TYPE, EXTENDS(ErrorLocation) :: SourceLocation
    !> Pointer to the element in the global source vector.
    !!
    !! If not associated, then we have no idea what the file was.
    !!
    !! This could also have been an index into that vector, which might 
    !! have been even more frugal.
    CLASS(Source), POINTER :: file => NULL()
    !> Line number in the source.
    !!
    !! If zero then the line number hasn't been specified.
    INTEGER :: line = 0
    !> Column number in the source.
    !!
    !! If zero then the column number hasn't been specified.
    INTEGER :: column = 0
  CONTAINS
    PROCEDURE :: ToString => srcloc_ToString
  END TYPE SourceLocation
  
  !> Create a source position object.
  INTERFACE SourceLocationIfortBug
    MODULE PROCEDURE SourceLocation_from_file
    MODULE PROCEDURE NoLocation_
  END INTERFACE SourceLocationIfortBug
  
CONTAINS
  
  !*****************************************************************************
  !!
  !> Construct a SourceLocation object from a file and a column position.
  !!
  !! @param[in]     file              The file (a source of source) that 
  !! the source location refers to.
  !!
  !! @param[in]     column            The column position in the file for 
  !! the location.
  !!
  !! @returns A properly constructed SourceLocation object.
  !!
  !! The line position in the file comes from the current line property 
  !! of @a file.
  
  FUNCTION SourceLocation_from_file(file, column) RESULT(tp)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    ! Pointer attribute on this argument means we can't be pure.
    CLASS(Source), INTENT(IN), POINTER :: file
    INTEGER, INTENT(IN) :: column
    TYPE(SourceLocation) :: tp
    
    !***************************************************************************
    
    tp%file => file
    tp%line = file%line
    tp%column = column
    
  END FUNCTION SourceLocation_from_file
  
  
  !*****************************************************************************
  !!
  !> Construct a no-position SourceLocation object.
  !!
  !! @returns A SourceLocation object that is defined, but doesn't nominate 
  !! any particular location.
  
  PURE FUNCTION NoLocation_() RESULT(no_pos)
    
    TYPE(SourceLocation) :: no_pos
    
    !***************************************************************************
    
    no_pos%file => NULL()
    no_pos%line = 0
    no_pos%column = 0
    
  END FUNCTION NoLocation_
  
  
  !*****************************************************************************
  !!
  !> Implementation of SourceLocation%ToString - provide a string 
  !! representation of the source location.
  !!
  !! @param[in]     loc               The source location object.
  !!
  !! @returns A character representation of @a loc.
  !!
  !! Really this should be influenced by host configuration.  Not sure how 
  !! to achieve that.
  
  PURE FUNCTION srcloc_ToString(loc) RESULT(str)
    
    USE CharUtils
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(SourceLocation), INTENT(IN) :: loc
    
    ! Function result
    CHARACTER(:), ALLOCATABLE :: str
    
    !***************************************************************************
    
    IF (ASSOCIATED(loc%file)) THEN
      str = loc%file%name 
    ELSE
      str = ''
    END IF
    
    IF (loc%line /= 0) THEN
      str = str // '(' // ToString(loc%line)
      IF (loc%column /= 0) THEN
        str = str // ',' // ToString(loc%column) // ')'
      ELSE
        str = str // ')'
      END IF
    END IF
    
  END FUNCTION srcloc_ToString
  
END MODULE SourceLocations
