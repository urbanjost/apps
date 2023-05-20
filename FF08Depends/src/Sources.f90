! $Id: Sources.f90 2363 2016-08-22 01:29:48Z ian $
! ff08 source code copyright 2012 M.E.G.M.S.  See LICENCE.txt for licence.
!> @file
!! Defines the Sources module.


!*******************************************************************************
!!
!> Defines the Source type that defines the interface for a source of Source.
!!
!! We have resisted the temptation to name the type for a source of source 
!! as a bottle.  Boom-tish.

MODULE Sources
  
  USE CompilerKinds
  
  IMPLICIT NONE
  
  PRIVATE
  
  !-----------------------------------------------------------------------------
  ! Expose module procedures and interfaces.
  
  PUBLIC :: AddSource
  PUBLIC :: GetSource
  PUBLIC :: ReleaseSources
  PUBLIC :: SourceCount
  
  PUBLIC :: OpenSourceEntity
  
  !-----------------------------------------------------------------------------
  
  !> Abstract parent class for a source of ... source.
  !!
  !! This is more or less just a source of text that remembers a position.
  TYPE, PUBLIC, ABSTRACT :: Source
    !> Name of the source.
    CHARACTER(:, KIND=fnck), ALLOCATABLE :: name
    
    !> Number of the last line read from the source.
    !!
    !! A non-zero value indicates that the source has been read from since it 
    !! was last reset.
    INTEGER :: line = 0
  CONTAINS
    !> Call to read a line from the source.
    PROCEDURE(src_ReadLine), DEFERRED :: ReadLine
    !> Reset the source object back to its initial state.
    PROCEDURE(src_Reset), DEFERRED :: Reset
  END TYPE Source
  
  !> Deferred binding interfaces for Source.
  ABSTRACT INTERFACE
    !> Get a line of source.
    !!
    !! @param[in,out] src               The source file object.
    !!
    !! @param[out]    line              The line of source.
    !!
    !! @param[out]    err               Error object.
    SUBROUTINE src_ReadLine(src, line, err)
      USE CompilerKinds
      USE Errors
      IMPORT :: Source
      IMPLICIT NONE
      !-------------------------------------------------------------------------
      CLASS(Source), INTENT(INOUT) :: src
      CHARACTER(:,KIND=scck), INTENT(OUT), ALLOCATABLE :: line
      TYPE(Error), INTENT(OUT) :: err
    END SUBROUTINE src_ReadLine
    
    !> Reset the source object back to it's initial state.
    !!
    !! @param[in,out] src               The source file object.
    SUBROUTINE src_Reset(src)
      IMPORT :: Source
      IMPLICIT NONE
      !-------------------------------------------------------------------------
      CLASS(Source), INTENT(INOUT) :: src
    END SUBROUTINE src_Reset
  END INTERFACE
  
  !> Element in a vector of sources, potentially of different types.
  TYPE :: SourceElement
    !> The item for a particular element.
    CLASS(Source), ALLOCATABLE :: item
  END TYPE SourceElement
  
  !> All the sources accessed by the processor.
  !!
  !! The processor never forgets a source of source over the entire duration 
  !! of the program (unless ReleaseSources is called).  This ensures that any 
  !! position references continue to be valid references to the source.
  !!
  !! Currently we tend to store pointers to the items in this vector in 
  !! various places (tokens and source forms).  It may have been more 
  !! sensible in the token case to simply store the index.
  TYPE(SourceElement), ALLOCATABLE, TARGET, SAVE :: source_vector(:)
  
  !> Element in a vector source references, potentially of different types.
  TYPE, PUBLIC :: SourceRefElement
    !>  The item for a particular element.
    !!
    !!  If associated, then this should be a pointer to an element of 
    !! the source_vector module variable above.
    CLASS(Source), POINTER :: item
  END TYPE SourceRefElement
  
  !-----------------------------------------------------------------------------
  
  !> A source of source that comes from some sort of data object (a constant 
  !! or variable).  Public attribute via access-stmt above.
  TYPE, EXTENDS(Source), PUBLIC :: SourceEntity
    !> The data that makes up the source.
    CHARACTER(:,scck), ALLOCATABLE :: records(:)
  CONTAINS
    PROCEDURE :: ReadLine => sourceentity_ReadLine
    PROCEDURE :: Reset => sourceentity_Reset
  END TYPE
  
CONTAINS
  
  !*****************************************************************************
  !!
  !> Add a source to the global list of sources.
  !!
  !! @param[in,out] item              The source object to be added to the 
  !! global list.  The allocation for the object gets moved into the global 
  !! list so the thing associated with the @a item argument will be not 
  !! allocated after this call.
  !!
  !! @param[out[    ptr               A pointer to that source object, after 
  !! it has been added to the list.
  
  SUBROUTINE AddSource(item, ptr)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(Source), INTENT(INOUT), ALLOCATABLE :: item
    CLASS(Source), INTENT(OUT), POINTER :: ptr
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Temporary array for growing the global array.
    TYPE(SourceElement), ALLOCATABLE, TARGET :: tmp(:)
    
    INTEGER :: i              ! Source index
    
    !***************************************************************************
    
    IF (.NOT. ALLOCATED(source_vector)) THEN
      ALLOCATE(tmp(1))
    ELSE
      ALLOCATE(tmp(SIZE(source_vector)+1))
      DO i = 1, SIZE(source_vector)
        ! Things that point to source_vector(i)%item will now point to 
        ! tmp(i)%item as both have target attribute.
        CALL MOVE_ALLOC(source_vector(i)%item, tmp(i)%item)
      END DO
    END IF
    
    ! Store our new object.
    CALL MOVE_ALLOC(item, tmp(SIZE(tmp))%item)
    CALL MOVE_ALLOC(tmp, source_vector)
    
    ptr => source_vector(SIZE(source_vector))%item
    
  END SUBROUTINE AddSource
  
  
  !*****************************************************************************
  !!
  !> Release all source objects.
  !!
  !! This is primarily to aid leak checking.
  
  SUBROUTINE ReleaseSources
    
    IF (ALLOCATED(source_vector)) DEALLOCATE(source_vector)
    
  END SUBROUTINE ReleaseSources
  
  
  !*****************************************************************************
  !!
  !> Get the number of sources in the global vector.
  !!
  !! @returns The size of the global source vector.
  
  PURE FUNCTION SourceCount() RESULT(c)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    ! Function result
    INTEGER :: c
    
    !***************************************************************************
    
    IF (ALLOCATED(source_vector)) THEN
      c = SIZE(source_vector)
    ELSE
      c = 0
    END IF
    
  END FUNCTION SourceCount
  
  
  !*****************************************************************************
  !!
  !> Return a pointer to a source at a particular location in the global 
  !! vector.
  !!
  !! @param[in]     is                The index of the source.
  !!
  !! @param[out]    ptr               A pointer that points to the source 
  !! object that is at that location.
  !!
  !! The source object must exist at that location.
  
  SUBROUTINE GetSource(is, ptr)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    INTEGER, INTENT(IN) :: is
    CLASS(Source), INTENT(OUT), POINTER :: ptr
    
    !***************************************************************************
    
    ptr => source_vector(is)%item
    
  END SUBROUTINE GetSource
  
  
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! Procedures for SourceEntity's.
  
  
  !*****************************************************************************
  !!
  !> Construct a SourceEntity.
  !!
  !! @param[in]     str               The data source for the source entity.  
  !! This is also used as the source's name.  Each element in @a str becomes 
  !! one line from the point of view of things reading the SourceEntity.
  !!
  !! @param[out]    source_entity     A SourceEntity object that will return 
  !! its lines from @a str.
  
  SUBROUTINE OpenSourceEntity(str, source_entity)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CHARACTER(*,KIND=scck), INTENT(IN) :: str(:)
    TYPE(SourceEntity), INTENT(OUT), POINTER :: source_entity
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! The new source object prior to addition to the global list.
    CLASS(Source), ALLOCATABLE :: new_source_file
    
    ! The new source object, after addition to the global list.
    CLASS(Source), POINTER :: list_ptr
    
    !***************************************************************************
    
    ! Protect against silliness.
    IF (SIZE(str) > 0) THEN
      ALLOCATE(new_source_file, SOURCE=SourceEntity(  &
          NAME='{{' // TRIM(str(1)) // '}}', LINE=0, RECORDS=str ))
    ELSE
      ALLOCATE(new_source_file, SOURCE=SourceEntity( NAME='(empty)',  &
          LINE=0, RECORDS=str ))
    END IF
    CALL AddSource(new_source_file, list_ptr)
    
    SELECT TYPE (list_ptr)
    TYPE IS (SourceEntity)
      source_entity => list_ptr
    CLASS DEFAULT
      STOP 'Internal error in Sources%%OpenSourceEntity, &
          &ptr argument of AddSource came back with the wrong dynamic type.'
    END SELECT
    
  END SUBROUTINE OpenSourceEntity
  
  
  !*****************************************************************************
  !!
  !> Implementation of SourceEntity%ReadLine - return the next line from the 
  !! source entity.
  !!
  !! @param[in,out] src               The SourceEntity object.
  !!
  !! @param[out]    line              The line read from the SourceEntity.
  !!
  !! @param[out]    err               Error object.
  !!
  !! A line from a SourceEntity is equivalent to an element in the CHARACTER 
  !! array that was used to initialise the object.
  
  SUBROUTINE sourceentity_ReadLine(src, line, err)
    
    USE Errors
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(SourceEntity), INTENT(INOUT) :: src
    CHARACTER(:,KIND=scck), INTENT(OUT), ALLOCATABLE :: line
    TYPE(Error), INTENT(OUT) :: err
    
    !***************************************************************************
    
    ! First call src%line will be zero.
    src%line = src%line + 1
    
    ! Test for "end of file".  Flag by returning line unallocated.
    IF (src%line > SIZE(src%records)) RETURN
    
    ! Return the line from our list of records.
    line = src%records(src%line)
    
    err = NoError()
    
  END SUBROUTINE sourceentity_ReadLine
  
  
  !*****************************************************************************
  !!
  !> Implementation of SourceEntity%Reset - reset the source back to its 
  !! initial conditions.
  !!
  !! @param[in,out] src               The SourceEntity object.
  
  SUBROUTINE sourceentity_Reset(src)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(SourceEntity), INTENT(INOUT) :: src
    
    !***************************************************************************
    
    src%line = 0
    
  END SUBROUTINE sourceentity_Reset
  
END MODULE Sources
