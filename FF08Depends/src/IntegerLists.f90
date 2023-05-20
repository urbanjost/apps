! $Id: IntegerLists.f90 1167 2014-02-19 05:45:19Z ian $
! ff08 source code copyright 2012 M.E.G.M.S.  See LICENCE.txt for licence.
!> @file
!! Defines the IntegerLists module.


!*******************************************************************************
!!
!> Operations on lists (vectors) of integers.
!!
!! By defining a type that can represent a 1D array of integers you can then 
!! have arrays of 1D arrays.
!!
!! IntegerList, IntegerElement, IntegerVector - multiple possibilities 
!! existed for the type name.  *List had been used elsewhere for similar 
!! stuff, so that's what it is.  *Vector would probably have been more 
!! appropriate as an order is maintained.
!!
!! Ifort 12.1 has some bugs around operations using array constructors and 
!! derived types, so some of the procedures in this module are longer than 
!! they might otherwise need to be (or procedures exist when the equivalent 
!! operation could easily be expressed directly in client code).
!!
!! Bloody ifort 12.1.  Bane of my life.

MODULE IntegerLists
  
  IMPLICIT NONE
  
  PRIVATE
  
  !-----------------------------------------------------------------------------
  ! Expose module procedures and interfaces
  
  PUBLIC :: Append
  PUBLIC :: Collapse
  PUBLIC :: Find
  PUBLIC :: Prepend
  PUBLIC :: Remove
  
  !-----------------------------------------------------------------------------
  ! The IntegerList type
  
  !> Type to represent an integer list.
  !!
  !! Only really useful in arrays of IntegerLists, where the length 
  !! of the list in each element needs to vary.  Otherwise you can just use 
  !! a plain vector of integers.
  TYPE, PUBLIC :: IntegerList
    !> The items in the list.
    INTEGER, ALLOCATABLE :: items(:)
  END TYPE IntegerList
  
  !-----------------------------------------------------------------------------
  ! Interfaces
  
  !> Append items to an existing list.
  INTERFACE Append
    MODULE PROCEDURE Append_
  END INTERFACE Append
  
  !> Collapse multiple lists into a single list.
  INTERFACE Collapse
    MODULE PROCEDURE Collapse_s
  END INTERFACE Collapse
  
  !> Find an integer in a vector of IntegerLists.
  INTERFACE Find
    MODULE PROCEDURE Find_
  END INTERFACE Find
  
  !> Prepend items to a list.
  INTERFACE Prepend
    MODULE PROCEDURE Prepend_
  END INTERFACE Prepend
  
  !> Remove items from a list.
  INTERFACE Remove
    MODULE PROCEDURE Remove_
  END INTERFACE Remove
  
CONTAINS
  
  !*****************************************************************************
  !!
  !> Append a vector of integers to a vector of integer lists.
  !!
  !! @param[in,out] list              The vector of integer lists.
  !!
  !! @param[in]     items             The vector of integers.
  
  SUBROUTINE Append_(list, items)
  
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(IntegerList), INTENT(INOUT), ALLOCATABLE :: list(:)
    INTEGER, INTENT(IN) :: items(:)
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: sz             ! Size of the existing list.
    
    ! Temporary for growing the list.
    TYPE(IntegerList), ALLOCATABLE :: tmp(:)
    
    !***************************************************************************
    
    IF (.NOT. ALLOCATED(list)) THEN
      sz = 0
      ALLOCATE(tmp(1))
    ELSE
      sz = SIZE(list)
      ALLOCATE(tmp(sz+1))
      tmp(1:sz) = list
    END IF
    
    tmp(sz+1)%items = items
    CALL MOVE_ALLOC(tmp, list)
    
  END SUBROUTINE Append_
  
  
  !*****************************************************************************
  !!
  !> Find an integer in a vector of IntegerLists.
  !!
  !! @param[in]     list              The vector of IntegerLists to search.
  !!
  !! @param[in]     item              The integer to search for.
  !!
  !! @returns The element index of @a list that contains @a item.
  
  FUNCTION Find_(list, item) RESULT(i)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(IntegerList), INTENT(IN) :: list(:)
    INTEGER, INTENT(IN) :: item
    
    ! Function result
    INTEGER :: i
    
    !***************************************************************************
    
    DO i = 1, SIZE(list)
      IF (ANY(list(i)%items == item)) RETURN
    END DO
    i = 0
    
  END FUNCTION Find_
  
  
  !*****************************************************************************
  !!
  !> Prepends a vector of integers to a vector of IntegerLists.
  !!
  !! This is equivalent to the operation:
  !! @code
  !! list = [ IntegerList(items), list ]
  !! @endcode
  !!
  !! but the explicit operations below may be more efficient.
  !!
  !! This is also a work around to an ifort 12.1 bug.
  
  SUBROUTINE Prepend_(list, items)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(IntegerList), INTENT(INOUT), ALLOCATABLE :: list(:)
    INTEGER, INTENT(IN) :: items(:)
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: sz             ! Existing size of the list.
    
    ! Temporary for growing the list.
    TYPE(IntegerList), ALLOCATABLE :: tmp(:)
    
    !***************************************************************************
    
    IF (.NOT. ALLOCATED(list)) THEN
      sz = 0
      ALLOCATE(tmp(1))
    ELSE
      sz = SIZE(list)
      ALLOCATE(tmp(sz+1))
      tmp(2:sz+1) = list
    END IF
    
    tmp(1)%items = items
    CALL MOVE_ALLOC(tmp, list)
    
  END SUBROUTINE Prepend_
  
  
  !*****************************************************************************
  !!
  !> Collapse a vector of IntegerLists into a single IntegerList.
  !!
  !! @param[in]     list              The vector of IntegerLists.
  !!
  !! @param[in]     start             Starting index for the collapse.
  !!
  !! All items that are in elements of @a list from @a start on will end up 
  !! in @a list(@a start).  If there are duplicate integers in the collapsed 
  !! elements then there will be duplicates in the resulting element.
  
  SUBROUTINE Collapse_s(list, start)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(IntegerList), INTENT(INOUT), ALLOCATABLE :: list(:)
    INTEGER, INTENT(IN) :: start
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: i              ! List index.
    
    !***************************************************************************
    
    DO i = start + 1, SIZE(list)
      list(start)%items = [list(start)%items, list(i)%items]
    END DO
    list = list(1:start)
    
  END SUBROUTINE Collapse_s
  
  
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! Operations on integer arrays
  !
  ! Note that these don't work on IntegerList's.
  
  
  !*****************************************************************************
  !!
  !> Remove integers from one integer array that are in another integer array.
  !!
  !! @param[in]     a                 The array to have items possibly 
  !! removed from it.
  !!
  !! @param[in]     b                 The items to be removed.
  !!
  !! Remove items that are in @a b from @a a.
  
  SUBROUTINE Remove_(a, b)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    INTEGER, INTENT(INOUT), ALLOCATABLE :: a(:)
    INTEGER, INTENT(IN) :: b(:)
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Temporary vector of integers.
    INTEGER, ALLOCATABLE :: tmp(:)
    
    INTEGER :: i              ! Index into a.
    
    !***************************************************************************
    
    ALLOCATE(tmp(0))
    DO i = 1, SIZE(a)
      IF (ALL(a(i) /= b)) tmp = [tmp, a(i)]
    END DO
    CALL MOVE_ALLOC(tmp, a)
    
  END SUBROUTINE Remove_
  
  
  !*****************************************************************************
  !!
  !> Create a list of indices from logical mask.
  !!
  !! @param[in]     mask              The logical mask.
  !!
  !! @returns The indices for the elements in mask that are true.
  
  FUNCTION Indices_(mask) RESULT(indices)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    LOGICAL, INTENT(IN) :: mask(:)
    
    ! Function result.
    INTEGER :: indices(COUNT(mask))
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: im             ! Mask index.
    INTEGER :: ii             ! Output index.
    
    !***************************************************************************
    
    ii = 1
    DO im = 1, SIZE(mask)
      IF (mask(im)) THEN
        indices(ii) = im
        ii = ii + 1
      END IF
    END DO
    
  END FUNCTION Indices_
  
END MODULE IntegerLists
