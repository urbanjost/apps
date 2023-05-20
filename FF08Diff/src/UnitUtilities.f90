! $Id: UnitUtilities.f90 894 2012-10-08 00:32:30Z ian $
! ff08 source code copyright 2012 M.E.G.M.S.  See LICENCE.txt for licence.
!> @file
!! Defines the UnitUtilities module.


!*******************************************************************************
!!
!> Procedures for managing unit numbers.
!!
!! This module is redundant with the introduction of F2008's NEWUNIT specifier 
!! for the open statement, however, ff08 is currently written against F2003.
!!
!! We assume that the unit number range from 1000 to 1999 is exclusively 
!! available for our use.
!!
!! Nothing in this module actually relies on the returned unit numbers being 
!! used as logical unit numbers - we just manage the in-use or not-in-use 
!! status of a list of numbers (we don't use INQUIRE statements to make sure 
!! that some other inconsistent piece of code has decided to use one of the 
!! unit numbers nominally under our control without this code knowing).
!!
!! This module uses module variables to manage it's state.  It is not 
!! threadsafe.
!!
!! We use a simple sorted linear list to manage the units in use.  If large 
!! numbers of units are in use then this will not be terribly efficient.

MODULE UnitUtilities
  
  IMPLICIT NONE
  
  PRIVATE
  
  !-----------------------------------------------------------------------------
  
  PUBLIC :: GetUnit
  PUBLIC :: ReleaseUnit
  
  !-----------------------------------------------------------------------------
  
  !> Get a free unit number, marking it in-use in the process.
  INTERFACE GetUnit
    MODULE PROCEDURE GetUnit_
  END INTERFACE GetUnit
  
  !> Release a unit number, marking it as not in-use in the process.
  INTERFACE ReleaseUnit
    MODULE PROCEDURE ReleaseUnit_
  END INTERFACE ReleaseUnit
  
  !-----------------------------------------------------------------------------
  
  !> First unit number in the range that this module manages.
  INTEGER, PARAMETER :: first_unit = 1000
  
  !> Last unit number in the range that this module manages.
  INTEGER, PARAMETER :: last_unit = 9999
  
  !> Last unit number that was returned by GetUnit.
  !!
  !! Zero indicates that GetUnit is yet to be called.
  INTEGER :: last_used = 0
  
  !> List of the unit numbers that have been given out by GetUnit but are yet to 
  !! be given back by ReleaseUnit.  This is maintained in ascending order.
  !!
  !! Numbers that are not in this list are free to be returned by future calls 
  !! to GetUnit.
  INTEGER, ALLOCATABLE :: units_in_use(:)
  
CONTAINS
  
  !*****************************************************************************
  !!
  !> Get the next available (not in use) unit number.
  !!
  !! @param[out]    unit              A unit number that was not in use prior 
  !! to the call.
  !!
  !! The unit number in @a unit is marked in use.  Call ReleaseUnit to then 
  !! mark it as available again.
  
  SUBROUTINE GetUnit_(unit)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    INTEGER, INTENT(OUT) :: unit
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: initial_unit   ! Starting search number.
    
    !***************************************************************************
    
    ! get initial unit to test
    IF (last_used == 0) THEN
      unit = first_unit
    ELSE
      unit = last_used + 1
      IF (unit > last_unit) unit = first_unit
    END IF
    
    initial_unit = unit
    DO
      ! Check if it is in use
      IF (.NOT. in_use(unit)) EXIT
      
      ! Get new unit.
      unit = unit + 1
      IF (unit > last_unit) unit = first_unit
      IF (unit == initial_unit) THEN
        ! We've looped back - there are no free units
        unit= -1
        RETURN
      END IF
    END DO
    
    ! Add the select unit to the list of units in use.
    CALL set_in_use(unit)
    
  END SUBROUTINE GetUnit_
    
  
  !*****************************************************************************
  !!
  !> Release a unit number (mark it as not being in use).
  !!
  !! @param[in]     unit              The number of the unit to be released.
  !!
  !! The unit number becomes available again to be returned from a call to 
  !! GetUnit.
  !!
  !! We remove the unit from the units_in_use module variable and shrink that 
  !! array.
  !!
  !! If the unit number was not marked as in use then we do nothing.
  
  SUBROUTINE ReleaseUnit_(unit)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    INTEGER, INTENT(IN) :: unit
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: iu             ! Unit in use index.
    
    ! Temporary array for shrinking the units in use array.
    INTEGER, ALLOCATABLE :: tmp(:)
    
    !***************************************************************************
    
    IF (.NOT. ALLOCATED(units_in_use)) RETURN
    
    ! Find the index 
    DO iu = 1, SIZE(units_in_use)
      IF (units_in_use(iu) == unit) EXIT
    END DO
    
    ! Unit wasn't in use.
    IF (iu > SIZE(units_in_use)) RETURN
    
    ALLOCATE(tmp(SIZE(units_in_use) - 1))
    tmp(:iu-1) = units_in_use(:iu-1)
    tmp(iu:) = units_in_use(iu+1:)
    
  END SUBROUTINE ReleaseUnit_
  
  
  !*****************************************************************************
  !!
  !> Test whether a unit number is in use or not.
  !!
  !! @param[in]     unit              The unit number to test.
  !!
  !! @returns .TRUE. if the unit number in @a unit was present in the list 
  !! of units in the units_in_use module variable, .FALSE. otherwise.
  
  FUNCTION in_use(unit) RESULT(b)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    INTEGER, INTENT(IN) :: unit
    
    ! Function result
    LOGICAL :: b
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: iu             ! Unit in use index.
    
    !***************************************************************************
    
    b = ALLOCATED(units_in_use)
    IF (.NOT. b) RETURN
    
    DO iu = 1, SIZE(units_in_use)
      IF (units_in_use(iu) > unit) EXIT
      b = unit == units_in_use(iu)
      IF (b) RETURN
    END DO
    
    b = .FALSE.
    
  END FUNCTION in_use
  
  
  !*****************************************************************************
  !!
  !> Mark a certain unit number as being in-use by adding it to the 
  !! units_in_use module variable.
  !!
  !! @param[in]     unit              Number of the unit to mark in use.
  !!
  !! This procedure maintains the sort order (ascending) of the numbers stored 
  !! in units_in_use.
  
  SUBROUTINE set_in_use(unit)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    INTEGER, INTENT(IN) :: unit
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: iu             ! Unit in use index.
    
    ! Temporary array for growing the units in use array.
    INTEGER, ALLOCATABLE :: tmp(:)
    
    !***************************************************************************
    
    IF (.NOT. ALLOCATED(units_in_use)) THEN
      ALLOCATE(units_in_use(1))
      units_in_use(1) = unit
      RETURN
    END IF
    
    DO iu = 1, SIZE(units_in_use)
      IF (units_in_use(iu) > unit) EXIT
    END DO
    
    ALLOCATE(tmp(SIZE(units_in_use) + 1))
    tmp(:iu-1) = units_in_use(:iu-1)
    tmp(iu) = unit
    tmp(iu+1:) = units_in_use(iu:)
    CALL MOVE_ALLOC(tmp, units_in_use)
    
  END SUBROUTINE set_in_use
  
END MODULE UnitUtilities
