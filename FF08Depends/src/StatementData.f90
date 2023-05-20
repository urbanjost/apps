! $Id: StatementData.f90 2181 2016-06-05 01:32:08Z ian $
! ff08 source code copyright 2012 M.E.G.M.S.  See LICENCE.txt for licence.
!> @file
!! Defines the StatementData module.


!*******************************************************************************
!!
!> Defines the StData type and associated procedures.

MODULE StatementData
  
  USE LabelsUtils
  USE Tokens
  ! If client code wants the integer form of GetStName then it needs to 
  ! USE that specifically.
  USE Statements, used_getstname => GetStName
  
  IMPLICIT NONE
  
  PRIVATE
  
  !-----------------------------------------------------------------------------
  
  PUBLIC :: AddStmt
  PUBLIC :: Move
  PUBLIC :: GetStName
  
  !-----------------------------------------------------------------------------
  
  !> Statement data.
  !!
  !! The tokens, label (if present) and basic classification of a statement.
  !!
  !! To avoid having multiple copies of statement information extant at the 
  !! same time a Move procedure is provided that copys the non-allocatable 
  !! components and moves the allocatable components.  This move procedure 
  !! must be updated if components are added or changed.
  !!
  !! (This whole "move the statement data" strategy is now a bit out of 
  !! date, because practical requirements meant that we needed to make 
  !! a copy anyway.)
  TYPE, PUBLIC :: StData
    !> Statement type as determined by Classify.
    INTEGER :: ist
    !> Statement label.  Not allocated if the statement had no label.
    TYPE(Label), ALLOCATABLE :: statement_label
    !> The tokens that make up the statement.
    !!
    !! Should always be allocated once an StData object has been 
    !! constructed.
    !!
    !! Other code assumes that this has a size greater than zero - for 
    !! example when looking for a token to get the location of the start 
    !! of the statement for error reporting, so make sure this has a 
    !! size greater than zero.
    TYPE(Token), ALLOCATABLE :: tlist(:)
  END TYPE StData
  
  !> Add a statement or multiple statements to an array of statements.
  INTERFACE AddStmt
    MODULE PROCEDURE AddStmt_
  END INTERFACE AddStmt
  
  !> Move the contents of a StData variable to another StData variable.
  INTERFACE Move
    MODULE PROCEDURE Move_
  END INTERFACE Move
  
  !> Get the syntax rule name of a statement.
  INTERFACE GetStName
    MODULE PROCEDURE GetStName_stmt
  END INTERFACE GetStName
  
CONTAINS
  
  !*****************************************************************************
  !!
  !> Add a StData scalar to an allocatable array of StData elements.
  !!
  !! @param[in,out] list              The array of StData elements.  This 
  !! will be grown (reallocated) to accomodate the additional element.  It 
  !! does not need to be initially allocated.
  !!
  !! @param[in,out] stmt              The statement to add to the list.
  !!
  !! After the call the token list inside of @a stmt (stmt%tlist) will have 
  !! been moved across to storage internal to @a list.
  
  SUBROUTINE AddStmt_(list, stmt)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(StData), INTENT(INOUT), ALLOCATABLE :: list(:)
    TYPE(StData), INTENT(INOUT) :: stmt
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Temporary for growing the list of statements.
    TYPE(StData), ALLOCATABLE :: tmp(:)
    
    INTEGER :: i              ! List index.
    INTEGER :: sz             ! Existing list size.
    
    !***************************************************************************
    
    IF (.NOT. ALLOCATED(list)) THEN
      sz = 0
    ELSE
      sz = SIZE(list)
    END IF
    ALLOCATE(tmp(sz+1))
    
    ! Move over items already in the list.
    DO i = 1, SIZE(list)
      CALL Move(tmp(i), list(i))
    END DO
    
    ! Move over the new item.
    CALL Move(tmp(sz+1), stmt)
    
    ! Transfer the allocation.
    CALL MOVE_ALLOC(tmp, list)
    
  END SUBROUTINE AddStmt_
  
  
  !*****************************************************************************
  !!
  !> Move the information associated with one StData variable to another 
  !! StData variable.
  !!
  !! @param[out]    dest              The variable to receive the information.
  !!
  !! @param[in,out] source            The variable to move from.  This contains 
  !! no useful statement information after the move.
  !!
  !! Note the order of the arguments was selected to be consistent with 
  !! the order required for defined assignment.
  !!
  !! Useful for storing statement information that is currently in a scalar 
  !! into an array or a component of a derived type.
  !!
  !! An alternative to the move is to just copy @a source to @a dest, but this 
  !! move operation may save on time and memory.
  
  SUBROUTINE Move_(dest, source)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(StData), INTENT(OUT) :: dest
    TYPE(StData), INTENT(INOUT) :: source
    
    !***************************************************************************
    
    dest%ist = source%ist
    CALL MOVE_ALLOC(source%statement_label, dest%statement_label)
    CALL MOVE_ALLOC(source%tlist, dest%tlist)
    
  END SUBROUTINE Move_
  
  
  !*****************************************************************************
  !!
  !> Get the suntax rule name of a statement.
  !!
  !! @param[in]     The statement data for the statement.
  !!
  !! @returns The syntax rule name for the statement.
  
  FUNCTION GetStName_stmt(stmt) RESULT(name)
  
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(StData), INTENT(IN) :: stmt
    
    ! Function result
    CHARACTER(:), ALLOCATABLE :: name
    
    !***************************************************************************
    
    name = used_getstname(stmt%ist)
    
  END FUNCTION GetStName_stmt
  
END MODULE StatementData
