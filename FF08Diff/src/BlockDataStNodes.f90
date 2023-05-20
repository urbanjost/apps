! $Id: BlockDataStNodes.f90 1538 2015-02-15 23:59:13Z ian $
! ff08 source code copyright 2012 M.E.G.M.S.  See LICENCE.txt for licence.
!> @file
!! Defines the BlockDataStNodes


!*******************************************************************************
!!
!> Defines the BlockDataStNode and associated procedures for representing 
!! /block-data/ program units in the statement tree.

MODULE BlockDataStNodes
  
  USE StNodes
  USE ProgUnitStNodes
  
  IMPLICIT NONE
  
  PUBLIC :: FindBlockData
  
  !-----------------------------------------------------------------------------
  
  !> Type to represent a block data program unit
  TYPE, PUBLIC, EXTENDS(ProgUnitStNode) :: BlockDataStNode
  CONTAINS
    PROCEDURE :: BuildTree => blockdata_BuildTree
    PROCEDURE :: GetPart => blockdata_GetPart
    PROCEDURE :: Query => blockdata_Query
    PROCEDURE :: Modify => blockdata_Modify
    PROCEDURE :: GetName => blockdata_GetName
  END TYPE BlockDataStNode
  
CONTAINS
  
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! Procedures for the BlockDataStNode type.
  
  
  !*****************************************************************************
  !!
  !> Implementation of BlocDataStNode%BuildTree - incorporate the current 
  !! statement into the statement tree.
  !!
  !! @param[in,out] st                The statement tree node.
  !!
  !! @param[in,out] stmt              The current statement.
  !!
  !! @param[out]    new_tip           The new tip of the statement tree after 
  !! the statement has been processed.
  !!
  !! @param[out]    err_list          List of errors.
  
  RECURSIVE SUBROUTINE blockdata_BuildTree(st, stmt, new_tip, err_list)
    
    USE Errors
    USE Statements
    USE StatementData
    USE StNodes
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(BlockDataStNode), INTENT(INOUT), TARGET :: st
    TYPE(StData), INTENT(INOUT) :: stmt
    CLASS(StNode), INTENT(OUT), POINTER :: new_tip
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !***************************************************************************
    
    CALL DoBuildTree( st, stmt, istBlockData, istEndBlockData, new_tip,  &
        err_list )
    
  END SUBROUTINE blockdata_BuildTree
  
  
  !*****************************************************************************
  !!
  !> Implementation of BlockDataStNode%GetPart - get the syntax parts of 
  !! the current statement tree node.
  !!
  !! @returns The part stack associated with the node.
  
  FUNCTION blockdata_GetPart(st) RESULT(ipt)
    
    USE SyntaxParts
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(BlockDataStNode), INTENT(IN) :: st
    
    ! Function result
    INTEGER :: ipt
    
    !***************************************************************************
    
    ipt = iptBlockData
    
  END FUNCTION blockdata_GetPart
  
  
  !*****************************************************************************
  !!
  !> Implementation of BlockDataStNode%Query - apply visitor object operations 
  !! to this node and its children.
  !!
  !! @param[in]     st                The statement tree node being traversed.
  !!
  !! @param[in,out] visitor           The object to execute the query action.
  
  SUBROUTINE blockdata_Query(st, visitor)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(BlockDataStNode), INTENT(IN), TARGET :: st
    CLASS(StNodeQueryVisitor), INTENT(INOUT) :: visitor
    
    !***************************************************************************
    
    ! Execute against ourselves.
    CALL visitor%ExecuteNode(st)
    
    ! Opening statement, if present.
    IF (ALLOCATED(st%first)) CALL visitor%ExecuteStmt(st, st%first)
    
    ! Forward to specificaction section, if present.
    IF (ALLOCATED(st%spec)) CALL st%spec%Query(visitor)
    
    ! Final statement.
    CALL visitor%ExecuteStmt(st, st%last)
    
  END SUBROUTINE blockdata_Query
  
  
  !*****************************************************************************
  !!
  !> Implementation of BlockDataStNode%Modify - apply modify visitor object 
  !! operations to this node and its children.
  !!
  !! @param[in,out] st                The statement tree node being traversed.
  !!
  !! @param[in,out] visitor           The object to execute the modify action.
  
  SUBROUTINE blockdata_Modify(st, visitor)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(BlockDataStNode), INTENT(INOUT), TARGET :: st
    CLASS(StNodeModifyVisitor), INTENT(INOUT) :: visitor
    
    !***************************************************************************
    
    ! Execute against ourselves.
    CALL visitor%ExecuteNode(st)
    
    ! Opening statement, if present.
    IF (ALLOCATED(st%first)) CALL visitor%ExecuteStmt(st, st%first)
    
    ! Forward to specificaction section, if present.
    IF (ALLOCATED(st%spec)) CALL st%spec%Modify(visitor)
    
    ! Final statement.
    CALL visitor%ExecuteStmt(st, st%last)
    
  END SUBROUTINE blockdata_Modify
  
  
  !*****************************************************************************
  !!
  !> Implementation of BlockDataStNode::GetName - get the 
  !! name of the separate module subprogram.
  !!
  !! @param[in]     st                The statement tree node.
  !!
  !! @param[out]    name              The name of the separate module 
  !! subprogram, or not allocated on error.
  
  SUBROUTINE blockdata_GetName(st, name)
    
    USE BlockDataStmtsUtils
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(BlockDataStNode), INTENT(IN) :: st
    CHARACTER(:), INTENT(OUT), ALLOCATABLE :: name
    
    !***************************************************************************
    
    CALL GetBlockDataName(st%first%tlist, name)
    
  END SUBROUTINE blockdata_GetName
  
  
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  !
  
  
  !*****************************************************************************
  !!
  !> Find a block data program unit in a vector of program units with the 
  !! given name.
  !!
  !! @param[in]     prog_units        The array of program units.
  !!
  !! @param[in]     name              The name of the block data unit to 
  !! search for.  If zero length then the search is for an unnamed block 
  !! data unit.
  !!
  !! @param[out]    idx               The index of the first node that 
  !! represents a /block-data/ in @a prog_units that has the matching 
  !! name.
  
  SUBROUTINE FindBlockData(prog_units, name, idx)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(ProgUnitVector), INTENT(IN) :: prog_units(:)
    CHARACTER(*), INTENT(IN) :: name
    INTEGER, INTENT(OUT) :: idx
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Name of the item already in the list being compared.
    CHARACTER(:), ALLOCATABLE :: other_name
    
    !***************************************************************************
    
    DO idx = 1, SIZE(prog_units)
      SELECT TYPE (item => prog_units(idx)%item)
      TYPE IS (BlockDataStNode)
        CALL prog_units(idx)%item%GetName(other_name)
        IF (ALLOCATED(other_name)) THEN
          IF (name == other_name) RETURN
        END IF
      END SELECT
    END DO
    
    idx = 0                   ! Not found.
    
  END SUBROUTINE FindBlockData
  
END MODULE BlockDataStNodes
