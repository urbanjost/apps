! $Id: SelectTypeConstructStNodes.f90 1319 2014-09-10 03:59:22Z ian $
!> @file
!! Defines the SelectTypeConstructStNodes module.


!*******************************************************************************
!!
!> Defines the SelectTypeConstructStNodes type.

MODULE SelectTypeConstructStNodes
  
  USE ExecStNodes
  USE StatementData
  USE StNodes
  
  IMPLICIT NONE
  
  PRIVATE
  
  !-----------------------------------------------------------------------------
  
  !> Statement tree node for a /case-construct/.
  TYPE, EXTENDS(SelectBlockStNode), PUBLIC :: SelectTypeConstructStNode
  CONTAINS
    ! Bindings inherited from StNode.
    PROCEDURE :: BuildTree => st_BuildTree
    PROCEDURE  :: GetPart => st_GetPart
  END TYPE SelectTypeConstructStNode
  
CONTAINS
  
  !*****************************************************************************
  !!
  !> Implementation of SelectTypeConstructStNode%BuildTree - incorporate the 
  !! current statement into the statement tree.
  !!
  !! @param[in,out] st                The statement tree node.
  !!
  !! @param[in,out] stmt              The current statement.  
  !! 
  !! @param[out]    new_tip           The new tip of the statement tree after
  !! the statement has been processed.  
  !!
  !! @param[out]    err_list          List of errors.  
  
  RECURSIVE SUBROUTINE st_BuildTree(st, stmt, new_tip, err_list)
    
    USE Errors
    USE ErrorCodes
    USE ErrorLevels
    USE Statements
    USE StatementData
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(SelectTypeConstructStNode), INTENT(INOUT), TARGET :: st
    TYPE(StData), INTENT(INOUT) :: stmt
    CLASS(StNode), INTENT(OUT), POINTER :: new_tip
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
      
    !***************************************************************************
    
    CALL DoBuildTree( st, stmt, istSelectType, istEndSelectType,  &
        istTypeGuard, new_tip, err_list )
    
  END SUBROUTINE st_BuildTree
  
  
  !*****************************************************************************
  !!
  !> Implementation of SelectTypeConstructStNode%GetPart - get the part 
  !! associated with the current statement tree node.
  !!
  !! @returns The part stack associated with the node.
  
  FUNCTION st_GetPart(st) RESULT(ipt)
    
    USE SyntaxParts
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(SelectTypeConstructStNode), INTENT(IN) :: st
    
    ! Function result
    INTEGER :: ipt
    
    !***************************************************************************
    
    ipt = iptSelectTypeConstruct
    
  END FUNCTION st_GetPart
  
END MODULE SelectTypeConstructStNodes
