! $Id: CaseConstructStNodes.f90 1319 2014-09-10 03:59:22Z ian $
! ff08 source code copyright 2012 M.E.G.M.S.  See LICENCE.txt for licence.
!> @file
!! Defines the CaseConstructStNodes module.


!*******************************************************************************
!!
!> Defines the CaseConstructStNode type.

MODULE CaseConstructStNodes
  
  USE ExecStNodes
  USE StatementData
  USE StNodes
  
  IMPLICIT NONE
  
  PRIVATE
  
  !-----------------------------------------------------------------------------
  
  !> Statement tree node for a /case-construct/.
  TYPE, EXTENDS(SelectBlockStNode), PUBLIC :: CaseConstructStNode
  CONTAINS
    PROCEDURE :: BuildTree => case_BuildTree
    PROCEDURE :: GetPart => case_GetPart
  END TYPE CaseConstructStNode
  
CONTAINS
  
  !*****************************************************************************
  !!
  !> Implementation of CaseConstructStNode%BuildTree - incorporate the current 
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
  
  RECURSIVE SUBROUTINE case_BuildTree(st, stmt, new_tip, err_list)
    
    USE Errors
    USE ErrorCodes
    USE ErrorLevels
    USE Statements
    USE StatementData
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(CaseConstructStNode), INTENT(INOUT), TARGET :: st
    TYPE(StData), INTENT(INOUT) :: stmt
    CLASS(StNode), INTENT(OUT), POINTER :: new_tip
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !***************************************************************************
    
    CALL DoBuildTree( st, stmt, istSelectCase, istEndSelect, istCase,  &
        new_tip, err_list )
    
  END SUBROUTINE case_BuildTree
  
  
  !*****************************************************************************
  !!
  !> Implementation of CaseConstructStNode%GetPart - get the part 
  !! associated with the current statement tree node.
  !!
  !! @returns The part stack associated with the node.
  
  FUNCTION case_GetPart(st) RESULT(ipt)
    
    USE SyntaxParts
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(CaseConstructStNode), INTENT(IN) :: st
    
    ! Function result
    INTEGER  :: ipt
    
    !***************************************************************************
    
    ipt = iptCaseConstruct
    
  END FUNCTION case_GetPart
  
END MODULE CaseConstructStNodes
