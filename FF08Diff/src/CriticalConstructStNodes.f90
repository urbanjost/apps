! $Id: CriticalConstructStNodes.f90 1319 2014-09-10 03:59:22Z ian $
! ff08 source code copyright 2012 M.E.G.M.S.  See LICENCE.txt for licence.
!> @file
!! Defines the CriticalConstructStNodes module.


!*******************************************************************************
!!
!> Defines the CriticalConstructStNode and associated procedures to 
!! represent /critical-construct/'s in the statement tree.

MODULE CriticalConstructStNodes
  
  USE ExecStNodes
  USE StNodes
  
  IMPLICIT NONE
  
  PRIVATE
  
  !-----------------------------------------------------------------------------
  
  !> Statement tree node for an /associate-construct/.
  TYPE, EXTENDS(BasicBlockStNode), PUBLIC :: CriticalConstructStNode
  CONTAINS
    ! Bindings inherited from StNode.
    PROCEDURE :: BuildTree => critical_BuildTree
    PROCEDURE :: GetPart => critical_GetPart
  END TYPE CriticalConstructStNode
  
CONTAINS
  
  !*****************************************************************************
  !!
  !> Implementation of CriticalConstructStNode%BuildTree - incorporate the 
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
  
  RECURSIVE SUBROUTINE critical_BuildTree(st, stmt, new_tip, err_list)
    
    USE Errors
    USE Statements
    USE StatementData
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(CriticalConstructStNode), INTENT(INOUT), TARGET :: st
    TYPE(StData), INTENT(INOUT) :: stmt
    CLASS(StNode), INTENT(OUT), POINTER :: new_tip
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !***************************************************************************
    
    CALL DoBuildTree( st, stmt, istCritical, istEndCritical,  &
        new_tip, err_list )
    
  END SUBROUTINE critical_BuildTree
  
  
  !*****************************************************************************
  !!
  !> Implementation of CriticalConstructStNode%GetPart - get the syntax part 
  !! for the current statement tree node.
  !!
  !! @returns The part stack associated with the node.
  
  FUNCTION critical_GetPart(st) RESULT(ipt)
    
    USE SyntaxParts
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(CriticalConstructStNode), INTENT(IN) :: st
    
    ! Function result
    INTEGER :: ipt
    
    !***************************************************************************
    
    ipt = iptCriticalConstruct
    
  END FUNCTION critical_GetPart
  
END MODULE CriticalConstructStNodes
