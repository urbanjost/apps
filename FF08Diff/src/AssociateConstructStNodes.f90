! $Id: AssociateConstructStNodes.f90 1462 2014-11-15 01:27:04Z ian $
! ff08 source code copyright 2012 M.E.G.M.S.  See LICENCE.txt for licence.
!> @file
!! Defines the AssociateConstructStNodes module.


!*******************************************************************************
!!
!> Defines the AssociateConstructStNode type to represent 
!! /associate-construct/'s in the statement tree.

MODULE AssociateConstructStNodes
  
  USE ExecStNodes
  USE StNodes
  
  IMPLICIT NONE
  
  PRIVATE
  
  !-----------------------------------------------------------------------------
  
  !> Statement tree node for an /associate-construct/.
  TYPE, EXTENDS(BasicBlockStNode), PUBLIC :: AssociateConstructStNode
  CONTAINS
    ! Bindings inherited ultimately from StNode.
    PROCEDURE :: BuildTree => assoc_BuildTree
    PROCEDURE :: GetPart => assoc_GetPart
  END TYPE AssociateConstructStNode
  
CONTAINS
  
  !*****************************************************************************
  !!
  !> Implementation of AssociateStNode%BuildTree - incorporate the current 
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
  
  RECURSIVE SUBROUTINE assoc_BuildTree(st, stmt, new_tip, err_list)
    
    USE Errors
    USE Statements
    USE StatementData
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(AssociateConstructStNode), INTENT(INOUT), TARGET :: st
    TYPE(StData), INTENT(INOUT) :: stmt
    CLASS(StNode), INTENT(OUT), POINTER :: new_tip
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !***************************************************************************
    
    CALL DoBuildTree( st, stmt, istAssociate, istEndAssociate, new_tip,  &
        err_list )
    
  END SUBROUTINE assoc_BuildTree
  
  
  !*****************************************************************************
  !!
  !> Implementation of AssociateStNode%GetPart - get the part stack associated 
  !! with the current statement tree node.
  !!
  !! @param[in]     st                The statement tree node.
  !!
  !! @returns The part stack associated with the node.
  
  FUNCTION assoc_GetPart(st) RESULT(ipt)
    
    USE SyntaxParts
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(AssociateConstructStNode), INTENT(IN) :: st
    
    ! Function result
    INTEGER :: ipt
    
    !***************************************************************************
    
    ipt = iptAssociateConstruct
    
  END FUNCTION assoc_GetPart
  
END MODULE AssociateConstructStNodes
