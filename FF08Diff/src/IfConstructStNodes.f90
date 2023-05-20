! $Id: IfConstructStNodes.f90 2195 2016-06-06 14:51:14Z ian $
! ff08 source code copyright 2012 M.E.G.M.S.  See LICENCE.txt for licence.
!> @file
!! Defines the IfStNodes module.


!*******************************************************************************
!!
!> Defines the IfStNode type to represent /if-construct/'s in the statement
!! tree.

MODULE IfConstructStNodes
  
  USE ExecStNodes
  USE StatementData
  USE StNodes
  
  IMPLICIT NONE
  
  PRIVATE
  
  !-----------------------------------------------------------------------------
  
  !> StNode for an /if-construct/.
  TYPE, EXTENDS(ConditionalBlockStNode), PUBLIC :: IfConstructStNode
  CONTAINS
    ! Procedures inherited from StNode.
    PROCEDURE :: BuildTree => if_BuildTree
    PROCEDURE :: GetPart => if_GetPart
  END TYPE IfConstructStNode
  
CONTAINS
  
  !*****************************************************************************
  !!
  !> Implementation of IfStNode%BuildTree - incorporate the current 
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
  
  RECURSIVE SUBROUTINE if_BuildTree(st, stmt, new_tip, err_list)
    
    USE Errors
    USE Statements
    USE StatementData
      
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(IfConstructStNode), INTENT(INOUT), TARGET :: st
    TYPE(StData), INTENT(INOUT) :: stmt
    CLASS(StNode), INTENT(OUT), POINTER :: new_tip
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !***************************************************************************
    
    CALL DoBuildTree(  &
        st,  &
        stmt,  &
        istIfThen,  &
        istElseIf,  &
        istElse,  &
        istEndIf,  &
        new_tip,  &
        err_list )
    
  END SUBROUTINE if_BuildTree
  
  
  !*****************************************************************************
  !!
  !> Implementation of IfStNode%GetPart - get the syntax part associated with 
  !! this statement tree node.
  
  FUNCTION if_GetPart(st) RESULT(ipt)
    
    USE SyntaxParts
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(IfConstructStNode), INTENT(IN) :: st
    
    ! Function result
    INTEGER :: ipt
    
    !***************************************************************************
    
    ipt = iptIfConstruct
    
  END FUNCTION if_GetPart
  
END MODULE IfConstructStNodes

