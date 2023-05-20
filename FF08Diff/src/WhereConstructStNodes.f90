! $Id: WhereConstructStNodes.f90 1319 2014-09-10 03:59:22Z ian $
! ff08 source code copyright 2012 M.E.G.M.S.  See LICENCE.txt for licence.
!> @file
!! Defines the WhereConstructStNodes module.


!*******************************************************************************
!!
!> Defines the WhereConstructStNode type to represent /where-construct/'s in 
!! the statement tree.

MODULE WhereConstructStNodes
  
  USE ExecStNodes
  USE StatementData
  USE StNodes
  
  IMPLICIT NONE
  
  PRIVATE
  
  !-----------------------------------------------------------------------------
  
  !> Statement tree node for an /where-construct/.
  TYPE, EXTENDS(ConditionalBlockStNode), PUBLIC :: WhereConstructStNode
  CONTAINS
    ! Bindings inherited from StNode.
    PROCEDURE :: BuildTree => where_BuildTree
    PROCEDURE :: GetPart => where_GetPart
  END TYPE WhereConstructStNode
  
CONTAINS
  
  !*****************************************************************************
  !!
  !> Implementation of WhereConstructStNode%BuildTree - incorporate the 
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
  
  RECURSIVE SUBROUTINE where_BuildTree(st, stmt, new_tip, err_list)
    
    USE Errors
    USE Statements
    USE StatementData
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(WhereConstructStNode), INTENT(INOUT), TARGET :: st
    TYPE(StData), INTENT(INOUT) :: stmt
    CLASS(StNode), INTENT(OUT), POINTER :: new_tip
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !***************************************************************************
    
    CALL DoBuildTree( st, stmt, istWhereConstruct, istMaskedElsewhere,  &
        istElsewhere, istEndWhere, new_tip, err_list )
    
  END SUBROUTINE where_BuildTree
  
  
  !*****************************************************************************
  !!
  !> Implementation of WhereConstructStNode%GetPart - get the syntax part 
  !! associated with the current statement tree node.
  !!
  !! @returns The part stack associated with the node.
  
  FUNCTION where_GetPart(st) RESULT(ipt)
    
    USE SyntaxParts
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(WhereConstructStNode), INTENT(IN) :: st
    
    ! Function result
    INTEGER :: ipt
    
    !***************************************************************************
    
    ipt = iptWhereConstruct
    
  END FUNCTION where_GetPart
  
END MODULE WhereConstructStNodes
