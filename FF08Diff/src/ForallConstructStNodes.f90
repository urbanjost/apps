! $Id: ForallConstructStNodes.f90 2873 2019-04-03 20:43:57Z ian $
! ff08 source code copyright 2012 M.E.G.M.S.  See LICENCE.txt for licence.
!> @file
!! Defines the ForallConstructStNodes module.


!*******************************************************************************
!!
!> Defines the ForallConstructStNode to represent /forall-construct/'s in the
!! statement tree.

MODULE ForallConstructStNodes
  
  USE ExecStNodes
  USE StNodes
  
  IMPLICIT NONE
  
  PRIVATE
  
  !-----------------------------------------------------------------------------
  
  !> Statement tree node for an /forall-construct/.
  TYPE, EXTENDS(BasicBlockStNode), PUBLIC :: ForallConstructStNode
  CONTAINS
    ! Bindings inherited from StNode.
    PROCEDURE :: BuildTree => forall_BuildTree
    PROCEDURE :: GetPart => forall_GetPart
    PROCEDURE :: Check => forall_Check
  END TYPE ForallConstructStNode
  
CONTAINS
  
  !*****************************************************************************
  !!
  !> Implementation of ForallConstructStNode%BuildTree - incorporate the 
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
  
  RECURSIVE SUBROUTINE forall_BuildTree(st, stmt, new_tip, err_list)
    
    USE Errors
    USE Statements
    USE StatementData
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(ForallConstructStNode), INTENT(INOUT), TARGET :: st
    TYPE(StData), INTENT(INOUT) :: stmt
    CLASS(StNode), INTENT(OUT), POINTER :: new_tip
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !***************************************************************************
    
    CALL DoBuildTree(  &
        st,  &
        stmt,  &
        istForallConstruct,  &
        istEndForall,  &
        new_tip,  &
        err_list )
    
  END SUBROUTINE forall_BuildTree
  
  
  !*****************************************************************************
  !!
  !> Implementation of ForallConstructStNode%GetPart - get the syntax part 
  !! associated  with the current statement tree node.
  !!
  !! @returns The part stack associated with the node.
  
  FUNCTION forall_GetPart(st) RESULT(ipt)
    
    USE SyntaxParts
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(ForallConstructStNode), INTENT(IN) :: st
    
    ! Function result
    INTEGER :: ipt
    
    !***************************************************************************
    
    ipt = iptForAllConstruct
    
  END FUNCTION forall_GetPart
  
  
  !****************************************************************************
  !!
  !> Implementation of ForallConstructStNode%Check - check that the 
  !! node complies with the rules of the language.
  !!
  !! @param[in]     st                The statement tree node.
  !!
  !! @param[out]    err_list          List of errors
  !!
  !! We check that the child statements are consistent with R756.
  
  SUBROUTINE forall_Check(st, err_list)
    
    USE Errors
    USE ErrorCodes
    USE Statements
    USE SyntaxParts
    USE ParseUtils
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(ForallConstructStNode), INTENT(IN) :: st
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !---------------------------------------------------------------------------
    ! Locals
    
    INTEGER :: i              ! Child statement index.
    
    !***************************************************************************
    
    ALLOCATE(err_list(0))
    
    DO i = 1, SIZE(st%exec%children)
      ASSOCIATE(child => st%exec%children(i)%item)
        ! This is where we check R756 and R757.
        IF ( .NOT. ANY( child%GetPart() == [  &
            istAssignment,  &
            istPointerAssignment,  &
            istWhere,  &
            istForall,  &
            iptWhereConstruct,  &
            iptForallConstruct ] ) ) THEN
          CALL Add( err_list,  &
              CODE=errSyntax,  &
              COMPONENT='R756',  &
              LOCATION=child%GetLocation(),  &
              MSG='A ' // GetPartName(child%GetPart())  &
                // ' is not permitted in a /forall-construct/.' )
        END IF
      END ASSOCIATE
    END DO
    
  END SUBROUTINE forall_Check
  
END MODULE ForallConstructStNodes
