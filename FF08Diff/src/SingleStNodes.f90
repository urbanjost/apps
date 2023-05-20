! $Id: SingleStNodes.f90 2916 2019-04-17 22:10:57Z ian $
! ff08 source code copyright 2012 M.E.G.M.S.  See LICENCE.txt for licence.
!> @file
!! Defines the SingleStNodes module.


!*******************************************************************************
!!
!> Defines the SingleStNode type.

MODULE SingleStNodes
  
  USE StatementData
  USE StNodes
  
  IMPLICIT NONE
  
  PRIVATE
  
  !-----------------------------------------------------------------------------
  
  !> Abstract extension of a StNode that provides for storage of a single 
  !! statement.
  !!
  !! The BuildTree implementation saves the statement data and then sets 
  !! the current node to be the parent of this node.  The node has no context.
  TYPE, EXTENDS(StNode), PUBLIC :: SingleStNode
    TYPE(StData), ALLOCATABLE :: stmt
  CONTAINS
    PROCEDURE :: BuildTree => single_BuildTree
    PROCEDURE :: Check => single_Check
    PROCEDURE :: GetPart => single_GetPart
    PROCEDURE :: DumpTree => single_DumpTree
    PROCEDURE :: Query => single_Query
    PROCEDURE :: Modify => single_Modify
    PROCEDURE :: GetLocation => single_GetLocation
    PROCEDURE :: GetLabel => single_GetLabel
  END TYPE SingleStNode
  
CONTAINS
  
  !*****************************************************************************
  !!
  !> Implementation of SingleStNode%BuildTree - incorporate the current
  !! statement into the statement tree.
  !!
  !! @param[in,out] st                The statement tree node.
  !!
  !! @param[in,out] stmt              The current statement.
  !! 
  !! @param[out]    new_tip           The new tip of the statement tree after
  !! the statement has been processed.  For SingleStNode processing this
  !! is always the parent.
  !!
  !! @param[out]    err_list          List of errors.
  
  RECURSIVE SUBROUTINE single_BuildTree(st, stmt, new_tip, err_list)
    
    USE Errors
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(SingleStNode), INTENT(INOUT), TARGET :: st
    TYPE(StData), INTENT(INOUT) :: stmt
    CLASS(StNode), INTENT(OUT), POINTER :: new_tip
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !***************************************************************************
    
    ALLOCATE(err_list(0))
    
    IF (ALLOCATED(st%stmt)) THEN
      STOP 'A SingleStNode is receiving more than one statement.'
    ELSE
      ALLOCATE(st%stmt)
      CALL Move(st%stmt, stmt)
    END IF
    
    ! We never retain the context - always bounce it back up.
    new_tip => st%parent
    
  END SUBROUTINE single_BuildTree
  
  
  !****************************************************************************
  !!
  !> Implementation of SingleStNode%Check - check that the node complies with 
  !! the rules of the language.
  !!
  !! @param[in]     st                The statement tree node.
  !!
  !! @param[out]    err_list          List of errors
  !!
  !! The default implementation does nothing.
  
  SUBROUTINE single_Check(st, err_list)
    
    USE Errors
    USE ErrorCodes
    USE ErrorLevels
    USE Statements
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(SingleStNode), INTENT(IN) :: st
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !***************************************************************************
    
    ALLOCATE(err_list(0))
    
    ! Statement function statements are not checked in the following because 
    ! they can be erroneously classified - that check has to happen during 
    ! the semantic phase.
    SELECT CASE (st%stmt%ist)
    CASE (istEntry)
      CALL Add( err_list,  &
          CODE=errObsolescentEntryStmt,  &
          LEVEL=errLevelWarning,  &
          COMPONENT='R1240',  &
          LOCATION=st%GetLocation(),  &
          MSG='An /entry-stmt/ is an obsolescent feature.' )
    CASE (istArithmeticIf)
      CALL Add( err_list,  &
          CODE=errObsolescentArithmeticIfStmt,  &
          LEVEL=errLevelWarning,  &
          COMPONENT='R853',  &
          LOCATION=st%GetLocation(),  &
          MSG='An /arithmetic-if-stmt/ is an obsolescent feature.' )
    CASE (istComputedGoto)
      CALL Add( err_list,  &
          CODE=errObsolescentComputedGotoStmt,  &
          LEVEL=errLevelWarning,  &
          COMPONENT='R852',  &
          LOCATION=st%GetLocation(),  &
          MSG='A /computed-goto-stmt/ is an obsolescent feature.' )
    END SELECT
    
  END SUBROUTINE single_Check
  
  
  !*****************************************************************************
  !!
  !> Implementation of SingleStNode%GetPart - return the part number for 
  !! the statement tree node.
  !!
  !! Because single statement nodes have no child nodes, they never exist
  !! at the tip of the node tree.  Therefore they don't have a part.
  !!
  !! So we return the statement number instead.
  
  FUNCTION single_GetPart(st) RESULT(ipt)
    
    USE SyntaxParts
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(SingleStNode), INTENT(IN) :: st
    
    ! Function result
    INTEGER :: ipt
    
    !***************************************************************************
    
    ipt = st%stmt%ist
    
  END FUNCTION single_GetPart
  
  
  !*****************************************************************************
  !!
  !> Implementation of SingleStNode%DumpTree - print debugging information 
  !! about this node.
  !!
  !! @param[in]     st                The statement tree node.
  !!
  !! @param[in]     unit              The unit to dump to.
  !!
  !! @param[in]     pfx               Prefix to write at the start 
  !! of each record (for indentation).
  
  SUBROUTINE single_DumpTree(st, unit, pfx)
  
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(SingleStNode), INTENT(IN), TARGET :: st
    INTEGER, INTENT(IN) :: unit
    CHARACTER(*), INTENT(IN) :: pfx
    
    !***************************************************************************
    
    IF (ALLOCATED(st%stmt))  &
        WRITE (unit, fmt_stmtname) pfx, GetStName(st%stmt)
    
  END SUBROUTINE single_DumpTree
  
  
  !*****************************************************************************
  !!
  !> Implementation of SingleStNode%Query - apply visitor object operations 
  !! to this node and its children.
  !!
  !! @param[in]     st                The statement tree node being traversed.
  !!
  !! @param[in,out] visitor           The object to execute the query action.
  
  SUBROUTINE single_Query(st, visitor)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(SingleStNode), INTENT(IN), TARGET :: st
    CLASS(StNodeQueryVisitor), INTENT(INOUT) :: visitor
    
    !***************************************************************************
    
    ! Execute against ourselves.
    CALL visitor%ExecuteNode(st)
    
    ! Execute against the statement that we manage.
    CALL visitor%ExecuteStmt(st, st%stmt)
    
  END SUBROUTINE single_Query
  
  
  !*****************************************************************************
  !!
  !> Implementation of SingleStNode%Modify - apply modify visitor object 
  !! operations to this node and its children.
  !!
  !! @param[in,out] st                The statement tree node being traversed.
  !!
  !! @param[in,out] visitor           The object to execute the modify action.
  
  SUBROUTINE single_Modify(st, visitor)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(SingleStNode), INTENT(INOUT), TARGET :: st
    CLASS(StNodeModifyVisitor), INTENT(INOUT) :: visitor
    
    !***************************************************************************
    
    ! Execute against ourselves.
    CALL visitor%ExecuteNode(st)
    
    ! Execute against the statement that we manage.
    CALL visitor%ExecuteStmt(st, st%stmt)
    
  END SUBROUTINE single_Modify
  
  
  !*****************************************************************************
  !!
  !> Implementation of SingleStNode%GetLocation - get the starting source 
  !! location of the node.
  !!
  !! @param[in]     st                The node of interest.
  !!
  !! @returns The source location of the start of the node.
  
  FUNCTION single_GetLocation(st) RESULT(loc)
    
    USE SourceLocations
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(SingleStNode), INTENT(IN) :: st
    
    ! Function result.
    TYPE(SourceLocation) :: loc
    
    !***************************************************************************
    
    loc = QueryLocation(st%stmt%tlist(1))
    
  END FUNCTION single_GetLocation
  
  
  !*****************************************************************************
  !!
  !> Implementation of SingleStNode%GetLabel - get the label of the node.
  !!
  !! @param[in]     st                The node of interest.
  !!
  !! @returns The statement label of the the node.
  
  FUNCTION single_GetLabel(st) RESULT(label)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(SingleStNode), INTENT(IN) :: st
    
    ! Function result.
    TYPE(LabelWrapper) :: label
    
    !***************************************************************************
    
    ! ifort bug - allocatable component in structure constructor.
    if (allocated(st%stmt%statement_label)) then
      label = LabelWrapper(st%stmt%statement_label)
    end if
    
  END FUNCTION single_GetLabel
  
END MODULE SingleStNodes
