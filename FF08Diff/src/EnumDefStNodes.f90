! $Id: EnumDefStNodes.f90 2795 2019-03-22 09:15:08Z ian $
! ff08 source code copyright 2012 M.E.G.M.S.  See LICENCE.txt for licence.
!> @file
!! Defines the EnumDefStNodes module.


!*******************************************************************************
!!
!> Defines the EnumDefStNode type and related procedures.

MODULE EnumDefStNodes
  
  USE StatementData
  USE StNodes
  
  IMPLICIT NONE
  
  PRIVATE
  
  !-----------------------------------------------------------------------------
  
  !> Statement tree node for a enumeration definition.
  TYPE, PUBLIC, EXTENDS(StNode) :: EnumDefStNode
    !> The enum-def-stmt that starts the block.
    !!
    !! Always allocated after a successful build.
    TYPE(StData), ALLOCATABLE :: first
    !> The end-enum-def-stmt that ends the block.
    !!
    !! Always allocated after a successful build.
    TYPE(StData), ALLOCATABLE :: last
    !> The enum-stmt's that make up the definitions inside the block.
    !!
    !! Always allocated after a successful build, may be zero sized.
    TYPE(StData), ALLOCATABLE :: defs(:)
  CONTAINS
    ! Bindings inherited from StNode.
    PROCEDURE :: BuildTree => ed_BuildTree
    PROCEDURE :: GetPart => ed_GetPart
    PROCEDURE :: DumpTree => ed_DumpTree
    PROCEDURE :: Query => ed_Query
    PROCEDURE :: Modify => ed_Modify
    PROCEDURE :: GetLocation => ed_GetLocation
    PROCEDURE :: GetLabel => ed_GetLabel
  END TYPE EnumDefStNode
  
CONTAINS
  
  !*****************************************************************************
  !!
  !> Implementation of EnumDefStNode%BuildTree - incorporate the current 
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
  !!
  !! The /enum-def/ construct starts with an /enum-def-stmt/, may contain 
  !! /enumerator-def-stmt/'s and then finishes with an /end-enum-stmt/.
  
  RECURSIVE SUBROUTINE ed_BuildTree(st, stmt, new_tip, err_list)
    
    USE Errors
    USE ErrorCodes
    USE ErrorLevels
    USE Statements
    USE StatementData
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(EnumDefStNode), INTENT(INOUT), TARGET :: st
    TYPE(StData), INTENT(INOUT) :: stmt
    CLASS(StNode), INTENT(OUT), POINTER :: new_tip
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Temporary for growing the st%defs component.
    TYPE(StData), ALLOCATABLE :: tmp(:)
    
    !***************************************************************************
    
    ALLOCATE(err_list(0))
    
    IF (.NOT. ALLOCATED(st%first)) THEN
      ALLOCATE(st%first)
      IF (stmt%ist /= istEnumDef) THEN
        STOP 'Internal error in EnumDefStNodes%%ed_BuildTree, &
            &the first statement of an /enum-def/ is''t an /enum-def-stmt/.'
      END IF
      CALL Move(st%first, stmt)
      new_tip => st
    ELSE IF (stmt%ist == istEndEnum) THEN
      IF (ALLOCATED(st%last)) THEN
        STOP 'Internal error in EnumDefStNodes%%ed_BuildTree, &
            &the last statement of an /enum-def/ was encountered twice.'
      ELSE
        ALLOCATE(st%last)
      END IF
      new_tip => st%parent
      CALL Move(st%last, stmt)
    ELSE IF (stmt%ist == istEnumeratorDef) THEN
      IF (.NOT. ALLOCATED(st%defs)) THEN
        ALLOCATE(tmp(1))
      ELSE
        ALLOCATE(tmp(SIZE(st%defs) + 1))
        tmp(:SIZE(st%defs)) = st%defs
      END IF
      CALL Move(tmp(SIZE(tmp)), stmt)
      CALL MOVE_ALLOC(tmp, st%defs)
    ELSE
      CALL Add( err_list, CODE=errInvalidContext, COMPONENT='R458',  &
          LOCATION=QueryLocation(stmt%tlist(1)),  &
          MSG='A /' // GetStName(stmt%ist) // '/ is not valid inside a &
            &/enum-def/ block.' )
    END IF
    
  END SUBROUTINE ed_BuildTree
  
  
  !*****************************************************************************
  !!
  !> Implementation of EnumDefStNode%GetPart - get the part stack associated 
  !! with the current statement tree node.
  !!
  !! @returns The part stack associated with the node.
  
  FUNCTION ed_GetPart(st) RESULT(ipt)
    
    USE SyntaxParts
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(EnumDefStNode), INTENT(IN) :: st
    
    ! Function result
    INTEGER :: ipt
    
    !***************************************************************************
    
    ipt = iptEnumDef
    
  END FUNCTION ed_GetPart
  
  
  !*****************************************************************************
  !!
  !> Implementation of EnumDefStNode%DumpTree - write debugging information 
  !! about the node and its children.
  !!
  !! @param[in]     st                The statement tree node.
  !!
  !! @param[in]     unit              The unit to dump to.
  !!
  !! @param[in]     pfx               Prefix to write at the start 
  !! of each record (for indentation).
  
  SUBROUTINE ed_DumpTree(st, unit, pfx)
    
    USE SyntaxParts
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(EnumDefStNode), INTENT(IN), TARGET :: st
    INTEGER, INTENT(IN) :: unit
    CHARACTER(*), INTENT(IN) :: pfx
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: i              ! Enumerator statement index.
    
    !***************************************************************************
    
    WRITE (unit, fmt_partname) pfx, GetPartName(st)
    
    IF (ALLOCATED(st%first))  &
        WRITE (unit, fmt_stmtname) pfx // '  ', GetStName(st%first)
    IF (ALLOCATED(st%defs)) THEN
      DO i = 1, SIZE(st%defs)
        WRITE (unit, fmt_stmtname) pfx // '  ', GetStName(st%defs(i))
      END DO
    END IF
    IF (ALLOCATED(st%last))  &
        WRITE (unit, fmt_stmtname) pfx // '  ', GetStName(st%last)
    
  END SUBROUTINE ed_DumpTree
  
  
  !*****************************************************************************
  !!
  !> Implementation of EnumDefStNode%Query - apply visitor object operations 
  !! to this node and its children.
  !!
  !! @param[in]     st                The statement tree node being traversed.
  !!
  !! @param[in,out] visitor           The object to execute the query action.
  
  SUBROUTINE ed_Query(st, visitor)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(EnumDefStNode), INTENT(IN), TARGET :: st
    CLASS(StNodeQueryVisitor), INTENT(INOUT) :: visitor
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: i              ! Child definition statement index.
    
    !***************************************************************************
    
    ! Execute against ourselves.
    CALL visitor%ExecuteNode(st)
    
    ! Execute against the initial statement.
    CALL visitor%ExecuteStmt(st, st%first)
    
    ! Execute against the child definition statements.
    DO i = 1, SIZE(st%defs)
      CALL visitor%ExecuteStmt(st, st%defs(i))
    END DO
    
    ! Execute against the closing statement.
    CALL visitor%ExecuteStmt(st, st%last)
    
  END SUBROUTINE ed_Query
  
  
  !*****************************************************************************
  !!
  !> Implementation of EnumDefStNode%Modify - apply modify visitor object 
  !! operations to this node and its children.
  !!
  !! @param[in,out] st                The statement tree node being traversed.
  !!
  !! @param[in,out] visitor           The object to execute the query action.
  
  SUBROUTINE ed_Modify(st, visitor)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(EnumDefStNode), INTENT(INOUT), TARGET :: st
    CLASS(StNodeModifyVisitor), INTENT(INOUT) :: visitor
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: i              ! Child definition statement index.
    
    !***************************************************************************
    
    ! Execute against ourselves.
    CALL visitor%ExecuteNode(st)
    
    ! Execute against the initial statement.
    CALL visitor%ExecuteStmt(st, st%first)
    
    ! Execute against the child definition statements.
    DO i = 1, SIZE(st%defs)
      CALL visitor%ExecuteStmt(st, st%defs(i))
    END DO
    
    ! Execute against the closing statement.
    CALL visitor%ExecuteStmt(st, st%last)
    
  END SUBROUTINE ed_Modify
  
  
  !*****************************************************************************
  !!
  !> Implementation of EnumDefStNode%GetLocation - get the starting source 
  !! location of the node.
  !!
  !! @param[in]     st                The node of interest.
  !!
  !! @returns The source location of the start of the node.
  
  FUNCTION ed_GetLocation(st) RESULT(loc)
    
    USE SourceLocations
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(EnumDefStNode), INTENT(IN) :: st
    
    ! Function result.
    TYPE(SourceLocation) :: loc
    
    !***************************************************************************
    
    loc = QueryLocation(st%first%tlist(1))
    
  END FUNCTION ed_GetLocation
  
  
  !*****************************************************************************
  !!
  !> Implementation of EnumDefStNode%GetLabel - get the label 
  !! of the node.
  !!
  !! @param[in]     st                The node of interest.
  !!
  !! @returns The statement label of the the node.
  
  FUNCTION ed_GetLabel(st) RESULT(label)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(EnumDefStNode), INTENT(IN) :: st
    
    ! Function result.
    TYPE(LabelWrapper) :: label
    
    !***************************************************************************
    
    ! ifort bug - allocatable component in structure constructor.
    if (allocated(st%first%statement_label)) then
      label = LabelWrapper(st%first%statement_label)
    end if
    
  END FUNCTION ed_GetLabel
  
END MODULE EnumDefStNodes
