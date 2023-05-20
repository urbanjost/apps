! $Id: BlockConstructStNodes.f90 2795 2019-03-22 09:15:08Z ian $
! ff08 source code copyright 2012 M.E.G.M.S.  See LICENCE.txt for licence.
!> @file
!! Defines the BlockStNodes module.


!*******************************************************************************
!!
!! Defines the BlockStNode type.

MODULE BlockConstructStNodes
  
  USE ExecStNodes
  USE SpecStNodes
  USE StatementData
  USE StNodes
  
  IMPLICIT NONE
  
  PRIVATE
  
  !-----------------------------------------------------------------------------
  
  !> Statement tree node for a /block-construct/.
  TYPE, PUBLIC, EXTENDS(StNode) :: BlockConstructStNode
    !> The opening /block-stmt/.
    !!
    !! Might not be allocated in the case of a PROGRAM that doesn't have 
    !! an opening statement.
    TYPE(StData), ALLOCATABLE :: first
    
    !> The specification statements for the program unit.
    !!
    !! Not allocated if there were no specification statements.
    TYPE(SpecStNode), ALLOCATABLE :: spec
    
    !> The executable statements for the program unit.
    !!
    !! Not allocated if there were no executable statements.
    TYPE(ExecStNode), ALLOCATABLE :: exec
    
    !> The closing statement /end-block-stmt/.
    TYPE(StData), ALLOCATABLE :: last
  CONTAINS
    ! Bindings inherited from StNode.
    PROCEDURE :: BuildTree => block_BuildTree
    PROCEDURE :: GetPart => block_GetPart
    PROCEDURE :: DumpTree => block_DumpTree
    PROCEDURE :: Query => block_Query
    PROCEDURE :: Modify => block_Modify
    PROCEDURE :: GetLocation => block_GetLocation
    PROCEDURE :: GetLabel => block_GetLabel
  END TYPE BlockConstructStNode
  
CONTAINS
  
  !*****************************************************************************
  !!
  !> Implementation of BlockConstructStNode%BuildTree - incorporate the 
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
  
  RECURSIVE SUBROUTINE block_BuildTree(st, stmt, new_tip, err_list)
    
    USE Errors
    USE ErrorCodes
    USE ErrorLevels
    USE Statements
    USE StatementData
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(BlockConstructStNode), INTENT(INOUT), TARGET :: st
    TYPE(StData), INTENT(INOUT) :: stmt
    CLASS(StNode), INTENT(OUT), POINTER :: new_tip
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !***************************************************************************
    
    ALLOCATE(err_list(0))
    
    IF (.NOT. ALLOCATED(st%first)) THEN
      ALLOCATE(st%first)
      IF (stmt%ist /= istBlock) THEN
        STOP 'Internal error in BlockConstructStNodes%%block_BuildTree, &
            &the first statement of a /block-construct/ was not a &
            &/block-stmt/.'
      END IF
      CALL Move(st%first, stmt)
      new_tip => st
    ELSE IF (stmt%ist == istEndBlock) THEN
      IF (ALLOCATED(st%last)) THEN
        STOP 'Internal error in BlockConstructStNodes%%block_BuildTree, &
            &the last statement of a /block-construct/ was &
            &encountered twice.'
      END IF
      ALLOCATE(st%last)
      CALL Move(st%last, stmt)
      new_tip => st%parent
    ELSE IF (ALLOCATED(st%exec)) THEN
      CALL Add( err_list, CODE=errInvalidContext, COMPONENT='R807',  &
          LOCATION=QueryLocation(stmt%tlist(1)),  &
          MSG='A /' // GetStName(stmt%ist) // '/ is not valid at this &
            &point inside a /block-construct/.' )
    ELSE IF (ALLOCATED(st%spec)) THEN
      ALLOCATE(st%exec)
      st%exec%parent => st
      CALL st%exec%BuildTree(stmt, new_tip, err_list)
    ELSE
      ALLOCATE(st%spec)
      st%spec%parent => st
      CALL st%spec%BuildTree(stmt, new_tip, err_list)
    END IF
    
  END SUBROUTINE block_BuildTree
  
  
  !*****************************************************************************
  !!
  !> Implementation of BlockStNode%GetPart - get the syntax part associated 
  !! with the current statement tree node.
  !!
  !! @returns The part stack associated with the node.
  
  FUNCTION block_GetPart(st) RESULT(ipt)
    
    USE SyntaxParts
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(BlockConstructStNode), INTENT(IN) :: st
    
    ! Function result
    INTEGER :: ipt
    
    !***************************************************************************
    
    ipt = iptBlockConstruct
    
  END FUNCTION block_GetPart
  
  
  !*****************************************************************************
  !!
  !> Implementation of BlockStNode%DumpTree - dump the contents of this
  !! node and its child nodes.
  !!
  !! @param[in]     st                The statement tree node.
  !!
  !! @param[in]     unit              The unit to dump to.
  !!
  !! @param[in]     pfx               Prefix to write at the start 
  !! of each record (for indentation).
  
  RECURSIVE SUBROUTINE block_DumpTree(st, unit, pfx)
    
    USE SyntaxParts
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(BlockConstructStNode), INTENT(IN), TARGET :: st
    INTEGER, INTENT(IN) :: unit
    CHARACTER(*), INTENT(IN) :: pfx
    
    !***************************************************************************
    
    WRITE (unit, fmt_partname) pfx, GetPartName(st)
    
    IF (ALLOCATED(st%first))  &
        WRITE (unit, fmt_stmtname) pfx // '  ', GetStName(st%first)
    IF (ALLOCATED(st%spec)) CALL st%spec%DumpTree(unit, pfx // '  ')
    IF (ALLOCATED(st%exec)) CALL st%exec%DumpTree(unit, pfx // '  ')
    IF (ALLOCATED(st%last))  &
        WRITE (unit, fmt_stmtname) pfx // '  ', GetStName(st%last)
    
  END SUBROUTINE block_DumpTree
  
  
  !*****************************************************************************
  !!
  !> Implementation of BlockConstructStNode%Query - apply visitor object 
  !! operations to this node and its children.
  !!
  !! @param[in]     st                The statement tree node being traversed.
  !!
  !! @param[in,out] visitor           The object to execute the query action.
  
  RECURSIVE SUBROUTINE block_Query(st, visitor)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(BlockConstructStNode), INTENT(IN), TARGET :: st
    CLASS(StNodeQueryVisitor), INTENT(INOUT) :: visitor
    
    !***************************************************************************
    
    ! Execute against ourselves.
    CALL visitor%ExecuteNode(st)
    
    ! Execute against the initial statement.
    CALL visitor%ExecuteStmt(st, st%first)
    
    ! Foward query to the specification part, if present.
    IF (ALLOCATED(st%spec)) CALL st%spec%Query(visitor)
    
    ! Forward query to the execution part, if present.  This might contain 
    ! another block - hence this procedure is recursive.
    IF (ALLOCATED(st%exec)) CALL st%exec%Query(visitor)
    
    ! Execute against the closing statement.
    CALL visitor%ExecuteStmt(st, st%last)
    
  END SUBROUTINE block_Query
  
  
  !*****************************************************************************
  !!
  !> Implementation of BlockConstructStNode%Modify - apply modify visitor 
  !! object operations to this node and its children.
  !!
  !! @param[in,out] st                The statement tree node being traversed.
  !!
  !! @param[in,out] visitor           The object to execute the modify action.
  
  RECURSIVE SUBROUTINE block_Modify(st, visitor)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(BlockConstructStNode), INTENT(INOUT), TARGET :: st
    CLASS(StNodeModifyVisitor), INTENT(INOUT) :: visitor
    
    !***************************************************************************
    
    ! Execute against ourselves.
    CALL visitor%ExecuteNode(st)
    
    ! Execute against the initial statement.
    CALL visitor%ExecuteStmt(st, st%first)
    
    ! Foward query to the specification part, if present.
    IF (ALLOCATED(st%spec)) CALL st%spec%Modify(visitor)
    
    ! Forward query to the execution part, if present.  This might contain 
    ! another block - hence this procedure is recursive.
    IF (ALLOCATED(st%exec)) CALL st%exec%Modify(visitor)
    
    ! Execute against the closing statement.
    CALL visitor%ExecuteStmt(st, st%last)
    
  END SUBROUTINE block_Modify
  
  
  !*****************************************************************************
  !!
  !> Implementation of BlockConstructStNode%GetLocation - get the starting 
  !! source location of the node.
  !!
  !! @param[in]     st                The node of interest.
  !!
  !! @returns The source location of the start of the node.
  
  FUNCTION block_GetLocation(st) RESULT(loc)
    
    USE SourceLocations
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(BlockConstructStNode), INTENT(IN) :: st
    
    ! Function result.
    TYPE(SourceLocation) :: loc
    
    !***************************************************************************
    
    loc = QueryLocation(st%first%tlist(1))
    
  END FUNCTION block_GetLocation
  
  
  !*****************************************************************************
  !!
  !> Implementation of BlockConstructStNode%GetLabel - get the label 
  !! of the node.
  !!
  !! @param[in]     st                The node of interest.
  !!
  !! @returns The statement label of the the node.
  
  FUNCTION block_GetLabel(st) RESULT(label)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(BlockConstructStNode), INTENT(IN) :: st
    
    ! Function result.
    TYPE(LabelWrapper) :: label
    
    !***************************************************************************
    
    ! ifort bug - allocatable component in structure constructor.
    if (allocated(st%first%statement_label)) then
      label = LabelWrapper(st%first%statement_label)
    end if
    
  END FUNCTION block_GetLabel
  
END MODULE BlockConstructStNodes
