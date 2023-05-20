! $Id: ExecStNodes.f90 2915 2019-04-17 22:10:04Z ian $
! ff08 source code copyright 2012 M.E.G.M.S.  See LICENCE.txt for licence.
!> @file
!! Defines the ExecStNodes module.


!*******************************************************************************
!!
!> Defines StNode extensions for executable statements.
!!
!! Executable constructs follow several patterns:
!! - The basic block (encapsulated in BasicBlockStNode), where there is an 
!!   opening statement, a closing end-*-stmt and then an executable part 
!!   in between.  Constructs that fall into this category include the 
!!   /critical-construct/, the /associate-construct/, the /forall-construct/ 
!!   and the /block-do-construct/ where the starting statement is a 
!!   /nonlabel-do-stmt/.  Note that a /block-construct/ is not a BasicBlock 
!!   as it has a /specification-part/.
!! - The select block (encapsulated in SelectBlockStNode), where there is an 
!!   opening statement and a closing end-*-stmt around a series of blocks 
!!   that consist of a condition statement and an /execution-part/.  
!!   Constructs that fall into this category include the /case-construct/ 
!!   and the /select-type-construct/.
!! - The conditional block (encapsulated in ConditionalBlockStNode), where 
!!   there are three kinds of blocks in a series - the first being mandatory, 
!!   the second kind able to be repeated zero or more times and the third kind 
!!   being optional.  Constructs that fall into this category include the 
!!   /if-construct/ and the /where-construct/.
!! - Single statement executable statements, such as the /action-stmt/'s 
!!   bar the /end-*-stmt/'s.

MODULE ExecStNodes
  
  USE StatementData
  USE StNodes
  
  IMPLICIT NONE
  
  PRIVATE
  
  !-----------------------------------------------------------------------------
  
  PUBLIC :: DoBuildTree
  
  PUBLIC :: MakeExecStNode
  
  !-----------------------------------------------------------------------------
  
  !> StNode extension for the executable part of a program unit.
  TYPE, PUBLIC, EXTENDS(StNode) :: ExecStNode
    !> The child nodes of the part.
    TYPE(StNodeList), ALLOCATABLE :: children(:)
  CONTAINS
    ! Bindings inherited from StNode.
    PROCEDURE :: BuildTree => exec_BuildTree
    PROCEDURE :: Check => exec_Check
    PROCEDURE :: GetPart => exec_GetPart
    PROCEDURE :: Terminate => exec_Terminate
    PROCEDURE :: DumpTree => exec_DumpTree
    PROCEDURE :: Query => exec_Query
    PROCEDURE :: Modify => exec_Modify
    PROCEDURE :: GetLocation => exec_GetLocation
    PROCEDURE :: GetLabel => exec_GetLabel
    
    PROCEDURE :: IsEmpty => exec_IsEmpty
  END TYPE ExecStNode
  
  !-----------------------------------------------------------------------------
  
  !> Parent statement tree node for basic blocks, like /critical-construct/'s 
  !! and /associate-construct/'s.
  TYPE, EXTENDS(StNode), ABSTRACT, PUBLIC :: BasicBlockStNode
    !> The opening statement.
    TYPE(StData), ALLOCATABLE :: first
    
    !> The executable statements inside the construct.
    TYPE(ExecStNode), ALLOCATABLE :: exec
    
    !> The closing statement.
    !!
    !! For most types of block this is always allocated.  An exception is 
    !! the do construct, where it is not allocated if the do construct 
    !! is of the labelled variety.
    TYPE(StData), ALLOCATABLE :: last
  CONTAINS
    PROCEDURE :: DumpTree => basicblock_DumpTree
    PROCEDURE :: Query => basicblock_Query
    PROCEDURE :: Modify => basicblock_Modify
    PROCEDURE :: GetLocation => basicblock_GetLocation
    PROCEDURE :: GetLabel => basicblock_GetLabel
  END TYPE BasicBlockStNode
  
  !-----------------------------------------------------------------------------
  
  !> Sub-block of the select block or conditional block.
  !!
  !! If components are added to this type then the corresponding code in 
  !! move_sub_block needs to be updated.
  TYPE :: sub_block
    !> The case-stmt or type-guard-stmt.
    !!
    !! Always allocated after a successfull tree build.
    TYPE(StData) :: initial
    
    !> The executable part of the the block.
    !!
    !! This is an allocatable so that it can be moved from one parent array to 
    !! another larger parent array without breaking pointer associations to 
    !! this component.
    TYPE(ExecStNode), ALLOCATABLE :: exec
  END TYPE sub_block
  
  !> Parent statement tree node for select blocks, like /case-construct/'s 
  !! and /select-type-construct/'s.
  TYPE, EXTENDS(StNode), ABSTRACT, PUBLIC :: SelectBlockStNode
    !> The opening statement.
    !!
    !! Always allocated after a successful tree build.
    TYPE(StData), ALLOCATABLE :: first
    
    !> The various case bits.
    !!
    !! Always allocated after a successfull tree build.
    TYPE(sub_block), ALLOCATABLE :: blocks(:)
    
    !> The closing statement.
    !!
    !! Always allocated after a successful tree build.
    TYPE(StData), ALLOCATABLE :: last
  CONTAINS
    PROCEDURE :: DumpTree => selectblock_DumpTree
    PROCEDURE :: Query => selectblock_Query
    PROCEDURE :: Modify => selectblock_Modify
    PROCEDURE :: GetLocation => selectblock_GetLocation
    PROCEDURE :: GetLabel => selectblock_GetLabel
  END TYPE SelectBlockStNode
  
  !-----------------------------------------------------------------------------
  
  !> Statement tree node for a conditional construct.
  TYPE, EXTENDS(StNode), ABSTRACT, PUBLIC :: ConditionalBlockStNode
    !> The bits started with the initial if or where and after any subsequent 
    !! else if or masked elsewhere.
    !!
    !! Always allocated and always at least size one after a successful tree 
    !! build.
    TYPE(sub_block), ALLOCATABLE :: blocks(:)
    
    !> The bits after the else bit or elsewhere.
    !!
    !! Not allocate if there is no such block.
    TYPE(sub_block), ALLOCATABLE :: else_block
    
    !> The /end-if-stmt/ or /end-where-stmt/.
    TYPE(StData), ALLOCATABLE :: last
  CONTAINS
    PROCEDURE :: DumpTree => conditional_DumpTree
    PROCEDURE :: Query => conditional_Query
    PROCEDURE :: Modify => conditional_Modify
    PROCEDURE :: GetLocation => conditional_GetLocation
    PROCEDURE :: GetLabel => conditional_GetLabel
  END TYPE ConditionalBlockStNode
  
  !-----------------------------------------------------------------------------
  
  !> Routines that do the heavy lifting for the BuildTree binding.
  INTERFACE DoBuildTree
    MODULE PROCEDURE DoBuildTree_basicblock
    MODULE PROCEDURE DoBuildTree_select
    MODULE PROCEDURE DoBuildTree_conditional
  END INTERFACE DoBuildTree
  
  !-----------------------------------------------------------------------------
  
  
  INTERFACE
    !> External subprogram that allocates StNode's of the right type for a 
    !! particular executable statement.
    !!
    !! @param[in]     ist             The statement number.
    !!
    !! @param[out]    node            The StNode that corresponds to the 
    !! statement in @a ist, or not allocated if no suitable extension was 
    !! known.  The match is carried out on the statement that starts 
    !! a particular syntax part.
    SUBROUTINE ExecStNodeFactory(ist, node)
      IMPORT :: StNode
      IMPLICIT NONE
      !-------------------------------------------------------------------------
      INTEGER, INTENT(IN) :: ist
      CLASS(StNode), INTENT(OUT), ALLOCATABLE :: node
    END SUBROUTINE ExecStNodeFactory
  END INTERFACE
  
CONTAINS
  
  !*****************************************************************************
  !!
  !> Implementation of ExecStNode%BuildTree - incorporate the current 
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
  
  RECURSIVE SUBROUTINE exec_BuildTree(st, stmt, new_tip, err_list)
    
    USE Errors
    USE Statements
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(ExecStNode), INTENT(INOUT), TARGET :: st
    TYPE(StData), INTENT(INOUT) :: stmt
    CLASS(StNode), INTENT(OUT), POINTER :: new_tip
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Temporary for a child node to be added to the array of child nodes.
    CLASS(StNode), ALLOCATABLE :: node
    
    !***************************************************************************
    
    ALLOCATE(err_list(0))
    
    IF (.NOT. ALLOCATED(st%children)) ALLOCATE(st%children(0))
    
    CALL ExecStNodeFactory(stmt%ist, node)
    IF (ALLOCATED(node)) THEN
      node%parent => st
      CALL AddNode(st%children)
      CALL MOVE_ALLOC(node, st%children(SIZE(st%children))%item)
      new_tip => st%children(SIZE(st%children))%item
      CALL st%children(SIZE(st%children))%item%BuildTree( stmt,  &
          new_tip, err_list )
    ELSE
      ! We don't know what this is - ask mum.
      CALL st%parent%BuildTree(stmt, new_tip, err_list)
    END IF
    
  END SUBROUTINE exec_BuildTree
  
  
  !****************************************************************************
  !!
  !> Implementation of ExecStNode%Check - check that the node complies with 
  !! the rules of the language.
  !!
  !! @param[in]     st                The statement tree node.
  !!
  !! @param[out]    err_list          List of errors
  !!
  !! We check for obsoleted statement ordering (/data-stmt/ in an 
  !! /execution-part/.
  
  SUBROUTINE exec_Check(st, err_list)
    
    USE Errors
    USE ErrorCodes
    USE ErrorLevels
    USE SingleStNodes, ONLY: SingleStNode
    USE Statements
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(ExecStNode), INTENT(IN) :: st
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: i              ! Child node index.
    
    !***************************************************************************
    
    ALLOCATE(err_list(0))
    
    ! This scan isn't particularly efficient (perhaps we should identify 
    ! this during BuildTree), but it gets us there.
    DO i = 1, SIZE(st%children)
      SELECT TYPE (child => st%children(i)%item)
      TYPE IS (SingleStNode)
        IF (child%stmt%ist == istData) THEN
          CALL Add( err_list,  &
              CODE=errDataStmtInExecutionPart,  &
              LEVEL=errLevelWarning,  &
              COMPONENT='R209',  &
              LOCATION=child%GetLocation(),  &
              MSG='Having a /' // GetStName(child%stmt%ist) // '/ in an &
                &/execution-part/ is obsolescent.' )
        END IF
      END SELECT
    END DO
    
  END SUBROUTINE exec_Check
  
  
  !*****************************************************************************
  !!
  !> Implementation of ExecStNode%GetParts - get the syntax part associated 
  !! with this statement tree node.
  !!
  !! @returns The part associated with the node.
  
  FUNCTION exec_GetPart(st) RESULT(ipt)
    
    USE SyntaxParts
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(ExecStNode), INTENT(IN) :: st
    
    ! Function result.
    INTEGER :: ipt
    
    !***************************************************************************
    
    ipt = iptExecutionPart
    
  END FUNCTION exec_GetPart
  
  
  !*****************************************************************************
  !!
  !> Implementation of StNode%Terminate - called when a parent node decides 
  !! that it has finished.
  !!
  !! @param[in]     st                The statement tree node being terminated.
  !!
  !! @paramin]      eof               The statement tree node was terminated 
  !! by end of file.  @a stmt is not defined.
  !!
  !! @param[in]     stmt              The statement that was incorporated into 
  !! the tree and triggered termination.  Not defined if @a eof is true.
  !!
  !! @param[out]    err_list          List of errors.
  !!
  !! An execution part can be terminated arbitrarily.
  
  SUBROUTINE exec_Terminate(st, eof, stmt, err_list)
    
    USE Errors
    USE ErrorCodes
    USE StatementData
    USE SyntaxParts
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(ExecStNode), INTENT(INOUT), TARGET :: st
    LOGICAL, INTENT(IN) :: eof
    TYPE(StData), INTENT(IN) :: stmt
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !***************************************************************************
    
    ALLOCATE(err_list(0))
    
  END SUBROUTINE exec_Terminate
  
  
  !*****************************************************************************
  !!
  !> Implementation of ExecStNode%DumpTree - print debugging information 
  !! about this node and its children to a logical unit.
  !!
  !! @param[in]     st                The statement tree node.
  !!
  !! @param[in]     unit              The unit to dump to.
  !!
  !! @param[in]     pfx               Prefix to write at the start 
  !! of each record (for indentation).
  
  SUBROUTINE exec_DumpTree(st, unit, pfx)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(ExecStNode), INTENT(IN), TARGET :: st
    INTEGER, INTENT(IN) :: unit
    CHARACTER(*), INTENT(IN) :: pfx
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: i              ! Child statement tree node index.
    
    !***************************************************************************
    
    WRITE (unit, fmt_partname) pfx, GetPartName(st)
    
    DO i = 1, SIZE(st%children)
      CALL st%children(i)%item%DumpTree(unit, pfx // '  ')
    END DO
    
  END SUBROUTINE exec_DumpTree
  
  
  !*****************************************************************************
  !!
  !> Implementation of ExecStNode%Query - apply query visitor object 
  !! operations to this node and its children.
  !!
  !! @param[in]     st                The statement tree node being traversed.
  !!
  !! @param[in,out] visitor           The object to execute the query action.
  
  ! Recursive because an execution part can contain an execution part (via 
  ! a block construct).
  RECURSIVE SUBROUTINE exec_Query(st, visitor)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(ExecStNode), INTENT(IN), TARGET :: st
    CLASS(StNodeQueryVisitor), INTENT(INOUT) :: visitor
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: i              ! Child statement tree node index.
    
    !***************************************************************************
    
    ! Execute against ourselves.
    CALL visitor%ExecuteNode(st)
    
    ! Forward the query to the child nodes.
    DO i = 1, SIZE(st%children)
      CALL st%children(i)%Query(visitor)
    END DO
    
  END SUBROUTINE exec_Query
  
  
  !*****************************************************************************
  !!
  !> Implementation of ExecStNode%Modify - apply modify visitor object 
  !! operations to this node and its children.
  !!
  !! @param[in,out] st                The statement tree node being traversed.
  !!
  !! @param[in,out] visitor           The object to execute the modify action.
  
  ! Recursive because an execution part can contain an execution part (via 
  ! a block construct).
  RECURSIVE SUBROUTINE exec_Modify(st, visitor)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(ExecStNode), INTENT(INOUT), TARGET :: st
    CLASS(StNodeModifyVisitor), INTENT(INOUT) :: visitor
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: i              ! Child statement tree node index.
    
    !***************************************************************************
    
    ! Execute against ourselves.
    CALL visitor%ExecuteNode(st)
    
    ! Forward the query to the child nodes.
    DO i = 1, SIZE(st%children)
      CALL st%children(i)%Modify(visitor)
    END DO
    
  END SUBROUTINE exec_Modify
  
  
  !*****************************************************************************
  !!
  !> Implementation of ExecStNode%GetLocation - get the starting source 
  !! location of the node.
  !!
  !! @param[in]     st                The node of interest.
  !!
  !! @returns The source location of the start of the node.
  
  FUNCTION exec_GetLocation(st) RESULT(loc)
    
    USE SourceLocations
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(ExecStNode), INTENT(IN) :: st
    
    ! Function result.
    TYPE(SourceLocation) :: loc
    
    !***************************************************************************
    
    IF (SIZE(st%children) > 0) THEN
      loc = st%children(1)%item%GetLocation()
    ELSE
      ! The part doesn't really exist.
      loc = SourceLocationIfortBug()
    END IF
    
  END FUNCTION exec_GetLocation
  
  
  !*****************************************************************************
  !!
  !> Implementation of ExecStNode%GetLabel - get the label 
  !! of the node.
  !!
  !! @param[in]     st                The node of interest.
  !!
  !! @returns The statement label of the the node.
  
  FUNCTION exec_GetLabel(st) RESULT(label)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(ExecStNode), INTENT(IN) :: st
    
    ! Function result.
    TYPE(LabelWrapper) :: label
    
    !***************************************************************************
    
    IF (SIZE(st%children) /= 0) THEN
      label = st%children(1)%item%GetLabel()
    END IF
    
  END FUNCTION exec_GetLabel
  
  
  !*****************************************************************************
  !!
  !> Implementation of ExecStNode%IsEmpty - test whether the execution 
  !! part actually contains any statements.
  !!
  !! @param[in]     st                The node being queried.
  !!
  !! The nature of tree building means that a execution part might be 
  !! created when there are actually no statements in the part.  This 
  !! procedure allows that to be tested after a successful tree build.
  
  FUNCTION exec_IsEmpty(st) RESULT(b)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(ExecStNode), INTENT(IN) :: st
    
    ! Function result.
    LOGICAL :: b
    
    !***************************************************************************
    
    b = SIZE(st%children) == 0
    
  END FUNCTION exec_IsEmpty
  
  
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  
  
  !*****************************************************************************
  !!
  !> Incorporate the current statement into the statement tree for a 
  !! BasicBlockStNode extension.
  !!
  !! @param[in,out] st                The statement tree node.
  !!
  !! @param[in,out] stmt              The current statement.
  !!
  !! @param[in]     ist_first         Index of the statement that starts the 
  !! block.
  !!
  !! @param[in]     ist_last          Index of the statement that finishes the 
  !! block.
  !! 
  !! @param[out]    new_tip           The new tip of the statement tree after 
  !! the statement has been processed.
  !!
  !! @param[out]    err_list          List of errors.
  
  RECURSIVE SUBROUTINE DoBuildTree_basicblock( st, stmt, ist_first,  &
      ist_last, new_tip, err_list )
    
    USE CharUtils
    USE Errors
    USE ErrorCodes
    USE ErrorLevels
    USE Statements
    USE StatementData
    USE SyntaxParts
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(BasicBlockStNode), INTENT(INOUT), TARGET :: st
    TYPE(StData), INTENT(INOUT) :: stmt
    INTEGER, INTENT(IN) :: ist_first
    INTEGER, INTENT(IN) :: ist_last
    CLASS(StNode), INTENT(OUT), POINTER :: new_tip
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Buffer for assembling component number.
    CHARACTER(:), ALLOCATABLE :: comp
    
    !***************************************************************************
    
    ALLOCATE(err_list(0))
    
    ! Component for error reporting
    comp = 'R' // ToString(st%GetPart())
    
    IF (.NOT. ALLOCATED(st%first)) THEN
      ALLOCATE(st%first)
      IF (stmt%ist /= ist_first) THEN
        STOP 'Internal error in ExecStNodes%%DoBuildTree_basicblock, &
            &the first statement wasn''t the expected type of statement.'
      END IF
      CALL Move(st%first, stmt)
      ! Jump straight into the executable section
      ALLOCATE(st%exec)
      st%exec%parent => st
      new_tip => st%exec
    ELSE IF (stmt%ist == ist_last) THEN
      IF (ALLOCATED(st%last)) THEN
        STOP 'Internal error in ExecStNodes%%DoBuildTree_basicblock, &
            &the last statement was encountered twice.'
      ELSE
        ALLOCATE(st%last)
      END IF
      CALL Move(st%last, stmt)
      new_tip => st%parent
    ELSE 
      CALL Add( err_list,  &
          CODE=errInvalidContext,  &
          COMPONENT=comp,  &
          LOCATION=QueryLocation(stmt%tlist(1)),  &
          MSG='A /' // GetStName(stmt%ist) // '/ is not valid at this &
            &point inside a /' // GetPartName(st) // '/.' )
    END IF
    
  END SUBROUTINE DoBuildTree_basicblock
  
  
  !*****************************************************************************
  !!
  !> Implementation of BasicBlockStNode%DumpTree - write a debugging 
  !! representation of the node.
  !!
  !! @param[in]     st                The statement tree node.
  !!
  !! @param[in]     unit              Logical unit connected for formatted 
  !! output.
  !!
  !! @param[in]     pfx               Prefix to write before each record.
  
  SUBROUTINE basicblock_DumpTree(st, unit, pfx)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(BasicBlockStNode), INTENT(IN), TARGET :: st
    INTEGER, INTENT(IN) :: unit
    CHARACTER(*), INTENT(IN) :: pfx
    
    !***************************************************************************
    
    WRITE (unit, fmt_partname) pfx, GetPartName(st)
    
    IF (ALLOCATED(st%first))  &
        WRITE (unit, fmt_stmtname) pfx // '  ', GetStName(st%first)
    IF (ALLOCATED(st%exec)) CALL st%exec%DumpTree(unit, pfx // '  ')
    IF (ALLOCATED(st%last))  &
        WRITE (unit, fmt_stmtname) pfx // '  ', GetStName(st%last)
    
  END SUBROUTINE basicblock_DumpTree
  
  
  !*****************************************************************************
  !!
  !> Implementation of BasicBlockStNode%Query - apply visitor object 
  !! operations to this node and its children.
  !!
  !! @param[in]     st                The statement tree node being traversed.
  !!
  !! @param[in,out] visitor           The object to execute the query action.
  
  ! Recursive because of nested executable constructs.
  RECURSIVE SUBROUTINE basicblock_Query(st, visitor)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(BasicBlockStNode), INTENT(IN), TARGET :: st
    CLASS(StNodeQueryVisitor), INTENT(INOUT) :: visitor
    
    !***************************************************************************
    
    ! Execute against ourselves.
    CALL visitor%ExecuteNode(st)
    
    ! Execute against our opening statement.
    CALL visitor%ExecuteStmt(st, st%first)
    
    ! Forward the query to the executable part.
    CALL st%exec%Query(visitor)
    
    ! Execute against our closing statement.
    IF (ALLOCATED(st%last)) THEN
      CALL visitor%ExecuteStmt(st, st%last)
    END IF
    
  END SUBROUTINE basicblock_Query
  
  
  !*****************************************************************************
  !!
  !> Implementation of BasicBlockStNode%Modify - apply modify visitor object 
  !! operations to this node and its children.
  !!
  !! @param[in,out] st                The statement tree node being traversed.
  !!
  !! @param[in,out] visitor           The object to execute the modify action.
  
  ! Recursive because of nested executable constructs.
  RECURSIVE SUBROUTINE basicblock_Modify(st, visitor)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(BasicBlockStNode), INTENT(INOUT), TARGET :: st
    CLASS(StNodeModifyVisitor), INTENT(INOUT) :: visitor
    
    !***************************************************************************
    
    ! Execute against ourselves.
    CALL visitor%ExecuteNode(st)
    
    ! Execute against our opening statement.
    CALL visitor%ExecuteStmt(st, st%first)
    
    ! Forward the query to the executable part.
    CALL st%exec%Modify(visitor)
    
    ! Execute against our closing statement.
    IF (ALLOCATED(st%last)) THEN
      CALL visitor%ExecuteStmt(st, st%last)
    END IF
    
  END SUBROUTINE basicblock_Modify
  
  
  !*****************************************************************************
  !!
  !> Implementation of BasicBlockStNode%GetLocation - get the starting source 
  !! location of the node.
  !!
  !! @param[in]     st                The node of interest.
  !!
  !! @returns The source location of the start of the node.
  
  FUNCTION basicblock_GetLocation(st) RESULT(loc)
    
    USE SourceLocations
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(BasicBlockStNode), INTENT(IN) :: st
    
    ! Function result.
    TYPE(SourceLocation) :: loc
    
    !***************************************************************************
    
    loc = QueryLocation(st%first%tlist(1))
    
  END FUNCTION basicblock_GetLocation
  
  
  !*****************************************************************************
  !!
  !> Implementation of BasicBlockStNode%GetLabel - get the label 
  !! of the node.
  !!
  !! @param[in]     st                The node of interest.
  !!
  !! @returns The statement label of the the node.
  
  FUNCTION basicblock_GetLabel(st) RESULT(label)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(BasicBlockStNode), INTENT(IN) :: st
    
    ! Function result.
    TYPE(LabelWrapper) :: label
    
    !***************************************************************************
    
    ! ifort bug - allocatable component in structure constructor.
    if (allocated(st%first%statement_label)) then
      label = LabelWrapper(st%first%statement_label)
    end if
    
  END FUNCTION basicblock_GetLabel
  
  
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  !
  
  
  !*****************************************************************************
  !!
  !> Incorporate the current statement into the statement tree for a 
  !! SelectBlockStNode extension.
  !!
  !! @param[in,out] st                The statement tree node.
  !!
  !! @param[in,out] stmt              The current statement.
  !!
  !! @param[in]     ist_first         The statement index that opens the 
  !! block.
  !!
  !! @param[in]     ist_last          The statement index that closes the 
  !! block.
  !!
  !! @param[in]     ist_initial       The statement index that starts a new 
  !! sub-block.
  !! 
  !! @param[out]    new_tip           The new tip of the statement tree after 
  !! the statement has been processed.
  !!
  !! @param[out]    err_list          List of errors.
  
  RECURSIVE SUBROUTINE DoBuildTree_select(st, stmt, ist_first, ist_last,  &
      ist_initial, new_tip, err_list)
    
    USE CharUtils
    USE Errors
    USE ErrorCodes
    USE ErrorLevels
    USE Statements
    USE StatementData
    USE SyntaxParts
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(SelectBlockStNode), INTENT(INOUT), TARGET :: st
    TYPE(StData), INTENT(INOUT) :: stmt
    INTEGER, INTENT(IN) :: ist_first
    INTEGER, INTENT(IN) :: ist_initial
    INTEGER, INTENT(IN) :: ist_last
    CLASS(StNode), INTENT(OUT), POINTER :: new_tip
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Temporary for growing the blocks component.  The target attribute is 
    ! required so that pointers to subobjects of the blocks component remain 
    ! valid across the move.
    TYPE(sub_block), ALLOCATABLE, TARGET :: tmp(:)
    
    ! Buffer for assembling component number.
    CHARACTER(:), ALLOCATABLE :: comp
    
    !***************************************************************************
    
    ALLOCATE(err_list(0))
    
    comp = 'R' // ToString(st%GetPart())
    
    IF (.NOT. ALLOCATED(st%first)) THEN
      ALLOCATE(st%first)
      IF (stmt%ist /= ist_first) THEN
        CALL Add( err_list,  &
            CODE=errInternal,  &
            LEVEL=errLevelFatal,  &
            COMPONENT=comp,  &
            LOCATION=QueryLocation(stmt%tlist(1)),  &
            MSG='First statement of a ' // GetPartName(st)  &
              // ' isn''t a ' // GetStName(ist_first) // '.' )
      END IF
      CALL Move(st%first, stmt)
      new_tip => st
    ELSE IF (stmt%ist == ist_last) THEN
      IF (ALLOCATED(st%last)) THEN
        CALL Add( err_list,  &
            CODE=errInternal,  &
            LEVEL=errLevelFatal,  &
            COMPONENT=comp,  &
            LOCATION=QueryLocation(stmt%tlist(1)),  &
            MSG='Last statement of a ' // GetPartName(st)  &
              // ' was encountered twice.' )
      ELSE
        ALLOCATE(st%last)
      END IF
      CALL Move(st%last, stmt)
      new_tip => st%parent
    ELSE IF (stmt%ist == ist_initial) THEN
      ! Create a new sub-block for the section.
      IF (.NOT. ALLOCATED(st%blocks)) THEN
        ALLOCATE(st%blocks(1))
      ELSE
        ALLOCATE(tmp(SIZE(st%blocks) + 1))
        ! We need to move the child executable blocks across so that we 
        ! preserve the pointers within grand child blocks to their parent.
        CALL move_sub_block(st%blocks, tmp(:SIZE(st%blocks)))
        CALL MOVE_ALLOC(tmp, st%blocks)
      END IF
      CALL Move(st%blocks(SIZE(st%blocks))%initial, stmt)
      ! Create an executable part of the sub block.
      ALLOCATE(st%blocks(SIZE(st%blocks))%exec)
      st%blocks(SIZE(st%blocks))%exec%parent => st
      ! Tip becomes the execution-part of the sub-block.
      new_tip => st%blocks(SIZE(st%blocks))%exec
    ELSE
      CALL Add( err_list, CODE=errInvalidContext,  &
          COMPONENT=comp, LOCATION=QueryLocation(stmt%tlist(1)),  &
          MSG='A /' // GetStName(stmt%ist) // '/ is not valid at this &
            &point inside a /' // GetPartName(st) // '/.' )
    END IF
    
  END SUBROUTINE DoBuildTree_select
  
  
  !*****************************************************************************
  !!
  !> Move the contents of one sub_block to another.
  !!
  !! @param[in,out] from              The source sub_block.
  !!
  !! @param[out]    to                The destination sub_block.
  
  ELEMENTAL SUBROUTINE move_sub_block(from, to)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(sub_block), INTENT(INOUT), TARGET :: from
    TYPE(sub_block), INTENT(OUT), TARGET :: to
    
    !***************************************************************************
    
    to%initial = from%initial
    CALL MOVE_ALLOC(from%exec, to%exec)
    
  END SUBROUTINE move_sub_block
  
  
  !*****************************************************************************
  !!
  !> Execute a query action against this sub-block.
  !!
  !! @param[in]     block             The sub block being queried.
  !!
  !! @param[in]     parent            The parent node of the block.  This 
  !! is used as the parent of the initial statement of the block when the 
  !! query action is executed.
  !!
  !! @param[in,out] visitor           Object encapsulating the query action.
  
  ! Recursive because of nested executable constructs.
  RECURSIVE SUBROUTINE query_sub_block(block, parent, visitor)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(sub_block), INTENT(IN) :: block
    CLASS(StNode), INTENT(IN), TARGET :: parent
    CLASS(StNodeQueryVisitor), INTENT(INOUT) :: visitor
    
    !***************************************************************************
    
    ! Execute the action against the initial statement of the sub block.
    CALL visitor%ExecuteStmt(parent, block%initial)
    
    ! Forward the query to the child nodes of the sub block.
    CALL block%exec%Query(visitor)
    
  END SUBROUTINE query_sub_block
  
  
  !*****************************************************************************
  !!
  !> Execute a modify action against this sub-block.
  !!
  !! @param[in]     block             The sub block being modified.
  !!
  !! @param[in]     parent            The parent node of the block.  This 
  !! is used as the parent of the initial statement of the block when the 
  !! modify action is executed.
  !!
  !! @param[in,out] visitor           Object encapsulating the modify action.
  
  ! Recursive because of nested executable constructs.
  RECURSIVE SUBROUTINE modify_sub_block(block, parent, visitor)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(sub_block), INTENT(INOUT) :: block
    CLASS(StNode), INTENT(INOUT), TARGET :: parent
    CLASS(StNodeModifyVisitor), INTENT(INOUT) :: visitor
    
    !***************************************************************************
    
    ! Execute the action against the initial statement of the sub block.
    CALL visitor%ExecuteStmt(parent, block%initial)
    
    ! Forward the query to the child nodes of the sub block.
    CALL block%exec%Modify(visitor)
    
  END SUBROUTINE modify_sub_block
  
  
  !*****************************************************************************
  !!
  !> Implementation of SelectBlockStNode%DumpTree - print debugging 
  !! information about this node.
  !!
  !! @param[in]     st                The statement tree node.
  !!
  !! @param[in]     unit              The unit to dump to.
  !!
  !! @param[in]     pfx               Prefix to write at the start 
  !! of each record (for indentation).
  
  SUBROUTINE selectblock_DumpTree(st, unit, pfx)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(SelectBlockStNode), INTENT(IN), TARGET :: st
    INTEGER, INTENT(IN) :: unit
    CHARACTER(*), INTENT(IN) :: pfx
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: i              ! Sub block index.
    
    !***************************************************************************
    
    WRITE (unit, fmt_partname) pfx, GetPartName(st)
    
    IF (ALLOCATED(st%first))  &
        WRITE (unit, fmt_stmtname) pfx // '  ', GetStName(st%first)
    IF (ALLOCATED(st%blocks)) THEN
      DO i = 1, SIZE(st%blocks)
        WRITE (unit, fmt_stmtname) pfx // '  ', GetStName(st%blocks(i)%initial)
        CALL st%blocks(i)%exec%DumpTree(unit, pfx // '  ')
      END DO
    END IF
    IF (ALLOCATED(st%last))  &
        WRITE (unit, fmt_stmtname) pfx // '  ', GetStName(st%last)
    
  END SUBROUTINE selectblock_DumpTree
  
  
  !*****************************************************************************
  !!
  !> Implementation of SelectBlockStNode%Query - apply visitor object 
  !! operations to this node and its children.
  !!
  !! @param[in]     st                The statement tree node being traversed.
  !!
  !! @param[in,out] visitor           The object to execute the query action.
  
  ! Recursive because of nested executable constructs.
  RECURSIVE SUBROUTINE selectblock_Query(st, visitor)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(SelectBlockStNode), INTENT(IN), TARGET :: st
    CLASS(StNodeQueryVisitor), INTENT(INOUT) :: visitor
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: i              ! Sub block index.
    
    !***************************************************************************
    
    ! Execute against ourselves.
    CALL visitor%ExecuteNode(st)
    
    ! Execute against our opening statement.
    CALL visitor%ExecuteStmt(st, st%first)
    
    ! Forward the query to any nodes stored in the block.
    DO i = 1, SIZE(st%blocks)
      CALL query_sub_block(st%blocks(i), st, visitor)
    END DO
    
    ! Execute against our closing statement.
    CALL visitor%ExecuteStmt(st, st%last)
    
  END SUBROUTINE selectblock_Query
  
  
  !*****************************************************************************
  !!
  !> Implementation of SelectBlockStNode%Modify - apply modify visitor object 
  !! operations to this node and its children.
  !!
  !! @param[in,out] st                The statement tree node being traversed.
  !!
  !! @param[in,out] visitor           The object to execute the modify action.
  
  ! Recursive because of nested executable constructs.
  RECURSIVE SUBROUTINE selectblock_Modify(st, visitor)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(SelectBlockStNode), INTENT(INOUT), TARGET :: st
    CLASS(StNodeModifyVisitor), INTENT(INOUT) :: visitor
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: i              ! Sub block index.
    
    !***************************************************************************
    
    ! Execute against ourselves.
    CALL visitor%ExecuteNode(st)
    
    ! Execute against our opening statement.
    CALL visitor%ExecuteStmt(st, st%first)
    
    ! Forward the query to any nodes stored in the block.
    DO i = 1, SIZE(st%blocks)
      CALL modify_sub_block(st%blocks(i), st, visitor)
    END DO
    
    ! Execute against our closing statement.
    CALL visitor%ExecuteStmt(st, st%last)
    
  END SUBROUTINE selectblock_Modify
  
  
  !*****************************************************************************
  !!
  !> Implementation of SelectBlockStNode%GetLocation - get the starting source 
  !! location of the node.
  !!
  !! @param[in]     st                The node of interest.
  !!
  !! @returns The source location of the start of the node.
  
  FUNCTION selectblock_GetLocation(st) RESULT(loc)
    
    USE SourceLocations
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(SelectBlockStNode), INTENT(IN) :: st
    
    ! Function result.
    TYPE(SourceLocation) :: loc
    
    !***************************************************************************
    
    loc = QueryLocation(st%first%tlist(1))
    
  END FUNCTION selectblock_GetLocation
  
  
  !*****************************************************************************
  !!
  !> Implementation of SelectBlockStNode%GetLabel - get the label 
  !! of the node.
  !!
  !! @param[in]     st                The node of interest.
  !!
  !! @returns The statement label of the the node.
  
  FUNCTION selectblock_GetLabel(st) RESULT(label)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(SelectBlockStNode), INTENT(IN) :: st
    
    ! Function result.
    TYPE(LabelWrapper) :: label
    
    !***************************************************************************
    
    ! ifort bug - allocatable component in structure constructor.
    if (allocated(st%first%statement_label)) then
      label = LabelWrapper(st%first%statement_label)
    end if
    
  END FUNCTION selectblock_GetLabel
  
  
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  !
  
  
  !*****************************************************************************
  !!
  !> Incorporate the current statement into the statement tree for a 
  !! ConditionalBlockStNode extension.
  !!
  !! @param[in,out] st                The statement tree node.
  !!
  !! @param[in,out] stmt              The current statement.
  !!
  !! @param[in]     ist_first         The statement index that opens the 
  !! block.
  !!
  !! @param[in]     ist_last          The statement index that closes the 
  !! block.
  !! 
  !! @param[out]    new_tip           The new tip of the statement tree after 
  !! the statement has been processed.
  !!
  !! @param[out]    err_list          List of errors.
  
  RECURSIVE SUBROUTINE DoBuildTree_conditional( st, stmt, ist_first,  &
      ist_second, ist_third, ist_last, new_tip, err_list )
    
    USE CharUtils
    USE Errors
    USE ErrorCodes
    USE ErrorLevels
    USE Statements
    USE StatementData
    USE StNodes
    USE SyntaxParts
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(ConditionalBlockStNode), INTENT(INOUT), TARGET :: st
    TYPE(StData), INTENT(INOUT) :: stmt
    INTEGER, INTENT(IN) :: ist_first
    INTEGER, INTENT(IN) :: ist_second
    INTEGER, INTENT(IN) :: ist_third
    INTEGER, INTENT(IN) :: ist_last
    CLASS(StNode), INTENT(OUT), POINTER :: new_tip
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Temporary for growing the blocks component.  The target attribute is 
    ! required so that pointers to sub-objects of the block component remain 
    ! valid across the move.
    TYPE(sub_block), ALLOCATABLE, TARGET :: tmp(:)
    
    ! Buffer for assembling component number.
    CHARACTER(:), ALLOCATABLE :: comp
    
    !***************************************************************************
    
    ALLOCATE(err_list(0))
    
    comp = 'R' // ToString(st%GetPart())
    
    IF (.NOT. ALLOCATED(st%blocks)) THEN
      ALLOCATE(st%blocks(1))
      IF (stmt%ist /= ist_first) THEN
        CALL Add( err_list,   &
            CODE=errInternal,  &
            LEVEL=errLevelFatal,  &
            COMPONENT=comp,  &
            LOCATION=QueryLocation(stmt%tlist(1)),  &
            MSG='First statement of a ' // GetPartName(st)  &
              // ' isn''t a ' // GetStName(ist_first) // '.' )
      END IF
      CALL Move(st%blocks(1)%initial, stmt)
      ! Create an executable part, set it as the curren tip.
      ALLOCATE(st%blocks(1)%exec)
      st%blocks(1)%exec%parent => st
      new_tip => st%blocks(1)%exec
    ELSE IF (stmt%ist == ist_second) THEN
      ! Add a new sub block to the this statement tree node.
      ALLOCATE(tmp(SIZE(st%blocks) + 1))
      CALL move_sub_block(st%blocks, tmp(:SIZE(st%blocks)))
      CALL MOVE_ALLOC(tmp, st%blocks)
      CALL Move(st%blocks(SIZE(st%blocks))%initial, stmt)
      ! Create the executable part of the sub-block.
      ALLOCATE(st%blocks(SIZE(st%blocks))%exec)
      st%blocks(SIZE(st%blocks))%exec%parent => st
      ! The new tip of the statement tree is that new executable part.
      new_tip => st%blocks(SIZE(st%blocks))%exec
    ELSE IF (stmt%ist == ist_third) THEN
      ! Create the else sub block.
      ALLOCATE(st%else_block)
      CALL Move(st%else_block%initial, stmt)
      ! Create the executable part of the sub block.
      ALLOCATE(st%else_block%exec)
      st%else_block%exec%parent => st
      ! The new tip of the statement tree is that new executable part.
      new_tip => st%else_block%exec
    ELSE IF (stmt%ist == ist_last) THEN
      IF (ALLOCATED(st%last)) THEN
        CALL Add( err_list,  &
            CODE=errInternal,  &
            LEVEL=errLevelFatal,  &
            COMPONENT=comp,  &
            LOCATION=QueryLocation(stmt%tlist(1)),  &
            MSG='Last statement of a /' // GetPartName(st)  &
              // '/ was encountered twice.' )
      ELSE
        ALLOCATE(st%last)
      END IF
      CALL Move(st%last, stmt)
      new_tip => st%parent
    ELSE
      CALL Add( err_list,  &
          CODE=errInvalidContext,  &
          COMPONENT=comp,  &
          LOCATION=QueryLocation(stmt%tlist(1)),  &
          MSG='A /' // GetStName(stmt%ist)  &
            // '/ is not valid at this point inside a /'  &
            // GetPartName(st) // '/.' )
    END IF
      
  END SUBROUTINE DoBuildTree_conditional
  
  
  !*****************************************************************************
  !!
  !> Implementation of ConditionalBlockStNode%DumpTree - print debugging 
  !! information about this node.
  !!
  !! @param[in]     st                The statement tree node.
  !!
  !! @param[in]     unit              The unit to dump to.
  !!
  !! @param[in]     pfx               Prefix to write at the start 
  !! of each record (for indentation).
  
  SUBROUTINE conditional_DumpTree(st, unit, pfx)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(ConditionalBlockStNode), INTENT(IN), TARGET :: st
    INTEGER, INTENT(IN) :: unit
    CHARACTER(*), INTENT(IN) :: pfx
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: i              ! Child block index.
    
    !***************************************************************************
    
    WRITE (unit, fmt_partname) pfx, GetPartName(st)
    
    IF (ALLOCATED(st%blocks)) THEN
      DO i = 1, SIZE(st%blocks)
        WRITE (unit, fmt_stmtname) pfx // '  ', GetStName(st%blocks(i)%initial)
        CALL st%blocks(i)%exec%DumpTree(unit, pfx // '  ')
      END DO
    END IF
    IF (ALLOCATED(st%else_block)) THEN
      WRITE (unit, fmt_stmtname) pfx // '  ', GetStName(st%else_block%initial)
      CALL st%else_block%exec%DumpTree(unit, pfx // '  ')
    END IF
    IF (ALLOCATED(st%last)) PRINT fmt_stmtname, pfx // '  ', GetStName(st%last)
    
  END SUBROUTINE conditional_DumpTree
  
  
  !*****************************************************************************
  !!
  !> Implementation of ConditionalBlockStNode%Query - apply visitor object 
  !! operations to this node and its children.
  !!
  !! @param[in]     st                The statement tree node being traversed.
  !!
  !! @param[in,out] visitor           The object to execute the query action.
  
  ! Recursive because of nested executable constructs.
  RECURSIVE SUBROUTINE conditional_Query(st, visitor)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(ConditionalBlockStNode), INTENT(IN), TARGET :: st
    CLASS(StNodeQueryVisitor), INTENT(INOUT) :: visitor
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: i              ! Sub block index.
    
    !***************************************************************************
    
    ! Execute against ourselves.
    CALL visitor%ExecuteNode(st)
    
    ! Forward the query to any nodes stored in the block.
    DO i = 1, SIZE(st%blocks)
      CALL query_sub_block(st%blocks(i), st, visitor)
    END DO
    
    ! Forward the query to the else block, if present.
    IF (ALLOCATED(st%else_block)) THEN
      CALL query_sub_block(st%else_block, st, visitor)
    END IF
    
    ! Execute against our closing statement.
    CALL visitor%ExecuteStmt(st, st%last)
    
  END SUBROUTINE conditional_Query
  
  
  !*****************************************************************************
  !!
  !> Implementation of ConditionalBlockStNode%Modify - apply modify visitor 
  !! object operations to this node and its children.
  !!
  !! @param[in,out] st                The statement tree node being traversed.
  !!
  !! @param[in,out] visitor           The object to execute the modify action.
  
  ! Recursive because of nested executable constructs.
  RECURSIVE SUBROUTINE conditional_Modify(st, visitor)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(ConditionalBlockStNode), INTENT(INOUT), TARGET :: st
    CLASS(StNodeModifyVisitor), INTENT(INOUT) :: visitor
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: i              ! Sub block index.
    
    !***************************************************************************
    
    ! Execute against ourselves.
    CALL visitor%ExecuteNode(st)
    
    ! Forward the query to any nodes stored in the block.
    DO i = 1, SIZE(st%blocks)
      CALL modify_sub_block(st%blocks(i), st, visitor)
    END DO
    
    ! Forward the query to the else block, if present.
    IF (ALLOCATED(st%else_block)) THEN
      CALL modify_sub_block(st%else_block, st, visitor)
    END IF
    
    ! Execute against our closing statement.
    CALL visitor%ExecuteStmt(st, st%last)
    
  END SUBROUTINE conditional_Modify
  
  
  !*****************************************************************************
  !!
  !> Implementation of ConditionalBlockStNode%GetLocation - get the starting 
  !! source location of the node.
  !!
  !! @param[in]     st                The node of interest.
  !!
  !! @returns The source location of the start of the node.
  
  FUNCTION conditional_GetLocation(st) RESULT(loc)
    
    USE SourceLocations
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(ConditionalBlockStNode), INTENT(IN) :: st
    
    ! Function result.
    TYPE(SourceLocation) :: loc
    
    !***************************************************************************
    
    loc = QueryLocation(st%blocks(1)%initial%tlist(1))
    
  END FUNCTION conditional_GetLocation
  
  
  !*****************************************************************************
  !!
  !> Implementation of ConditionalBlockStNode%GetLabel - get the label 
  !! of the node.
  !!
  !! @param[in]     st                The node of interest.
  !!
  !! @returns The statement label of the the node.
  
  FUNCTION conditional_GetLabel(st) RESULT(label)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(ConditionalBlockStNode), INTENT(IN) :: st
    
    ! Function result.
    TYPE(LabelWrapper) :: label
    
    !***************************************************************************
    
    ! ifort bug - allocatable component in structure constructor.
    if (allocated(st%blocks(1)%initial%statement_label)) then
      label = LabelWrapper(st%blocks(1)%initial%statement_label)
    end if
    
  END FUNCTION conditional_GetLabel
  
  
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  !
  
  
  !*****************************************************************************
  !!
  !> Make an ExecStNode from a single statements worth of tokens.
  !!
  !! This is used to convert the child token list from the single statement 
  !! forms of constructs into an ExecStNode for later parsing.
  
  SUBROUTINE MakeExecStNode(tlist, ist, exec_st)
    
    USE SingleStNodes
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Token), INTENT(INOUT), ALLOCATABLE :: tlist(:)
    INTEGER, INTENT(IN) :: ist
    TYPE(ExecStNode), INTENT(OUT) :: exec_st
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! The single statement.
    TYPE(SingleStNode), ALLOCATABLE :: action_st
    
    !***************************************************************************
    
    ALLOCATE(action_st)
    ALLOCATE(action_st%stmt)
    action_st%stmt%ist = ist
    ! statement label left unallocated.
    CALL MOVE_ALLOC(tlist, action_st%stmt%tlist)
    
    ALLOCATE(exec_st%children(1))
    CALL MOVE_ALLOC(action_st, exec_st%children(1)%item)
    
  END SUBROUTINE MakeExecStNode
  
END MODULE ExecStNodes
