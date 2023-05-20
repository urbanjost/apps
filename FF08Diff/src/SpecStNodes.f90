! $Id: SpecStNodes.f90 2905 2019-04-12 22:32:58Z ian $
! ff08 source code copyright 2012 M.E.G.M.S.  See LICENCE.txt for licence.
!> @file
!! Defines the SpecStNodes module.


!*******************************************************************************
!!
!> Defines the SpecStNode type to represent the specification-part of the
!! statement tree.

MODULE SpecStNodes
  
  USE StatementData
  USE StNodes
  
  IMPLICIT NONE
  
  PRIVATE
  
  !-----------------------------------------------------------------------------
  
  !> A collection of specification statements for a scope.
  !!
  !! The /implicit-part/ of the specification section could be handled 
  !! as a separate StNode, but then we'd have to contend with parameter, entry 
  !! and format statements at two different levels of the tree.
  TYPE, PUBLIC, EXTENDS(StNode) :: SpecStNode
    !> The /use-stmt/'s and /import-stmt/'s.
    !!
    !! The use and import statements are stored in the order that they are 
    !! encountered.  Syntax rules require that these are contiguous at the 
    !! start of the /specification-part/.
    !!
    !! Always allocated but perhaps to zero length, once the BuildTree pass 
    !! has successfully completed.
    TYPE(StData), ALLOCATABLE :: use_stmts(:)
    
    !> The /implicit-stmt/'s, /parameter-stmt/'s, /format-stmt/'s and 
    !! /entry-stmt/'s that are in the /implicit-part/ (R205) that follows any 
    !! /use-stmt/'s or /import-stmt/'s.
    !!
    !! This part is terminated by the last /implicit-stmt/.  We don't know 
    !! whether we have seen the last /implicit-stmt/ until after the fact, 
    !! so we temporarily put any valid statement into this part, and then 
    !! move any extras into the children component once we have decided that 
    !! the implicit part is complete.
    !!
    !! Always allocated but perhaps to zero length, once the BuildTree pass 
    !! has successfully completed.
    TYPE(StNodeList), ALLOCATABLE :: implicit_part_nodes(:)
    
    !> Statements and constructs other than /use-stmt/'s, 
    !! /implicit-part-stmt/'s and /import-stmt/'s that make up the 
    !! specification section (/declaration-construct/'s).
    !!
    !! Always allocated but perhaps to zero length, once the BuildTree pass 
    !! has successfully completed.
    TYPE(StNodeList), ALLOCATABLE :: children(:)
    
    !> Flag to indicate that a statement other than a /use-stmt/ has been 
    !! encountered in the specification section (so we've finished building 
    !! the /use-stmt/ part of the /specification-part/).
    LOGICAL, PRIVATE :: use_finished = .FALSE.
    
    !> Flag to indicate that a statement other than a /use-stmt/ or 
    !! an /import-stmt/ has been encountered (so we've finished building 
    !! the /import-stmt/ part of the /specification-part/).
    LOGICAL, PRIVATE :: import_finished = .FALSE.
    
    !> Flag to indicate that a statement other than a /use-stmt/, an 
    !! /import-stmt/ or /implicit-part-stmt/ has been encountered (so we've 
    !! finished building the /implicit-part/ of the specification section 
    !! and have moved into the /declaration-construct/ part.
    !!
    !! /implicit-part-stmt/'s include /implicit-stmt/'s (the last of which 
    !! terminates the part), /format-stmt/'s, /entry-stmt/'s and 
    !! /parameter-stmt/'s.
    LOGICAL, PRIVATE :: implicit_finished = .FALSE.
    
    !> Index of the last /implicit-stmt/ in implicit_part_nodes that we 
    !! have seen, zero if we are yet to see any.
    INTEGER, PRIVATE :: last_implicit_idx = 0
  CONTAINS
    ! Bindings inherited from StNode.
    PROCEDURE :: BuildTree => spec_BuildTree
    PROCEDURE :: GetPart => spec_GetPart
    PROCEDURE :: Terminate => spec_Terminate
    PROCEDURE :: DumpTree => spec_DumpTree
    PROCEDURE :: Query => spec_Query
    PROCEDURE :: Modify => spec_Modify
    PROCEDURE :: GetLocation => spec_GetLocation
    PROCEDURE :: GetLabel => spec_GetLabel
    
    PROCEDURE :: IsEmpty => spec_IsEmpty
  END TYPE SpecStNode
  
  !-----------------------------------------------------------------------------
  
  INTERFACE
    !> Allocate an appropriate StNode extension based on a statement.
    !!
    !! @param[in]     ist               The statement number.
    !!
    !! @param[in]     implicit_part     True if the statement is in 
    !! the /implicit-part/ of the /specification-part/, false 
    !! otherwise.
    !!
    !! @param[out]    node              The statement tree node that 
    !! corresponds to ist, or not allocated if there is no such node.
    SUBROUTINE SpecStNodeFactory(ist, implicit_part, node)
      USE StNodes
      IMPLICIT NONE
      !-------------------------------------------------------------------------
      INTEGER, INTENT(IN) :: ist
      LOGICAL, INTENT(IN) :: implicit_part
      CLASS(StNode), INTENT(OUT), ALLOCATABLE :: node
    END SUBROUTINE SpecStNodeFactory
  END INTERFACE
  
CONTAINS
  
  !****************************************************************************
  !!
  !> Implementation of SpecStNode%BuildTree - incorporate the current
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
  
  RECURSIVE SUBROUTINE spec_BuildTree(st, stmt, new_tip, err_list)
    
    USE Errors
    USE ErrorCodes
    USE Statements
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(SpecStNode), INTENT(INOUT), TARGET :: st
    TYPE(StData), INTENT(INOUT) :: stmt
    CLASS(StNode), INTENT(OUT), POINTER :: new_tip
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Temporary for passing to the specification StNode factory.
    CLASS(StNode), ALLOCATABLE :: tmp_stnode
    
    ! Error list for child procedure calls.
    TYPE(Error), ALLOCATABLE :: sub_err_list(:)
    
    !***************************************************************************
    
    ALLOCATE(err_list(0))
    
    !---------------------------------------------------------------------------
    
    ! Every path through this IF needs to set new_tip.
    
    IF (stmt%ist == istUse) THEN
      
      IF (st%use_finished) THEN
        CALL Add( err_list,  &
            CODE=errInvalidContext,  &
            LOCATION=QueryLocation(stmt%tlist(1)),  &
            MSG='USE statement incorrectly positioned' )
      END IF
      CALL AddStmt(st%use_stmts, stmt)
      new_tip => st
      
    ELSE IF (stmt%ist == istImport) THEN
      
      IF (st%import_finished) THEN
        CALL Add( err_list,  &
            CODE=errInvalidContext,  &
            LOCATION=QueryLocation(stmt%tlist(1)),  &
            MSG='IMPORT statement incorrectly positioned' )
      END IF
      CALL AddStmt(st%use_stmts, stmt)
      st%use_finished = .TRUE.
      new_tip => st
      
    ELSE IF (stmt%ist == istImplicit) THEN
      
      CALL do_implicit_part_node(st, stmt, new_tip, err_list)
      
    ! The following statements can be part of the /implicit-part/.  Otherwise 
    ! they are part of the /declaration-constructs/'s that belong to the 
    ! /specification-part/.
    ELSE IF ( .NOT. st%implicit_finished  &
        .AND. ( (stmt%ist == istEntry)  &
          .OR. (stmt%ist == istParameter)  &
          .OR. (stmt%ist == istFormat) ) ) THEN
      
      CALL do_implicit_part_node(st, stmt, new_tip, sub_err_list)
      
    ELSE
      
      st%use_finished = .TRUE.
      st%import_finished = .TRUE.
      IF (.NOT. ALLOCATED(st%use_stmts)) ALLOCATE(st%use_stmts(0))
      IF (.NOT. st%implicit_finished) THEN
        IF (.NOT. ALLOCATED(st%implicit_part_nodes)) THEN
          ALLOCATE(st%implicit_part_nodes(0))
        ELSE
          ! Move statements in st%implicit_part_nodes after the last 
          ! /implicit-stmt/ across.
          CALL move_implicit_part_nodes(  &
              st,  &
              st%last_implicit_idx + 1,  &
              sub_err_list )
          CALL Add(err_list, sub_err_list)
        END IF
        st%implicit_finished = .TRUE.
      END IF
      
      ! Allocate a temporary variable so we don't unnecessarily expand the 
      ! size of the statements component if the statement doesn't belong 
      ! to the specification section.
      CALL SpecStNodeFactory(stmt%ist, .FALSE., tmp_stnode)
      IF (ALLOCATED(tmp_stnode)) THEN
        ! The statement was a specification statement.
        IF (.NOT. ALLOCATED(st%children)) THEN
          ALLOCATE(st%children(1))
        ELSE
          CALL AddNode(st%children)
        END IF
        ASSOCIATE(last_node => st%children(SIZE(st%children)))
          CALL MOVE_ALLOC(tmp_stnode, last_node%item)
          last_node%item%parent => st
          CALL last_node%item%BuildTree(  &
              stmt,  &
              new_tip,  &
              sub_err_list )
        END ASSOCIATE
      ELSE
        ! Not a specification statement.  Might be an executable statement or 
        ! the end of the parent context - bounce it back up the tree to find 
        ! out
        new_tip => st%parent
        IF (.NOT. ALLOCATED(st%children)) ALLOCATE(st%children(0))
        CALL st%parent%BuildTree(stmt, new_tip, sub_err_list)
      END IF
      CALL Add(err_list, sub_err_list)
      
    END IF
    
  END SUBROUTINE spec_BuildTree
  
  
  !****************************************************************************
  !!
  !> Implementation of SpecStNode%GetPart - get the part stack associated 
  !! with the current statement tree node.
  !!
  !! @param[in]     st                The statement tree node.
  !!
  !! @returns The part associated with the node.
  
  FUNCTION spec_GetPart(st) RESULT(ipt)
    
    USE SyntaxParts
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(SpecStNode), INTENT(IN) :: st
    
    ! Function result
    INTEGER :: ipt
    
    !***************************************************************************
    
    ipt = iptSpecificationPart
    
  END FUNCTION spec_GetPart
  
  
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
  !! The specification part can be terminated arbitrarily.
  
  SUBROUTINE spec_Terminate(st, eof, stmt, err_list)
    
    USE Errors
    USE StatementData
    USE SyntaxParts
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(SpecStNode), INTENT(INOUT), TARGET :: st
    LOGICAL, INTENT(IN) :: eof
    TYPE(StData), INTENT(IN) :: stmt
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !***************************************************************************
    
    ALLOCATE(err_list(0))
    
    ! If terminate is called on @a st externally, perhaps as part of 
    ! testing, then spec_BuildTree may not have zero sized allocated 
    ! all components.  Fix that here.
    
    IF (.NOT. ALLOCATED(st%use_stmts)) ALLOCATE(st%use_stmts(0))
    IF (.NOT. ALLOCATED(st%implicit_part_nodes))  &
        ALLOCATE(st%implicit_part_nodes(0))
    IF (.NOT. ALLOCATED(st%children)) ALLOCATE(st%children(0))
    
  END SUBROUTINE spec_Terminate
  
  
  !*****************************************************************************
  !!
  !> Implementation of SpecStNode%Dump - print debugging information about 
  !! this node and its child nodes.
  !!
  !! @param[in]     st                The statement tree node.
  !!
  !! @param[in]     unit              The unit to dump to.
  !!
  !! @param[in]     pfx               Prefix to write at the start 
  !! of each record (for indentation).
  
  RECURSIVE SUBROUTINE spec_DumpTree(st, unit, pfx)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(SpecStNode), INTENT(IN), TARGET :: st
    INTEGER, INTENT(IN) :: unit
    CHARACTER(*), INTENT(IN) :: pfx
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: i              ! Child statement/node index.
    
    !***************************************************************************
  
    PRINT fmt_partname, pfx, GetPartName(st)
    
    IF (ALLOCATED(st%use_stmts)) THEN
      DO i = 1, SIZE(st%use_stmts)
        WRITE (unit, fmt_stmtname) pfx // '  ', GetStName(st%use_stmts(i))
      END DO
    END IF
    
    IF (ALLOCATED(st%implicit_part_nodes)) THEN
      DO i = 1, SIZE(st%implicit_part_nodes)
        CALL st%implicit_part_nodes(i)%item%DumpTree(unit, pfx // '  ')
      END DO
    END IF
    
    IF (ALLOCATED(st%children)) THEN
      DO i = 1, SIZE(st%children)
        CALL st%children(i)%item%DumpTree(unit, pfx // '  ')
      END DO
    END IF
    
  END SUBROUTINE spec_DumpTree
  
  
  !*****************************************************************************
  !!
  !> Worker routine for spec_BuildTree - process an apparent 
  !! /implicit-part/ statement.
  !!
  !! @param[in,out] st                The statement tree node for the 
  !! specification part.
  !!
  !! @param[in,out] stmt              The current statement.
  !! 
  !! @param[out]    new_tip           The new tip of the statement tree after 
  !! the statement has been processed.
  !!
  !! @param[out]    err_list          List of errors.
  
  SUBROUTINE do_implicit_part_node(st, stmt, new_tip, err_list)
    
    USE Errors
    USE ErrorCodes
    USE Statements
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(SpecStNode), INTENT(INOUT), TARGET :: st
    TYPE(StData), INTENT(INOUT) :: stmt
    CLASS(StNode), INTENT(OUT), POINTER :: new_tip
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Temporary for passing to the specification StNode factory.
    CLASS(StNode), ALLOCATABLE :: tmp_stnode
    
    ! Error list for child procedure calls.
    TYPE(Error), ALLOCATABLE :: sub_err_list(:)
    
    !***************************************************************************
    
    ALLOCATE(err_list(0))
    
    IF (st%implicit_finished .AND. (stmt%ist == istImplicit)) THEN
      CALL Add( err_list,  &
          CODE=errInvalidContext,  &
          LOCATION=QueryLocation(stmt%tlist(1)),  &
          MSG='IMPLICIT statement incorrectly positioned' )
      ! Continue on error, by adding the implicit statement to 
      ! the end of the list of nodes for the implicit part.
    END IF
    st%use_finished = .TRUE.
    st%import_finished = .TRUE.
    IF (.NOT. ALLOCATED(st%use_stmts)) ALLOCATE(st%use_stmts(0))
      
    CALL SpecStNodeFactory(stmt%ist, .TRUE., tmp_stnode)
    IF (.NOT. ALLOCATED(st%implicit_part_nodes)) THEN
      ALLOCATE(st%implicit_part_nodes(1))
    ELSE
      CALL AddNode(st%implicit_part_nodes)
    END IF
    ASSOCIATE( last_node  &
        => st%implicit_part_nodes(SIZE(st%implicit_part_nodes)) )
      CALL MOVE_ALLOC(tmp_stnode, last_node%item)
      last_node%item%parent => st
      CALL last_node%item%BuildTree(  &
          stmt,  &
          new_tip,  &
          sub_err_list )
      CALL Add(err_list, sub_err_list)
    END ASSOCIATE
    
    IF (stmt%ist == istImplicit)  &
        st%last_implicit_idx = SIZE(st%implicit_part_nodes)
    
  END SUBROUTINE do_implicit_part_node
  
  
  !*****************************************************************************
  !!
  !> Worker routine for spec_BuildTree - move statements from the 
  !! implicit_part_stmt component across to the statements component.
  !!
  !! @param[in,out] st                The statement tree node for the 
  !! specification part.
  !!
  !! @param[in]     start             Index of the first statement in 
  !! the implicit_part_stmts component of @a st that needs to be moved 
  !! across.  This may well be beyond the end of the implicit_part_stmts 
  !! component, in which case this procedure does nothing.
  !!
  !! @param[out]    err_list          List of errors.
  !!
  !! This procedure exists because we don't necessarily know that we have 
  !! left the implicit part until we have seen a statement that cannot 
  !! be part of the implicit part and we have seen the last implicit 
  !! statement.
  
  SUBROUTINE move_implicit_part_nodes(st, start, err_list)
    
    USE Errors
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    INTEGER, INTENT(IN) :: start
    CLASS(SpecStNode), INTENT(INOUT), TARGET :: st
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: i              ! implicit_part_stmts index.
    
    !***************************************************************************
    
    ALLOCATE(err_list(0))
    
    !---------------------------------------------------------------------------
    
    DO i = start, SIZE(st%implicit_part_nodes)
      IF (.NOT. ALLOCATED(st%children)) THEN
        ALLOCATE(st%children(1))
      ELSE
        CALL AddNode(st%children)
      END IF
      
      CALL MOVE_ALLOC(  &
          st%implicit_part_nodes(i)%item,  &
          st%children(SIZE(st%children))%item )
    END DO
    
    IF (start <= SIZE(st%implicit_part_nodes))  &
        st%implicit_part_nodes = st%implicit_part_nodes(:start-1)
    
  END SUBROUTINE move_implicit_part_nodes
  
  
  !*****************************************************************************
  !!
  !> Implementation of SpecStNode%Query - apply visitor object 
  !! operations to this node and its children.
  !!
  !! @param[in]     st                The statement tree node being traversed.
  !!
  !! @param[in,out] visitor           The object to execute the query action.
  
  ! Recursive because a specification part can contain another specification 
  ! part via interface bodies.
  RECURSIVE SUBROUTINE spec_Query(st, visitor)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(SpecStNode), INTENT(IN), TARGET :: st
    CLASS(StNodeQueryVisitor), INTENT(INOUT) :: visitor
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: i              ! Statement or child node index.
    
    !***************************************************************************
    
    ! Execute against ourselves.
    CALL visitor%ExecuteNode(st)
    
    ! Execute against the use and import statements.
    DO i = 1, SIZE(st%use_stmts)
      CALL visitor%ExecuteStmt(st, st%use_stmts(i))
    END DO
    
    ! Forward query to child implicit nodes.
    DO i = 1, SIZE(st%implicit_part_nodes)
      CALL st%implicit_part_nodes(i)%Query(visitor)
    END DO
    
    ! Forward query to child nodes.
    DO i = 1, SIZE(st%children)
      CALL st%children(i)%Query(visitor)
    END DO
    
  END SUBROUTINE spec_Query
  
  
  !*****************************************************************************
  !!
  !> Implementation of SpecStNode%Modify - apply modify visitor object 
  !! operations to this node and its children.
  !!
  !! @param[in,out] st                The statement tree node being traversed.
  !!
  !! @param[in,out] visitor           The object to execute the query action.
  
  ! Recursive because a specification part can contain another specification 
  ! part via interface bodies.
  RECURSIVE SUBROUTINE spec_Modify(st, visitor)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(SpecStNode), INTENT(INOUT), TARGET :: st
    CLASS(StNodeModifyVisitor), INTENT(INOUT) :: visitor
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: i              ! Statement or child node index.
    
    !***************************************************************************
    
    ! Execute against ourselves.
    CALL visitor%ExecuteNode(st)
    
    ! Execute against the use and import statements.
    DO i = 1, SIZE(st%use_stmts)
      CALL visitor%ExecuteStmt(st, st%use_stmts(i))
    END DO
    
    ! Execute against the implicit part nodes.
    DO i = 1, SIZE(st%implicit_part_nodes)
      CALL st%implicit_part_nodes(i)%Modify(visitor)
    END DO
    
    ! Forward query to child nodes.
    DO i = 1, SIZE(st%children)
      CALL st%children(i)%Modify(visitor)
    END DO
    
  END SUBROUTINE spec_Modify
  
  
  !*****************************************************************************
  !!
  !> Implementation of SpecStNode%GetLocation - get the starting source 
  !! location of the node.
  !!
  !! @param[in]     st                The node of interest.
  !!
  !! @returns The source location of the start of the node.
  
  FUNCTION spec_GetLocation(st) RESULT(loc)
    
    USE SourceLocations
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(SpecStNode), INTENT(IN) :: st
    
    ! Function result.
    TYPE(SourceLocation) :: loc
    
    !***************************************************************************
    
    IF (SIZE(st%use_stmts) /= 0) THEN
      loc = QueryLocation(st%use_stmts(1)%tlist(1))
    ELSE IF (SIZE(st%implicit_part_nodes) /= 0) THEN
      loc = st%implicit_part_nodes(1)%item%GetLocation()
    ELSE IF (SIZE(st%children) /= 0) THEN
      loc = st%children(1)%item%GetLocation()
    ELSE
      loc = SourceLocationIfortBug()
    END IF
    
  END FUNCTION spec_GetLocation
  
  
  !*****************************************************************************
  !!
  !> Implementation of SpecStNode%GetLabel - get the label 
  !! of the node.
  !!
  !! @param[in]     st                The node of interest.
  !!
  !! @returns The statement label value of the the node.
  
  FUNCTION spec_GetLabel(st) RESULT(label)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(SpecStNode), INTENT(IN) :: st
    
    ! Function result.
    TYPE(LabelWrapper) :: label
    
    !***************************************************************************
    
    IF (SIZE(st%use_stmts) /= 0) THEN
      ! ifort bug - allocatable component in structure constructor.
      if (allocated(st%use_stmts(1)%statement_label)) then
        label = LabelWrapper(st%use_stmts(1)%statement_label)
      end if
    ELSE IF (SIZE(st%implicit_part_nodes) /= 0) THEN
      label = st%implicit_part_nodes(1)%item%GetLabel()
    ELSE IF (SIZE(st%children) /= 0) THEN
      label = st%children(1)%item%GetLabel()
    END IF
    
  END FUNCTION spec_GetLabel
  
  
  !*****************************************************************************
  !!
  !> Implementation of SpecStNode%IsEmpty - test whether the specification 
  !! part actually contains any statements.
  !!
  !! @param[in]     st                The node being queried.
  !!
  !! The nature of tree building means that a specification part might be 
  !! created when there are actually no statements in the part.  This 
  !! procedure allows that to be tested after a successful tree build.
  
  FUNCTION spec_IsEmpty(st) RESULT(b)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(SpecStNode), INTENT(IN) :: st
    
    ! Function result.
    LOGICAL :: b
    
    !***************************************************************************
    
    b = SIZE(st%use_stmts) == 0
    IF (.NOT. b) RETURN
    
    b = SIZE(st%implicit_part_nodes) == 0
    IF (.NOT. b) RETURN
    
    b = SIZE(st%children) == 0
    
  END FUNCTION spec_IsEmpty
  
END MODULE SpecStNodes
