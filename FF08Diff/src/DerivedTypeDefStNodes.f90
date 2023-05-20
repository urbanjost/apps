! $Id: DerivedTypeDefStNodes.f90 2850 2019-03-27 21:30:10Z ian $
! ff08 source code copyright 2012 M.E.G.M.S.  See LICENCE.txt for licence.
!> @file
!! Defines the DerivedTypeDefStNodes module.


!*******************************************************************************
!!
!> Defines the TypeBoundProcedurePartStNode to encapsulate the 
!! /type-bound-procedure-part/ syntax rule, and the DerivedTypeDefStNode type
!! to encapsulate the /derived-type-def/ syntax rule.

MODULE DerivedTypeDefStNodes
  
  USE StatementData
  USE StNodes
  
  IMPLICIT NONE
  
  PRIVATE
  
  !-----------------------------------------------------------------------------  
  
  !> Statement tree node to encapsulate the /type-bound-procedure-part/ of 
  !! a /derived-type-def/.
  TYPE, PUBLIC, EXTENDS(StNode) :: TypeBoundProcedurePartStNode
    !> The contains statement.
    !!
    !! Should always be allocated after a successful build.
    TYPE(StData), ALLOCATABLE :: contains
    !> The binding private statement.
    !!
    !! Only allocated if that statement is present.
    TYPE(StData), ALLOCATABLE :: binding_private
    !> The actual bindings.  These can be either 
    !! /type-bound-procedure-stmt/'s, /type-bound-generic-stmt/'s or 
    !! /final-procedure-stmt/'s.
    !!
    !! Always allocated, may be zero size.
    TYPE(StData), ALLOCATABLE :: type_bound_procs(:)
  CONTAINS
    PROCEDURE :: BuildTree => tbp_BuildTree
    PROCEDURE :: GetPart => tbp_GetPart
    PROCEDURE :: DumpTree => tbp_DumpTree
    PROCEDURE :: Query => tbp_Query
    PROCEDURE :: Modify => tbp_Modify
    PROCEDURE :: GetLocation => tbp_GetLocation
    PROCEDURE :: GetLabel => tbp_GetLabel
  END TYPE TypeBoundProcedurePartStNode
  
  
  !> Statement tree node to encapsulate a /derived-type-def/.
  TYPE, PUBLIC, EXTENDS(StNode) :: DerivedTypeDefStNode
    !> The first statement in the derived-type-def construct 
    !! (a /type-stmt/).
    TYPE(StData), ALLOCATABLE :: first
    !> Statements (/type-param-def-stmt/'s) that define the type parameters 
    !! of the type.
    TYPE(StData), ALLOCATABLE :: type_params(:)
    !> Either a /private-components-stmt/ or a /sequence-stmt/, that defines 
    !! either the accessibility or sequence association of components of the 
    !! type.
    TYPE(StData), ALLOCATABLE :: priv_or_seq
    !> Statements that define the components of the derived type.
    !!
    !! Consider making this a separate part.
    TYPE(StData), ALLOCATABLE :: components(:)
    !> The /type-bound-procedure-part/.
    TYPE(TypeBoundProcedurePartStNode), ALLOCATABLE :: tbp
    !> The last statement in the derived-type-def construct 
    !! (an /end-type-stmt/).
    TYPE(StData), ALLOCATABLE :: last
  CONTAINS
    ! Bindings inherited from StNode.
    PROCEDURE :: BuildTree => dtd_BuildTree
    PROCEDURE :: GetPart => dtd_GetPart
    PROCEDURE :: DumpTree => dtd_DumpTree
    PROCEDURE :: Query => dtd_Query
    PROCEDURE :: Modify => dtd_Modify
    PROCEDURE :: GetLocation => dtd_GetLocation
    PROCEDURE :: GetLabel => dtd_GetLabel
  END TYPE DerivedTypeDefStNode
  
CONTAINS
  
  
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! Procedures for bindings in TypeBoundProcedurePartStNode.
  
  
  !*****************************************************************************
  !!
  !> Implementation of TypeBoundProcedurePartStNode%BuildTree - incorporate 
  !! the current statement into the statement tree.
  !!
  !! @param[in,out] st                The statement tree node.
  !!
  !! @param[in,out] stmt              The current statement.
  !! 
  !! @param[out]    new_tip           The new tip of the statement tree after 
  !! the statement has been processed.
  !!
  !! @param[out]    err_list          List of errors.
  
  RECURSIVE SUBROUTINE tbp_BuildTree(st, stmt, new_tip, err_list)
    
    USE Errors
    USE ErrorCodes
    USE ErrorLevels
    USE Statements
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(TypeBoundProcedurePartStNode), INTENT(INOUT), TARGET:: st
    TYPE(StData), INTENT(INOUT) :: stmt
    CLASS(StNode), INTENT(OUT), POINTER :: new_tip
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !---------------------------------------------------------------------------
    ! Local constants
    
    ! Syntax rule number for the /type-bound-procedure-part/.
    CHARACTER(*), PARAMETER :: comp = 'R445'
    
    !***************************************************************************
    
    ALLOCATE(err_list(0))
    
    IF (.NOT. ALLOCATED(st%contains)) THEN
      IF (stmt%ist /= istContains) THEN
        STOP 'Internal error in DerivedTypeDefStNodes%%tbp_BuildTree, &
            &the first statement of a /type-bound-procedure-part/ was not &
            &a /contains-stmt/.'
      END IF
      ALLOCATE(st%contains)
      CALL Move(st%contains, stmt)
      new_tip => st
    ELSE IF (stmt%ist == istBindingPrivate) THEN
      IF (ALLOCATED(st%binding_private)) THEN
        CALL Add( err_list,  &
            CODE=errDuplicateStatement,  &
            COMPONENT=comp,  &
            LOCATION=QueryLocation(stmt%tlist(1)),  &
            MSG='A /binding-private-stmt/ has already been specified for &
              &this /type-bound-procedure-part/.' )
      ELSE
        IF (ALLOCATED(st%type_bound_procs)) THEN
          CALL Add( err_list,  &
              CODE=errInvalidContext,  &
              COMPONENT=comp,  &
              LOCATION=QueryLocation(stmt%tlist(1)),  &
              MSG='The /binding-private-stmt/ for a &
                &/type-bound-procedure-part/ must come before the first &
                &/type-bound-proc-binding/.' )
          ! We'll store it anyway.
        END IF
        ALLOCATE(st%binding_private)
        CALL Move(st%binding_private, stmt)
        new_tip => st
      END IF
    ELSE IF ( (stmt%ist == istTypeBoundProcedure)  &
        .OR. (stmt%ist == istTypeBoundGeneric)  &
        .OR. (stmt%ist == istFinalProcedure) ) THEN
      CALL AddStmt(st%type_bound_procs, stmt)
      new_tip => st
    ELSE
      ! Don't know what this is - ask mum.
      CALL st%Parent%BuildTree(stmt, new_tip, err_list)
    END IF
    
  END SUBROUTINE tbp_BuildTree
  
  
  !*****************************************************************************
  !!
  !> Implementation of TypeBoundProcedurePartStNode%GetPart - get the 
  !! syntax part of this statement tree node.
  !!
  !! @returns The part associated with the node.
  
  FUNCTION tbp_GetPart(st) RESULT(ipt)
    
    USE SyntaxParts
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(TypeBoundProcedurePartStNode), INTENT(IN) :: st
    
    ! Function result
    INTEGER :: ipt
    
    !***************************************************************************
    
    ipt = iptTypeBoundProcedurePart
    
  END FUNCTION tbp_GetPart
  
  
  !*****************************************************************************
  !!
  !> Implementation of TypeBoundProcedurePartStNode%DumpTree - print debugging 
  !! information about this node and its children.
  !!
  !! @param[in]     st                The statement tree node.
  !!
  !! @param[in]     unit              The unit to dump to.
  !!
  !! @param[in]     pfx               Prefix to write at the start 
  !! of each record (for indentation).
  
  SUBROUTINE tbp_DumpTree(st, unit, pfx)
  
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(TypeBoundProcedurePartStNode), INTENT(IN), TARGET :: st
    INTEGER, INTENT(IN) :: unit
    CHARACTER(*), INTENT(IN) :: pfx
        
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: i
    
    !***************************************************************************
    
    WRITE (unit, fmt_partname) pfx, GetPartName(st)
    
    IF (ALLOCATED(st%contains))  &
        WRITE (unit, fmt_stmtname) pfx // '  ', GetStName(st%contains)
    IF (ALLOCATED(st%binding_private))  &
        WRITE (unit, fmt_stmtname) pfx // '  ', GetStName(st%binding_private)
    IF (ALLOCATED(st%type_bound_procs)) THEN
      DO i = 1, SIZE(st%type_bound_procs)
        WRITE (unit, fmt_stmtname) pfx // '  ',  &
            GetStName(st%type_bound_procs(i))
      END DO
    END IF
    
  END SUBROUTINE tbp_DumpTree
  
  
  !*****************************************************************************
  !!
  !> Implementation of TypeBoundProcedurePartStNode%Query - apply visitor 
  !! object operations to this node and its children.
  !!
  !! @param[in]     st                The statement tree node being traversed.
  !!
  !! @param[in,out] visitor           The object to execute the query action.
  
  SUBROUTINE tbp_Query(st, visitor)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(TypeBoundProcedurePartStNode), INTENT(IN), TARGET :: st
    CLASS(StNodeQueryVisitor), INTENT(INOUT) :: visitor
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: i              ! Child definition statement index.
    
    !***************************************************************************
    
    ! Execute against ourselves.
    CALL visitor%ExecuteNode(st)
    
    ! Execute against the initial contains statement.
    CALL visitor%ExecuteStmt(st, st%contains)
    
    ! Execute against the binding private statement, if present.
    IF (ALLOCATED(st%binding_private)) THEN
      CALL visitor%ExecuteStmt(st, st%binding_private)
    END IF
    
    ! Execute against the child binding statements.
    DO i = 1, SIZE(st%type_bound_procs)
      CALL visitor%ExecuteStmt(st, st%type_bound_procs(i))
    END DO
    
  END SUBROUTINE tbp_Query
  
  
  !*****************************************************************************
  !!
  !> Implementation of TypeBoundProcedurePartStNode%Modify - apply 
  !! modify visitor object operations to this node and its children.
  !!
  !! @param[in,out] st                The statement tree node being traversed.
  !!
  !! @param[in,out] visitor           The object to execute the modify action.
  
  SUBROUTINE tbp_Modify(st, visitor)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(TypeBoundProcedurePartStNode), INTENT(INOUT), TARGET :: st
    CLASS(StNodeModifyVisitor), INTENT(INOUT) :: visitor
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: i              ! Child definition statement index.
    
    !***************************************************************************
    
    ! Execute against ourselves.
    CALL visitor%ExecuteNode(st)
    
    ! Execute against the initial contains statement.
    CALL visitor%ExecuteStmt(st, st%contains)
    
    ! Execute against the binding private statement, if present.
    IF (ALLOCATED(st%binding_private)) THEN
      CALL visitor%ExecuteStmt(st, st%binding_private)
    END IF
    
    ! Execute against the child binding statements.
    DO i = 1, SIZE(st%type_bound_procs)
      CALL visitor%ExecuteStmt(st, st%type_bound_procs(i))
    END DO
    
  END SUBROUTINE tbp_Modify
  
  
  !*****************************************************************************
  !!
  !> Implementation of TypeBoundProcedurePartStNode%GetLocation - get the 
  !! starting source location of the node.
  !!
  !! @param[in]     st                The node of interest.
  !!
  !! @returns The source location of the start of the node.
  
  FUNCTION tbp_GetLocation(st) RESULT(loc)
    
    USE SourceLocations
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(TypeBoundProcedurePartStNode), INTENT(IN) :: st
    
    ! Function result.
    TYPE(SourceLocation) :: loc
    
    !***************************************************************************
    
    loc = QueryLocation(st%contains%tlist(1))
    
  END FUNCTION tbp_GetLocation
  
  
  !*****************************************************************************
  !!
  !> Implementation of TypeBoundProcedurePartStNode%GetLabel - get the label 
  !! of the node.
  !!
  !! @param[in]     st                The node of interest.
  !!
  !! @returns The statement label of the the node.
  
  FUNCTION tbp_GetLabel(st) RESULT(label)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(TypeBoundProcedurePartStNode), INTENT(IN) :: st
    
    ! Function result.
    TYPE(LabelWrapper) :: label
    
    !***************************************************************************
    
    ! ifort bug - allocatable component in structure constructor.
    if (allocated(st%contains%statement_label)) then
      label = LabelWrapper(st%contains%statement_label)
    end if
    
  END FUNCTION tbp_GetLabel
  
  
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! Procedures for bindings in DerivedTypeDefStNode.
  
  
  !*****************************************************************************
  !!
  !> Implementation of DerivedTypeDefStNode%BuildTree - incorporate the 
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
  
  RECURSIVE SUBROUTINE dtd_BuildTree(st, stmt, new_tip, err_list)
    
    USE Errors
    USE ErrorCodes
    USE ErrorLevels
    USE Statements
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(DerivedTypeDefStNode), INTENT(INOUT), TARGET :: st
    TYPE(StData), INTENT(INOUT) :: stmt
    CLASS(StNode), INTENT(OUT), POINTER :: new_tip
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !---------------------------------------------------------------------------
    ! Local constants
    
    ! Syntax rule number for a /derived-type-def/.
    CHARACTER(*), PARAMETER :: comp = 'R425'
    
    !***************************************************************************
    
    ALLOCATE(err_list(0))
    
    IF (.NOT. ALLOCATED(st%first)) THEN
      
      IF (stmt%ist /= istDerivedType) THEN
        STOP 'Internal error in DerivedTypeDefStNodes%%dtd_BuildTree, &
            &the first statement of a /derived-type-def/ was not a &
            &/derived-type-stmt/.'
      END IF
      ALLOCATE(st%first)
      CALL Move(st%first, stmt)
      new_tip => st
      
    ELSE IF (stmt%ist == istEndType) THEN
      
      IF (ALLOCATED(st%last)) THEN
        STOP 'Internal error in DerivedTypeDefStNodes%%dtd_BuildTree, &
            &the last statement of a /derived-type-def/ was &
            &encountered twice.'
      ELSE
        ALLOCATE(st%last)
      END IF
      CALL Move(st%last, stmt)
      ! We are done.
      new_tip => st%parent
      
    ELSE IF (stmt%ist == istContains) THEN
      
      IF (ALLOCATED(st%tbp)) THEN
        CALL Add( err_list,  &
            CODE=errDuplicateStatement,  &
            COMPONENT=comp,  &
            LOCATION=QueryLocation(stmt%tlist(1)),  &
            MSG='A /contains-stmt/ has already appeared inside this &
              &/derived-type-def/.' )
        new_tip => st
      ELSE
        ALLOCATE(st%tbp)
        st%tbp%parent => st
        CALL st%tbp%BuildTree(stmt, new_tip, err_list)
      END IF
      
    ELSE IF (stmt%ist == istTypeParamDef) THEN
      
      IF (ALLOCATED(st%priv_or_seq) .OR. ALLOCATED(st%components)) THEN
        CALL Add( err_list,  &
            CODE=errInvalidContext,  &
            COMPONENT=comp,  &
            LOCATION=QueryLocation(stmt%tlist(1)),  &
            MSG='/type-param-def-stmt/''s must come before the &
              &/private-or-sequence/ statement and before the &
              &/component-part/ of the /derived-type-def/.' )
        ! We'll store it anyway.
      END IF
      CALL AddStmt(st%type_params, stmt)
      new_tip => st
      
    ELSE IF ( (stmt%ist == istPrivateComponents)  &
        .OR. (stmt%ist == istSequence) ) THEN
      
      IF (ALLOCATED(st%components)) THEN
        CALL Add( err_list,  &
            CODE=errInvalidContext,  &
            COMPONENT=comp,  &
            LOCATION=QueryLocation(stmt%tlist(1)),  &
            MSG='The /private-or-sequence/ statement must come before the &
              &/component-part/ of the /derived-type-def/.' )
        ! We'll process it anyway.
      END IF
      IF (ALLOCATED(st%priv_or_seq)) THEN
        IF (st%priv_or_seq%ist == stmt%ist) THEN
          CALL Add( err_list,  &
              CODE=errDuplicateStatement,  &
              COMPONENT=comp,  &
              LOCATION=QueryLocation(stmt%tlist(1)),  &
              MSG='A /' // GetStName(stmt%ist) // '/ has already appeared in &
                &this /derived-type-def/.' )
        ELSE
          CALL Add( err_list,  &
              CODE=errConflictingStatement,  &
              COMPONENT=comp,  &
              LOCATION=QueryLocation(stmt%tlist(1)),  &
              MSG='A /' // GetStName(stmt%ist) // '/ cannot appear along with &
                &a /' // GetStName(st%priv_or_seq%ist)  &
!                // ' (at ' // st%priv_or_seq%tlist(1)%pos%ToString() // ') '  &
                // '/ in the same /derived-type-def/.' )
        END IF
      ELSE
        ALLOCATE(st%priv_or_seq)
        CALL Move(st%priv_or_seq, stmt)
      END IF
      new_tip => st
      
    ELSE IF ( (stmt%ist == istDataComponentDef)  &
        .OR. (stmt%ist == istProcComponentDef) ) THEN
      
      CALL AddStmt(st%components, stmt)
      new_tip => st
      
    ELSE
      
      CALL Add( err_list,  &
          CODE=errInvalidContext,  &
          COMPONENT=comp,  &
          LOCATION=QueryLocation(stmt%tlist(1)),  &
          MSG='A /' // GetStName(stmt%ist) // '/ is not valid at this &
            &point inside a /derived-type-def/.' )
      new_tip => st
      
    END IF
    
  END SUBROUTINE dtd_BuildTree
  
  
  !*****************************************************************************
  !!
  !> Implementation of DerivedTypeDefStNode%GetParts - get the syntax parts
  !! associated with this statement tree node.
  !!
  !! @returns The part stack associated with the node.
  
  FUNCTION dtd_GetPart(st) RESULT(ipt)
    
    USE SyntaxParts
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(DerivedTypeDefStNode), INTENT(IN) :: st
    
    ! Function result
    INTEGER :: ipt
    
    !***************************************************************************
    
    ipt = iptDerivedTypeDef
    
  END FUNCTION dtd_GetPart
  
  
  !*****************************************************************************
  !!
  !> Implementation of DerivedTypeDefStNode%DumpTree - write debugging 
  !! information about the node and its children.
  !!
  !! @param[in]     st                The statement tree node.
  !!
  !! @param[in]     unit              The unit to dump to.
  !!
  !! @param[in]     pfx               Prefix to write at the start 
  !! of each record (for indentation).
  
  SUBROUTINE dtd_DumpTree(st, unit, pfx)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(DerivedTypeDefStNode), INTENT(IN), TARGET :: st
    INTEGER, INTENT(IN) :: unit
    CHARACTER(*), INTENT(IN) :: pfx
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: i
    
    !***************************************************************************
    
    WRITE (unit, fmt_partname) pfx, GetPartName(st)
    
    IF (ALLOCATED(st%first))  &
        WRITE (unit, fmt_stmtname) pfx // '  ', GetStName(st%first)
    IF (ALLOCATED(st%type_params)) THEN
      DO i = 1, SIZE(st%type_params)
        WRITE (unit, fmt_stmtname) pfx // '  ', GetStName(st%type_params(i))
      END DO
    END IF
    IF (ALLOCATED(st%priv_or_seq))  &
        WRITE (unit, fmt_stmtname) pfx // '  ', GetStName(st%priv_or_seq)
    IF (ALLOCATED(st%components)) THEN
      DO i = 1, SIZE(st%components)
        WRITE (unit, fmt_stmtname) pfx // '  ', GetStName(st%components(i))
      END DO
    END IF
    IF (ALLOCATED(st%tbp)) CALL st%tbp%DumpTree(unit, pfx // '  ')
    IF (ALLOCATED(st%last))  &
        WRITE (unit, fmt_stmtname) pfx // '  ', GetStName(st%last)
    
  END SUBROUTINE dtd_DumpTree
  
  
  !*****************************************************************************
  !!
  !> Implementation of DerivedTypeDefStNode%Query - apply visitor 
  !! object operations to this node and its children.
  !!
  !! @param[in]     st                The statement tree node being traversed.
  !!
  !! @param[in,out] visitor           The object to execute the query action.
  
  SUBROUTINE dtd_Query(st, visitor)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(DerivedTypeDefStNode), INTENT(IN), TARGET :: st
    CLASS(StNodeQueryVisitor), INTENT(INOUT) :: visitor
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: i              ! Child definition statement index.
    
    !***************************************************************************
    
    ! Execute against ourselves.
    CALL visitor%ExecuteNode(st)
    
    ! Execute against the initial type statement.
    CALL visitor%ExecuteStmt(st, st%first)
    
    ! Execute against the type parameter definition statements, if present.
    IF (ALLOCATED(st%type_params)) THEN
      DO i = 1, SIZE(st%type_params)
        CALL visitor%ExecuteStmt(st, st%type_params(i))
      END DO
    END IF
    
    ! Execute against the private or sequence statement, if present.
    IF (ALLOCATED(st%priv_or_seq)) THEN
      CALL visitor%ExecuteStmt(st, st%priv_or_seq)
    END IF
    
    ! Execute against the child binding statements.
    DO i = 1, SIZE(st%components)
      CALL visitor%ExecuteStmt(st, st%components(i))
    END DO
    
    ! Forward the query to the type bound procedure part, if present.
    IF (ALLOCATED(st%tbp)) CALL st%tbp%Query(visitor)
    
    ! Execute against the final end type statement.
    CALL visitor%ExecuteStmt(st, st%last)
    
  END SUBROUTINE dtd_Query
  
  
  !*****************************************************************************
  !!
  !> Implementation of DerivedTypeDefStNode%Modify - apply modify visitor 
  !! object operations to this node and its children.
  !!
  !! @param[in,out] st                The statement tree node being traversed.
  !!
  !! @param[in,out] visitor           The object to execute the modify action.
  
  SUBROUTINE dtd_Modify(st, visitor)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(DerivedTypeDefStNode), INTENT(INOUT), TARGET :: st
    CLASS(StNodeModifyVisitor), INTENT(INOUT) :: visitor
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: i              ! Child definition statement index.
    
    !***************************************************************************
    
    ! Execute against ourselves.
    CALL visitor%ExecuteNode(st)
    
    ! Execute against the initial type statement.
    CALL visitor%ExecuteStmt(st, st%first)
    
    ! Execute against the type parameter definition statements, if present.
    IF (ALLOCATED(st%type_params)) THEN
      DO i = 1, SIZE(st%type_params)
        CALL visitor%ExecuteStmt(st, st%type_params(i))
      END DO
    END IF
    
    ! Execute against the private or sequence statement, if present.
    IF (ALLOCATED(st%priv_or_seq)) THEN
      CALL visitor%ExecuteStmt(st, st%priv_or_seq)
    END IF
    
    ! Execute against the child binding statements.
    DO i = 1, SIZE(st%components)
      CALL visitor%ExecuteStmt(st, st%components(i))
    END DO
    
    ! Forward the query to the type bound procedure part, if present.
    IF (ALLOCATED(st%tbp)) CALL st%tbp%Modify(visitor)
    
    ! Execute against the final end type statement.
    CALL visitor%ExecuteStmt(st, st%last)
    
  END SUBROUTINE dtd_Modify
  
  
  !*****************************************************************************
  !!
  !> Implementation of DerivedTypeDefStNode%GetLocation - get the starting 
  !! source location of the node.
  !!
  !! @param[in]     st                The node of interest.
  !!
  !! @returns The source location of the start of the node.
  
  FUNCTION dtd_GetLocation(st) RESULT(loc)
    
    USE SourceLocations
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(DerivedTypeDefStNode), INTENT(IN) :: st
    
    ! Function result.
    TYPE(SourceLocation) :: loc
    
    !***************************************************************************
    
    loc = QueryLocation(st%first%tlist(1))
    
  END FUNCTION dtd_GetLocation
  
  
  !*****************************************************************************
  !!
  !> Implementation of DerivedTypeDefStNode%GetLabel - get the label 
  !! of the node.
  !!
  !! @param[in]     st                The node of interest.
  !!
  !! @returns The statement label of the the node.
  
  FUNCTION dtd_GetLabel(st) RESULT(label)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(DerivedTypeDefStNode), INTENT(IN) :: st
    
    ! Function result.
    TYPE(LabelWrapper) :: label
    
    !***************************************************************************
    
    ! ifort bug - allocatable component in structure constructor.
    if (allocated(st%first%statement_label)) then
      label = LabelWrapper(st%first%statement_label)
    end if
    
  END FUNCTION dtd_GetLabel
  
END MODULE DerivedTypeDefStNodes
