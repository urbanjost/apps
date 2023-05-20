! $Id: ProgUnitStNodes.f90 2931 2019-05-02 20:29:51Z ian $
! ff08 source code copyright 2012 M.E.G.M.S.  See LICENCE.txt for licence.
!> @file
!! Defines the ProgUnitStNodes module.


!*******************************************************************************
!!
!> Syntax tree nodes for program units, plus some helper procedures
!! for building the tree for such nodes.

MODULE ProgUnitStNodes
  
  USE ExecStNodes
  USE StNodes
  USE StatementData
  USE SpecStNodes
  
  IMPLICIT NONE
  
  PRIVATE
  
  PUBLIC :: DoBuildTree
  
  PUBLIC :: AddNode
  PUBLIC :: FindMainProgram
  PUBLIC :: FindNamedItem
  
  ! Work around for a bug that we are yet to isolate, where we cannot 
  ! deallocate the spec or exec components.
  PUBLIC :: has_spec
  PUBLIC :: has_exec
  
  !-----------------------------------------------------------------------------
  
  !> Type for the basic storage requirements of program-unit like 
  !! statement tree nodes.
  !!
  !! All program units must have a closing END statement of some form, 
  !! and they also all have a specification section, so these are 
  !! stored in this type.  All program units may have an opening statement, 
  !! (in the case of the main program this is optional), so that's stored 
  !! here too.
  !!
  !! In addition to acting as a parent type for program unit types proper, 
  !! we also use this for things that look like program units - such 
  !! as interface bodies.
  TYPE, PUBLIC, EXTENDS(StNode), ABSTRACT :: ProgUnitLikeStNode
    !> The opening statement - PROGRAM, SUBROUTINE, FUNCTION, etc.
    !!
    !! Might not be allocated in the case of a PROGRAM that doesn't have 
    !! an opening statement.
    TYPE(StData), ALLOCATABLE :: first
    
    !> The specification statements for the program unit.
    !!
    !! May not be allocated if the program unit had no specification 
    !! statements.
    TYPE(SpecStNode), ALLOCATABLE :: spec
    
    !> The closing statement, an END statement of some sort.
    !!
    !! Always allocated after a successful tree build.
    TYPE(StData), ALLOCATABLE :: last
  CONTAINS
    PROCEDURE :: DumpTree => progunitlike_DumpTree
    PROCEDURE :: Terminate => progunitlike_Terminate
    PROCEDURE :: GetLocation => progunitlike_GetLocation
    PROCEDURE :: GetLabel => progunitlike_GetLabel
  END TYPE ProgUnitLikeStNode
  
  !> Type to represent the statements that make up a program unit or 
  !! subprogram.
  !!
  !! This is essentially a glorified StNode specially for program units.  
  !! It is probable that we might want to query/rearrange the order/etc 
  !! the structure at a program unit level, so having this intermediate 
  !! gives us the opportunity to expand the interface beyond the very 
  !! basics offered by StNode.
  TYPE, PUBLIC, EXTENDS(ProgUnitLikeStNode), ABSTRACT :: ProgUnitStNode
    !> If true then attempts to add program units with the same name or 
    !! two main programs to the node will fail with an error.
    LOGICAL :: ensure_unique = .FALSE.
    
    !> Flag to indicate whether internal procedures are permitted.
    !!
    !! This is only checked during Check - the initial tree build 
    !! will still suceed with it set to false.
    !!
    !! This is only relevant for the ultimate extensions for function 
    !! and subroutine subprograms.  However, restrictions around 
    !! foward referencing the type of an allocatable component in 
    !! F2003 mean that we cannot have a vector of ExecUnitStNodes 
    !! inside InternalSubprogramPartStNode.  Hence we have brought 
    !! some of the components that should be in ExecUnitStNode into 
    !! this type.
    LOGICAL :: internal_ok = .TRUE.
  CONTAINS
    !> Retrieve the name of the program unit, if it can be determined 
    !! from the sequence of tokens associated with the first statement.
    !!
    !! Must not be called unless the tree building pass has been successfully 
    !! completed for the subject node.
    PROCEDURE(progunit_GetName), DEFERRED :: GetName
  END TYPE ProgUnitStNode
  
  ! Interfaces for deferred bindings in ProgUnitStNode.
  ABSTRACT INTERFACE
    !> Interface for ProgUnitStNode%GetName - get the name of the program 
    !! unit.
    !!
    !! Must not be called unless the tree building pass has been successfully 
    !! completed for the subject node.
    !!
    !! (Many extensions will rely on the "first" statement component to have 
    !! been allocated.)
    !!
    !! @param[in]     st              The statement tree node for the 
    !! program unit.
    !!
    !! @param[out]    name            The program unit name.  Note that this 
    !! is default character kind, regardless of the kind of the underlying 
    !! source.
    !!
    !! If a name cannot be determined (perhaps due to a syntax issue) then 
    !! the @a name argument will be unallocated on return.  If the program 
    !! unit is unnamed (relevant to main programs and block data units) then 
    !! the @a name argument will be zero length on return.
    SUBROUTINE progunit_GetName(st, name)
      IMPORT :: ProgUnitStNode
      IMPLICIT NONE
      !---------------------------------------------------------------------------
      CLASS(ProgUnitStNode), INTENT(IN) :: st
      CHARACTER(:), INTENT(OUT), ALLOCATABLE :: name
    END SUBROUTINE progunit_GetName
  END INTERFACE
  
  !> A element for an array (typically one dimensional) of program units.
  TYPE, PUBLIC :: ProgUnitVector
    !> The item for a particular element in the array.
    CLASS(ProgUnitStNode), ALLOCATABLE :: item
  CONTAINS
    PROCEDURE :: Query => puv_Query
    PROCEDURE :: Modify => puv_Modify
  END TYPE ProgUnitVector
  
  !-----------------------------------------------------------------------------
  
  !> Type to represent the /internal-subprogram-part/ of a /main-program/ 
  !! or a /function-subprogram/ or a /subroutine-subprogram/.
  TYPE, PUBLIC, EXTENDS(StNode) :: InternalSubprogramPartStNode
    !> If true then attempts to add program units with the same name or 
    !! two main programs to the node will fail with an error.
    !!
    !! See comments for the simillarly named component in BaseStNode.  Child 
    !! subprogram nodes will inherit this setting.
    LOGICAL :: ensure_unique = .FALSE.
    
    !> The contains-stmt that starts the part.
    !!
    !! Will be allocated after a successful tree build.
    TYPE(StData), ALLOCATABLE :: contains
    
    !> The program units that make up the main body of the part.
    !!
    !! Will be allocated (but may be zero size) after a successful 
    !! tree build.
    TYPE(ProgUnitVector), ALLOCATABLE :: subprograms(:)
  CONTAINS
    ! Bindings inherited from StNode.
    PROCEDURE :: BuildTree => isp_BuildTree
    PROCEDURE :: GetPart => isp_GetPart
    PROCEDURE :: DumpTree => isp_DumpTree
    PROCEDURE :: Query => isp_Query
    PROCEDURE :: Modify => isp_Modify
    PROCEDURE :: GetLocation => isp_GetLocation
    PROCEDURE :: GetLabel => isp_GetLabel
  END TYPE InternalSubprogramPartStNode
    
  !> Type to represent program units that additionally have an executable 
  !! section and an internal procedure section.
  !!
  !! This includes /main-program/'s, /function-subprogram/'s and 
  !! /subroutine-subprogram/'s.
  TYPE, EXTENDS(ProgUnitStNode), PUBLIC, ABSTRACT :: ExecUnitStNode
    !> The executable statements for the program unit.
    !!
    !! May not be allocated if the program unit had no executable statements.
    TYPE(ExecStNode), ALLOCATABLE :: exec
    
    !> The internal subprogram part of the program unit.
    !!
    !! Will not be allocated if there was no such part.
    TYPE(InternalSubprogramPartStNode), ALLOCATABLE :: internal
  CONTAINS
    PROCEDURE :: DumpTree => execunit_DumpTree
    PROCEDURE :: Query => execunit_Query
    PROCEDURE :: Modify => execunit_Modify
  END TYPE ExecUnitStNode
  
  !> Type to represent a main program.
  TYPE, PUBLIC, EXTENDS(ExecUnitStNode) :: MainProgramStNode
  CONTAINS
    PROCEDURE :: BuildTree => mainprogram_BuildTree
    PROCEDURE :: GetPart => mainprogram_GetPart
    PROCEDURE :: GetName => mainprogram_GetName
    PROCEDURE :: GetLocation => mainprogram_GetLocation
    PROCEDURE :: GetLabel => mainprogram_GetLabel
  END TYPE MainProgramStNode
  
  !> Type to represent a subroutine program unit.
  TYPE, PUBLIC, EXTENDS(ExecUnitStNode) :: SubroutineSubprogramStNode
  CONTAINS
    PROCEDURE :: BuildTree => subroutine_BuildTree
    PROCEDURE :: GetPart => subroutine_GetPart
    PROCEDURE :: GetName => subroutine_GetName
    PROCEDURE :: Check => subroutine_Check
  END TYPE SubroutineSubprogramStNode
  
  !> Type to represent a function program unit.
  TYPE, PUBLIC, EXTENDS(ExecUnitStNode) :: FunctionSubprogramStNode
  CONTAINS
    PROCEDURE :: BuildTree => function_BuildTree
    PROCEDURE :: GetPart => function_GetPart
    PROCEDURE :: GetName => function_GetName
    PROCEDURE :: Check => function_Check
  END TYPE FunctionSubprogramStNode
  
  !> Type to represent a separate module subprogram.
  !!
  !! This isn't a program unit!  What is it doing here??
  TYPE, PUBLIC, EXTENDS(ExecUnitStNode) :: SeparateModuleSubprogramStNode
  CONTAINS
    PROCEDURE :: BuildTree => sms_BuildTree
    PROCEDURE :: GetPart => sms_GetPart
    PROCEDURE :: GetName => sms_GetName
  END TYPE SeparateModuleSubprogramStNode
  
  !-----------------------------------------------------------------------------
  ! Interfaces
  
  INTERFACE DoBuildTree
    MODULE PROCEDURE DoBuildTree_progunitlike
    MODULE PROCEDURE DoBuildTree_execunit
    MODULE PROCEDURE DoBuildTree_isp
  END INTERFACE DoBuildTree
  
  !> Add a node to a list of nodes.
  INTERFACE AddNode
    MODULE PROCEDURE AddNode_
  END INTERFACE AddNode
  
CONTAINS
  
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! Procedures for ProgUnitStNode
  
  
  !*****************************************************************************
  !!
  !> Implementation of ProgUnitLikeStNode%DumpTree - print debugging 
  !! information about this node and its children.
  !!
  !! @param[in]     st                The statement tree node.
  !!
  !! @param[in]     unit              The unit to dump to.
  !!
  !! @param[in]     pfx               Prefix to write at the start 
  !! of each record (for indentation).
  
  SUBROUTINE progunitlike_DumpTree(st, unit, pfx)
    
    USE SyntaxParts
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(ProgUnitLikeStNode), INTENT(IN), TARGET :: st
    INTEGER, INTENT(IN) :: unit
    CHARACTER(*), INTENT(IN) :: pfx
    
    !***************************************************************************
    
    WRITE (unit, fmt_partname) pfx, GetPartName(st)
    
    IF (ALLOCATED(st%first))  &
        WRITE (unit, fmt_stmtname) pfx // '  ', GetStName(st%first)
    
    IF (ALLOCATED(st%spec)) CALL st%spec%DumpTree(unit, pfx // '  ')
    
    IF (ALLOCATED(st%last))  &
        WRITE (unit, fmt_stmtname) pfx // '  ', GetStName(st%last)
    
  END SUBROUTINE progunitlike_DumpTree
  
  
  !*****************************************************************************
  !!
  !> Implementation of ProgUnitLikeStNode%Terminate - called when a parent 
  !! node decides that it has finished.
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
  
  SUBROUTINE progunitlike_Terminate(st, eof, stmt, err_list)
    
    USE CharUtils
    USE Errors
    USE ErrorCodes
    USE StatementData
    USE SyntaxParts
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(ProgUnitLikeStNode), INTENT(INOUT), TARGET :: st
    LOGICAL, INTENT(IN) :: eof
    TYPE(StData), INTENT(IN) :: stmt
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Component for error reporting, built from the syntax part.
    CHARACTER(:), ALLOCATABLE :: comp
    
    !***************************************************************************
    
    ALLOCATE(err_list(0))
    
    comp = 'R' // ToString(st%GetPart())
    
    CALL Add( err_list,  &
        CODE=errUnterminatedBlock,  &
        COMPONENT=comp,  &
        MSG='A /' // GetPartName(st) // '/ was not terminated with an &
          &end statement.' )
    
  END SUBROUTINE progunitlike_Terminate
  
  
  !*****************************************************************************
  !!
  !> Implementation of ProgUnitLikeStNode%GetLocation - get the starting 
  !! source location of the node.
  !!
  !! @param[in]     st                The node of interest.
  !!
  !! @returns The source location of the start of the node.
  
  FUNCTION progunitlike_GetLocation(st) RESULT(loc)
    
    USE SourceLocations
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(ProgUnitLikeStNode), INTENT(IN) :: st
    
    ! Function result.
    TYPE(SourceLocation) :: loc
    
    !***************************************************************************
    
    ! We are robust to the first statement not being allocated, even though 
    ! the extension where this can occur overrides this procedure anyway.
    IF (ALLOCATED(st%first)) THEN
      loc = QueryLocation(st%first%tlist(1))
    ELSE IF (has_spec(st)) THEN
      loc = st%spec%GetLocation()
    ELSE
      loc = QueryLocation(st%last%tlist(1))
    END IF
    
  END FUNCTION progunitlike_GetLocation
  
  
  !*****************************************************************************
  !!
  !> Implementation of ProgUnitLikeStNode%GetLabel - get the label 
  !! of the node.
  !!
  !! @param[in]     st                The node of interest.
  !!
  !! @returns The statement label of the the node.
  
  FUNCTION progunitlike_GetLabel(st) RESULT(label)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(ProgUnitLikeStNode), INTENT(IN) :: st
    
    ! Function result.
    TYPE(LabelWrapper) :: label
    
    !***************************************************************************
    
    ! We are robust to the first statement not being allocated, even though 
    ! the extension where this can occur overrides this procedure anyway.
    IF (ALLOCATED(st%first)) THEN
      ! ifort bug - allocatable component in structure constructor.
      if (allocated(st%first%statement_label)) then
        label = LabelWrapper(st%first%statement_label)
      end if
    ELSE IF (has_spec(st)) THEN
      label = st%spec%GetLabel()
    ELSE
      ! ifort bug - allocatable component in structure constructor.
      if (allocated(st%last%statement_label)) then
        label = LabelWrapper(st%last%statement_label)
      end if
    END IF
    
  END FUNCTION progunitlike_GetLabel
  
  
  !*****************************************************************************
  !!
  !> Worker routine for types that extend ProgUnitStNode to 
  !! incorporate the current statement into the syntax tree.
  !!
  !! @param[in,out] st                The program unit statement tree node.
  !!
  !! @param[in,out] stmt              Statement data for the current 
  !! statement.
  !!
  !! @param[in]     ist_first         Statement number (ist*) of the 
  !! statement that starts the program unit.
  !!
  !! @param[in]     ist_last          Statement number (ist*) of the 
  !! statement that finishes the program unit.
  !!
  !! @param[out]    new_tip           Tip of the statement tree after the 
  !! current statement has been processed.
  !!
  !! @param[out]    err_list          List of errors.
  
  RECURSIVE SUBROUTINE DoBuildTree_progunitlike( st, stmt, ist_first, &
      ist_last, new_tip, err_list )
    
    USE CharUtils
    USE Errors
    USE ErrorCodes
    USE ErrorLevels
    USE Statements
    USE SyntaxParts
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(ProgUnitLikeStNode), INTENT(INOUT), TARGET :: st
    TYPE(StData), INTENT(INOUT) :: stmt
    INTEGER, INTENT(IN) :: ist_first
    INTEGER, INTENT(IN) :: ist_last
    CLASS(StNode), INTENT(OUT), POINTER :: new_tip
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    CHARACTER(:), ALLOCATABLE :: comp
    
    !***************************************************************************
    
    ALLOCATE(err_list(0))
    
    comp = 'R' // ToString(st%GetPart())
    
    IF (.NOT. ALLOCATED(st%first)) THEN
      IF (stmt%ist /= ist_first) THEN
        CALL Add( err_list,  &
            CODE=errInternal,  &
            LEVEL=errLevelFatal,  &
            COMPONENT=comp,  &
            LOCATION=QueryLocation(stmt%tlist(1)),  &
            MSG='First statement of a /' // GetPartName(st)  &
              // '/ isn''t a /' // GetStName(ist_first) // '/.' )
      END IF
      ALLOCATE(st%first)
      CALL Move(st%first, stmt)
      ! Jump straight into the specification section.
      ALLOCATE(st%spec)
      st%spec%parent => st
      new_tip => st%spec
    ELSE IF (stmt%ist == ist_last) THEN
      IF (ALLOCATED(st%last)) THEN
        CALL Add( err_list,  &
            CODE=errInternal, LEVEL=errLevelFatal,  &
            COMPONENT=comp,  &
            LOCATION=QueryLocation(stmt%tlist(1)),  &
            MSG='Last statement of a /' // GetPartName(st)  &
              // '/ was encountered twice.' )
      ELSE
        ALLOCATE(st%last)
      END IF
      CALL Move(st%last, stmt)
      ! Some housekeeping - if the spec part is empty, then 
      ! there is no point keeping it around.  ifort bug?
      !IF (st%spec%IsEmpty()) DEALLOCATE(st%spec)
      
      new_tip => st%parent
    ELSE 
      CALL Add( err_list,  &
          CODE=errInvalidContext,  &
          COMPONENT=comp,  &
          LOCATION=QueryLocation(stmt%tlist(1)),  &
          MSG='A /' // GetStName(stmt%ist) // '/ is not valid at this &
            &point inside a /' // GetPartName(st) // '/.' )
    END IF
    
  END SUBROUTINE DoBuildTree_progunitlike
  
  
  !*****************************************************************************
  !!
  !> Test whether the spec component is meaningful.
  !!
  !! @param[in]     st                The object with the specification part.
  !!
  !! @returns true if the component that represents the specification part 
  !! is allocated and not empty.
  
  FUNCTION has_spec(st)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(ProgUnitLikeStNode), INTENT(IN) :: st
    
    ! Function result
    LOGICAL :: has_spec
    
    !***************************************************************************
    
    has_spec = ALLOCATED(st%spec)
    IF (.NOT. has_spec) RETURN
    
    has_spec = .NOT. st%spec%IsEmpty()
    
  END FUNCTION has_spec
  
  
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! Procedures for the InternalSubprogramPartStNode
  
  
  !*****************************************************************************
  !!
  !> Implementation of InternalSubprogramPartStNode%BuildTree - incorporate 
  !! the current statement into the statement tree.
  !!
  !! @param[in,out] st                The statement tree node.
  !!
  !! @param[in,out] stmt              The current statement.
  !! 
  !! @param[out]    new_tip           The new tip of the statement tree 
  !! after the statement has been processed.
  !!
  !! @param[out]    err_list          List of errors.
  
  RECURSIVE SUBROUTINE isp_BuildTree(st, stmt, new_tip, err_list)
    
    USE Errors
    USE Statements
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(InternalSubprogramPartStNode), INTENT(INOUT), TARGET :: st
    TYPE(StData), INTENT(INOUT) :: stmt
    CLASS(StNode), INTENT(OUT), POINTER :: new_tip
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !***************************************************************************
    
    ! The third argument is false to indicate that separate module subprograms 
    ! are not valid in the subprogram part.  The fourth argument indicates 
    ! that child procedures may not themselves have internal procedures 
    ! (it is used to set the internal_ok component).
    CALL DoBuildTree(st, stmt, .FALSE., .FALSE., new_tip, err_list)
    
  END SUBROUTINE isp_BuildTree
  
  
  !*****************************************************************************
  !!
  !> Worker routine for types that extend InternalSubprogramPartStNode to 
  !! incorporate the current statement into the syntax tree.
  !!
  !! @param[in,out] st                The statement tree node.
  !!
  !! @param[in,out] stmt              The current statement.
  !!
  !! @param[in]     mp_subprogram_ok  Flag to indicate that the list of 
  !! subprograms can include separate-module-subprograms.
  !!
  !! @param[in]     internal_ok       Flag to indicate that child 
  !! subprograms of this part may themselves have an internal subprogram 
  !! part.
  !!
  !! @param[out]    new_tip           The new tip of the statement tree after 
  !! the statement has been processed.
  !!
  !! @param[out]    err_list          List of errors.
  
  RECURSIVE SUBROUTINE DoBuildTree_isp( st, stmt, mp_subprogram_ok,  &
      internal_ok, new_tip, err_list )
    
    USE CompilerKinds
    USE Errors
    USE ErrorCodes
    USE ErrorLevels
    USE Statements
    USE Tokens
    
    USE FunctionStmtsUtils
    USE SubroutineStmtsUtils
    USE MPSubprogramStmtsUtils
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(InternalSubprogramPartStNode), INTENT(INOUT), TARGET :: st
    TYPE(StData), INTENT(INOUT) :: stmt
    LOGICAL, INTENT(IN) :: mp_subprogram_ok
    LOGICAL, INTENT(IN) :: internal_ok
    CLASS(StNode), INTENT(OUT), POINTER :: new_tip
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !---------------------------------------------------------------------------
    ! Local variables.
    
    ! Name of the subprogram being added.
    CHARACTER(:,KIND=scck), ALLOCATABLE :: new_name
    
    ! Indicates an attempt to add a duplicate subprogram.
    LOGICAL :: duplicate
    
    ! Temporary node for building bits of the tree that we later discard.
    CLASS(ProgUnitStNode), ALLOCATABLE :: tmp_node
    
    ! Component for the unqiue name in scope error.
    CHARACTER(*), PARAMETER :: unique_identifier_comp = '16.3.1p3'
    
    !***************************************************************************
    
    ALLOCATE(err_list(0))
    
    IF (.NOT. ALLOCATED(st%subprograms)) ALLOCATE(st%subprograms(0))
    
    ! The statements that we are sensitive to are /contains-stmt/, 
    ! /function-stmt/, /subroutine-stmt/ and /mp-subprogram-stmt/.  The first
    ! statement encountered must be a /contains-stmt/, and it must only 
    ! be encountered once.  The other statements trigger building of a new 
    ! child subprogram node.  Unknown statements get passed back up to 
    ! the parent node - in the usual course of events the only unknown 
    ! statement will be the end statement that terminates whatever 
    ! subprogram or program unit that this internal subprogram part 
    ! belongs to.
    
    IF (.NOT. ALLOCATED(st%contains)) THEN
      
      IF (stmt%ist /= istContains) THEN
        STOP 'Internal error in ProgUnitStNodes%%DoBuildTree_isp, &
            &the first statement of a /module-subprogram-part/ or &
            &/internal-subprogram-part/ is not a /contains-stmt/.'
      END IF
      ALLOCATE(st%contains)
      CALL Move(st%contains, stmt)
      new_tip => st
      
    !---------------------------------------------------------------------------
    ELSE IF (stmt%ist == istFunction) THEN
      
      IF (st%ensure_unique) THEN
        CALL GetFunctionName(stmt%tlist, new_name)
        IF (ALLOCATED(new_name)) THEN
          CALL check_subprogram_name(st%subprograms, new_name, duplicate)
          IF (duplicate) THEN
            CALL Add( err_list,  &
                CODE=errSubprogramOrUnitAlreadyDefined,  &
                COMPONENT=unique_identifier_comp,  &
                LOCATION=QueryLocation(stmt%tlist(1)),  &
                MSG='The name of the function "' // new_name // '" has &
                  &already been used in a definition in the current scope &
                  &for a different subprogram.' )
            ALLOCATE(FunctionSubprogramStNode :: tmp_node)
            CALL dispatch(tmp_node)
            RETURN
          END IF
        END IF
      END IF
      
      CALL grow_subprograms
      ALLOCATE( FunctionSubprogramStNode ::  &
          st%subprograms(SIZE(st%subprograms))%item )
      CALL dispatch(st%subprograms(SIZE(st%subprograms))%item)
      
    !---------------------------------------------------------------------------
    ELSE IF (stmt%ist == istSubroutine) THEN
      
      IF (st%ensure_unique) THEN
        CALL GetSubroutineName(stmt%tlist, new_name)
        IF (ALLOCATED(new_name)) THEN
          CALL check_subprogram_name(st%subprograms, new_name, duplicate)
          IF (duplicate) THEN
            CALL Add( err_list,  &
                CODE=errSubprogramOrUnitAlreadyDefined,  &
                COMPONENT=unique_identifier_comp,  &
                LOCATION=QueryLocation(stmt%tlist(1)),  &
                MSG='The name of the subroutine "' // new_name // '" has &
                  &already been used in a definition in the current scope &
                  &for a different subprogram.' )
            ALLOCATE(SubroutineSubprogramStNode :: tmp_node)
            CALL dispatch(tmp_node)
            RETURN
          END IF
        END IF
      END IF
      
      CALL grow_subprograms
      ALLOCATE( SubroutineSubprogramStNode ::  &
          st%subprograms(SIZE(st%subprograms))%item )
      CALL dispatch(st%subprograms(SIZE(st%subprograms))%item)
      
    !---------------------------------------------------------------------------
    ELSE IF (stmt%ist == istMPSubprogram) THEN
      
      IF (mp_subprogram_ok) THEN
        IF (st%ensure_unique) THEN
          CALL GetProcedureName(stmt%tlist, new_name)
          IF (ALLOCATED(new_name)) THEN
            CALL check_subprogram_name(st%subprograms, new_name, duplicate)
            IF (duplicate) THEN
              CALL Add( err_list,  &
                  CODE=errSubprogramOrUnitAlreadyDefined,  &
                  COMPONENT=unique_identifier_comp,  &
                  LOCATION=QueryLocation(stmt%tlist(1)),  &
                  MSG='The name of the separate module procedure "' &
                    // new_name // '" has &
                    &already been used in a definition in the current scope &
                    &for a different subprogram.' )
              ALLOCATE(SeparateModuleSubprogramStNode :: tmp_node)
              CALL dispatch(tmp_node)
              RETURN
            END IF
          END IF
        END IF
        
        CALL grow_subprograms
        ALLOCATE( SeparateModuleSubprogramStNode :: &
            st%subprograms(SIZE(st%subprograms))%item )
        CALL dispatch(st%subprograms(SIZE(st%subprograms))%item)
      ELSE
        ! Do we want this to be an error in the build tree stage?  An 
        ! alternative possibility is to handle this later.
        CALL Add( err_list,  &
            CODE=errInvalidContext,  &
            COMPONENT='R210',  &
            LOCATION=QueryLocation(stmt%tlist(1)),  &
            MSG='A /separate-module-subprogram/ is not valid in an &
              &/internal-subprogram-part/.'  )
      END IF
      
    !---------------------------------------------------------------------------
    ELSE
      
      ! I don't know what it is, so ask mum.  Before we do, if we haven't 
      ! seen any subprograms then allocate the array to zero size.
      IF (.NOT. ALLOCATED(st%subprograms)) ALLOCATE(st%subprograms(0))
      CALL st%parent%BuildTree(stmt, new_tip, err_list)
      
    END IF
    
  CONTAINS
    
    ! Worker routine to grow the list of subprograms.
    SUBROUTINE grow_subprograms
      !-------------------------------------------------------------------------
      TYPE(ProgUnitVector), ALLOCATABLE :: tmp(:)
      INTEGER :: i
      !*************************************************************************
      IF (.NOT. ALLOCATED(st%subprograms)) THEN
        ALLOCATE(st%subprograms(1))
      ELSE
        ALLOCATE(tmp(SIZE(st%subprograms) + 1))
        DO i = 1, SIZE(st%subprograms)
          CALL MOVE_ALLOC(st%subprograms(i)%item, tmp(i)%item)
        END DO
        CALL MOVE_ALLOC(tmp, st%subprograms)
      END IF
    END SUBROUTINE grow_subprograms
    
    ! Worker routine to dispatch a child subprogram building activity, 
    ! once the nature of the child is known.
    SUBROUTINE dispatch(item)
      CLASS(ProgUnitStNode), INTENT(INOUT), TARGET :: item
      !-------------------------------------------------------------------------
      ! Error list for the child build call.
      TYPE(Error), ALLOCATABLE :: sub_err_list(:)
      !*************************************************************************
      item%parent => st
      item%internal_ok = internal_ok
      item%ensure_unique = st%ensure_unique
      CALL item%BuildTree(stmt, new_tip, sub_err_list)
      CALL Add(err_list, sub_err_list)
    END SUBROUTINE dispatch
    
  END SUBROUTINE DoBuildTree_isp
  
  
  !*****************************************************************************
  !!
  !> Implementation of InternalSubprogramPartStNode%GetPart - get the 
  !! syntax part of the current statement tree node.
  !!
  !! @returns The part stack associated with the node.
  
  FUNCTION isp_GetPart(st) RESULT(ipt)
    
    USE SyntaxParts
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(InternalSubprogramPartStNode), INTENT(IN) :: st
    
    ! Function result
    INTEGER :: ipt
    
    !***************************************************************************
    
    ipt = iptInternalSubprogramPart
    
  END FUNCTION isp_GetPart
  
  
  !*****************************************************************************
  !!
  !> Implementation of InternalSubprogramPartStNode%DumpTree - write 
  !! debugging information about the node and its children.
  !!
  !! @param[in]     st                The statement tree node.
  !!
  !! @param[in]     unit              The unit to dump to.
  !!
  !! @param[in]     pfx               Prefix to write at the start 
  !! of each record (for indentation).
  
  SUBROUTINE isp_DumpTree(st, unit, pfx)
    
    USE SyntaxParts
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(InternalSubprogramPartStNode), INTENT(IN), TARGET :: st
    INTEGER, INTENT(IN) :: unit
    CHARACTER(*), INTENT(IN) :: pfx
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: i              ! Internal subprogram index.
    
    !***************************************************************************
    
    WRITE (unit, fmt_partname) pfx, GetPartName(st)
    
    IF (ALLOCATED(st%contains))  &
        WRITE (unit, fmt_stmtname) pfx // '  ', GetStName(st%contains)
    
    IF (ALLOCATED(st%subprograms)) THEN
      DO i = 1, SIZE(st%subprograms)
        CALL st%subprograms(i)%item%DumpTree(unit, pfx // '  ')
      END DO
    END IF
    
  END SUBROUTINE isp_DumpTree
  
  
  !*****************************************************************************
  !!
  !> Implementation of InternalSubprogramPartStNode%Query - apply visitor 
  !! object operations to this node and its children.
  !!
  !! @param[in]     st                The statement tree node being traversed.
  !!
  !! @param[in,out] visitor           The object to execute the query action.
  
  ! Recursive because a module subprogram part (which uses this type) can 
  ! have a procedure with internal procedures.
  RECURSIVE SUBROUTINE isp_Query(st, visitor)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(InternalSubprogramPartStNode), INTENT(IN), TARGET :: st
    CLASS(StNodeQueryVisitor), INTENT(INOUT) :: visitor
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: i              ! Internal procedure index.
    
    !***************************************************************************
    
    ! Execute against ourselves.
    CALL visitor%ExecuteNode(st)
    
    ! Contains statement.
    CALL visitor%ExecuteStmt(st, st%contains)
    
    ! Forward to internal subprograms.
    DO i = 1, SIZE(st%subprograms)
      CALL st%subprograms(i)%item%Query(visitor)
    END DO
    
  END SUBROUTINE isp_Query
  
  
  !*****************************************************************************
  !!
  !> Implementation of InternalSubprogramPartStNode%Modify - apply 
  !! modify visitor object operations to this node and its children.
  !!
  !! @param[in,out] st                The statement tree node being traversed.
  !!
  !! @param[in,out] visitor           The object to execute the modify action.
  
  ! Recursive because a module subprogram part (which uses this type) can 
  ! have a procedure with internal procedures.
  RECURSIVE SUBROUTINE isp_Modify(st, visitor)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(InternalSubprogramPartStNode), INTENT(INOUT), TARGET :: st
    CLASS(StNodeModifyVisitor), INTENT(INOUT) :: visitor
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: i              ! Internal procedure index.
    
    !***************************************************************************
    
    ! Execute against ourselves.
    CALL visitor%ExecuteNode(st)
    
    ! Contains statement.
    CALL visitor%ExecuteStmt(st, st%contains)
    
    ! Forward to internal subprograms.
    DO i = 1, SIZE(st%subprograms)
      CALL st%subprograms(i)%item%Modify(visitor)
    END DO
    
  END SUBROUTINE isp_Modify
  
  
  !*****************************************************************************
  !!
  !> Implementation of InternalSubprogramPartStNode%GetLocation - get the 
  !! starting source location of the node.
  !!
  !! @param[in]     st                The node of interest.
  !!
  !! @returns The source location of the start of the node.
  
  FUNCTION isp_GetLocation(st) RESULT(loc)
    
    USE SourceLocations
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(InternalSubprogramPartStNode), INTENT(IN) :: st
    
    ! Function result.
    TYPE(SourceLocation) :: loc
    
    !***************************************************************************
    
    loc = QueryLocation(st%contains%tlist(1))
    
  END FUNCTION isp_GetLocation
  
  
  !*****************************************************************************
  !!
  !> Implementation of InternalSubprogramPartStNode%GetLabel - get the label 
  !! of the node.
  !!
  !! @param[in]     st                The node of interest.
  !!
  !! @returns The statement label of the the node.
  
  FUNCTION isp_GetLabel(st) RESULT(label)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(InternalSubprogramPartStNode), INTENT(IN) :: st
    
    ! Function result.
    TYPE(LabelWrapper) :: label
    
    !***************************************************************************
    
    ! ifort bug - allocatable component in structure constructor.
    if (allocated(st%contains%statement_label)) then
      label = LabelWrapper(st%contains%statement_label)
    end if
    
  END FUNCTION isp_GetLabel
  
  
  !*****************************************************************************
  !!
  !> Worker routine to check that we don't have duplicate internal 
  !! subprogram names when a new subprogram is about to be added.
  !!
  !! @param[in]     subprograms       The existing list of subprograms.
  !!
  !! @param[in]     new_name          The name of the subprogram that is 
  !! about to be added.
  !!
  !! @param[out]    duplicate         Flag to indicate that the new name 
  !! will duplicate an existing name.
  
  SUBROUTINE check_subprogram_name(subprograms, new_name, duplicate)
    
    !-------------------------------------------------------------------------
    ! Arguments
    
    TYPE(ProgUnitVector), INTENT(IN) :: subprograms(:)
    CHARACTER(*), INTENT(IN) :: new_name
    LOGICAL, INTENT(OUT) :: duplicate
    
    !-------------------------------------------------------------------------
    ! Local variables
    
    ! Name of an existing subprogram being compared against.
    CHARACTER(:), ALLOCATABLE :: other_name
    
    INTEGER :: i            ! Internal subprogram index.
    
    !*************************************************************************
    
    duplicate = .FALSE.
    DO i = 1, SIZE(subprograms)
      CALL subprograms(i)%item%GetName(other_name)
      ! We are robust to the first statement of an existing subprogram 
      ! being malformed.
      IF (ALLOCATED(other_name)) THEN
        duplicate = new_name == other_name
        IF (duplicate) RETURN
      END IF
    END DO
    
  END SUBROUTINE check_subprogram_name
  
  
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! Procedures for the ExecUnitStNode
  
  
  !*****************************************************************************
  !!
  !> Implementation of ExecUnitStNode%DumpTree - write 
  !! debugging information about the node and its children.
  !!
  !! @param[in]     st                The statement tree node.
  !!
  !! @param[in]     unit              The unit to dump to.
  !!
  !! @param[in]     pfx               Prefix to write at the start 
  !! of each record (for indentation).
  
  SUBROUTINE execunit_DumpTree(st, unit, pfx)
    
    USE SyntaxParts
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(ExecUnitStNode), INTENT(IN), TARGET :: st
    INTEGER, INTENT(IN) :: unit
    CHARACTER(*), INTENT(IN) :: pfx
    
    !***************************************************************************
    
    WRITE (unit, fmt_partname) pfx, GetPartName(st)
    
    IF (ALLOCATED(st%first))  &
        WRITE (unit, fmt_stmtname) pfx // '  ', GetStName(st%first)
    IF (ALLOCATED(st%spec)) CALL st%spec%DumpTree(unit, pfx // '  ')
    IF (ALLOCATED(st%exec)) CALL st%exec%DumpTree(unit, pfx // '  '  )
    IF (ALLOCATED(st%internal)) CALL st%internal%DumpTree(unit ,pfx // '  ')
    IF (ALLOCATED(st%last))  &
        WRITE (unit, fmt_stmtname) pfx // '  ', GetStName(st%last)
    
  END SUBROUTINE execunit_DumpTree
  
  
  !*****************************************************************************
  !!
  !> Implementation of ExecUnitStNode%Query - apply visitor object operations 
  !! to this node and its children.
  !!
  !! @param[in]     st                The statement tree node being traversed.
  !!
  !! @param[in,out] visitor           The object to execute the query action.
  
  ! Recursive because of internal procedures.
  RECURSIVE SUBROUTINE execunit_Query(st, visitor)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(ExecUnitStNode), INTENT(IN), TARGET :: st
    CLASS(StNodeQueryVisitor), INTENT(INOUT) :: visitor
    
    !***************************************************************************
    
    ! Execute against ourselves.
    CALL visitor%ExecuteNode(st)
    
    ! Opening statement, if present.
    IF (ALLOCATED(st%first)) CALL visitor%ExecuteStmt(st, st%first)
    
    ! Forward to specificaction section, if present.
    IF (ALLOCATED(st%spec)) CALL st%spec%Query(visitor)
    
    ! Forward to executable part, if present.
    IF (ALLOCATED(st%exec)) CALL st%exec%Query(visitor)
    
    ! Forward to internal subprogram part, if present.
    IF (ALLOCATED(st%internal)) CALL st%internal%Query(visitor)
    
    ! Final statement.
    CALL visitor%ExecuteStmt(st, st%last)
    
  END SUBROUTINE execunit_Query
  
  
  !*****************************************************************************
  !!
  !> Implementation of ExecUnitStNode%Modify - apply modify visitor object 
  !! operations to this node and its children.
  !!
  !! @param[in,out] st                The statement tree node being traversed.
  !!
  !! @param[in,out] visitor           The object to execute the modify action.
  
  ! Recursive because of internal procedures.
  RECURSIVE SUBROUTINE execunit_Modify(st, visitor)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(ExecUnitStNode), INTENT(INOUT), TARGET :: st
    CLASS(StNodeModifyVisitor), INTENT(INOUT) :: visitor
    
    !***************************************************************************
    
    ! Execute against ourselves.
    CALL visitor%ExecuteNode(st)
    
    ! Opening statement, if present.
    IF (ALLOCATED(st%first)) CALL visitor%ExecuteStmt(st, st%first)
    
    ! Forward to specificaction section, if present.
    IF (ALLOCATED(st%spec)) CALL st%spec%Modify(visitor)
    
    ! Forward to executable part, if present.
    IF (ALLOCATED(st%exec)) CALL st%exec%Modify(visitor)
    
    ! Forward to internal subprogram part, if present.
    IF (ALLOCATED(st%internal)) CALL st%internal%Modify(visitor)
    
    ! Final statement.
    CALL visitor%ExecuteStmt(st, st%last)
    
  END SUBROUTINE execunit_Modify
  
  
  !*****************************************************************************
  !!
  !> Implementation of ExecUnitStNode%BuildTree - incorporate the current 
  !! statement into the statement tree.
  !!
  !! @param[in,out] st                The statement tree node.
  !!
  !! @param[in,out] stmt              The current statement.
  !!
  !! @param[in]     ist_first         Statement number (ist*) of the 
  !! statement that starts the program unit.
  !!
  !! @param[in]     first_optional    Flag that indicates that the 
  !! first statement is optional - for example the program statement 
  !! is optional at the start of a main program.
  !!
  !! @param[in]     ist_last          Statement number (ist*) of the 
  !! statement that finishes the program unit.
  !!
  !! @param[out]    new_tip           The new tip of the statement tree after 
  !! the statement has been processed.  
  !!
  !! @param[out]    err_list          List of errors.
  
  RECURSIVE SUBROUTINE DoBuildTree_execunit( st, stmt, ist_first,  &
      first_optional, ist_last, new_tip, err_list )
    
    USE CharUtils
    USE Errors
    USE ErrorCodes
    USE ErrorLevels
    USE Statements
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(ExecUnitStNode), INTENT(INOUT), TARGET :: st
    TYPE(StData), INTENT(INOUT) :: stmt
    INTEGER, INTENT(IN) :: ist_first
    LOGICAL, INTENT(IN) :: first_optional
    INTEGER, INTENT(IN) :: ist_last
    CLASS(StNode), INTENT(OUT), POINTER :: new_tip
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Component for error reporting.
    CHARACTER(:), ALLOCATABLE :: comp
    
    !***************************************************************************
    
    ALLOCATE(err_list(0))
    
    comp = 'R' // ToString(st%GetPart())
    
    ! If we haven't seen the first statement, or a statement from any other 
    ! part (implicit in the test for %spec).
    IF ( .NOT. ALLOCATED(st%first)  &
        .AND. (.NOT. ALLOCATED(st%spec)) ) THEN
      IF (first_optional) THEN
        IF (stmt%ist == ist_first) THEN
          ALLOCATE(st%first)
          CALL Move(st%first, stmt)
        END IF
      ELSE
        ALLOCATE(st%first)
        IF (stmt%ist /= ist_first) THEN
          CALL Add( err_list,  &
              CODE=errInternal,  &
              LEVEL=errLevelFatal,  &
              COMPONENT=comp,  &
              LOCATION=QueryLocation(stmt%tlist(1)),  &
              MSG='First statement of a /' // GetPartName(st)  &
                // '/ isn''t a /' // GetStName(ist_first) // '/.' )
        END IF
        CALL Move(st%first, stmt)
      END IF
      ! Jump straight into the specification section.
      ALLOCATE(st%spec)
      st%spec%parent => st
      IF (.NOT. ALLOCATED(st%first)) THEN
        ! The first statement wasn't ist_first and the ist_first statement 
        ! was "optional".  Pass the "first" statement onto the specification 
        ! part.  If that part doesn't know what to do with it, then it 
        ! will reset the tip back to us.
        CALL st%spec%BuildTree(stmt, new_tip, err_list)
      ELSE
        ! The tip is the specification part but we don't have a statement 
        ! to give it yet.  Direct the next statement that way.  If the 
        ! specification part doesn't know what to do with the next statement, 
        ! then it will reset the tip back to us.
        new_tip => st%spec
      END IF
      ! When we next see the tip st%spec will be allocated - so we know that 
      ! the specification part is complete.
    ELSE IF (.NOT. ALLOCATED(st%exec)) THEN
      ! Switching from spec to exec.
      ALLOCATE(st%exec)
      st%exec%parent => st
      CALL st%exec%BuildTree(stmt, new_tip, err_list)
      ! When the execution part encouters a statement that it cannot handle 
      ! it will reset the tip back to us.  Because st%exec is allocated we 
      ! know that we are past the execution part.
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
      
      ! Some housekeeping - if the exec and spec parts are empty, then 
      ! there is no point keeping them around.  ifort bug?
      !IF (st%spec%IsEmpty()) DEALLOCATE(st%spec)
      !IF (st%exec%IsEmpty()) DEALLOCATE(st%exec)
      
      new_tip => st%parent
    ELSE IF (stmt%ist == istContains) THEN
      IF (ALLOCATED(st%internal)) THEN
        CALL Add( err_list,  &
            CODE=errInvalidContext,  &
            COMPONENT=comp,  &
            LOCATION=QueryLocation(stmt%tlist(1)),  &
            MSG='A /contains-stmt/ has already been seen in &
              &this program unit.' )
        new_tip => st
      ELSE
        ALLOCATE(st%internal)
        st%internal%parent => st
        ! Inherit the unique match characteristics of the parent node.
        st%internal%ensure_unique = st%ensure_unique
        CALL st%internal%BuildTree(stmt, new_tip, err_list)
      END IF
    ELSE
      CALL Add( err_list,  &
          CODE=errInvalidContext,  &
          COMPONENT=comp,  &
          LOCATION=QueryLocation(stmt%tlist(1)),  &
          MSG='A /' // GetStName(stmt%ist) // '/ is not valid at this &
            &point inside a /' // GetPartName(st) // '/.' )
    END IF
    
  END SUBROUTINE DoBuildTree_execunit
  
  
  !*****************************************************************************
  !!
  !> Test whether the exec component is meaningful.
  !!
  !! @param[in]     st                The object with the execution part.
  !!
  !! @returns true if the component that represents the execution part 
  !! is allocated and not empty.
  
  FUNCTION has_exec(st)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(ExecUnitStNode), INTENT(IN) :: st
    
    ! Function result
    LOGICAL :: has_exec
    
    !***************************************************************************
    
    has_exec = ALLOCATED(st%exec)
    IF (.NOT. has_exec) RETURN
    
    has_exec = .NOT. st%exec%IsEmpty()
    
  END FUNCTION has_exec
  
  
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! Procedures for the MainProgramStNode
  
  
  !*****************************************************************************
  !!
  !> Implementation of MainProgramStNode%BuildTree - incorporate the current 
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
  
  RECURSIVE SUBROUTINE mainprogram_BuildTree(st, stmt, new_tip, err_list)
    
    USE Errors
    USE Statements
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(MainProgramStNode), INTENT(INOUT), TARGET :: st
    TYPE(StData), INTENT(INOUT) :: stmt
    CLASS(StNode), INTENT(OUT), POINTER :: new_tip
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !***************************************************************************
    
    CALL DoBuildTree(  &
        st,  &
        stmt,  &
        istProgram,  &
        .TRUE.,  &
        istEndProgram,  &
        new_tip,  &
        err_list )
    
  END SUBROUTINE mainprogram_BuildTree
  
  
  !*****************************************************************************
  !!
  !> Implementation of MainProgramStNode%GetPart - get the 
  !! syntax part of the current statement tree node.
  !!
  !! @returns The part stack associated with the node.
  
  FUNCTION mainprogram_GetPart(st) RESULT(pt)
    
    USE SyntaxParts
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(MainProgramStNode), INTENT(IN) :: st
    
    ! Function result
    INTEGER :: pt
    
    !***************************************************************************
    
    pt = iptMainProgram
    
  END FUNCTION mainprogram_GetPart
  
  
  !*****************************************************************************
  !!
  !> Implementation of MainProgramStNode::GetName - get the 
  !! name of the main program.
  !!
  !! @param[in]     st                The statement tree node.
  !!
  !! @param[out]    name              The name of the main program, 
  !! zero length if there is no name or not allocated on error.
  !!
  !! Zero length is possible as the opening /program-stmt/ is optional 
  !! for a main program.
  
  SUBROUTINE mainprogram_GetName(st, name)
    
    USE ProgramStmtsUtils
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(MainProgramStNode), INTENT(IN) :: st
    CHARACTER(:), INTENT(OUT), ALLOCATABLE :: name
    
    !***************************************************************************
    
    IF (.NOT. ALLOCATED(st%first)) THEN
      name = ''
      RETURN
    END IF
    
    CALL GetProgramName(st%first%tlist, name)
    
  END SUBROUTINE mainprogram_GetName
  
  
  !*****************************************************************************
  !!
  !> Implementation of MainProgramStNode%GetLocation - get the starting 
  !! source location of the node.
  !!
  !! @param[in]     st                The node of interest.
  !!
  !! @returns The source location of the start of the node.
  
  FUNCTION mainprogram_GetLocation(st) RESULT(loc)
    
    USE SourceLocations
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(MainProgramStNode), INTENT(IN) :: st
    
    ! Function result.
    TYPE(SourceLocation) :: loc
    
    !***************************************************************************
    
    IF (ALLOCATED(st%first)) THEN
      loc = QueryLocation(st%first%tlist(1))
    ELSE IF (has_spec(st)) THEN
      loc = st%spec%GetLocation()
    ELSE IF (has_exec(st)) THEN
      loc = st%exec%GetLocation()
    ELSE IF (ALLOCATED(st%internal)) THEN
      loc = st%internal%GetLocation()
    ELSE
      loc = QueryLocation(st%last%tlist(1))
    END IF
    
  END FUNCTION mainprogram_GetLocation
  
  
  !*****************************************************************************
  !!
  !> Implementation of MainProgramStNode%GetLabel - get the label 
  !! of the node.
  !!
  !! @param[in]     st                The node of interest.
  !!
  !! @returns The statement label of the the node.
  
  FUNCTION mainprogram_GetLabel(st) RESULT(label)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(MainProgramStNode), INTENT(IN) :: st
    
    ! Function result.
    TYPE(LabelWrapper) :: label
    
    !***************************************************************************
    
    IF (ALLOCATED(st%first)) THEN
      ! ifort bug - allocatable component in structure constructor.
      if (allocated(st%first%statement_label)) then
        label = LabelWrapper(st%first%statement_label)
      end if
    ELSE IF (has_spec(st)) THEN
      label = st%spec%GetLabel()
    ELSE IF (has_exec(st)) THEN
      label = st%exec%GetLabel()
    ELSE IF (ALLOCATED(st%internal)) THEN
      label = st%internal%GetLabel()
    ELSE
      ! ifort bug - allocatable component in structure constructor.
      if (allocated(st%last%statement_label)) then
        label = LabelWrapper(st%last%statement_label)
      end if
    END IF
    
  END FUNCTION mainprogram_GetLabel
  
  
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! Procedures for the SubroutineStNode type.
  
  
  !*****************************************************************************
  !!
  !> Implementation of SubroutineSubprogramStNode%BuildTree - incorporate the 
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
  
  RECURSIVE SUBROUTINE subroutine_BuildTree(st, stmt, new_tip, err_list)
    
    USE Errors
    USE Statements
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(SubroutineSubprogramStNode), INTENT(INOUT), TARGET :: st
    TYPE(StData), INTENT(INOUT) :: stmt
    CLASS(StNode), INTENT(OUT), POINTER :: new_tip
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !***************************************************************************
    
    CALL DoBuildTree(  &
        st,  &
        stmt,  &
        istSubroutine,  &
        .FALSE.,  &
        istEndSubroutine,  &
        new_tip, err_list )
    
  END SUBROUTINE subroutine_BuildTree
  
  
  !*****************************************************************************
  !!
  !> Implementation of SubroutineSubprogramStNode%GetPart - get the 
  !! syntax part of the current statement tree node.
  !!
  !! @returns The part stack associated with the node.
  
  FUNCTION subroutine_GetPart(st) RESULT(pt)
    
    USE SyntaxParts
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(SubroutineSubprogramStNode), INTENT(IN) :: st
    
    ! Function result
    INTEGER :: pt
    
    !***************************************************************************
    
    pt = iptSubroutineSubprogram
    
  END FUNCTION subroutine_GetPart
  
  
  !*****************************************************************************
  !!
  !> Implementation of SubroutineSubprogramStNode::GetName - get the 
  !! name of the separate module subprogram.
  !!
  !! @param[in]     st                The statement tree node.
  !!
  !! @param[out]    name              The name of the subroutine, 
  !! or not allocated on error.
  
  SUBROUTINE subroutine_GetName(st, name)
    
    USE SubroutineStmtsUtils
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(SubroutineSubprogramStNode), INTENT(IN) :: st
    CHARACTER(:), INTENT(OUT), ALLOCATABLE :: name
    
    !***************************************************************************
    
    CALL GetSubroutineName(st%first%tlist, name)
    
  END SUBROUTINE subroutine_GetName
  
  
  !****************************************************************************
  !!
  !> Implementation of SubroutineSubprogramStNode%Check - check that the 
  !! node complies with the rules of the language.
  !!
  !! @param[in]     st                The statement tree node.
  !!
  !! @param[out]    err_list          List of errors
  !!
  !! The default implementation does nothing.
  
  SUBROUTINE subroutine_Check(st, err_list)
    
    USE CharUtils
    USE Errors
    USE ErrorCodes
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(SubroutineSubprogramStNode), INTENT(IN) :: st
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !***************************************************************************
    
    ALLOCATE(err_list(0))
    
    IF (ALLOCATED(st%internal) .AND. (.NOT. st%internal_ok)) THEN
      CALL Add( err_list,  &
          CODE=errInternalSubprogramHadInternalSubprogram,  &
          COMPONENT='C1260 on R' // ToString(st%GetPart()),  &
          LOCATION=st%internal%GetLocation(),  &
          MSG='An internal subroutine subprogram is not &
            &permitted to have an internal subprogram part.' )
    END IF
    
  END SUBROUTINE subroutine_Check
  
  
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! Procedures for the FunctionStNode type.
  
  
  !*****************************************************************************
  !!
  !> Implementation of FunctionSubprogramStNode%BuildTree - incorporate the 
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
  
  RECURSIVE SUBROUTINE function_BuildTree(st, stmt, new_tip, err_list)
    
    USE Errors
    USE Statements
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(FunctionSubprogramStNode), INTENT(INOUT), TARGET :: st
    TYPE(StData), INTENT(INOUT) :: stmt
    CLASS(StNode), INTENT(OUT), POINTER :: new_tip
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !***************************************************************************
    
    CALL DoBuildTree(  &
        st,  &
        stmt,  &
        istFunction,  &
        .FALSE.,  &
        istEndFunction,  &
        new_tip, err_list )
    
  END SUBROUTINE function_BuildTree
  
  
  !*****************************************************************************
  !!
  !> Implementation of FunctionSubprogramStNode%GetPart - get the 
  !! syntax part of the current statement tree node.
  !!
  !! @returns The part stack associated with the node.
  
  FUNCTION function_GetPart(st) RESULT(pt)
    
    USE SyntaxParts
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(FunctionSubprogramStNode), INTENT(IN) :: st
    
    ! Function result
    INTEGER :: pt
    
    !***************************************************************************
    
    pt = iptFunctionSubprogram
    
  END FUNCTION function_GetPart
  
  
  !*****************************************************************************
  !!
  !> Implementation of FunctionSubprogramStNode::GetName - get the 
  !! name of the separate module subprogram.
  !!
  !! @param[in]     st                The statement tree node.
  !!
  !! @param[out]    name              The name of the function, 
  !! or not allocated on error.
  
  SUBROUTINE function_GetName(st, name)
    
    USE CompilerKinds
    USE FunctionStmtsUtils
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(FunctionSubprogramStNode), INTENT(IN) :: st
    CHARACTER(:), INTENT(OUT), ALLOCATABLE :: name
    
    !***************************************************************************
    
    CALL GetFunctionName(st%first%tlist, name)
    
  END SUBROUTINE function_GetName
  
  
  !****************************************************************************
  !!
  !> Implementation of FunctionSubprogramStNode%Check - check that the 
  !! node complies with the rules of the language.
  !!
  !! @param[in]     st                The statement tree node.
  !!
  !! @param[out]    err_list          List of errors
  !!
  !! The default implementation does nothing.
  
  SUBROUTINE function_Check(st, err_list)
    
    USE CharUtils
    USE Errors
    USE ErrorCodes
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(FunctionSubprogramStNode), INTENT(IN) :: st
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !***************************************************************************
    
    ALLOCATE(err_list(0))
    
    IF (ALLOCATED(st%internal) .AND. (.NOT. st%internal_ok)) THEN
      CALL Add( err_list,  &
          CODE=errInternalSubprogramHadInternalSubprogram,  &
          COMPONENT='C1257 on R' // ToString(st%GetPart()),  &
          LOCATION=st%internal%GetLocation(),  &
          MSG='An internal function subprogram is not &
            &permitted to have an internal subprogram part.' )
    END IF
    
  END SUBROUTINE function_Check
  
  
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! Procedures for the the SeparateModuleSubprogramStNode type.
  
  
  !*****************************************************************************
  !!
  !> Implementation of SeparateModuleSubprogramStNode%BuildTree - incorporate 
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
  
  RECURSIVE SUBROUTINE sms_BuildTree(st, stmt, new_tip, err_list)
    
    USE Errors
    USE Statements
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(SeparateModuleSubprogramStNode), INTENT(INOUT), TARGET :: st
    TYPE(StData), INTENT(INOUT) :: stmt
    CLASS(StNode), INTENT(OUT), POINTER :: new_tip
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !***************************************************************************
    
    CALL DoBuildTree(  &
        st,  &
        stmt,  &
        istMPSubprogram,  &
        .FALSE.,  &
        istEndMPSubprogram,  &
        new_tip,  &
        err_list )
    
  END SUBROUTINE sms_BuildTree
  
  
  !*****************************************************************************
  !!
  !> Implementation of SeparateModuleSubprogramStNode%GetPart - get the 
  !! syntax part of the current statement tree node.
  !!
  !! @returns The part stack associated with the node.
  
  FUNCTION sms_GetPart(st) RESULT(pt)
    
    USE SyntaxParts
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(SeparateModuleSubprogramStNode), INTENT(IN) :: st
    
    ! Function result
    INTEGER :: pt
    
    !***************************************************************************
    
    pt = iptSeparateModuleSubprogram
    
  END FUNCTION sms_GetPart
  
  
  !*****************************************************************************
  !!
  !> Implementation of SeparateModuleSubprogramStNode::GetName - get the 
  !! name of the separate module subprogram.
  !!
  !! @param[in]     st                The statement tree node.
  !!
  !! @param[out]    name              The name of the separate module 
  !! subprogram, or not allocated on error.
  
  SUBROUTINE sms_GetName(st, name)
    
    USE MPSubprogramStmtsUtils
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(SeparateModuleSubprogramStNode), INTENT(IN) :: st
    CHARACTER(:), INTENT(OUT), ALLOCATABLE :: name
    
    !***************************************************************************
    
    CALL GetProcedureName(st%first%tlist, name)
    
  END SUBROUTINE sms_GetName
  
  
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! Helper procedures related to ProgUnitVector.
  
  
  !*****************************************************************************
  !!
  !> Implementation of ProgUnitVector%Query - forward the Query call to the 
  !! item component.
  !!
  !! @param[in]     st                The program unit tree node vector 
  !! being traversed.
  !!
  !! @param[in,out] visitor           The object carrying out the query action.
  
  SUBROUTINE puv_Query(st, visitor)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(ProgUnitVector), INTENT(IN) :: st
    CLASS(StNodeQueryVisitor), INTENT(INOUT) :: visitor
    
    !***************************************************************************
    
    CALL st%item%Query(visitor)
    
  END SUBROUTINE puv_Query
  
  
  !*****************************************************************************
  !!
  !> Implementation of ProgUnitVector%Modify - forward the Modify call to the 
  !! item component.
  !!
  !! @param[in,out] st                The program unit tree node vector 
  !! being traversed.
  !!
  !! @param[in,out] visitor           The object carrying out the modify 
  !! action.
  
  SUBROUTINE puv_Modify(st, visitor)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(ProgUnitVector), INTENT(INOUT) :: st
    CLASS(StNodeModifyVisitor), INTENT(INOUT) :: visitor
    
    !***************************************************************************
    
    CALL st%item%Modify(visitor)
    
  END SUBROUTINE puv_Modify
  
  
  !*****************************************************************************
  !!
  !> Add a ProgUnitStNode to a ProgUnitVector.
  !!
  !! @param[in,out] list              The list (vector) of StNodeList.
  !!
  !! Client code needs to allocate and define the added element.
  
  SUBROUTINE AddNode_(list)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(ProgUnitVector), INTENT(INOUT), ALLOCATABLE :: list(:)
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Temporary for growing list of nodes.  The target attribute is requied 
    ! so that pointers maintain their validity over the move.
    TYPE(ProgUnitVector), ALLOCATABLE, TARGET :: tmp(:)
    INTEGER :: sz
    INTEGER :: i
    
    !***************************************************************************
    
    IF (ALLOCATED(list)) THEN
      sz = SIZE(list)
      ALLOCATE(tmp(sz+1))
      DO i = 1, sz
        ! The move alloc means that pointers to list(i)%item continue to point 
        ! to tmp(i)%item.
        CALL MOVE_ALLOC(list(i)%item, tmp(i)%item)
      END DO
    ELSE
      sz = 0
      ALLOCATE(tmp(1))
    END IF
    
    CALL MOVE_ALLOC(tmp, list)
    
  END SUBROUTINE AddNode_
  
  
  !*****************************************************************************
  !!
  !> Find a main program in an array of program units.
  !!
  !! @param[in]     prog_units        The array of program units.
  !!
  !! @param[out]    idx               The index of the first node that 
  !! represents a /main-program/ in @a prog_units.
  
  SUBROUTINE FindMainProgram(prog_units, idx)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(ProgUnitVector), INTENT(IN) :: prog_units(:)
    INTEGER, INTENT(OUT) :: idx
    
    !***************************************************************************
    
    DO idx = 1, SIZE(prog_units)
      SELECT TYPE (item => prog_units(idx)%item)
      TYPE IS (MainProgramStNode) ; RETURN
      END SELECT
    END DO
    
    idx = 0                   ! Not found.
    
  END SUBROUTINE FindMainProgram
  
  
  !*****************************************************************************
  !!
  !> Find a main program in an array of program units.
  !!
  !! @param[in]     prog_units        The array of program units.
  !!
  !! @param[in]     name              The name of the item to search for.
  !!
  !! @param[out]    idx               The index of the first node that 
  !! represents a /main-program/ in @a prog_units.
  
  SUBROUTINE FindNamedItem(prog_units, name, idx)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(ProgUnitVector), INTENT(IN) :: prog_units(:)
    CHARACTER(*), INTENT(IN) :: name
    INTEGER, INTENT(OUT) :: idx
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Name of the item already in the list being compared.
    CHARACTER(:), ALLOCATABLE :: other_name
    
    !***************************************************************************
    
    DO idx = 1, SIZE(prog_units)
      CALL prog_units(idx)%item%GetName(other_name)
      IF (ALLOCATED(other_name)) THEN
        IF (name == other_name) RETURN
      END IF
    END DO
    
    idx = 0                   ! Not found.
    
  END SUBROUTINE FindNamedItem
  
END MODULE ProgUnitStNodes
