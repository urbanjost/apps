! $Id: ModuleStNodes.f90 2795 2019-03-22 09:15:08Z ian $
!> @file
!! Defines the ModuleStNodes module.


!*******************************************************************************
!!
!> Defines the statement tree node types that support Modules - 
!! ModuleSubprogramPartStNode, ModuleStNode and SubmoduleStNode.

MODULE ModuleStNodes
  
  USE ProgUnitStNodes
  USE StNodes
  
  IMPLICIT NONE
  
  !-----------------------------------------------------------------------------
  
  !> Statement tree node for the /module-subprogram-part/ of a module 
  !! or submodule.
  !!
  !! This extends the InternalSubprogramPartStNode by allowing 
  !! /separate-module-subprogram/'s as an option for the subprograms 
  !! that are accepted.
  TYPE, PUBLIC, EXTENDS(InternalSubprogramPartStNode)  &
      :: ModuleSubprogramPartStNode
  CONTAINS
    PROCEDURE :: BuildTree => msp_BuildTree
    PROCEDURE :: GetPart => msp_GetPart
  END TYPE ModuleSubprogramPartStNode
  
  !> Type that is used as a common parent for the module and submodule 
  !! program units.
  TYPE, PUBLIC, EXTENDS(ProgUnitStNode), ABSTRACT :: ModBaseStNode
    TYPE(ModuleSubprogramPartStNode), ALLOCATABLE :: mod_procs
  CONTAINS
    PROCEDURE :: DumpTree => modbase_DumpTree
    PROCEDURE :: Query => modbase_Query
    PROCEDURE :: Modify => modbase_Modify
  END TYPE ModBaseStNode
  
  !> Type to represent a module program unit.
  TYPE, PUBLIC, EXTENDS(ModBaseStNode) :: ModuleStNode
  CONTAINS
    PROCEDURE :: BuildTree => module_BuildTree
    PROCEDURE :: GetPart => module_GetPart
    PROCEDURE :: GetName => module_GetName
  END TYPE ModuleStNode
  
  !> Type to represent a submodule program unit.
  TYPE, PUBLIC, EXTENDS(ModBaseStNode) :: SubmoduleStNode
  CONTAINS
    PROCEDURE :: BuildTree => submodule_BuildTree
    PROCEDURE :: GetPart => submodule_GetPart
    PROCEDURE :: GetName => submodule_GetName
  END TYPE SubmoduleStNode
  
CONTAINS
  
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! Procedures for the ModuleSubprogramPartStNode type.
  
  
  !*****************************************************************************
  !!
  !> Implementation of ModuleSubprogramPartStNode%BuildTree - incorporate the 
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
  
  RECURSIVE SUBROUTINE msp_BuildTree(st, stmt, new_tip, err_list)
    
    USE Errors
    USE StatementData
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(ModuleSubprogramPartStNode), INTENT(INOUT), TARGET :: st
    TYPE(StData), INTENT(INOUT) :: stmt
    CLASS(StNode), INTENT(OUT), POINTER :: new_tip
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !***************************************************************************
    
    ! This should resolve to ProgUnitStNodes::DoBuildTree_isp.  The third 
    ! argument indicates that separate module subprograms are valid 
    ! in this subprogram-part.  The fourth argument specifies that the 
    ! module procedures may themselves have an internal subprogram part.
    CALL DoBuildTree(st, stmt, .TRUE., .TRUE., new_tip, err_list)
    
  END SUBROUTINE msp_BuildTree
  
  
  !*****************************************************************************
  !!
  !> Implementation of Module SubprogramPartStNode%GetPart - get the part 
  !! associated with the current statement tree node.
  !!
  !! @returns The part (an ipt* constant) associated with the node.
  
  FUNCTION msp_GetPart(st) RESULT(pt)
    
    USE SyntaxParts
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(ModuleSubprogramPartStNode), INTENT(IN) :: st
    
    ! Function result
    INTEGER :: pt
    
    !***************************************************************************
    
    pt = iptModuleSubprogramPart
    
  END FUNCTION msp_GetPart
  
  
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! Procedures for the ModBaseStNode type.
  
  
  !*****************************************************************************
  !!
  !> Implementation of ModBaseStNode%BuildTree - incorporate the current
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
  
  RECURSIVE SUBROUTINE do_build_tree( st, stmt, ist_first, ist_last,  &
      new_tip, err_list )
    
    USE CharUtils
    USE Errors
    USE ErrorCodes
    USE ErrorLevels
    USE ErrorCodes
    USE Statements
    USE StatementData
    USE Tokens
      
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(ModBaseStNode), INTENT(INOUT), TARGET :: st
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
        STOP 'Internal error in ModuleStNodes%%do_build_tree, &
            &the first statement of a /module/ or /submodule/ is not a &
            &/module-stmt/ or /submodule-stmt/ respectively.'
      END IF
      ALLOCATE(st%first)
      CALL Move(st%first, stmt)
      ! Jump straight into the specification section.
      ALLOCATE(st%spec)
      st%spec%parent => st
      ! The tip is the specification part but we don't have a statement
      ! to give it yet.
      new_tip => st%spec
      
    ELSE IF (stmt%ist == ist_last) THEN
      
      IF (ALLOCATED(st%last)) THEN
        STOP 'Internal error in ModuleStNodes%%do_build_tree, &
            &the last statement of the program unit was encountered twice.'
      ELSE
        ALLOCATE(st%last)
      END IF
      CALL Move(st%last, stmt)
      
      ! Some housekeeping - if the spec part is empty, then 
      ! there is no point keeping it around.  ifort bug?
      !IF (st%spec%IsEmpty()) DEALLOCATE(st%spec)
      
      new_tip => st%parent
      
    ELSE IF (stmt%ist == istContains) THEN
      
      IF (ALLOCATED(st%mod_procs)) THEN
        CALL Add( err_list,  &
            CODE=errInvalidContext,  &
            COMPONENT=comp,  &
            LOCATION=QueryLocation(stmt%tlist(1)),  &
            MSG='A /contains-stmt/ has already been seen in this &
              &program unit.' )
        new_tip => st
      ELSE
        ALLOCATE(st%mod_procs)
        st%mod_procs%parent => st
        ! Permit one level of internal procedures.
        st%mod_procs%ensure_unique = st%ensure_unique
        CALL st%mod_procs%BuildTree(stmt, new_tip, err_list)
      END IF
      
    ELSE
      
      CALL Add( err_list,  &
          CODE=errInvalidContext,  &
          COMPONENT=comp,  &
          LOCATION=QueryLocation(stmt%tlist(1)),  &
          MSG='A /' // GetStName(stmt%ist) // '/ is not valid at this &
            &point inside a /' // GetPartName(st) // '/.' )
      
    END IF
    
  END SUBROUTINE do_build_tree
  
  
  !*****************************************************************************
  !!
  !> Implementation of ModBaseStNode%DumpTree - write debugging information 
  !! about this node to a logical unit.
  !!
  !! @param[in]     st                The statement tree node.
  !!
  !! @param[in]     unit              The unit to dump to.
  !!
  !! @param[in]     pfx               Prefix to write at the start of each 
  !! record (for indentation).
  
  SUBROUTINE modbase_DumpTree(st, unit, pfx)
    
    USE StatementData
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(ModBaseStNode), INTENT(IN), TARGET :: st
    INTEGER, INTENT(IN) :: unit
    CHARACTER(*), INTENT(IN) :: pfx
    
    !***************************************************************************
    
    WRITE (unit, fmt_partname) pfx, GetPartName(st)
    
    IF (ALLOCATED(st%first))  &
        WRITE (unit, fmt_stmtname) pfx // '  ', GetStName(st%first)
    IF (ALLOCATED(st%spec)) CALL st%spec%DumpTree(unit, pfx // '  ')
    IF (ALLOCATED(st%mod_procs)) CALL st%mod_procs%DumpTree(unit, pfx // '  ')
    IF (ALLOCATED(st%first))  &
        WRITE (unit, fmt_stmtname) pfx // '  ', GetStName(st%last)
    
  END SUBROUTINE modbase_DumpTree
  
  
  !*****************************************************************************
  !!
  !> Implementation of ModBaseStNode%Query - apply visitor object operations 
  !! to this node and its children.
  !!
  !! @param[in]     st                The statement tree node being traversed.
  !!
  !! @param[in,out] visitor           The object to execute the query action.
  
  SUBROUTINE modbase_Query(st, visitor)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(ModBaseStNode), INTENT(IN), TARGET :: st
    CLASS(StNodeQueryVisitor), INTENT(INOUT) :: visitor
    
    !***************************************************************************
    
    ! Execute against ourselves.
    CALL visitor%ExecuteNode(st)
    
    ! Opening statement, if present.
    IF (ALLOCATED(st%first)) CALL visitor%ExecuteStmt(st, st%first)
    
    ! Forward to specificaction section, if present.
    IF (ALLOCATED(st%spec)) CALL st%spec%Query(visitor)
    
    ! Forward to internal subprogram part, if present.
    IF (ALLOCATED(st%mod_procs)) CALL st%mod_procs%Query(visitor)
    
    ! Final statement.
    CALL visitor%ExecuteStmt(st, st%last)
    
  END SUBROUTINE modbase_Query
  
  
  !*****************************************************************************
  !!
  !> Implementation of ModBaseStNode%Modify - apply modify visitor object 
  !! operations to this node and its children.
  !!
  !! @param[in,out] st                The statement tree node being traversed.
  !!
  !! @param[in,out] visitor           The object to execute the modify action.
  
  SUBROUTINE modbase_Modify(st, visitor)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(ModBaseStNode), INTENT(INOUT), TARGET :: st
    CLASS(StNodeModifyVisitor), INTENT(INOUT) :: visitor
    
    !***************************************************************************
    
    ! Execute against ourselves.
    CALL visitor%ExecuteNode(st)
    
    ! Opening statement, if present.
    IF (ALLOCATED(st%first)) CALL visitor%ExecuteStmt(st, st%first)
    
    ! Forward to specificaction section, if present.
    IF (ALLOCATED(st%spec)) CALL st%spec%Modify(visitor)
    
    ! Forward to internal subprogram part, if present.
    IF (ALLOCATED(st%mod_procs)) CALL st%mod_procs%Modify(visitor)
    
    ! Final statement.
    CALL visitor%ExecuteStmt(st, st%last)
    
  END SUBROUTINE modbase_Modify
  
  
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! Procedures for the ModuleStNode type.
  
  
  !*****************************************************************************
  !!
  !> Implementation of ModuleStNode%BuildTree - incorporate the 
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
  
  SUBROUTINE module_BuildTree(st, stmt, new_tip, err_list)
    
    USE Errors
    USE Statements
    USE StatementData
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(ModuleStNode), INTENT(INOUT), TARGET :: st
    TYPE(StData), INTENT(INOUT) :: stmt
    CLASS(StNode), INTENT(OUT), POINTER :: new_tip
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !***************************************************************************
    
    ! Common handling with submodules, bar the first and last statements.
    CALL do_build_tree(st, stmt, istModule, istEndModule, new_tip, err_list)
    
  END SUBROUTINE module_BuildTree
  
  
  !*****************************************************************************
  !!
  !> Implementation of ModuleStNode%GetPart - return the part number for 
  !! the statement tree node.
  !!
  !! @returns The part is a module.
  
  FUNCTION module_GetPart(st) RESULT(ipt)
    
    USE SyntaxParts
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(ModuleStNode), INTENT(IN) :: st
    
    ! Function result
    INTEGER :: ipt
    
    !***************************************************************************
    
    ipt = iptModule
    
  END FUNCTION module_GetPart
  
  
  !*****************************************************************************
  !!
  !> Implementation of ModuleStNode::GetName - get the name of the module.
  !!
  !! @param[in]     st                The statement tree node.
  !!
  !! @param[out]    name              The name of the separate module 
  !! subprogram, or not allocated on error.
  
  SUBROUTINE module_GetName(st, name)
    
    USE ModuleStmtsUtils
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(ModuleStNode), INTENT(IN) :: st
    CHARACTER(:), INTENT(OUT), ALLOCATABLE :: name
    
    !***************************************************************************
    
    CALL GetModuleName(st%first%tlist, name)
    
  END SUBROUTINE module_GetName
  
  
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! Procedures for the SubmoduleStNode
  
  
  !*****************************************************************************
  !!
  !> Implementation of SubmoduleStNode%BuildTree - incorporate the 
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
  
  SUBROUTINE submodule_BuildTree(st, stmt, new_tip, err_list)
    
    USE Errors
    USE Statements
    USE StatementData
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(SubmoduleStNode), INTENT(INOUT), TARGET :: st
    TYPE(StData), INTENT(INOUT) :: stmt
    CLASS(StNode), INTENT(OUT), POINTER :: new_tip
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !***************************************************************************
    
    ! Common handling with modules, bar the first and last statements.
    CALL do_build_tree(  &
        st,  &
        stmt,  &
        istSubmodule,  &
        istEndSubmodule,  &
        new_tip,  &
        err_list )
    
  END SUBROUTINE submodule_BuildTree
  
  
  !*****************************************************************************
  !!
  !> Implementation of SubmoduleStNode%GetPart - get the part number for the 
  !! statement tree node.
  !!
  !! @returns The part is a submodule.
  
  FUNCTION submodule_GetPart(st) RESULT(ipt)
    
    USE SyntaxParts
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(SubmoduleStNode), INTENT(IN) :: st
    
    ! Function result
    INTEGER :: ipt
    
    !***************************************************************************
    
    ipt = iptSubmodule
    
  END FUNCTION submodule_GetPart
  
  
  !*****************************************************************************
  !!
  !> Implementation of SubmoduleStNode::GetName - get the name of the 
  !! submodule.
  !!
  !! @param[in]     st                The statement tree node.
  !!
  !! @param[out]    name              The name of the separate module 
  !! subprogram, or not allocated on error.
  
  SUBROUTINE submodule_GetName(st, name)
    
    USE SubmoduleStmtsUtils
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(SubmoduleStNode), INTENT(IN) :: st
    CHARACTER(:), INTENT(OUT), ALLOCATABLE :: name
    
    !***************************************************************************
    
    CALL GetSubmoduleName(st%first%tlist, name)
    
  END SUBROUTINE submodule_GetName
  
END MODULE ModuleStNodes
