! $Id: BaseStNodes.f90 2931 2019-05-02 20:29:51Z ian $
! ff08 source code copyright 2012 M.E.G.M.S.  See LICENCE.txt for licence.
!> @file
!! Defines the BaseStNodes module.


!*******************************************************************************
!!
!> Defines the BaseStNode type.

MODULE BaseStNodes
  
  USE ProgUnitStNodes
  USE Tokens
  USE Statements
  USE StNodes
  
  IMPLICIT NONE
  
  PRIVATE
  
  !-----------------------------------------------------------------------------
  
  PUBLIC :: MergeTrees
  
  !> The node that starts the tree - that represents iptProgram (R201).
  !!
  !! This is the parent node for everything.  It has various program units 
  !! (programs, modules, submodules, block-data, functions and subroutines) 
  !! as its children.
  !!
  !! At some stage (and maybe again in the future) this was called 
  !! ProgramStNode.  Things were better then.
  TYPE, PUBLIC, EXTENDS(StNode) :: BaseStNode
    !> If true then attempts to add program units with the same name or 
    !! two main programs to the node (or similar) will fail with an error.
    !!
    !! Client code that doesn't want to deal with potential conflicts in 
    !! unit names can set this to .TRUE.  If the client code can deal with 
    !! such conflicts (perhaps because it wants to allow the user to 
    !! select which program unit is significant) then leave this at the 
    !! default of .FALSE.
    !!
    !! This value is propagated to child nodes that will do their own 
    !! uniqueness testing.
    !!
    !! @todo Consider whether we want to make that propagation optional.
    LOGICAL :: ensure_unique = .FALSE.
    !> The vector of program units.
    !!
    !! Will be allocated (but may be zero size, if the program is very 
    !! small) after a successful tree build.
    TYPE(ProgUnitVector), ALLOCATABLE :: prog_units(:)
  CONTAINS
    PROCEDURE :: BuildTree => base_BuildTree
    PROCEDURE :: GetPart => base_GetPart
    PROCEDURE :: Terminate => base_Terminate
    PROCEDURE :: DumpTree => base_DumpTree
    PROCEDURE :: Query => base_Query
    PROCEDURE :: Modify => base_Modify
    PROCEDURE :: GetLocation => base_GetLocation
    PROCEDURE :: GetLabel => base_GetLabel
  END TYPE BaseStNode
  
  ! Component for the unique global identifier requirement.
  CHARACTER(*), PARAMETER :: unique_name_comp = '16.2p2'
  
CONTAINS
  
  !*****************************************************************************
  !!
  !> Implementation of BaseStNode%BuildTree - incorporate the current 
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
  
  RECURSIVE SUBROUTINE base_BuildTree(st, stmt, new_tip, err_list)
    
    USE Errors
    USE ErrorCodes
    USE BlockDataStNodes
    USE ModuleStNodes
    USE StatementData
    
    USE BlockDataStmtsUtils
    USE FunctionStmtsUtils
    USE ModuleStmtsUtils
    USE SubmoduleStmtsUtils
    USE SubroutineStmtsUtils
    USE ProgramStmtsUtils
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(BaseStNode), INTENT(INOUT), TARGET :: st
    TYPE(StData), INTENT(INOUT) :: stmt
    CLASS(StNode), INTENT(OUT), POINTER :: new_tip
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Indicates that a conflicting program unit has already been seen.
    INTEGER :: conflict_idx
    
    ! Name of the current program unit.
    CHARACTER(:), ALLOCATABLE :: unit_name
    
    ! Orphan tree node used to continue the tree build on program units that 
    ! we will not integrate into the current tree.
    CLASS(ProgUnitStNode), ALLOCATABLE :: tmp_unit
    
    !***************************************************************************
    
    ALLOCATE(err_list(0))
    
    IF (.NOT. ALLOCATED(st%prog_units)) ALLOCATE(st%prog_units(0))
    
    ! This particular procedure responds to any statement that might start 
    ! a program unit.  Specifically this includes a /subroutine-stmt/, 
    ! a /function-stmt/, a /module-stmt/, a /block-data-stmt/ and a 
    ! /submodule-stmt/.  It also includes any statement - given that a 
    ! main program can commence without a leading /program-stmt/.  Any 
    ! statement then includes /program-stmt/.
    
    SELECT CASE (stmt%ist)
    !---------------------------------------------------------------------------
    CASE (istFunction)
      IF (st%ensure_unique) THEN
        CALL GetFunctionName(stmt%tlist, unit_name)
        IF (ALLOCATED(unit_name)) THEN
          CALL FindNamedItem(st%prog_units, unit_name, conflict_idx)
          IF (conflict_idx /= 0) THEN
            CALL Add( err_list, CODE=errSubprogramOrUnitAlreadyDefined,  &
                COMPONENT=unique_name_comp,  &
                LOCATION=QueryLocation(stmt%tlist(1)),  &
                MSG='A program unit with a name of "' // unit_name  &
                  // '" has already been seen in this program &
                  &at ' // '<@todo>' // '.' )
            ALLOCATE(FunctionSubprogramStNode :: tmp_unit)
            CALL dispatch(tmp_unit)
            RETURN
          END IF
        END IF
      END IF
      
      CALL AddNode(st%prog_units)
      ALLOCATE( FunctionSubprogramStNode ::  &
          st%prog_units(SIZE(st%prog_units))%item )
      CALL dispatch(st%prog_units(SIZE(st%prog_units))%item)
      
    !---------------------------------------------------------------------------
    CASE (istSubroutine)
      IF (st%ensure_unique) THEN
        CALL GetSubroutineName(stmt%tlist, unit_name)
        IF (ALLOCATED(unit_name)) THEN
          CALL FindNamedItem(st%prog_units, unit_name, conflict_idx)
          IF (conflict_idx /= 0) THEN
            CALL Add( err_list, CODE=errSubprogramOrUnitAlreadyDefined,  &
                COMPONENT=unique_name_comp,  &
                LOCATION=QueryLocation(stmt%tlist(1)),  &
                MSG='A program unit with a name of "' // unit_name  &
                  // '" has already been seen in this program &
                  &at ' // '<@todo>' // '.' )
            ALLOCATE(SubroutineSubprogramStNode :: tmp_unit)
            CALL dispatch(tmp_unit)
            RETURN
          END IF
        END IF
      END IF
      
      CALL AddNode(st%prog_units)
      ALLOCATE( SubroutineSubprogramStNode ::  &
          st%prog_units(SIZE(st%prog_units))%item )
      CALL dispatch(st%prog_units(SIZE(st%prog_units))%item)
      
    !---------------------------------------------------------------------------
    CASE (istBlockData)
      IF (st%ensure_unique) THEN
        ! There is an additional restriction on block data that there cannot 
        ! be two unnamed block data program units.
        CALL GetBlockDataName(stmt%tlist, unit_name)
        IF (ALLOCATED(unit_name)) THEN
          IF (LEN(unit_name) /= 0) THEN
            CALL FindNamedItem(st%prog_units, unit_name, conflict_idx)
            IF (conflict_idx /= 0) THEN
              CALL Add( err_list, CODE=errSubprogramOrUnitAlreadyDefined,  &
                  COMPONENT=unique_name_comp,  &
                  LOCATION=QueryLocation(stmt%tlist(1)),  &
                  MSG='A program unit with a name of "' // unit_name  &
                    // '" has already been seen in this program &
                    &at ' // '<@todo>' // '.' )
              ALLOCATE(BlockDataStNode :: tmp_unit)
              CALL dispatch(tmp_unit)
              RETURN
            END IF
          ELSE
            ! Unnamed block data - there can only be one.
            CALL FindBlockData(st%prog_units, '', conflict_idx)
            IF (conflict_idx /= 0) THEN
              CALL Add( err_list, CODE=errSubprogramOrUnitAlreadyDefined,  &
                  COMPONENT='11.3p5',  &
                  LOCATION=QueryLocation(stmt%tlist(1)),  &
                  MSG='An unnamed block data unit has already been seen in &
                    &this program at ' // '<@todo>' // '.' )
              ALLOCATE(BlockDataStNode :: tmp_unit)
              CALL dispatch(tmp_unit)
              RETURN
            END IF
          END IF
        END IF
      END IF
      
      CALL AddNode(st%prog_units)
      ALLOCATE(BlockDataStNode:: st%prog_units(SIZE(st%prog_units))%item)
      CALL dispatch(st%prog_units(SIZE(st%prog_units))%item)
      
    !---------------------------------------------------------------------------
    CASE (istModule)
      IF (st%ensure_unique) THEN
        CALL GetModuleName(stmt%tlist, unit_name)
        IF (ALLOCATED(unit_name)) THEN
          CALL FindNamedItem(st%prog_units, unit_name, conflict_idx)
          IF (conflict_idx /= 0) THEN
            CALL Add( err_list, CODE=errSubprogramOrUnitAlreadyDefined,  &
                COMPONENT=unique_name_comp,  &
                LOCATION=QueryLocation(stmt%tlist(1)),  &
                MSG='A program unit with a name of "' // unit_name  &
                  // '" has already been seen in this program &
                  &at ' // '<@todo>' // '.' )
            ALLOCATE(ModuleStNode :: tmp_unit)
            CALL dispatch(tmp_unit)
            RETURN
          END IF
        END IF
      END IF
      
      CALL AddNode(st%prog_units)
      ALLOCATE(ModuleStNode:: st%prog_units(SIZE(st%prog_units))%item)
      CALL dispatch(st%prog_units(SIZE(st%prog_units))%item)
      
    !---------------------------------------------------------------------------
    CASE (istSubmodule)
      IF (st%ensure_unique) THEN
        ! Submodule identifiers are things that cannot be names, hence the 
        ! comparison in FindNamedItem will be limited to submodules.
        CALL GetSubmoduleIdentifier(stmt%tlist, unit_name)
        IF (ALLOCATED(unit_name)) THEN
          CALL FindNamedItem(st%prog_units, unit_name, conflict_idx)
          IF (conflict_idx /= 0) THEN
            CALL Add( err_list, CODE=errSubprogramOrUnitAlreadyDefined,  &
                COMPONENT=unique_name_comp,  &
                LOCATION=QueryLocation(stmt%tlist(1)),  &
                MSG='A submodule with an identifier of "' // unit_name  &
                  // '" has already been seen in this program &
                  &at ' // '<@todo>' // '.' )
            ALLOCATE(SubmoduleStNode :: tmp_unit)
            CALL dispatch(tmp_unit)
            RETURN
          END IF
        END IF
      END IF
      
      CALL AddNode(st%prog_units)
      ALLOCATE(SubmoduleStNode:: st%prog_units(SIZE(st%prog_units))%item)
      CALL dispatch(st%prog_units(SIZE(st%prog_units))%item)
      
    !---------------------------------------------------------------------------
    CASE DEFAULT
      ! Jump into the main-program.  Perhaps we are dealing with a 
      ! /prorgram-stmt/, perhaps we have a main program without an opening 
      ! statement, perhaps this is actually the end statement for a 
      ! main program that has no contents.  The build tree action for 
      ! a main program needs to be robust to all of these cases.
      IF (st%ensure_unique) THEN
        ! Have we seen a /main-program/ previously?  There can only be one, 
        ! regardless of its name.
        CALL FindMainProgram(st%prog_units, conflict_idx)
        IF (conflict_idx /= 0) THEN
          CALL Add( err_list, CODE=errSubprogramOrUnitAlreadyDefined,  &
              COMPONENT='2.2.2p1', LOCATION=QueryLocation(stmt%tlist(1)),  &
              MSG='A /main-program/ has already been seen in this program &
                &at ' // '<@todo>' // '.' )
          ALLOCATE(MainProgramStNode :: tmp_unit)
          CALL dispatch(tmp_unit)
          RETURN
        END IF
        ! If the main program has a name, have we seen other units with the 
        ! same name?
        IF (stmt%ist == istProgram) THEN
          CALL GetProgramName(stmt%tlist, unit_name)
          IF (ALLOCATED(unit_name)) THEN
            IF (LEN(unit_name) /= 0) THEN
              CALL FindNamedItem(st%prog_units, unit_name, conflict_idx)
              IF (conflict_idx /= 0) THEN
                CALL Add( err_list, CODE=errSubprogramOrUnitAlreadyDefined,  &
                    COMPONENT=unique_name_comp,  &
                    LOCATION=QueryLocation(stmt%tlist(1)),  &
                    MSG='A program unit with a name of "' // unit_name  &
                      // '" has already been seen in this program &
                      &at ' // '<@todo>' // '.' )
                ALLOCATE(MainProgramStNode :: tmp_unit)
                CALL dispatch(tmp_unit)
                RETURN
              END IF
            END IF
          END IF
        END IF
      END IF
      
      CALL AddNode(st%prog_units)
      ALLOCATE(MainProgramStNode :: st%prog_units(SIZE(st%prog_units))%item)
      CALL dispatch(st%prog_units(SIZE(st%prog_units))%item)
      
    END SELECT
    
  CONTAINS
    
    !***************************************************************************
    !!
    !> Worker utility that initially configures the given item then 
    !! progresses the build tree action on that item.
    !!
    !! @param[in,out] item            Child program unit item.
    
    SUBROUTINE dispatch(item)
      
      !-------------------------------------------------------------------------
      ! Arguments.
      
      CLASS(ProgUnitStNode), INTENT(INOUT), TARGET :: item
      
      !-------------------------------------------------------------------------
      ! Local variables.
      
      TYPE(Error), ALLOCATABLE :: sub_err_list(:)
      
      !*************************************************************************
      
      item%parent => st
      ! Inherit the ensure unique characteristic.
      item%ensure_unique = st%ensure_unique
      
      new_tip => item
      CALL item%BuildTree(stmt, new_tip, sub_err_list)
      CALL Add(err_list, sub_err_list)
      
    END SUBROUTINE dispatch
    
  END SUBROUTINE base_BuildTree
  
  
  !*****************************************************************************
  !!
  !> Implementation of BaseStNode%GetPart - get the syntax part of the
  !! current statement tree node.
  !!
  !! @param[in]     st                The statement tree node of interest.
  !!
  !! @returns The part associated with the node.
  
  FUNCTION base_GetPart(st) RESULT(ipt)
    
    USE SyntaxParts
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(BaseStNode), INTENT(IN) :: st
    
    ! Function result
    INTEGER :: ipt
    
    !***************************************************************************
    
    ipt = iptProgram
    
  END FUNCTION base_GetPart
  
  
  !*****************************************************************************
  !!
  !> Implementation of BaseStNode%Terminate - terminate the statement tree 
  !! node.
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
  
  SUBROUTINE base_Terminate(st, eof, stmt, err_list)
    
    USE Errors
    USE StatementData
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(BaseStNode), INTENT(INOUT), TARGET :: st
    LOGICAL, INTENT(IN) :: eof
    TYPE(StData), INTENT(IN) :: stmt
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !***************************************************************************
    
    ALLOCATE(err_list(0))
    
  END SUBROUTINE base_Terminate
  
  
  !*****************************************************************************
  !!
  !> Implementation of BaseStNode%Dump - write a debugging representation of 
  !! the node to a logical unit.
  !!
  !! @param[in]     st                The statement tree node.
  !!
  !! @param[in]     unit              The unit to dump to.
  !!
  !! @param[in]     pfx               Prefix to write at the start 
  !! of each record (for indentation).
  
  SUBROUTINE base_DumpTree(st, unit, pfx)
    
    USE SyntaxParts
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(BaseStNode), INTENT(IN), TARGET :: st
    INTEGER, INTENT(IN) :: unit
    CHARACTER(*), INTENT(IN) :: pfx
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: i              ! Program unit index.
    
    !***************************************************************************
    
    WRITE (unit, fmt_partname) pfx, GetPartName(st)
    IF (.NOT. ALLOCATED(st%prog_units)) RETURN
    
    DO i = 1, SIZE(st%prog_units)
      CALL st%prog_units(i)%item%DumpTree(unit, pfx // '  ')
    END DO
    
  END SUBROUTINE base_DumpTree
  
  
  !*****************************************************************************
  !!
  !> Implementation of BaseStNode%Query - apply visitor object operations 
  !! to this node and its children.
  !!
  !! @param[in]     st                The statement tree node being traversed.
  !!
  !! @param[in,out] visitor           The object to execute the query action.
  
  SUBROUTINE base_Query(st, visitor)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(BaseStNode), INTENT(IN), TARGET :: st
    CLASS(StNodeQueryVisitor), INTENT(INOUT) :: visitor
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: i              ! Program unit index.
    
    !***************************************************************************
    
    ! Execute against ourself.
    CALL visitor%ExecuteNode(st)
    
    ! Pass query onto our children.
    DO i = 1, SIZE(st%prog_units)
      CALL st%prog_units(i)%Query(visitor)
    END DO
    
  END SUBROUTINE base_Query
  
  
  !*****************************************************************************
  !!
  !> Implementation of BaseStNode%Modify - apply modify visitor object 
  !! operations to this node and its children.
  !!
  !! @param[in,out] st                The statement tree node being traversed.
  !!
  !! @param[in,out] visitor           The object to execute the modify action.
  
  SUBROUTINE base_Modify(st, visitor)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(BaseStNode), INTENT(INOUT), TARGET :: st
    CLASS(StNodeModifyVisitor), INTENT(INOUT) :: visitor
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: i              ! Program unit index.
    
    !***************************************************************************
    
    ! Execute against ourself.
    CALL visitor%ExecuteNode(st)
    
    ! Pass onto our children.
    DO i = 1, SIZE(st%prog_units)
      CALL st%prog_units(i)%Modify(visitor)
    END DO
    
  END SUBROUTINE base_Modify
  
  
  !*****************************************************************************
  !!
  !> Implementation of BaseStNode%GetLocation - get the starting source 
  !! location of the node.
  !!
  !! @param[in]     st                The node of interest.
  !!
  !! @returns The source location of the start of the node.
  
  FUNCTION base_GetLocation(st) RESULT(loc)
    
    USE SourceLocations
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(BaseStNode), INTENT(IN) :: st
    
    ! Function result.
    TYPE(SourceLocation) :: loc
    
    !***************************************************************************
    
    IF (SIZE(st%prog_units) == 0) THEN
      loc = SourceLocationIfortBug()
    ELSE
      loc = st%prog_units(1)%item%GetLocation()
    END IF
    
  END FUNCTION base_GetLocation
  
  
  !*****************************************************************************
  !!
  !> Implementation of BaseStNode%GetLabel - get the label 
  !! of the node.
  !!
  !! @param[in]     st                The node of interest.
  !!
  !! @returns The statement label of the the node.
  !!
  !! Returning the first statement label of the entire program isn't 
  !! particularly useful, but we have to return something.
  
  FUNCTION base_GetLabel(st) RESULT(label)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(BaseStNode), INTENT(IN) :: st
    
    ! Function result.
    TYPE(LabelWrapper) :: label
    
    !***************************************************************************
    
    IF (SIZE(st%prog_units) /= 0) THEN
      label = st%prog_units(1)%item%GetLabel()
    END IF
    
  END FUNCTION base_GetLabel
  
  
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! These procedures work with BaseStNode type objects, but are not bindings.
  
  
  !*****************************************************************************
  !!
  !> Merge two statement trees together.
  !!
  !! @param[in,out] target            The first tree to merge, and the tree 
  !! that will receive the results of the merge.
  !!
  !! @param[in,out] other             The other tree to merge in.  This node 
  !! will be empty after a successful merge.  If the ensure_unique component 
  !! in @a target is true, then program units that would otherwise have caused 
  !! uniqueness issues in @a target will have been left in @a other.
  !!
  !! @param[out]    err_list          List of errors.
  
  SUBROUTINE MergeTrees(target, other, err_list)
    
    USE Errors
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(BaseStNode), INTENT(INOUT), TARGET :: target
    CLASS(BaseStNode), INTENT(INOUT) :: other
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !***************************************************************************
    
    ALLOCATE(err_list(0))
    
    IF (.NOT. ALLOCATED(other%prog_units)) RETURN
    IF (.NOT. ALLOCATED(target%prog_units)) ALLOCATE(target%prog_units(0))
    
    IF (target%ensure_unique) THEN
      CALL do_unique_merge(target, other, err_list)
    ELSE
      CALL do_simple_merge(target, other)
    END IF
    
  END SUBROUTINE MergeTrees
  
  
  !*****************************************************************************
  !!
  !> Perform a simple merge of two program unit trees.
  !!
  !! @param[in,out] target            The first tree to merge, and the tree 
  !! that will receive the results of the merge.
  !!
  !! @param[in,out] other             The other tree to merge in.  This node 
  !! will be empty after a successful merge.
  !!
  !! We rely on the allocation status and size checks in the MergeTrees call.
  
  SUBROUTINE do_simple_merge(target, other)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(BaseStNode), INTENT(INOUT), TARGET :: target
    CLASS(BaseStNode), INTENT(INOUT) :: other
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Temporary array to accomodate the merge results.
    TYPE(ProgUnitVector), ALLOCATABLE :: tmp(:)
    
    INTEGER :: i              ! Program unit index.
    
    !***************************************************************************
    
    ALLOCATE(tmp(SIZE(target%prog_units) + SIZE(other%prog_units)))
    
    ! Move the allocation across.
    DO i = 1, SIZE(target%prog_units)
      CALL MOVE_ALLOC(target%prog_units(i)%item, tmp(i)%item)
    END DO
    DO i = 1, SIZE(other%prog_units)
      CALL MOVE_ALLOC( other%prog_units(i)%item,  &
          tmp(i+SIZE(target%prog_units))%item )
      ! Meet your new mum.
      tmp(i+SIZE(target%prog_units))%item%parent => target
    END DO
    
    ! Clean up the other tree.
    ! other%prog_units = [ StNodeList:: ]      ! ifort 15.0 bug
    DEALLOCATE(other%prog_units) ; ALLOCATE(other%prog_units(0))
    
    ! Put the temporary where it needs to go.
    CALL MOVE_ALLOC(tmp, target%prog_units)
    
  END SUBROUTINE do_simple_merge
  
  
  !*****************************************************************************
  !!
  !> Perform a unique merge of two program unit trees.
  !!
  !! @param[in,out] target            The first tree to merge, and the tree 
  !! that will receive the results of the merge.
  !!
  !! @param[in,out] other             The other tree to merge in.  This node 
  !! will be empty after a successful merge.  If the ensure_unique component 
  !! in @a target is true, then program units that would otherwise have caused 
  !! uniqueness issues in @a target will have been left in @a other.
  !!
  !! @param[out]    err_list          List of errors.
  
  SUBROUTINE do_unique_merge(target, other, err_list)
    
    USE BlockDataStNodes
    USE Errors
    USE ErrorCodes
    USE SourceLocations
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(BaseStNode), INTENT(INOUT), TARGET :: target
    CLASS(BaseStNode), INTENT(INOUT) :: other
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: i              ! Program unit index in other.
    INTEGER :: conflict_idx   ! Conflicting program unit index.
    INTEGER :: num            ! Number of units remaining in other.
    
    ! Location of a previous definition of a program unit in @a target.
    TYPE(SourceLocation) :: loc
    
    ! Name of the program unit in @a other being considered.
    CHARACTER(:), ALLOCATABLE :: other_name
    
    ! Temporary array to allow other to be resized.
    TYPE(ProgUnitVector), ALLOCATABLE :: tmp(:)
    
    !***************************************************************************
    
    ALLOCATE(err_list(0))
    
    DO i = 1, SIZE(other%prog_units)
      ! Some unit specific constraints.  After the select type we do a 
      ! general check for a name clash, where a name exists.
      SELECT TYPE (other_item => other%prog_units(i)%item)
      TYPE IS (MainProgramStNode)
        ! See if we already have a main program in target.
        CALL FindMainProgram(target%prog_units, conflict_idx)
        IF (conflict_idx /= 0) THEN
          loc = target%prog_units(conflict_idx)%item%GetLocation()
          CALL Add( err_list,  &
              CODE=errSubprogramOrUnitAlreadyDefined,  &
              COMPONENT='2.2.2p1',  &
              LOCATION=other_item%GetLocation(),  &
              MSG='A /main-program/ has already been seen in this program &
                &at ' // loc%ToString() // '.' )
          CYCLE
        END IF
        
      TYPE IS (BlockDataStNode)
        CALL other_item%GetName(other_name)
        IF (ALLOCATED(other_name)) THEN
          IF (LEN(other_name) == 0) THEN
            ! Unnamed block data - only can be one.
            CALL FindBlockData(target%prog_units, '', conflict_idx)
            IF (conflict_idx /= 0) THEN
              loc = target%prog_units(conflict_idx)%item%GetLocation()
              CALL Add( err_list,  &
                  CODE=errSubprogramOrUnitAlreadyDefined,  &
                  COMPONENT=unique_name_comp,  &
                  LOCATION=other_item%GetLocation(),  &
                  MSG='An unnamed block data program unit &
                    &has already been seen in this program &
                    &at ' // loc%ToString() // '.' )
              CYCLE
            END IF
          END IF
        END IF
        
      END SELECT
      
      ! Do we have a name clash?
      CALL other%prog_units(i)%item%GetName(other_name)
      IF (ALLOCATED(other_name)) THEN
        IF (LEN(other_name) /= 0) THEN
          CALL FindNamedItem(target%prog_units, other_name, conflict_idx)
          IF (conflict_idx /= 0) THEN
            loc = target%prog_units(conflict_idx)%item%GetLocation()
            CALL Add( err_list,  &
                CODE=errSubprogramOrUnitAlreadyDefined,  &
                COMPONENT=unique_name_comp,  &
                LOCATION=other%prog_units(i)%item%GetLocation(),  &
                MSG='A program unit with a name of "' // other_name  &
                  // '" has already been seen in this program &
                  &at ' // loc%ToString() // '.' )
            CYCLE
          END IF
        END IF
      END IF
        
      ! Move the unit across.
      CALL AddNode(target%prog_units)
      CALL MOVE_ALLOC(  &
          other%prog_units(i)%item, &
          target%prog_units(SIZE(target%prog_units))%item )
      target%prog_units(SIZE(target%prog_units))%item%parent => target
      
    END DO
    
    ! @a target is done - now resize @a other.
    num = 0
    DO i = 1, SIZE(other%prog_units)
      IF (ALLOCATED(other%prog_units(i)%item)) num = num + 1
    END DO
    
    ALLOCATE(tmp(num))
    num = 0
    DO i = 1, SIZE(other%prog_units)
      IF (ALLOCATED(other%prog_units(i)%item)) THEN
        num = num + 1
        CALL MOVE_ALLOC(other%prog_units(i)%item, tmp(num)%item)
      END IF
    END DO
    
    CALL MOVE_ALLOC(tmp, other%prog_units)
    
  END SUBROUTINE do_unique_merge
  
END MODULE BaseStNodes
