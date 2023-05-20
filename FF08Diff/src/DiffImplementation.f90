! $Id: DiffImplementation.f90 2913 2019-04-17 21:32:21Z ian $
!> @file
!! Defines the DiffImplementation module.


!*******************************************************************************
!!
!> Procedures to implement the FF08Diff program.

MODULE DiffImplementation
  
  USE StNodes
  USE StatementData
  
  IMPLICIT NONE
  
  PRIVATE
  
  PUBLIC :: Execute
  
  !> Format specification to use for reporting general errors, that may 
  !! not be specific to a particular file.
  CHARACTER(*), PARAMETER :: fmt_err = "(A)"
  
  !> Format specification for output of a difference.
  CHARACTER(*), PARAMETER :: fmt_diff = "(A,/,A,/,2X,A)"
  
  !-----------------------------------------------------------------------------
  
  !> Type to traverse the statement tree and produce a statement list.
  TYPE, EXTENDS(StNodeQueryVisitor) :: flatten_visitor
    !> The statement list that results from the flattening.
    TYPE(StData), ALLOCATABLE :: stmts(:)
    !> The last index in stmts that has been used.
    INTEGER :: ilast = 0
  CONTAINS
    PROCEDURE :: ExecuteNode => flatten_ExecuteNode
    PROCEDURE :: ExecuteStmt => flatten_ExecuteStmt
  END TYPE flatten_visitor
  
CONTAINS
  
  !*****************************************************************************
  !!
  !> Executes the file set differencing.
  !!
  !! @param[in]     left_list         List of the file specifications that 
  !! nominate the files to include in the left set of the comparison.  
  !! The mapping from file specifications to file names is defined by the host.
  !!
  !! @param[in]     right_list        List of the file specifications that 
  !! nominate the files to include in the right set of the comparison.  
  !! The mapping from file specifications to file names is defined by the host.
  !!
  !! @param[in]     out_unit          Logical unit connected for formatted 
  !! output to use for reporting the results of the comparison.
  !!
  !! @param[in]     err_unit          Logical unit connected for formatted 
  !! output to use for reporting any errors.
  !!
  !! @param[in]     stop_code         Error code - non-zero on error with 
  !! a value consistent with the stop codes reported by the main program.
  !!
  !! We build each set of source files into its own statement tree.  If 
  !! either tree build fails for any reason then we terminate the 
  !! differencing.  We then call compare_programs to start progressive 
  !! comparison and reporting of differences between the two trees.
  
  SUBROUTINE Execute( left_list, right_list, out_unit, err_unit,  &
      show_warnings, stop_code )
    
    USE BaseStNodes
    USE BuildTreePass
    USE CompilerHosts
    USE DefaultHosts
    USE Errors
    USE Sources
    USE Strings
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(String), INTENT(IN) :: left_list(:)
    TYPE(String), INTENT(IN) :: right_list(:)
    INTEGER, INTENT(IN) :: out_unit
    INTEGER, INTENT(IN) :: err_unit
    LOGICAL, INTENT(IN) :: show_warnings
    INTEGER, INTENT(OUT) :: stop_code
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! File names for the left set.
    TYPE(String), ALLOCATABLE :: left_names(:)
    
    ! File names for the right set.
    TYPE(String), ALLOCATABLE :: right_names(:)
    
    ! Error object for procedure calls.
    TYPE(Error), ALLOCATABLE :: err_list(:)
    
    ! Compiler host object to define host behaviour.
    TYPE(DefaultHost) :: host
    
    ! The combined left statement tree.
    TYPE(BaseStNode), POINTER :: left_tree
    
    ! The combined left statement tree.
    TYPE(BaseStNode), POINTER :: right_tree
    
    ! The statement tree for the current file.
    TYPE(BaseStNode), POINTER :: current
    
    INTEGER :: ie             ! Error index.
    INTEGER :: if             ! File index.
    INTEGER :: diff           ! Difference flag.
    
    !***************************************************************************
    
    !---------------------------------------------------------------------------
    ! Prepare the list of file names
    
    ! Convert the list of file specifications to a list of file names.  In 
    ! many cases this will be a simple copy.
    CALL ExpandSpecsToNames(host, left_list, left_names, err_list)
    DO ie = 1, SIZE(err_list)
      WRITE (err_unit, fmt_err) .Describe. err_list(ie)
    END DO
    IF (Failure(err_list)) THEN
      stop_code = 2
      RETURN
    END IF
    IF (SIZE(left_names) == 0) THEN
      WRITE (err_unit, "('There are no files in the left set.')")
      stop_code = 2
      RETURN
    END IF
    
    CALL ExpandSpecsToNames(host, right_list, right_names, err_list)
    DO ie = 1, SIZE(err_list)
      WRITE (err_unit, fmt_err) .Describe. err_list(ie)
    END DO
    IF (Failure(err_list)) THEN
      stop_code = 2
      RETURN
    END IF
    IF (SIZE(right_names) == 0) THEN
      WRITE (err_unit, "('There are no files in the right set.')")
      stop_code = 2
      RETURN
    END IF
    
    !---------------------------------------------------------------------------
    ! Build the statement trees.
    
    ALLOCATE(left_tree)
    left_tree%ensure_unique = .TRUE.
    ALLOCATE(left_tree%prog_units(0))
    
    DO if = 1, SIZE(left_names)
      CALL DoBuildTreePass(  &
          host,  &
          left_names(if)%item,  &
          current,  &
          err_list,  &
          .TRUE. )
      DO ie = 1, SIZE(err_list)
        IF (show_warnings .OR. Failure(err_list(ie))) THEN
          WRITE (err_unit, fmt_err) .Describe. err_list(ie)
        END IF
      END DO
      IF (.NOT. Failure(err_list)) THEN
        CALL MergeTrees(left_tree, current, err_list)
        DO ie = 1, SIZE(err_list)
          WRITE (err_unit, fmt_err) .Describe. err_list(ie)
        END DO
      END IF
      ! current is allocated in DoBuildTreePass, so deallocate it here.
      DEALLOCATE(current)
      IF (Failure(err_list)) THEN
        stop_code = 2
        RETURN
      END IF
    END DO
    
    ALLOCATE(right_tree)
    right_tree%ensure_unique = .TRUE.
    ALLOCATE(right_tree%prog_units(0))
    
    DO if = 1, SIZE(right_names)
      CALL DoBuildTreePass(  &
          host,  &
          right_names(if)%item,  &
          current,  &
          err_list,  &
          .TRUE. )
      DO ie = 1, SIZE(err_list)
        IF (show_warnings .OR. Failure(err_list(ie))) THEN
          WRITE (err_unit, fmt_err) .Describe. err_list(ie)
        END IF
      END DO
      IF (.NOT. Failure(err_list)) THEN
        CALL MergeTrees(right_tree, current, err_list)
        DO ie = 1, SIZE(err_list)
          WRITE (err_unit, fmt_err) .Describe. err_list(ie)
        END DO
      END IF
      ! current is allocated in DoBuildTreePass, so deallocate it here.
      DEALLOCATE(current)
      IF (Failure(err_list)) THEN
        stop_code = 2
        RETURN
      END IF
    END DO
    
    !---------------------------------------------------------------------------
    ! Conduct the differencing.
    
    diff = 0                  ! Sets are the same until they are different.
    
    CALL compare_programs(left_tree, right_tree, out_unit, err_unit, diff)
    
    ! Report overall result.
    IF (diff > 0) THEN
      WRITE (out_unit, "('The sets are semantically different.')")
      stop_code = 1
    ELSE IF (diff < 0) THEN
      WRITE (out_unit, "('Errors in the source prevented the differences &
          &from being determined.')")
      stop_code = 2
    ELSE
      WRITE (out_unit, "('The sets are semantically equivalent.')")
      stop_code = 0
    END IF
    
    !---------------------------------------------------------------------------
    
    CALL ReleaseSources()
    
  END SUBROUTINE Execute
  
  
  !*****************************************************************************
  !!
  !> Compare two programs.
  !!
  !! @param[in]     left              The statement tree node for the left 
  !! program.
  !!
  !! @param[in]     right             The statement tree node for the right 
  !! program.
  !!
  !! @param[in]     out_unit          Logical unit connected for formatted 
  !! output to use for reporting the results of the comparison.
  !!
  !! @param[in]     err_unit          Logical unit connected for formatted 
  !! putput to use for reporting errors that prevent the comparison.
  !!
  !! @param[in,out] diff              Set to true if a difference was 
  !! detected.  Not changed if no difference was detected.
  !!
  !! The programs don't need to be complete programs - we are really just 
  !! using the BaseStNode object as a container of program units.
  
  SUBROUTINE compare_programs(left, right, out_unit, err_unit, diff)
    
    USE BaseStNodes
    USE BlockDataStNodes
    USE ModuleStNodes
    USE ProgUnitStNodes
    USE SourceLocations
    USE SyntaxParts
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(BaseStNode), INTENT(IN), TARGET :: left
    TYPE(BaseStNode), INTENT(IN), TARGET :: right
    INTEGER, INTENT(IN) :: out_unit
    INTEGER, INTENT(IN) :: err_unit
    INTEGER, INTENT(INOUT) :: diff
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Whether the program units in the right set have been considered.
    LOGICAL :: considered(SIZE(right%prog_units))
    
    INTEGER :: il             ! Left index.
    INTEGER :: ir             ! Right index.
    
    ! Pointers to type specific program unit objects.  Used for further 
    ! unit specific comparisons.
    TYPE(MainProgramStNode), POINTER :: p_main_prog
    TYPE(SubroutineSubprogramStNode), POINTER :: p_subroutine
    TYPE(FunctionSubprogramStNode), POINTER :: p_function
    TYPE(ModuleStNode), POINTER :: p_module
    TYPE(SubmoduleStNode), POINTER :: p_submodule
    TYPE(BlockDataStNode), POINTER :: p_blockdata
    
    ! Name of the program unit being considered.
    CHARACTER(:), ALLOCATABLE :: unit_name
    
    ! Left set location for a difference.
    TYPE(SourceLocation) :: left_loc
    
    ! Right set location for a difference.
    TYPE(SourceLocation) :: right_loc
    
    !***************************************************************************
    
    !---------------------------------------------------------------------------
    ! Sanity check - is there any source in each set?
    
    IF (SIZE(left%prog_units) == 0 .AND. SIZE(right%prog_units) == 0) THEN
      ! No programs.
      RETURN
    ELSE IF (SIZE(left%prog_units) == 0) THEN
      left_loc = SourceLocation()
      right_loc = right%GetLocation()
      
      WRITE (out_unit, fmt_diff)  &
          left_loc%ToString(), right_loc%ToString(),  &
          'The left set is empty, the right set is not empty.'
    ELSE IF (SIZE(right%prog_units) == 0) THEN
      left_loc = left%GetLocation()
      right_loc = SourceLocation()
      
      WRITE (out_unit, fmt_diff)  &
          left_loc%ToString(), right_loc%ToString(),  &
          'The left set is not empty, the right set is empty.'
    END IF
    
    !---------------------------------------------------------------------------
    ! Check right program unit names can be determined.  Doing this here in 
    ! one hit simplifies searching for program units based on name.
    
    DO ir = 1, SIZE(right%prog_units)
      CALL right%prog_units(ir)%item%GetName(unit_name)
      IF (.NOT. ALLOCATED(unit_name)) THEN
        diff = -1
        right_loc = right%prog_units(ir)%item%GetLocation()
        WRITE ( err_unit, "('The right set has a program unit at ',A,&
              &' with a name that can not be determinined.')" )  &
            right_loc%ToString()
        RETURN
      END IF
    END DO
    
    !---------------------------------------------------------------------------
    ! Run through each unit on the left, checking to see whether we can 
    ! find a match (positioning not relevant) on the right.
    
    ! Start by flagging all units on the right as not considered.
    considered = .FALSE.
    
    DO il = 1, SIZE(left%prog_units)
      CALL left%prog_units(il)%item%GetName(unit_name)
      IF (.NOT. ALLOCATED(unit_name)) THEN
        diff = -1
        left_loc = left%prog_units(il)%item%GetLocation()
        WRITE ( err_unit, "('The left set has a program unit at ',A,&
              &' with a name that can not be determinined.')" )  &
            left_loc%ToString()
        RETURN
      END IF
      
      SELECT TYPE (left_item => left%prog_units(il)%item)
      !-------------------------------------------------------------------------
      TYPE IS (MainProgramStNode)
        CALL FindMainProgram(right%prog_units, ir)
        IF (ir == 0) THEN
          diff = diff + 1
          left_loc = left_item%GetLocation()
          right_loc = SourceLocation()
          
          WRITE (out_unit, fmt_diff)  &
              left_loc%ToString(), right_loc%ToString(),  &
              'The left set has a /' // GetPartName(left_item%GetPart())  &
              // '/, the right set does not.'
          CYCLE
        END IF
        
        ! Get a reference to the relevant right hand program unit as a 
        ! MainProgramStNode object.
        SELECT TYPE (right_item => right%prog_units(ir)%item)
        TYPE IS (MainProgramStNode)
          p_main_prog => right_item
        CLASS DEFAULT
          STOP 'In DiffImplementation::compare_programs, a main program &
              &wasn''t a main program.'
        END SELECT
        CALL compare_main_programs( left_item, p_main_prog,  &
            out_unit, err_unit, diff )
        considered(ir) = .TRUE.
        
      !-------------------------------------------------------------------------
      TYPE IS (SubroutineSubprogramStNode)
        CALL left_item%GetName(unit_name)
        CALL find_subroutine(right%prog_units, unit_name, ir, p_subroutine)
        IF (ir == 0) THEN
          diff = diff + 1
          left_loc = left_item%GetLocation()
          right_loc = SourceLocation()
          
          WRITE (out_unit, fmt_diff)  &
              left_loc%ToString(), right_loc%ToString(),  &
              'The left set has a /' // GetPartName(left_item%GetPart())  &
              // '/ named "' // unit_name // '", the right set does not.'
          CYCLE
        END IF
        CALL compare_subprograms( left_item, p_subroutine, out_unit,  &
            err_unit, diff )
        considered(ir) = .TRUE.
        
      !-------------------------------------------------------------------------
      TYPE IS (FunctionSubprogramStNode)
        CALL left_item%GetName(unit_name)
        CALL find_function(right%prog_units, unit_name, ir, p_function)
        IF (ir == 0) THEN
          diff = diff + 1
          left_loc = left_item%GetLocation()
          right_loc = SourceLocation()
          
          WRITE (out_unit, fmt_diff)  &
              left_loc%ToString(), right_loc%ToString(),  &
              'The left set has a /' // GetPartName(left_item%GetPart())  &
              // '/ named "' // unit_name // '", the right set does not.'
          CYCLE
        END IF
        CALL compare_subprograms( left_item, p_function, out_unit,  &
            err_unit, diff )
        considered(ir) = .TRUE.
        
      !-------------------------------------------------------------------------
      TYPE IS (BlockDataStNode)
        CALL left_item%Getname(unit_name)
        CALL FindBlockData(right%prog_units, unit_name, ir)
        IF (ir == 0) THEN
          diff = diff + 1
          left_loc = left_item%GetLocation()
          right_loc = SourceLocation()
          IF (LEN(unit_name) == 0) THEN
            WRITE (out_unit, fmt_diff)  &
                left_loc%ToString(), right_loc%ToString(),  &
                'The left set has an unnamed /'  &
                // GetPartName(left_item%GetPart())  &
                // '/, the right set does not.'
          ELSE
            WRITE (out_unit, fmt_diff)  &
                left_loc%ToString(), right_loc%ToString(),  &
                'The left set has a /' // GetPartName(left_item%GetPart())  &
                // '/ named "' // unit_name // '", the right set does not.'
          END IF
          CYCLE
        END IF
        
        ! Get a reference to the relevant right hand program unit as a 
        ! MainProgramStNode object.
        SELECT TYPE (right_item => right%prog_units(ir)%item)
        TYPE IS (BlockDataStNode)
          p_blockdata => right_item
        CLASS DEFAULT
          STOP 'In DiffImplementation::compare_programs, a block data &
              &wasn''t a block data.'
        END SELECT
        CALL compare_progunitlike( left_item, p_blockdata, out_unit,  &
            err_unit, diff )
        considered(ir) = .TRUE.
        
      !-------------------------------------------------------------------------
      TYPE IS (ModuleStNode)
        CALL left_item%GetName(unit_name)
        CALL find_module(right, unit_name, ir, p_module)
        IF (ir == 0) THEN
          diff = diff + 1
          left_loc = left_item%GetLocation()
          right_loc = SourceLocation()
          
          WRITE (out_unit, fmt_diff)  &
              left_loc%ToString(), right_loc%ToString(),  &
              'The left set has a /' // GetPartName(left_item%GetPart())  &
              // '/ named "' // unit_name // '", the right set does not.'
          CYCLE
        END IF
        CALL compare_modbase(left_item, p_module, out_unit, err_unit, diff)
        considered(ir) = .TRUE.
        
      !-------------------------------------------------------------------------
      TYPE IS (SubmoduleStNode)
        CALL left_item%GetName(unit_name)
        CALL find_submodule(right, unit_name, ir, p_submodule)
        IF (ir == 0) THEN
          diff = diff + 1
          left_loc = left_item%GetLocation()
          right_loc = SourceLocation()
          
          WRITE (out_unit, fmt_diff)  &
              left_loc%ToString(), right_loc%ToString(),  &
              'The left set has a /' // GetPartName(left_item%GetPart())  &
              // '/ named "' // unit_name // '", the right set does not.'
          CYCLE
        END IF
        CALL compare_modbase(left_item, p_module, out_unit, err_unit, diff)
        considered(ir) = .TRUE.
        
      !-------------------------------------------------------------------------
      CLASS DEFAULT
        WRITE (err_unit, "('The unknown program unit was a ',A,'.')")  &
            GetPartName(left_item%GetPart())
        STOP 'In DiffImplementation::compare_programs, unknown program unit.'
        
      END SELECT
    END DO
    
    !---------------------------------------------------------------------------
    ! Check that all program units on the right have been accounted for.
    
    CALL compare_programs_leftovers( right%prog_units, considered,  &
        out_unit, diff )
    
  END SUBROUTINE compare_programs
  
  
  !*****************************************************************************
  !!
  !> Report program units from the right set that were not in the left 
  !! set.
  !!
  !! @param[in]     right             The right set program.
  !!
  !! @param[in]     considered        Flags for each program unit in the 
  !! right set that indicate whether it has been previously compared.
  !!
  !! @param[in]     out_unit          Logical unit connected for formatted 
  !! output to use for reporting the results of the comparison.
  !!
  !! @param[in,out] diff              Set to true if a difference was 
  !! detected.  Not changed if no difference was detected.
  
  SUBROUTINE compare_programs_leftovers(right, considered, out_unit, diff)
    
    USE ProgUnitStNodes
    USE SourceLocations
    USE SyntaxParts
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(ProgUnitVector), INTENT(IN) :: right(:)
    LOGICAL, INTENT(IN) :: considered(:)
    INTEGER, INTENT(IN) :: out_unit
    INTEGER, INTENT(INOUT) :: diff
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: ir             ! Right index.
    
    ! Syntax part name of the left over part.
    CHARACTER(:), ALLOCATABLE :: part_name
    
    ! name of the left over program unit.
    CHARACTER(:), ALLOCATABLE :: unit_name
    
    ! Left set location for a difference.
    TYPE(SourceLocation) :: left_loc
    
    ! Right set location for a difference.
    TYPE(SourceLocation) :: right_loc
    
    !***************************************************************************
    
    DO ir = 1, SIZE(considered)
      IF (considered(ir)) CYCLE
      
      diff = diff + 1
      part_name = GetPartName(right(ir)%item%GetPart())
      CALL right(ir)%item%GetName(unit_name)
      left_loc = SourceLocation()
      right_loc = right(ir)%item%GetLocation()
        
      IF (LEN(unit_name) == 0) THEN
        ! Should be main programs or block data only.
        WRITE (out_unit, fmt_diff)  &
            left_loc%ToString(), right_loc%ToString(),  &
            'The right set has an unnamed /' // part_name  &
            // '/ not in the left set.'
      ELSE
        WRITE (out_unit, fmt_diff)  &
            left_loc%ToString(), right_loc%ToString(),  &
            'The right set has a /' // part_name  &
            // '/ named "' // unit_name // '" not in the left set.'
      END IF
    END DO
    
  END SUBROUTINE compare_programs_leftovers
  
  
  !*****************************************************************************
  !!
  !> Compare an main program unit.
  !!
  !! @param[in]     left              The left main program.
  !!
  !! @param[in]     right             The right main program.
  !!
  !! @param[in]     out_unit          Logical unit connected for formatted 
  !! output to use for reporting the results of the comparison.
  !!
  !! @param[in]     err_unit          Logical unit connected for formatted 
  !! putput to use for reporting errors that prevent the comparison.
  !!
  !! @param[in,out] diff              Set to 1 if a difference was 
  !! detected, -1 if an error occurred, otherwise not changed.
  
  SUBROUTINE compare_main_programs(left, right, out_unit, err_unit, diff)
    
    USE ProgUnitStNodes
    USE SourceLocations
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(MainProgramStNode), INTENT(IN) :: left
    TYPE(MainProgramStNode), INTENT(IN) :: right
    INTEGER, INTENT(IN) :: out_unit
    INTEGER, INTENT(IN) :: err_unit
    INTEGER, INTENT(INOUT) :: diff
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    CHARACTER(:), ALLOCATABLE :: left_name
    CHARACTER(:), ALLOCATABLE :: right_name
    
    ! Left set location for a difference.
    TYPE(SourceLocation) :: left_loc
    
    ! Right set location for a difference.
    TYPE(SourceLocation) :: right_loc
    
    !***************************************************************************
    
    ! Is the program unit name the same?
    CALL left%GetName(left_name)
    CALL right%GetName(right_name)
    IF (left_name /= right_name) THEN
      diff = diff + 1
      left_loc = left%GetLocation()
      right_loc = right%GetLocation()
      
      IF (LEN(left_name) == 0) THEN
        WRITE (out_unit, fmt_diff)  &
            left_loc%ToString(), right_loc%ToString(),  &
            'The /main-program/ in the left set was unnamed, the &
            &/main-program/ in the right set was named "'  &
            // right_name // '".'
      ELSE IF (LEN(right_name) == 0) THEN
        WRITE (out_unit, fmt_diff)  &
            left_loc%ToString(), right_loc%ToString(),  &
            'The /main-program/ in the left set  was named "'  &
            // left_name // '", the /main-program/ in the right set was &
            &unnamed.'
      ELSE
        WRITE (out_unit, fmt_diff)  &
            left_loc%ToString(), right_loc%ToString(),  &
            'The /main-program/ in the left set was named "'  &
            // left_name  &
            // '", the /main-program/ in the right set was named "'  &
            // right_name // '".'
      END IF
    ELSE
      ! Compare tokens for the initial statement.  If one of the two 
      ! is has no initial statement and the other dos, then that will 
      ! result in a name difference tested and reported above.  If both 
      ! have no name statement then they are the same anyway.
      IF (ALLOCATED(left%first) .AND. ALLOCATED(right%first)) THEN
        CALL compare_stdata(left%first, right%first, out_unit, diff)
      END IF
    END IF
      
    CALL compare_exec_units(left, right, out_unit, err_unit, diff)
    
  END SUBROUTINE compare_main_programs
  
  
  !*****************************************************************************
  !!
  !> Compare two subprograms.
  !!
  !! @param[in]     left              The left main program.
  !!
  !! @param[in]     right             The right main program.
  !!
  !! @param[in]     out_unit          Logical unit connected for formatted 
  !! output to use for reporting the results of the comparison.
  !!
  !! @param[in]     err_unit          Logical unit connected for formatted 
  !! putput to use for reporting errors that prevent the comparison.
  !!
  !! @param[in,out] diff              Set to 1 if a difference was 
  !! detected, -1 if an error occurred, otherwise not changed.
  
  ! Recursive because external subprograms can have internal subprograms.
  RECURSIVE SUBROUTINE compare_subprograms( left, right, out_unit,  &
      err_unit, diff )
    
    USE ProgUnitStNodes
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(ExecUnitStNode), INTENT(IN) :: left
    CLASS(ExecUnitStNode), INTENT(IN) :: right
    INTEGER, INTENT(IN) :: out_unit
    INTEGER, INTENT(IN) :: err_unit
    INTEGER, INTENT(INOUT) :: diff
    
    !***************************************************************************
    
    CALL compare_stdata(left%first, right%first, out_unit, diff)
    
    CALL compare_exec_units(left, right, out_unit, err_unit, diff)
    
  END SUBROUTINE compare_subprograms
  
  
  !*****************************************************************************
  !!
  !> Compare two modules or submodules.
  !!
  !! @param[in]     left              The left module or submodule.
  !!
  !! @param[in]     right             The right module or submodule.
  !!
  !! @param[in]     out_unit          Logical unit connected for formatted 
  !! output to use for reporting the results of the comparison.
  !!
  !! @param[in]     err_unit          Logical unit connected for formatted 
  !! putput to use for reporting errors that prevent the comparison.
  !!
  !! @param[in,out] diff              Set to 1 if a difference was 
  !! detected, -1 if an error occurred, otherwise not changed.
  
  ! Recursive possibly because we typed the keyword in the wrong place.
  RECURSIVE SUBROUTINE compare_modbase( left, right, out_unit,  &
      err_unit, diff )
    
    USE ModuleStNodes
    USE SourceLocations
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(ModBaseStNode), INTENT(IN) :: left
    CLASS(ModBaseStNode), INTENT(IN) :: right
    INTEGER, INTENT(IN) :: out_unit
    INTEGER, INTENT(IN) :: err_unit
    INTEGER, INTENT(INOUT) :: diff
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Left set location for a difference.
    TYPE(SourceLocation) :: left_loc
    
    ! Right set location for a difference.
    TYPE(SourceLocation) :: right_loc
    
    !***************************************************************************
    
    !---------------------------------------------------------------------------
    ! Module or submodule statement.
    
    CALL compare_stdata(left%first, right%first, out_unit, diff)
    
    !---------------------------------------------------------------------------
    ! Specification part.
    
    CALL compare_progunitlike_spec(left, right, out_unit, err_unit, diff)
    
    !---------------------------------------------------------------------------
    ! Module procedures
    
    IF (ALLOCATED(left%mod_procs) .AND. ALLOCATED(right%mod_procs)) THEN
      CALL compare_internals( left%mod_procs, right%mod_procs, out_unit,  &
          err_unit, diff )
    ELSE IF (ALLOCATED(left%mod_procs)) THEN
      diff = diff + 1
      ! For the missing part we report the location as the unit itself.
      left_loc = left%mod_procs%GetLocation()
      right_loc = right%GetLocation()
      
      WRITE (out_unit, fmt_diff)  &
          left_loc%ToString(), right_loc%ToString(),  &
          'The left set has a /module-subprogram-part/, &
          &the right set does not.'
    ELSE IF (ALLOCATED(right%mod_procs)) THEN
      diff = diff + 1
      ! For the missing part we report the location as the unit itself.
      left_loc = left%GetLocation()
      right_loc = right%mod_procs%GetLocation()
      
      WRITE (out_unit, fmt_diff)  &
          left_loc%ToString(), right_loc%ToString(),  &
          'The left set has no /module-subprogram-part/, the right set does.'
    
    ! ELSE case is that both units have no internal subprogram part.
    END IF
    
    !---------------------------------------------------------------------------
    ! Last statement.
    
    CALL compare_stdata(left%last, right%last, out_unit, diff)
    
  END SUBROUTINE compare_modbase
  
  
  !*****************************************************************************
  !!
  !> Compare an Exec program unit.  This excludes the first statement of 
  !! the unit.
  !!
  !! We exclude the first statement because we want special handling for 
  !! the first statement of a main program.
  !!
  !! @param[in]     left              The left object.
  !!
  !! @param[in]     right             The right object.
  !!
  !! @param[in]     out_unit          Logical unit connected for formatted 
  !! output to use for reporting the results of the comparison.
  !!
  !! @param[in]     err_unit          Logical unit connected for formatted 
  !! putput to use for reporting errors that prevent the comparison.
  !!
  !! @param[in,out] diff              Incremented if a difference was 
  !! detected, -1 if an error occurred, otherwise not changed.
  !!
  !! This is also used for subprograms that have execution parts (which is 
  !! all subprograms).
  
  ! Recursive because main programs and external subprograms and module 
  ! subprograms can have internal subprograms.
  RECURSIVE SUBROUTINE compare_exec_units( left, right, out_unit,  &
      err_unit, diff )
    
    USE ProgUnitStNodes
    USE SourceLocations
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(ExecUnitStNode), INTENT(IN) :: left
    CLASS(ExecUnitStNode), INTENT(IN) :: right
    INTEGER, INTENT(IN) :: out_unit
    INTEGER, INTENT(IN) :: err_unit
    INTEGER, INTENT(INOUT) :: diff
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Left set location for a difference.
    TYPE(SourceLocation) :: left_loc
    
    ! Right set location for a difference.
    TYPE(SourceLocation) :: right_loc
    
    !***************************************************************************
    
    ! Specification part.
    CALL compare_progunitlike_spec(left, right, out_unit, err_unit, diff)
    
    !---------------------------------------------------------------------------
    ! Execution part.
    
    IF (has_exec(left) .AND. has_exec(right)) THEN
      CALL compare_stnodes(left%exec, right%exec, out_unit, diff)
    ELSE IF (has_exec(left)) THEN
      diff = diff + 1
      ! For the missing part we report the location as the unit itself.
      left_loc = left%exec%GetLocation()
      right_loc = right%GetLocation()
      
      WRITE (out_unit, fmt_diff)  &
          left_loc%ToString(), right_loc%ToString(),  &
          'The left set has an /execution-part/, the right set does not.'
    ELSE IF (has_exec(right)) THEN
      diff = diff + 1
      ! For the missing part we report the location as the unit itself.
      left_loc = left%GetLocation()
      right_loc = right%exec%GetLocation()
      
      WRITE (out_unit, fmt_diff)  &
          left_loc%ToString(), right_loc%ToString(),  &
          'The left set has no /execution-part/, the right set does.'
    
    ! ELSE case is that both units have no execution part.
    END IF
    
    !---------------------------------------------------------------------------
    ! Internal subprogram part.
    
    IF (ALLOCATED(left%internal) .AND. ALLOCATED(right%internal)) THEN
      CALL compare_internals( left%internal, right%internal, out_unit,  &
          err_unit, diff )
    ELSE IF (ALLOCATED(left%internal)) THEN
      diff = diff + 1
      ! For the missing part we report the location as the unit itself.
      left_loc = left%internal%GetLocation()
      right_loc = right%GetLocation()
      
      WRITE (out_unit, fmt_diff)  &
          left_loc%ToString(), right_loc%ToString(),  &
          'The left set has an /internal-subprogram-part/, &
          &the right set does not.'
    ELSE IF (ALLOCATED(right%internal)) THEN
      diff = diff + 1
      ! For the missing part we report the location as the unit itself.
      left_loc = left%GetLocation()
      right_loc = right%internal%GetLocation()
      
      WRITE (out_unit, fmt_diff)  &
          left_loc%ToString(), right_loc%ToString(),  &
          'The left set has no /internal-subprogram-part/, &
          &the right set does.'
    
    ! ELSE case is that both units have no internal subprogram part.
    END IF
    
    !---------------------------------------------------------------------------
    
    ! Last statement.
    CALL compare_stdata(left%last, right%last, out_unit, diff)
    
  END SUBROUTINE compare_exec_units
  
  
  !*****************************************************************************
  !!
  !> Compare two specification parts.
  !!
  !! @param[in]     left              The left object.
  !!
  !! @param[in]     right             The right object.
  !!
  !! @param[in]     out_unit          Logical unit connected for formatted 
  !! output for reporting differences.
  !!
  !! @param[in]     err_unit          Logical unit connected for formatted 
  !! putput to use for reporting errors that prevent the comparison.
  !!
  !! @param[in,out] diff              Incremented if a difference was 
  !! detected, -1 if an error occurred, otherwise not changed.
  
  ! Recursive because a specification part can have an interface body that 
  ! has a specification part.
  RECURSIVE SUBROUTINE compare_specification_parts( left, right, out_unit,  &
      err_unit, diff )
    
    USE CharUtils
    USE SingleStNodes
    USE EnumDefStNodes
    USE DerivedTypeDefStNodes
    USE InterfaceStNodes
    USE SourceLocations
    USE SpecStNodes
    USE Statements
    USE SyntaxParts
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(SpecStNode), INTENT(IN) :: left
    TYPE(SpecStNode), INTENT(IN) :: right
    INTEGER, INTENT(IN) :: out_unit
    INTEGER, INTENT(IN) :: err_unit
    INTEGER, INTENT(INOUT) :: diff
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: left_last      ! Index of the last use in the left set.
    INTEGER :: right_last     ! Index of the last use in the right set.
    INTEGER :: i              ! Node or statement index.
    INTEGER :: il             ! Node or statement index - left set.
    INTEGER :: ir             ! Node or statement index - right set.
    
    ! Left set location for a difference.
    TYPE(SourceLocation) :: left_loc
    
    ! Right set location for a difference.
    TYPE(SourceLocation) :: right_loc
    
    !***************************************************************************
    
    !---------------------------------------------------------------------------
    ! USE statements.
    !
    ! The ordering of use statements is not significant.  We should consider 
    ! that here.  But we don't, yet.
    
    ! Find the last use statement in the left.
    left_last = 0
    DO i = 1, SIZE(left%use_stmts)
      IF (left%use_stmts(i)%ist /= istUse) THEN
        left_last = i - 1
        EXIT
      END IF
    END DO
    
    ! Find the last use statement in the right.
    right_last = 0
    DO i = 1, SIZE(right%use_stmts)
      IF (right%use_stmts(i)%ist /= istUse) THEN
        right_last = i - 1
        EXIT
      END IF
    END DO
    
    ! Compare the use statements.
    DO i = 1, MIN(left_last, right_last)
      CALL compare_stdata(  &
          left%use_stmts(i),  &
          right%use_stmts(i),  &
          out_unit, diff )
    END DO
    
    ! Handle unequal number of use statements.
    IF (left_last /= right_last) THEN
      diff = diff + 1
      ! Report against the last common use or against the specification 
      ! part if there is no such thing.
      IF (i > 1) THEN
        left_loc = QueryLocation(left%use_stmts(i-1)%tlist(1))
        right_loc = QueryLocation(right%use_stmts(i-1)%tlist(1))
      ELSE
        left_loc = left%GetLocation()
        right_loc = right%GetLocation()
      END IF
      
      WRITE (out_unit, fmt_diff)  &
          left_loc%ToString(), right_loc%ToString(),  &
          'The number of /use-stmt/''s in the /specification-part/ &
          &of the left set is ' // ToString(left_last)  &
          // ', the number in the right set is '  &
          // ToString(right_last) // '.'
    END IF
    
    !---------------------------------------------------------------------------
    ! IMPORT statements.
    !
    ! These are stored in the same data structure as the use statements, 
    ! because we were lazy.  Like use statements, their ordering is not 
    ! significant, but we don't take that into account yet.
    
    ! Starting point depends on where we finished with the particular set 
    ! with respect to use statements.
    il = left_last + 1
    ir = right_last + 1
    DO
      IF ((il > SIZE(left%use_stmts)) .OR. (ir > SIZE(right%use_stmts))) EXIT
      
      CALL compare_stdata( left%use_stmts(il), right%use_stmts(ir),  &
          out_unit, diff )
      
      il = il + 1
      ir = ir + 1
    END DO
    
    ! Check number of import statements is the same.
    IF ( SIZE(left%use_stmts) - left_last  &
        /= SIZE(right%use_stmts) - right_last ) THEN
      diff = diff + 1
      ! Report against the last common import statement or last use statement, 
      ! of if there is no such thing, against the specification part itself.
      !
      ! This seems reasonable.
      IF (il > 1) THEN
        left_loc = QueryLocation(left%use_stmts(il-1)%tlist(1))
      ELSE
        left_loc = left%GetLocation()
      END IF
      IF (ir > 1) THEN
        right_loc = QueryLocation(right%use_stmts(ir-1)%tlist(1))
      ELSE
        right_loc = right%GetLocation()
      END IF
      
      WRITE (out_unit, fmt_diff)  &
          left_loc%ToString(), right_loc%ToString(),  &
          'The number of /import-stmt/''s in the /specification part/ &
          &of the left set is '  &
          // ToString(SIZE(left%use_stmts) - left_last)  &
          // ', the number in the right set is '  &
          // ToString(SIZE(right%use_stmts) - right_last) // '.'
    END IF
    
    !---------------------------------------------------------------------------
    ! implicit part stuff.
    !
    ! The ordering of statements in this part can be significant from the 
    ! point of view of program correctness - though the ordering of any format 
    ! statements in any part of the program is not significant (which includes 
    ! statements that are moved from execution part to specification part, 
    ! which would be tricky for use to correctly handle).
    !
    ! An example of program correctness is a parameter statement that defines 
    ! a constant that then has an implicit statement that sets a different 
    ! implicit typing for the first letter of the parameter from the default.
    
    DO i = 1, MIN(  &
        SIZE(left%implicit_part_nodes),  &
        SIZE(right%implicit_part_nodes) )
      IF ( left%implicit_part_nodes(i)%item%GetPart()  &
          /= right%implicit_part_nodes(i)%item%GetPart() ) THEN
        diff = diff + 1
        left_loc = left%implicit_part_nodes(i)%item%GetLocation()
        right_loc = right%implicit_part_nodes(i)%item%GetLocation()
        
        WRITE (out_unit, fmt_diff)  &
            left_loc%ToString(), right_loc%ToString(),  &
            'A /implicit-part-stmt/ in the left set is a /'  &
            // GetPartName(left%implicit_part_nodes(i)%item%GetPart())  &
            // '/, the statement in the right set is a /' // &
            GetPartName(right%implicit_part_nodes(i)%item%GetPart()) // '/.'
        CYCLE
      END IF
      
      SELECT TYPE (left_item => left%implicit_part_nodes(i)%item)
      TYPE IS (SingleStNode)
        SELECT TYPE (right_item => right%implicit_part_nodes(i)%item)
        TYPE IS (SingleStNode)
          CALL compare_stdata(  &
              left_item%stmt,  &
              right_item%stmt,  &
              out_unit,  &
              diff )
        END SELECT
        
      CLASS DEFAULT
        WRITE (out_unit, "('The unknown stnode is for a ',A)")  &
            GetPartName(left_item%GetPart())
        STOP 'In DiffImplementation::compare_specification_parts, an &
            &unknown implicit part StNode was encountered.'
      END SELECT
    END DO
    
    IF (SIZE(left%implicit_part_nodes) /= SIZE(right%implicit_part_nodes)) THEN
      diff = diff + 1
      IF (i > 1) THEN
        left_loc = left%implicit_part_nodes(i-1)%item%GetLocation()
        right_loc = right%implicit_part_nodes(i-1)%item%GetLocation()
      ELSE
        left_loc = left%GetLocation()
        right_loc = right%GetLocation()
      END IF
      
      WRITE (out_unit, fmt_diff)  &
          left_loc%ToString(), right_loc%ToString(),  &
          'The number of /implicit-part-stmt/''s in the /specification-part/ &
          &of the left set is ' // ToString(SIZE(left%implicit_part_nodes))  &
          // ', the number in the right set is '  &
          // ToString(SIZE(right%implicit_part_nodes)) // '.'
    END IF
    
    !---------------------------------------------------------------------------
    ! General specification constructs.
    !
    ! Format statements can be hidden in here - see comments above about 
    ! the significance of their ordering.
    
    DO i = 1, MIN(SIZE(left%children), SIZE(right%children))
      ! Things that are the same part have to have the same underlying 
      ! node type to store them, but things that have the same underlying 
      ! node type do not have to represent the same part.
      IF ( left%children(i)%item%GetPart()  &
          /= right%children(i)%item%GetPart() ) THEN
        diff = diff + 1
        left_loc = left%children(i)%item%GetLocation()
        right_loc = right%children(i)%item%GetLocation()
        
        WRITE (out_unit, fmt_diff)  &
            left_loc%ToString(), right_loc%ToString(),  &
            'A /declaration-construct/ in the left set is a /'  &
            // GetPartName(left%children(i)%item%GetPart())  &
            // '/, the construct in the right set is a /' // &
            GetPartName(right%children(i)%item%GetPart()) // '/.'
        CYCLE
      END IF
      
      SELECT TYPE (left_item => left%children(i)%item)
      TYPE IS (SingleStNode)
        SELECT TYPE (right_item => right%children(i)%item)
        TYPE IS (SingleStNode)
          CALL compare_stdata(left_item%stmt, right_item%stmt, out_unit, diff)
        END SELECT
        
      TYPE IS (InterfaceStNode)
        SELECT TYPE (right_item => right%children(i)%item)
        TYPE IS (InterfaceStNode)
          CALL compare_interface_block( left_item, right_item, out_unit,  &
              err_unit, diff)
        END SELECT
        
      TYPE IS (DerivedTypeDefStNode)
        SELECT TYPE (right_item => right%children(i)%item)
        TYPE IS (DerivedTypeDefStNode)
          ! No special handling yet.
          CALL compare_stnodes(left_item, right_item, out_unit, diff)
        END SELECT
        
      TYPE IS (EnumDefStNode)
        SELECT TYPE (right_item => right%children(i)%item)
        TYPE IS (EnumDefStNode)
          CALL compare_enumdef(left_item, right_item, out_unit, diff)
        END SELECT
        
      CLASS DEFAULT
        WRITE (out_unit, "('The unknown stnode is for a ',A)")  &
            GetPartName(left_item%GetPart())
        STOP 'In DiffImplementation::compare_specification_parts, an &
            &unknown StNode was encountered.'
      END SELECT
    END DO
    
    ! Check total number of children is the same.
    IF (SIZE(left%children) /= SIZE(right%children)) THEN
      diff = diff + 1
      ! Try and report against the last common child, but if there is no 
      ! such thing then we just report against the specification part itself.
      !
      ! The latter could probably be improved - we could try and report 
      ! against the last statement from the previous sections.
      IF (i > 1) THEN
        left_loc = left%children(i-1)%item%GetLocation()
        right_loc = right%children(i-1)%item%GetLocation()
      ELSE
        left_loc = left%GetLocation()
        right_loc = right%GetLocation()
      END IF
      
      WRITE (out_unit, fmt_diff)  &
          left_loc%ToString(), right_loc%ToString(),  &
          'The number of /declaration-construct/''s in the &
          &/specification-part/ of the left set is '  &
          // ToString(SIZE(left%children))  &
          // ', the number in the right set is '  &
          // ToString(SIZE(right%children)) // '.'
    END IF
    
  END SUBROUTINE compare_specification_parts
  
  
  !*****************************************************************************
  !!
  !> Compare two lists of internal or module subprograms.
  !!
  !! @param[in]     left              The left object.
  !!
  !! @param[in]     right             The right object.
  !!
  !! @param[in]     out_unit          Logical unit connected for formatted 
  !! output for reporting differences.
  !!
  !! @param[in]     err_unit          Logical unit connected for formatted 
  !! putput to use for reporting errors that prevent the comparison.
  !!
  !! @param[in,out] diff              Incremented if a difference was 
  !! detected, -1 if an error occurred, otherwise not changed.
  
  ! Recursive because this is called to handle module subprograms, which 
  ! may themselves have internal subprograms.
  RECURSIVE SUBROUTINE compare_internals(left, right, out_unit, err_unit, diff)
    
    USE ProgUnitStNodes
    USE SourceLocations
    USE SyntaxParts
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(InternalSubprogramPartStNode), INTENT(IN) :: left
    CLASS(InternalSubprogramPartStNode), INTENT(IN) :: right
    INTEGER, INTENT(IN) :: out_unit
    INTEGER, INTENT(IN) :: err_unit
    INTEGER, INTENT(INOUT) :: diff
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Whether the subprograms in the right set have been considered.
    LOGICAL :: considered(SIZE(right%subprograms))
    
    INTEGER :: il             ! Left index.
    INTEGER :: ir             ! Right index.
    
    ! Pointers to type specific subrpogram objects, used for further type 
    ! specific comparison.
    TYPE(SubroutineSubprogramStNode), POINTER :: p_subroutine
    TYPE(FunctionSubprogramStNode), POINTER :: p_function
    TYPE(SeparateModuleSubprogramStNode), POINTER :: p_procedure
    
    ! Name of the program unit being considered.
    CHARACTER(:), ALLOCATABLE :: unit_name
    
    ! Left set location for a difference.
    TYPE(SourceLocation) :: left_loc
    
    ! Right set location for a difference.
    TYPE(SourceLocation) :: right_loc
    
    !***************************************************************************
    
    CALL compare_stdata(left%contains, right%contains, out_unit, diff)
    
    !---------------------------------------------------------------------------
    ! Check right program unit names can be determined.  Doing this here in 
    ! one hit simplifies searching for program units based on name.
    
    DO ir = 1, SIZE(right%subprograms)
      CALL right%subprograms(ir)%item%GetName(unit_name)
      IF (.NOT. ALLOCATED(unit_name)) THEN
        diff = -1
        right_loc = right%subprograms(ir)%item%GetLocation()
        WRITE ( err_unit, "('The right set has a program unit at ',A,&
              &' with a name that can not be determinined.')" )  &
            right_loc%ToString()
        RETURN
      END IF
    END DO
    
    !---------------------------------------------------------------------------
    ! Run through each subprogram on the left, checking to see whether we 
    ! can find a match (positioning not relevant) on the right.
    
    ! Flag all subprograms as not considered.
    considered = .FALSE.
    
    DO il = 1, SIZE(left%subprograms)
      CALL left%subprograms(il)%item%GetName(unit_name)
      IF (.NOT. ALLOCATED(unit_name)) THEN
        diff = -1
        left_loc = left%subprograms(il)%item%GetLocation()
        WRITE ( err_unit, "('The left set has a subprogram at ',A,&
              &' with a name that can not be determinined.')" )  &
            left_loc%ToString()
        RETURN
      END IF
      
      SELECT TYPE (left_item => left%subprograms(il)%item)
      !-------------------------------------------------------------------------
      TYPE IS (SubroutineSubprogramStNode)
        CALL left_item%GetName(unit_name)
        CALL find_subroutine(right%subprograms, unit_name, ir, p_subroutine)
        IF (ir == 0) THEN
          diff = diff + 1
          left_loc = left_item%GetLocation()
          right_loc = SourceLocation()
          
          WRITE (out_unit, fmt_diff)  &
              left_loc%ToString(), right_loc%ToString(),  &
              'The left set has a /' // GetPartName(left_item%GetPart())  &
              // '/ named "' // unit_name // '", the right set does not.'
          CYCLE
        END IF
        CALL compare_subprograms( left_item, p_subroutine, out_unit,  &
            err_unit, diff )
        considered(ir) = .TRUE.
        
      !-------------------------------------------------------------------------
      TYPE IS (FunctionSubprogramStNode)
        CALL left_item%GetName(unit_name)
        CALL find_function(right%subprograms, unit_name, ir, p_function)
        IF (ir == 0) THEN
          diff = diff + 1
          left_loc = left_item%GetLocation()
          right_loc = SourceLocation()
          
          WRITE (out_unit, fmt_diff)  &
              left_loc%ToString(), right_loc%ToString(),  &
              'The left set has a /' // GetPartName(left_item%GetPart())  &
              // '/ named "' // unit_name // '", the right set does not.'
          CYCLE
        END IF
        CALL compare_subprograms( left_item, p_function, out_unit,  &
            err_unit, diff )
        considered(ir) = .TRUE.
        
      !-------------------------------------------------------------------------
      TYPE IS (SeparateModuleSubprogramStNode)
        CALL left_item%GetName(unit_name)
        CALL find_separateprocedure( right%subprograms, unit_name, ir,  &
            p_procedure )
        IF (ir == 0) THEN
          diff = diff + 1
          left_loc = left_item%GetLocation()
          right_loc = SourceLocation()
          
          WRITE (out_unit, fmt_diff)  &
              left_loc%ToString(), right_loc%ToString(),  &
              'The left set has a /' // GetPartName(left_item%GetPart())  &
              // '/ named "' // unit_name // '", the right set does not.'
          CYCLE
        END IF
        CALL compare_subprograms( left_item, p_procedure, out_unit,  &
            err_unit, diff )
        considered(ir) = .TRUE.
        
      !-------------------------------------------------------------------------
      CLASS DEFAULT
        WRITE (err_unit, "('The unknown subprogram was a ',A,'.')")  &
            GetPartName(left_item%GetPart())
        STOP 'In DiffImplementation::compare_internals, unknown subprogram.'
      END SELECT
    END DO
    
    !---------------------------------------------------------------------------
    ! Check that all subprograms on the right have been considered.
    
    CALL compare_programs_leftovers( right%subprograms, considered,  &
        out_unit, diff )
    
  END SUBROUTINE compare_internals
  
  
  !*****************************************************************************
  !!
  !> Perform a comparison of two InterfaceStNode objects.
  !!
  !! @param[in]     left              The left object.
  !!
  !! @param[in]     right             The right object.
  !!
  !! @param[in]     out_unit          Logical unit connected for formatted 
  !! output for reporting differences.
  !!
  !! @param[in]     err_unit          Logical unit connected for formatted 
  !! output for reporting errors.
  !!
  !! @param[in,out] diff              Incremented if a difference was 
  !! detected, -1 if an error occurred, otherwise not changed.
  
  ! Recursive because an interface block may have an interface body that has 
  ! an interface block in its specification part.
  RECURSIVE SUBROUTINE compare_interface_block( left, right, out_unit,  &
      err_unit, diff )
    
    USE CharUtils
    USE InterfaceStNodes
    USE InterfaceBodyStNodes
    USE ProcedureStmtsUtils
    USE ProgUnitStNodes
    USE SingleStNodes
    USE SourceLocations
    USE Strings
    USE SyntaxParts
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(InterfaceStNode), INTENT(IN) :: left
    CLASS(InterfaceStNode), INTENT(IN) :: right
    INTEGER, INTENT(IN) :: out_unit
    INTEGER, INTENT(IN) :: err_unit
    INTEGER, INTENT(INOUT) :: diff
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: i              ! Child statement index.
    
    ! Left set location for a difference.
    TYPE(SourceLocation) :: left_loc
    
    ! Right set location for a difference.
    TYPE(SourceLocation) :: right_loc
    
    INTEGER :: ir             ! Corresponding item on the right.
    
    LOGICAL :: considered(SIZE(right%bodies))
    INTEGER :: left_equivalent(SIZE(right%bodies))
    
    ! Name of the item if it is an interface body.
    CHARACTER(:), ALLOCATABLE :: item_name
    
    ! Name of the syntax part with a problem to report.
    CHARACTER(:), ALLOCATABLE :: part_name
    
    ! Names of the procedures mentioned in a procedure statement.
    TYPE(String), ALLOCATABLE :: proc_names(:)
    
    !***************************************************************************
    
    !---------------------------------------------------------------------------
    ! Opening INTERFACE statement.
    
    CALL compare_stdata(left%first, right%first, out_unit, diff)
    
    !---------------------------------------------------------------------------
    ! The interface specifications.
    !
    ! Potentially the order of interface bodies is significant, if an 
    ! interface body imports a procedure defined in a previous body - see the 
    ! ordering requirement on /import-stmt/ for example.
    !
    ! The order of procedure statements is not significant, but we don't 
    ! deal with that yet.
    
    considered = .FALSE.
    left_equivalent = 0
    DO i = 1, SIZE(left%bodies)
      SELECT TYPE (left_item => left%bodies(i)%item)
      CLASS IS (InterfaceBodyStNode)
        ! See if the right hand side has a matching left hand side.
        CALL left_item%GetName(item_name)
        IF (.NOT. ALLOCATED(item_name)) THEN
          diff = -1
          left_loc = left_item%GetLocation()
          WRITE (err_unit, "('The left set has an interface body at ',A,&
                &' with a name that cannot be determined.')")  &
              .ToString. left_loc
          RETURN
        END IF
        CALL find_interface_body(right%bodies, item_name, considered, ir)
        IF (ir == 0) THEN
          left_loc = left_item%GetLocation()
          right_loc = SourceLocation()
          WRITE (out_unit, fmt_diff)  &
              .ToString. left_loc, .ToString. right_loc,  &
              'The left set has an interface body for a subprogram named "'  &
              // item_name // '", the right set does not.'
          CYCLE
        END IF
        considered(ir) = .TRUE.
        left_equivalent(ir) = i
        SELECT TYPE (right_item => right%bodies(ir)%item)
        CLASS IS (InterfaceBodyStNode)
          CALL compare_progunitlike( left_item, right_item, out_unit,  &
              err_unit, diff )
        END SELECT
        
      TYPE IS (SingleStNode)
        ! See if the right hand side has a matching left hand side.  The 
        ! comparison is done on the basis of the first name in the statement.
        CALL GetProcedureNames(left_item%stmt%tlist, proc_names)
        IF (.NOT. ALLOCATED(proc_names)) THEN
          diff = -1
          left_loc = left_item%GetLocation()
          WRITE (err_unit, "('The left set has an /procedure-stmt/ at ',A,&
                &' with a list of names that cannot be determined.')")  &
              .ToString. left_loc
          RETURN
        END IF
        CALL find_procedure_stmt( right%bodies, proc_names(1)%item, &
            considered, ir )
        IF (ir == 0) THEN
          diff = diff + 1
          left_loc = left_item%GetLocation()
          right_loc = SourceLocation()
          WRITE (out_unit, fmt_diff)  &
              .ToString. left_loc, .ToString. right_loc,  &
              'The left set has a procedure statement for procedures named "'  &
              // join_names(proc_names) // '", the right set does not.'
         CYCLE
        END IF
        considered(ir) = .TRUE.
        SELECT TYPE (right_item => right%bodies(ir)%item)
        TYPE IS (SingleStNode)
          CALL compare_stdata(left_item%stmt, right_item%stmt, out_unit, diff)
        END SELECT
        
      CLASS DEFAULT
        WRITE (out_unit, "('The unknown stnode is for a ',A)")  &
            GetPartName(left_item%GetPart())
        STOP 'In DiffImplementation::compare_interface_block, an &
            &unknown StNode was encountered.'
        
      END SELECT
    END DO
    
    ! Check all interface bodies in the right set were considered.
    DO ir = 1, SIZE(considered)
      IF (considered(ir)) CYCLE
      
      diff = diff + 1
      part_name = GetPartName(right%bodies(ir)%item%GetPart())
      
      SELECT TYPE (right_item => right%bodies(ir)%item)
      CLASS IS (InterfaceBodyStNode)
        CALL right_item%GetName(item_name)
        IF (.NOT. ALLOCATED(item_name)) THEN
          diff = -1
          right_loc = right_item%GetLocation()
          WRITE ( err_unit, "('The right set has an /interface-body/ &
                &at ',A,' whose name could not be determined.')" )  &
              .ToString. right_loc
          RETURN
        END IF
        left_loc = SourceLocation()
        right_loc = right_item%GetLocation()
        WRITE (out_unit, fmt_diff)  &
            .ToString. left_loc,  &
            .ToString. right_loc,  &
            'The right set has a /' // part_name  &
            // '/ named "' // item_name // '" not in the left set.'
      CLASS IS (SingleStNode)
        CALL GetProcedureNames(right_item%stmt%tlist, proc_names)
        IF (.NOT. ALLOCATED(proc_names)) THEN
          diff = -1
          right_loc = right_item%GetLocation()
          WRITE ( err_unit, "('The right set has an /procedure-stmt/ &
                &at ',A,' for procedures whose names could not be &
                &determined.')" )  &
              .ToString. right_loc
          RETURN
        END IF
        left_loc = SourceLocation()
        right_loc = right_item%GetLocation()
        WRITE (out_unit, fmt_diff)  &
            .ToString. left_loc,  &
            .ToString. right_loc,  &
            'The right set has a /' // part_name  &
            // '/ for procedures named ' // join_names(proc_names)  &
            // ' not in the left set.'
      END SELECT
      
    END DO
    
    ! Check interface body order.
    i = 0
    DO ir = 1, SIZE(right%bodies)
      IF (left_equivalent(ir) /= 0) THEN
        IF (left_equivalent(ir) < i) THEN
          diff = diff + 1
          left_loc = left%bodies(left_equivalent(ir))%item%GetLocation()
          right_loc = right%bodies(ir)%GetLocation()
          WRITE (out_unit, fmt_diff)  &
              .ToString. left_loc,  &
              .ToString. right_loc,  &
              'The relative ordering of the interface bodies is different.'
        END IF
        i = left_equivalent(ir)
      END IF
    END DO
    
    !---------------------------------------------------------------------------
    ! Closing END INTERFACE statement.
    
    CALL compare_stdata(left%last, right%last, out_unit, diff)
    
  END SUBROUTINE compare_interface_block
  
  
  !*****************************************************************************
  !!
  !> Perform a comparison of two ProgUnitLikeStNode objects.
  !!
  !! @param[in]     left              The left object.
  !!
  !! @param[in]     right             The right object.
  !!
  !! @param[in]     out_unit          Logical unit connected for formatted 
  !! output for reporting differences.
  !!
  !! @param[in]     err_unit          Logical unit connected for formatted 
  !! output for reporting errors.
  !!
  !! @param[in,out] diff              Incremented if a difference was 
  !! detected, -1 if an error occurred, otherwise not changed.
  !!
  !! Use for block data and for function and subroutine interface bodies - 
  !! things that have an opening statement, a closing statement and a 
  !! specification part.
  
  ! Recursive because an interface body may have another interface body 
  ! in its specification part.
  RECURSIVE SUBROUTINE compare_progunitlike( left, right, out_unit,  &
      err_unit, diff )
    
    USE ProgUnitStNodes
    USE SourceLocations
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(ProgUnitLikeStNode), INTENT(IN) :: left
    CLASS(ProgUnitLikeStNode), INTENT(IN) :: right
    INTEGER, INTENT(IN) :: out_unit
    INTEGER, INTENT(IN) :: err_unit
    INTEGER, INTENT(INOUT) :: diff
    
    !***************************************************************************
    
    CALL compare_stdata(left%first, right%first, out_unit, diff)
    
    CALL compare_progunitlike_spec(left, right, out_unit, err_unit, diff)
    
    CALL compare_stdata(left%last, right%last, out_unit, diff)
    
  END SUBROUTINE compare_progunitlike
  
  
  !*****************************************************************************
  !!
  !> Perform a comparison of the specification part (only) of two 
  !! ProgUnitLikeStNode objects.
  !!
  !! @param[in]     left              The left object.
  !!
  !! @param[in]     right             The right object.
  !!
  !! @param[in]     out_unit          Logical unit connected for formatted 
  !! output for reporting differences.
  !!
  !! @param[in]     err_unit          Logical unit connected for formatted 
  !! output for reporting errors.
  !!
  !! @param[in,out] diff              Incremented if a difference was 
  !! detected, -1 if an error occurred, otherwise not changed.
  !!
  !! Called for any program unit or any subprogram that has a specification 
  !! part (which is all of them).
  
  ! Recursive because of interface bodies in specification parts.
  RECURSIVE SUBROUTINE compare_progunitlike_spec( left, right,  &
      out_unit, err_unit, diff )
    
    USE ProgUnitStNodes
    USE SourceLocations
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(ProgUnitLikeStNode), INTENT(IN) :: left
    CLASS(ProgUnitLikeStNode), INTENT(IN) :: right
    INTEGER, INTENT(IN) :: out_unit
    INTEGER, INTENT(IN) :: err_unit
    INTEGER, INTENT(INOUT) :: diff
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Left set location for a difference.
    TYPE(SourceLocation) :: left_loc
    
    ! Right set location for a difference.
    TYPE(SourceLocation) :: right_loc
    
    !***************************************************************************
    
    ! Use the has_spec test instead of just testing the ALLOCATED status to 
    ! work around the inability to deallocate an un-used specification part 
    ! in the build tree pass.
    
    IF (has_spec(left) .AND. has_spec(right)) THEN
      CALL compare_specification_parts( left%spec, right%spec,  &
          out_unit, err_unit, diff )
    ELSE IF (has_spec(left)) THEN
      diff = diff + 1
      left_loc = left%spec%GetLocation()
      right_loc = right%GetLocation()
      
      WRITE (out_unit, fmt_diff)  &
          left_loc%ToString(), right_loc%ToString(),  &
          'The left set has a /specification-part/, the right set does not.'
    ELSE IF (has_spec(right)) THEN
      diff = diff + 1
      left_loc = left%GetLocation()
      right_loc = right%spec%GetLocation()
      
      WRITE (out_unit, fmt_diff)  &
          left_loc%ToString(), right_loc%ToString(),  &
          'The left set has no /specification-part/, the right does.'
    ELSE
      ! Both objects have no specification part.
    END IF
    
  END SUBROUTINE compare_progunitlike_spec
  
  
  !*****************************************************************************
  !!
  !> Perform a comparison of two EnumDefStNode objects.
  !!
  !! @param[in]     left              The left object.
  !!
  !! @param[in]     right             The right object.
  !!
  !! @param[in]     out_unit          Logical unit connected for formatted 
  !! output for reporting differences.
  !!
  !! @param[in,out] diff              Incremented if a difference was 
  !! detected, -1 if an error occurred, otherwise not changed.
  
  SUBROUTINE compare_enumdef(left, right, out_unit, diff)
    
    USE CharUtils
    USE EnumDefStNodes
    USE SourceLocations
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(EnumDefStNode), INTENT(IN) :: left
    TYPE(EnumDefStNode), INTENT(IN) :: right
    INTEGER, INTENT(IN) :: out_unit
    INTEGER, INTENT(INOUT) :: diff
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: i              ! Child statement index.
    
    ! Left set location for a difference.
    TYPE(SourceLocation) :: left_loc
    
    ! Right set location for a difference.
    TYPE(SourceLocation) :: right_loc
    
    !***************************************************************************
    
    !---------------------------------------------------------------------------
    ! Opening statement
    
    CALL compare_stdata(left%first, right%first, out_unit, diff)
    
    !---------------------------------------------------------------------------
    ! Child definitions.
    
    DO i = 1, MIN(SIZE(left%defs), SIZE(right%defs))
      CALL compare_stdata(left%defs(i), right%defs(i), out_unit, diff)
    END DO
    
    IF (SIZE(left%defs) /= SIZE(right%defs)) THEN
      diff = diff + 1
      ! Report against the opening statement as it is always there.
      left_loc = QueryLocation(left%first%tlist(1))
      right_loc = QueryLocation(right%first%tlist(1))
      
      WRITE (out_unit, fmt_diff)  &
          left_loc%ToString(), right_loc%ToString(),  &
          'The number of /enumerator-def-stmt/''s in the /specification-part/ &
          &of the left set is ' // ToString(SIZE(left%defs))  &
          // ', the number in the right set is ' &
          // ToString(SIZE(right%defs)) // '.'
    END IF
    
    !---------------------------------------------------------------------------
    ! Closing statement.
    
    CALL compare_stdata(left%last, right%last, out_unit, diff)
    
  END SUBROUTINE compare_enumdef
  
  
  !*****************************************************************************
  !!
  !> Perform a comparison of two StNode objects.
  !!
  !! @param[in]     left              The left statement tree object.
  !!
  !! @param[in]     right             The right statement tree object.
  !!
  !! @param[in]     out_unit          Logical unit connected for formatted 
  !! output for reporting differences.
  !!
  !! @param[in,out] diff              Incremented if a difference was 
  !! detected, -1 if an error occurred, otherwise not changed.
  !!
  !! The comparison is done by flattening the statement tree and then 
  !! comparing each statement in turn.  Because this loses source structure 
  !! it should only be done when we think the user no longer cares about 
  !! having structural differences reported.
  
  SUBROUTINE compare_stnodes(left, right, out_unit, diff)
    
    USE SyntaxParts
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(StNode), INTENT(IN) :: left
    CLASS(StNode), INTENT(IN) :: right
    INTEGER, INTENT(IN) :: out_unit
    INTEGER, INTENT(INOUT) :: diff
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Object that carries out the statement tree flattening action.
    TYPE(flatten_visitor) :: visitor
    
    ! Statements from flattening the left tree.
    TYPE(StData), ALLOCATABLE :: left_stmts(:)
    
    ! Statements from flattening the right tree.
    TYPE(StData), ALLOCATABLE :: right_stmts(:)
    
    !***************************************************************************
    
    ! Flatten the left tree.
    CALL left%Query(visitor)
    left_stmts = visitor%stmts(:visitor%ilast)
    
    ! Reset the visitor, flatten the right tree.
    visitor = flatten_visitor()
    CALL right%Query(visitor)
    right_stmts = visitor%stmts(:visitor%ilast)
    
    ! Compare the two flattened statement lists.
    CALL compare_statement_vectors( left_stmts, right_stmts,  &
        GetPartName(left%GetPart()), out_unit, diff )
    
  END SUBROUTINE compare_stnodes
  
  
  !*****************************************************************************
  !!
  !> Perform a comparison of two arrays of StData objects (i.e. two arrays 
  !! of statements).
  !!
  !! @param[in]     left              The left array of statements.
  !!
  !! @param[in]     right             The right array of statements.
  !!
  !! @param[in]     part              Part name for the nodes for difference 
  !! reporting (we assume that both nodes represent the same part - if they 
  !! don't differencing is going to be rather noisy).
  !!
  !! @param[in]     out_unit          Logical unit connected for formatted 
  !! output for reporting differences.
  !!
  !! @param[in,out] diff              Incremented if a difference was 
  !! detected, -1 if an error occurred, otherwise not changed.
  
  SUBROUTINE compare_statement_vectors(left, right, part, out_unit, diff)
    
    USE CharUtils
    USE SourceLocations
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(StData), INTENT(IN) :: left(:)
    TYPE(StData), INTENT(IN) :: right(:)
    CHARACTER(*), INTENT(IN) :: part
    INTEGER, INTENT(IN) :: out_unit
    INTEGER, INTENT(INOUT) :: diff
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: i              ! Statement index.
    
    ! Left set location for a difference.
    TYPE(SourceLocation) :: left_loc
    
    ! Right set location for a difference.
    TYPE(SourceLocation) :: right_loc
    
    !***************************************************************************
    
    DO i = 1, MIN(SIZE(left), SIZE(right))
      CALL compare_stdata(left(i), right(i), out_unit, diff)
    END DO
    
    IF (SIZE(left) /= SIZE(right)) THEN
      diff = diff + 1
      IF (i > 1) THEN
        left_loc = QueryLocation(left(i-1)%tlist(1))
        right_loc = QueryLocation(right(i-1)%tlist(1))
      ELSE
        ! We shouldn't be being asked to compare a statement with an 
        ! empty statement anyway, but just in case.
        left_loc = SourceLocation()
        right_loc = SourceLocation()
      END IF
      
      WRITE (out_unit, fmt_diff)  &
          left_loc%ToString(), right_loc%ToString(),  &
          'The number of statements in the /' // part  &
          // '/ of the left set is ' // ToString(SIZE(left))  &
          // ', the number in the right set is ' &
          // ToString(SIZE(right)) //'.'
    END IF
    
  END SUBROUTINE compare_statement_vectors
  
  
  !*****************************************************************************
  !!
  !> Perform a comparison of two StData objects.
  !!
  !! @param[in]     left              The left statement data object.
  !!
  !! @param[in]     right             The right statement data object.
  !!
  !! @param[in]     out_unit          Logical unit connected for formatted 
  !! output for reporting differences.
  !!
  !! @param[in,out] diff              Incremented if a difference was 
  !! detected, -1 if an error occurred, otherwise not changed.
  
  SUBROUTINE compare_stdata(left, right, out_unit, diff)
    
    USE CharUtils
    USE SourceLocations
    USE StatementData
    USE Statements
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(StData), INTENT(IN) :: left
    TYPE(StData), INTENT(IN) :: right
    INTEGER, INTENT(IN) :: out_unit
    INTEGER, INTENT(INOUT) :: diff
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Left set location for a difference.
    TYPE(SourceLocation) :: left_loc
    
    ! Right set location for a difference.
    TYPE(SourceLocation) :: right_loc
    
    !***************************************************************************
    
    ! Compare labels.
    IF ( ALLOCATED(left%statement_label)  &
        .AND. (.NOT. ALLOCATED(right%statement_label)) ) THEN
      diff = diff + 1
      left_loc = .LocationOf. left%statement_label
      right_loc = QueryLocation(right%tlist(1))
      WRITE (out_unit, fmt_diff)  &
          left_loc%ToString(), right_loc%ToString(),  &
          'The statement label in the left set is '  &
          // ToString(.ValueOf. left%statement_label)  &
          // ', there is no corresponding statement label in the right set.'
    ELSE IF ( .NOT. ALLOCATED(left%statement_label)  &
        .AND. ALLOCATED(right%statement_label) ) THEN
      diff = diff + 1
      left_loc = QueryLocation(left%tlist(1))
      right_loc = .LocationOf. right%statement_label
      WRITE (out_unit, fmt_diff)  &
          left_loc%ToString(), right_loc%ToString(),  &
          'There is no statement label in the left set, the statement &
          &label in the right set is '  &
          // ToString(.ValueOf. right%statement_label) // '.'
    ELSE IF ( ALLOCATED(left%statement_label)  &
        .AND. ALLOCATED(right%statement_label)) THEN
      IF ( .ValueOf. left%statement_label  &
          /= .ValueOf. right%statement_label ) THEN
        diff = diff + 1
        left_loc = .LocationOf. left%statement_label
        right_loc = .LocationOf. right%statement_label
        WRITE (out_unit, fmt_diff)  &
            left_loc%ToString(), right_loc%ToString(),  &
            'The statement label in the left set is '  &
            // ToString(.ValueOf. left%statement_label)  &
            // ', the statement label in the right set is '  &
            // ToString(.ValueOf. right%statement_label) // '.'
      END IF
    END IF
    
    ! Compare statement types.
    IF (left%ist /= right%ist) THEN
      diff = diff + 1
      ! Report against first token in the statement.
      left_loc = QueryLocation(left%tlist(1))
      right_loc = QueryLocation(right%tlist(1))
      WRITE (out_unit, fmt_diff)  &
          left_loc%ToString(), right_loc%ToString(),  &
          'The left set has a /' // GetStName(left%ist)  &
          // '/, the right set has a /' // GetStName(right%ist) // '/.'
      RETURN    ! Fair assumption that token sequences will differ.
    END IF
    
    ! Compare token sequences.  This also implicitly compares construct 
    ! names, but at some stage we might want to look at those separately.
    CALL compare_tlists( left%tlist, right%tlist, GetStName(left%ist),  &
        out_unit, diff )
    
  END SUBROUTINE compare_stdata
  
  
  !*****************************************************************************
  !!
  !> Perform a brain-dead comparison of two token sequences.
  !!
  !! @param[in]     left              The left token sequence.  Must not be 
  !! zero sized.
  !!
  !! @param[in]     right             The right token sequence.  Must not be 
  !! zero sized.
  !!
  !! @param[in]     stmt_name         Syntax rule name for the statement - 
  !! for reporting differences.
  !!
  !! @param[in]     out_unit          Logical unit connected for formatted 
  !! output for reporting differences.
  !!
  !! @param[in,out] diff              Incremented if a difference was 
  !! detected, -1 if an error occurred, otherwise not changed.
  !!
  !! A zero sized token sequence should not make it out of the build tree 
  !! pass, so the constraint on sizes for @a left and @a right shouldn't 
  !! be a problem.
  
  SUBROUTINE compare_tlists(left, right, stmt_name, out_unit, diff)
    
    USE SourceLocations
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Token), INTENT(IN) :: left(:)
    TYPE(Token), INTENT(IN) :: right(:)
    CHARACTER(*), INTENT(IN) :: stmt_name
    INTEGER, INTENT(IN) :: out_unit
    INTEGER, INTENT(INOUT) :: diff
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: i              ! Token index.
    
    ! Left set location for a difference.
    TYPE(SourceLocation) :: left_loc
    
    ! Right set location for a difference.
    TYPE(SourceLocation) :: right_loc
    
    !***************************************************************************
    
    DO i = 1, MIN(SIZE(left), SIZE(right))
      IF (QueryType(left(i)) /= QueryType(right(i))) THEN
        diff = diff + 1
        left_loc = QueryLocation(left(i))
        right_loc = QueryLocation(right(i))
        WRITE (out_unit, fmt_diff)  &
            left_loc%ToString(), right_loc%ToString(),  &
            'There are different token types in a /' // stmt_name // '/.'
        EXIT
      END IF
      
      IF (QueryValue(left(i)) /= QueryValue(right(i))) THEN
        diff = diff + 1
        left_loc = QueryLocation(left(i))
        right_loc = QueryLocation(right(i))
        WRITE (out_unit, fmt_diff)  &
            left_loc%ToString(), right_loc%ToString(),  &
            'There are different token values in a /' // stmt_name // '/.'
        EXIT
      END IF
    END DO
    
    IF (SIZE(left) /= SIZE(right)) THEN
      diff = diff + 1
      ! Report against first token - tests above may have jumped out early
      left_loc = QueryLocation(left(1))
      right_loc = QueryLocation(right(1))
      
      WRITE (out_unit, fmt_diff)  &
          left_loc%ToString(), right_loc%ToString(),  &
          'There are differences in the number of tokens in a /'  &
          // stmt_name // '/.'
    END IF
    
  END SUBROUTINE compare_tlists
  
  
  !*****************************************************************************
  !!
  !> Make a descriptive string from a list of names.
  !!
  !! @param[in]     names             The list of names.
  !!
  !! @returns A string describing the names.
  
  FUNCTION join_names(names) RESULT(str)
    
    USE Strings
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(String), INTENT(IN) :: names(:)
    
    ! Function result.
    CHARACTER(:), ALLOCATABLE :: str
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: i              ! Name index.
    
    !***************************************************************************
    
    str = ''
    
    DO i = 1, SIZE(names) - 2
      str = '"' // names(i)%item // '", '
    END DO
    
    IF (SIZE(names) > 1) THEN
      str = str // '"' // names(i)%item // '" and '
      i = i + 1
    END IF
    
    str = str // '"' // names(i)%item // '"'
    
  END FUNCTION join_names
  
  
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! Some simple unit and subprogram vector search routines.
  
  
  !*****************************************************************************
  !!
  !> Find a subroutine in a vector of program units or subprograms.
  !!
  !! @param[in]     tree              The program node being searched.
  !!
  !! @param[in]     name              The name of the subroutine to 
  !! search for.
  !!
  !! @param[out]    idx               Index of a matching subroutine.  Zero if 
  !! no matching subroutine was found.
  !!
  !! @param[out]    ptr               A pointer to the tree node for the 
  !! subroutine if found, NULL() if not.
  !!
  !! We assume that there is only one subroutine with matching name in 
  !! the vector.
  
  SUBROUTINE find_subroutine(vector, name, idx, ptr)
    
    USE ProgUnitStNodes
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(ProgUnitVector), INTENT(IN), TARGET :: vector(:)
    CHARACTER(*), INTENT(IN) :: name
    INTEGER, INTENT(OUT) :: idx
    TYPE(SubroutineSubprogramStNode), INTENT(OUT), POINTER :: ptr
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Name of the subroutine being compared from the vector.
    CHARACTER(:), ALLOCATABLE :: other_name
    
    !***************************************************************************
    
    DO idx = 1, SIZE(vector)
      SELECT TYPE (item => vector(idx)%item)
      TYPE IS (SubroutineSubprogramStNode)
        CALL item%GetName(other_name)
        IF (other_name == name) THEN
          ptr => item
          RETURN
        END IF
      END SELECT
    END DO
    
    idx = 0
    ptr => NULL()
    
  END SUBROUTINE find_subroutine
  
  
  !*****************************************************************************
  !!
  !> Find a function in a vector of program units or subprograms.
  !!
  !! @param[in]     tree              The program node being searched.
  !!
  !! @param[in]     name              The name of the function to 
  !! search for.
  !!
  !! @param[out]    idx               Index of a matching function.  Zero if 
  !! no matching function was found.
  !!
  !! @param[out]    ptr               A pointer to the tree node for the 
  !! function if found, NULL() if not.
  !!
  !! We assume that there is only one function with matching name in 
  !! the vector.
  
  SUBROUTINE find_function(vector, name, idx, ptr)
    
    USE ProgUnitStNodes
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(ProgUnitVector), INTENT(IN), TARGET :: vector(:)
    CHARACTER(:), INTENT(IN), ALLOCATABLE :: name
    INTEGER, INTENT(OUT) :: idx
    TYPE(FunctionSubprogramStNode), INTENT(OUT), POINTER :: ptr
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Name of the subroutine being compared from the vector.
    CHARACTER(:), ALLOCATABLE :: other_name
    
    !***************************************************************************
    
    DO idx = 1, SIZE(vector)
      SELECT TYPE (item => vector(idx)%item)
      TYPE IS (FunctionSubprogramStNode)
        CALL item%GetName(other_name)
        IF (other_name == name) THEN
          ptr => item
          RETURN
        END IF
      END SELECT
    END DO
    
    idx = 0
    ptr => NULL()
    
  END SUBROUTINE find_function
  
  
  !*****************************************************************************
  !!
  !> Find a separate module procedure in a vector of program units or 
  !! subprograms.
  !!
  !! @param[in]     tree              The program node being searched.
  !!
  !! @param[in]     name              The name of the separate module 
  !! procedure to search for.
  !!
  !! @param[out]    idx               Index of a matching separate module 
  !! procedure.  Zero if no matching procedure was found.
  !!
  !! @param[out]    ptr               A pointer to the tree node for the 
  !! separate module procedure if found, NULL() if not.
  !!
  !! We assume that there is only one separate module procedure with matching 
  !! name in the vector.
  
  SUBROUTINE find_separateprocedure(vector, name, idx, ptr)
    
    USE ProgUnitStNodes
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(ProgUnitVector), INTENT(IN), TARGET :: vector(:)
    CHARACTER(:), INTENT(IN), ALLOCATABLE :: name
    INTEGER, INTENT(OUT) :: idx
    TYPE(SeparateModuleSubprogramStNode), INTENT(OUT), POINTER :: ptr
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Name of the subroutine being compared from the vector.
    CHARACTER(:), ALLOCATABLE :: other_name
    
    !***************************************************************************
    
    DO idx = 1, SIZE(vector)
      SELECT TYPE (item => vector(idx)%item)
      TYPE IS (SeparateModuleSubprogramStNode)
        CALL item%GetName(other_name)
        IF (other_name == name) THEN
          ptr => item
          RETURN
        END IF
      END SELECT
    END DO
    
    idx = 0
    ptr => NULL()
    
  END SUBROUTINE find_separateprocedure
  
  
  !*****************************************************************************
  !!
  !> Find a module in a vector of program units or subprograms.
  !!
  !! @param[in]     tree              The program node being searched.
  !!
  !! @param[in]     name              The name of the module to 
  !! search for.
  !!
  !! @param[out]    idx               Index of a matching module.  Zero if 
  !! no matching module was found.
  !!
  !! @param[out]    ptr               A pointer to the tree node for the 
  !! module if found, NULL() if not.
  !!
  !! We assume that there is only one module with matching name in the vector.
  
  SUBROUTINE find_module(tree, name, idx, ptr)
    
    USE BaseStNodes
    USE ModuleStNodes
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(BaseStNode), INTENT(IN), TARGET :: tree
    CHARACTER(:), INTENT(IN), ALLOCATABLE :: name
    INTEGER, INTENT(OUT) :: idx
    TYPE(ModuleStNode), INTENT(OUT), POINTER :: ptr
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Name of the subroutine being compared from the vector.
    CHARACTER(:), ALLOCATABLE :: other_name
    
    !***************************************************************************
    
    DO idx = 1, SIZE(tree%prog_units)
      SELECT TYPE (item => tree%prog_units(idx)%item)
      TYPE IS (ModuleStNode)
        CALL item%GetName(other_name)
        IF (other_name == name) THEN
          ptr => item
          RETURN
        END IF
      END SELECT
    END DO
    
    idx = 0
    ptr => NULL()
    
  END SUBROUTINE find_module
  
  
  !*****************************************************************************
  !!
  !> Find a submodule in a vector of program units or subprograms.
  !!
  !! @param[in]     tree              The program node being searched.
  !!
  !! @param[in]     name              The identifier for the submodule to 
  !! search for.
  !!
  !! @param[out]    idx               Index of a matching submodule.  Zero if 
  !! no matching submodule was found.
  !!
  !! @param[out]    ptr               A pointer to the tree node for the 
  !! submodule.
  !!
  !! We assume that there is only one submodule with matching identifier 
  !! in the vector if found, NULL() if not.
  
  SUBROUTINE find_submodule(tree, name, idx, ptr)
    
    USE BaseStNodes
    USE ModuleStNodes
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(BaseStNode), INTENT(IN), TARGET :: tree
    CHARACTER(:), INTENT(IN), ALLOCATABLE :: name
    INTEGER, INTENT(OUT) :: idx
    TYPE(SubmoduleStNode), INTENT(OUT), POINTER :: ptr
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Name of the subroutine being compared from the vector.
    CHARACTER(:), ALLOCATABLE :: other_name
    
    !***************************************************************************
    
    DO idx = 1, SIZE(tree%prog_units)
      SELECT TYPE (item => tree%prog_units(idx)%item)
      TYPE IS (SubmoduleStNode)
        CALL item%GetName(other_name)
        IF (other_name == name) THEN
          ptr => item
          RETURN
        END IF
      END SELECT
    END DO
    
    idx = 0
    ptr => NULL()
    
  END SUBROUTINE find_submodule
  
  
  !*****************************************************************************
  !!
  !> Find an interface body in an array of statement tree nodes.
  !!
  !! @param[in]     bodies            Array of the child interface bodies and 
  !! procedure statements.
  !!
  !! @param[in]     name              The name of the interface body to find.
  !!
  !! @param[in]     considered        Flag to indicate whether the particular 
  !! body has been previously "found" for a different name.
  !!
  !! @param[out]    idx               The index of the matching interface body, 
  !! or zero if no match was found.
  
  SUBROUTINE find_interface_body(bodies, name, considered, idx)
    
    USE InterfaceBodyStNodes
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(StNodeList), INTENT(IN) :: bodies(:)
    CHARACTER(*), INTENT(IN) :: name
    LOGICAL, INTENT(IN) :: considered(:)
    INTEGER, INTENT(OUT) :: idx
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Name of the item being considered.
    CHARACTER(:), ALLOCATABLE :: item_name
    
    !***************************************************************************
    
    DO idx = 1, SIZE(bodies)
      IF (considered(idx)) CYCLE
      
      SELECT TYPE (item => bodies(idx)%item)
      CLASS IS (InterfaceBodyStNode)
        CALL item%GetName(item_name)
        IF (.NOT. ALLOCATED(item_name)) CYCLE
        IF (item_name == name) RETURN
      END SELECT
      
    END DO
    
    idx = 0
    
  END SUBROUTINE find_interface_body
  
  
  !*****************************************************************************
  !!
  !> Find a procedure statement in an array of statement tree nodes.
  !!
  !! @param[in]     bodies            Array of the child interface bodies and 
  !! procedure statements.
  !!
  !! @param[in]     name              The name of the first procedure listed 
  !! in the procedure statement to find.
  !!
  !! @param[in]     considered        Flag to indicate whether the particular 
  !! statement has been previously "found" for a different name.
  !!
  !! @param[out]    idx               The index of the matching node, 
  !! or zero if no match was found.
  
  SUBROUTINE find_procedure_stmt(bodies, name, considered, idx)
    
    USE InterfaceBodyStNodes
    USE ProcedureStmtsUtils
    USE SingleStNodes
    USE Statements
    USE Strings
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(StNodeList), INTENT(IN) :: bodies(:)
    CHARACTER(*), INTENT(IN) :: name
    LOGICAL, INTENT(IN) :: considered(:)
    INTEGER, INTENT(OUT) :: idx
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Names in the procedure statment being considered.
    TYPE(String), ALLOCATABLE :: proc_names(:)
    
    !***************************************************************************
    
    DO idx = 1, SIZE(bodies)
      IF (considered(idx)) CYCLE
      
      SELECT TYPE (item => bodies(idx)%item)
      CLASS IS (SingleStNode)
        IF (item%stmt%ist /= istProcedure) CYCLE
        CALL GetProcedureNames(item%stmt%tlist, proc_names)
        IF (.NOT. ALLOCATED(proc_names)) CYCLE
        IF (proc_names(1) == name) RETURN
      END SELECT
      
    END DO
    
    idx = 0
    
  END SUBROUTINE find_procedure_stmt
  
  
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! Procedures for bindings in flatten_visitor
  
  
  !*****************************************************************************
  !!
  !> Implementation of flatten_visitor%ExecuteNode - called whenever a 
  !! statement tree node is visited in a tree traversal.
  !!
  !! @param[in,out] visitor           The object carrying out the traversal 
  !! operation.
  !!
  !! @param[in]     st                The node being traversed.
  !!
  !! This is a no-operation.
  
  SUBROUTINE flatten_ExecuteNode(visitor, st)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(flatten_visitor), INTENT(INOUT) :: visitor
    CLASS(StNode), INTENT(IN), TARGET :: st
    
    !***************************************************************************
    
    ! Does nothing.
    
  END SUBROUTINE flatten_ExecuteNode
  
  
  !*****************************************************************************
  !!
  !> Implementation of flatten_visitor%ExecuteStmt - called whenever a 
  !! statement stored in a statement tree node is traversed.
  !!
  !! @param[in,out] visitor           The object carrying out the traversal 
  !! operation.
  !!
  !! @param[in]     parent            The parent node of the statement.
  !!
  !! @param[in]     stmt              The statement being traversed.
  
  SUBROUTINE flatten_ExecuteStmt(visitor, parent, stmt)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(flatten_visitor), INTENT(INOUT) :: visitor
    CLASS(StNode), INTENT(IN), TARGET :: parent
    TYPE(StData), INTENT(IN) :: stmt
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Temporary for growing the statements array.
    TYPE(StData), ALLOCATABLE :: tmp(:)
    
    !***************************************************************************
    
    ! Make sure we have space.
    IF (.NOT. ALLOCATED(visitor%stmts)) THEN
      ALLOCATE(visitor%stmts(10))
      visitor%ilast = 0
    ELSE IF (visitor%ilast >= SIZE(visitor%stmts)) THEN
      ALLOCATE(tmp(SIZE(visitor%stmts)*2))
      tmp(:SIZE(visitor%stmts)) = visitor%stmts
      CALL MOVE_ALLOC(tmp, visitor%stmts)
    END IF
    
    ! Save the statement.
    visitor%ilast = visitor%ilast + 1
    visitor%stmts(visitor%ilast) = stmt
    
  END SUBROUTINE flatten_ExecuteStmt
  
END MODULE DiffImplementation
