! $Id: FileDependencyPass.f90 2960 2020-04-17 20:43:28Z ian $
! ff08 source code copyright 2012 M.E.G.M.S.  See LICENCE.txt for licence.
!> @file
!! Defines the FileDependencyPass module


!*******************************************************************************
!!
!> Routines for an optional pass through the source files to determine the 
!! prerequisites for compilation of the source files.
!!
!! We have finally settled on this pass being orthogonal to other passes.  
!! The typical sequence of passes requires the BuildTree pass, which 
!! was not able to build on the limited Contexter based processing used 
!! in this pass.  The BuildTree pass doesn't require compilation to be in 
!! the right order (so it doesn't require a FileDependency pass) and once 
!! the BuildTree pass is complete, then extracting the relevant information 
!! for a program unit (versus file) based dependency analysis is quite easy.
!!
!! Consequently we've stripped this pass down - it no longer uses the 
!! Contexter as the statements that are detected are always accurately 
!! detected by the Classifier even without context.  This has improved 
!! source granularity, which was important at one stage when the production 
!! compiler was playing up.
!!
!! In addition to the core SourceDependencyPass procedure there are some 
!! worker routines for common dependency related tasks.
!!
!! With hind-hind-sight, perhaps stripping the contexter out was a bad idea.  
!! With the contexter in the loop we could also track which files defined 
!! which external subprograms - which could be handy for things like the 
!! --root-unit option of FF08Depends.  At the moment the file containing 
!! the external subprogram needs to be specified using --root-file.  
!! This would elevate external subprograms to the same status as main 
!! program units.
!!
!! However, tracking all uses of external subprograms (not that these affect 
!! the dependency order, but it might affect the files that need to 
!! be included in the listing) would still be well and truely impossible.
!!
!! A middle ground could be to remember external subprograms that are 
!! referenced (or declared) in interface blocks.  With good coding practices 
!! this should incorporate all such external subprograms referenced by the 
!! program, but we'd still miss those referenced without an explicit 
!! interface.
!!
!! Under F2008, interface blocks may also declare separate module procedures, 
!! so some involvement of the function and subroutine statements would be 
!! necessary to filter out those cases.  Practically this would require 
!! splitting of the FunctionStmts and SubroutineStmts modules (similar to 
!! how UseStmts and ModuleStmts were split) to avoid the need to bring 
!! in 90% of the parsing library.  This split might be difficult, as function 
!! prefixes involve type declaration specs, which are messy to parse.
!!
!! @todo Test the feasibility of those module splits one day.
!!
!! We could, if we wanted to now, also allow block data program units to 
!! be returned in the @a defines argument of the SourceDependencyPass 
!! procedure.  We haven't done this yet because we are lazy.

MODULE FileDependencyPass
  
  USE Errors
  USE Sources
  USE SourceLocations
  USE StatementData
  
  IMPLICIT NONE
  
  PRIVATE
  
  !-----------------------------------------------------------------------------
  ! Expose module procedures and interfaces
  
  PUBLIC :: SourceDependencyPass
  PUBLIC :: Find
  
  !-----------------------------------------------------------------------------
  
  ! Program unit type ("put") indices.
  
  !> Program unit is a main program.
  INTEGER, PARAMETER, PUBLIC :: iputProgram = 1
  !> Program unit is a module.
  INTEGER, PARAMETER, PUBLIC :: iputModule = 2
  !> Program unit is a submodule.
  INTEGER, PARAMETER, PUBLIC :: iputSubmodule = 3
  !> Program unit is a block data unit.
  INTEGER, PARAMETER, PUBLIC :: iputBlockData = 4
  !> Program unit is a function subprogram.  This is not used.
  INTEGER, PARAMETER, PUBLIC :: iputFunction = 5
  !> Program unit is a subroutine subprogram.  This is not used.
  INTEGER, PARAMETER, PUBLIC :: iputSubroutine = 6
  
  !> Descriptive name to go with each iput* constant for error reporting.
  !
  ! We leave a space after each name to simplify fixed width output.
  CHARACTER(10), PARAMETER, PUBLIC :: putNames(6)  &
      = [ 'PROGRAM    ',  &
          'MODULE     ',  &
          'SUBMODULE  ',  &
          'BLOCKDATA  ',  &
          'FUNCTION   ',  &
          'SUBROUTINE ' ]
  
  !> Data for each program unit referenced in a file.
  
  TYPE, PUBLIC :: DependencyUnit
    !> An iput* constant that describes the type of reference or 
    !! definition.  iputBlockData, iputFunction and iputSubroutine cannot 
    !! be referenced.  A reference that is iputSubmodule means that 
    !! the thing being referenced is also being extended - the thing being 
    !! extended may be a module or submodule.
    INTEGER :: type
    
    !> Name of the program unit referenced or defined.
    !!
    !! For submodule definitions and submodule parent references this 
    !! is the identifier of the submodule.
    CHARACTER(:), ALLOCATABLE :: name
    
    !> The location of the definition or reference.
    TYPE(SourceLocation), ALLOCATABLE :: location
  END TYPE DependencyUnit
  
  !-----------------------------------------------------------------------------
  ! Interfaces
  
  !> Run the dependency pass on a single source.
  INTERFACE SourceDependencyPass
    MODULE PROCEDURE SourceDependencyPass_
    MODULE PROCEDURE SourceDependencyPass_form
  END INTERFACE SourceDependencyPass
  
  !> Find a program unit in a list of program units.
  INTERFACE Find
    MODULE PROCEDURE Find_progunit
  END INTERFACE Find
  
  !> All errors reported by this module have the following component.
  CHARACTER(*), PARAMETER :: comp = 'FileDependencyPass'
  
CONTAINS
  
  !*****************************************************************************
  !!
  !> Gets the modules and submodules that a source file defines, the 
  !! modules that a source file uses and the modules and submodules that 
  !! a submodule implements.
  !!
  !! @param[in]     host              Object that describes the 
  !! characteristics of the compiler host.
  !!
  !! @param[in]     filename          Filename of the source file.
  !!
  !! @param[out]    referenced        Modules and submodules referenced 
  !! by this file in upper case.  Repeat reference result in a single entry, 
  !! the location component indicates the first reference encountered.
  !!
  !! @param[out]    defines           Modules and submodules defined 
  !! by this file, with names in upper case.  Repeat definition results in 
  !! an error.
  !!
  !! @param[out]    includes          Include files referenced by this 
  !! file.
  !!
  !! @param[out]    err_list          Errors encounted while carrying out 
  !! the dependency analysis.  If you are interested in all errors 
  !! from parsing the file, then see the @a statements argument.
  !!
  !! @param[out]    source_obj        Optional source object used to 
  !! read the file.
  
  SUBROUTINE SourceDependencyPass_( host, filename,  &
      references, defines, includes, err_list, source_obj )
    
    USE Strings
    USE CompilerHosts
    USE Errors
    USE Scanner
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(CompilerHost), INTENT(IN) :: host
    CHARACTER(*), INTENT(IN) :: filename
    TYPE(DependencyUnit), INTENT(OUT), ALLOCATABLE :: references(:)
    TYPE(DependencyUnit), INTENT(OUT), ALLOCATABLE :: defines(:)
    TYPE(String), INTENT(OUT), ALLOCATABLE :: includes(:)
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    CLASS(Source), INTENT(OUT), POINTER, OPTIONAL :: source_obj
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Source form to read the file.
    CLASS(SourceForm), ALLOCATABLE :: form
    
    ! Error list for procedure calls
    TYPE(Error), ALLOCATABLE :: sub_err_list(:)
    
    !***************************************************************************
    
    CALL host%OpenSourceForm(filename, form, err_list)
    IF (Failure(err_list)) RETURN
    
    CALL SourceDependencyPass(  &
        host,  &
        form,  &
        references,  &
        defines,  &
        includes,  &
        sub_err_list )
    CALL Add(err_list, sub_err_list)
    
    ! Client code may need to know which source object corresponds to this 
    ! filename.  While the source form object has only local scope, the 
    ! underlying source object hangs around for the life of the program.
    IF (PRESENT(source_obj)) CALL form%GetSource(source_obj)
    
  END SUBROUTINE SourceDependencyPass_
  
  
  !*****************************************************************************
  !!
  !> Gets the modules and submodules that a source file defines, the 
  !! modules that a source file uses and the modules and submodules that 
  !! a submodule implements, where the source file is described by a source 
  !! form.
  !!
  !! @param[in]     host              Object that describes the 
  !! characteristics of the compiler host.
  !!
  !! @param[in]     form              Source form object for the source file.
  !!
  !! @param[out]    referenced        Modules and submodules referenced 
  !! by this file in upper case.  One entry for each reference.
  !!
  !! @param[out]    defines           Main programs, modules and submodules 
  !! defined by this file in upper case.  Repeat definition results in an 
  !! error.
  !!
  !! @param[out]    includes          Include files referenced by this 
  !! file.
  !!
  !! @param[out]    err_list          Errors encounted while carrying out 
  !! the dependency analysis.  If you are interested in all errors 
  !! from parsing the file, then see the @a statements argument.
  
  SUBROUTINE SourceDependencyPass_form( host, form, references,  &
      defines, includes, err_list )
    
    USE CompilerHosts
    USE Debug
    USE Errors
    USE Scanner
    USE Strings
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(CompilerHost), INTENT(IN) :: host
    CLASS(SourceForm), INTENT(INOUT) :: form
    TYPE(DependencyUnit), INTENT(OUT), ALLOCATABLE :: references(:)
    TYPE(DependencyUnit), INTENT(OUT), ALLOCATABLE :: defines(:)
    TYPE(String), INTENT(OUT), ALLOCATABLE :: includes(:)
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !---------------------------------------------------------------------------
    ! Local constants
    
    ! Maximum number of errors in a pass before we give up in disgust.
    INTEGER, PARAMETER :: max_errors = 20
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: error_count    ! Error count
    
    !***************************************************************************
    
    ALLOCATE(err_list(0))
    
    !---------------------------------------------------------------------------
    ! Start the output lists off empty.
    
    ALLOCATE(references(0))
    ALLOCATE(defines(0))
    ALLOCATE(includes(0))
    
    ! We track the number of errors per file.  If that count gets too big, 
    ! we give up analysing the file.
    error_count = 0
    
    !---------------------------------------------------------------------------
    ! Call the worker routine with empty output lists.
    
    CALL dependency_pass(  &
        host,  &
        max_errors,  &
        form,  &
        error_count,  &
        references,  &
        defines,  &
        includes,  &
        err_list )
    
    !---------------------------------------------------------------------------
    
    IF (DbgFlag) CALL dbg_dump
    
  CONTAINS
    
    !***************************************************************************
    !
    ! Dump debugging information to the debugging unit.  Made an internal 
    ! procedure so that debugging specific declarations don't clutter overall 
    ! logic.
    
    SUBROUTINE dbg_dump
      
      ! Source object for the current file.
      CLASS(Source), POINTER :: parent_source
      
      INTEGER :: i            ! Program unit/include index.
      
      !*************************************************************************
      
      CALL form%GetSource(parent_source)
      WRITE (DbgUnit, "('File:',A)") parent_source%Name
      WRITE (DbgUnit, "(2X,'Defines:',:,/,999(4X,A,A,:,/))")  &
          (putNames(defines(i)%type), defines(i)%name, i=1, SIZE(defines))
      WRITE (DbgUnit, "(2X,'References:',:,/,999(4X,A,A,:,/))")  &
          ( putNames(references(i)%type), references(i)%name,  &
            i = 1, SIZE(references) )
      WRITE (DbgUnit, "(2X,'Included:',:,/,999(4X,A,:,/))")  &
          (includes(i)%item, i = 1, SIZE(includes))
      
    END SUBROUTINE dbg_dump
    
  END SUBROUTINE SourceDependencyPass_form
  
  
  !*****************************************************************************
  !!
  !> Worker routine that gathers the dependency information (program unit 
  !! definitions and references) from a source file.
  !!
  !! @param[in]     host              Object that describes the 
  !! characteristics of the compiler host.
  !!
  !! @param[in]     max_errors        Maximum number of errors (see 
  !! @a err_count) before dependency processing on this file should stop.
  !!
  !! @param[in]     form              Source form object for the source file.
  !!
  !! @param[in,out] error_count       Number of dependency related errors 
  !! seen for the ultimate parent source file so far.  Updated with the 
  !! number of errors seen in this source file.
  !!
  !! @param[in,out] references        Modules and submodules referenced 
  !! by this file (and its parents so far) in upper case.  One entry for each 
  !! reference.
  !!
  !! @param[in,out] defines           Main programs, modules and 
  !! submodules defined by this file (and its parents so far) in upper 
  !! case.  Repeat definition results in an error.
  !!
  !! @param[in,out] includes          Include files referenced by this 
  !! file (and its parents so far).
  !!
  !! @param[out]    err_list          Errors encounted while carrying out 
  !! the dependency analysis.
  !!
  !! Recursive because processing of include lines will require this 
  !! procedure to be called recursively.
  
  RECURSIVE SUBROUTINE dependency_pass( host, max_errors, form,  &
      error_count, references, defines, includes, err_list )
    
    USE Strings
    USE CharUtils
    USE Classifier
    USE CompilerHosts
    USE CompilerKinds
    USE Debug
    USE Errors
    USE ErrorLevels
    USE IncludeLines
    USE LabelsUtils
    USE Scanner
    USE Statements
    USE Sources
    
    USE ModuleStmtsUtils
    USE ProgramStmtsUtils
    USE SubmoduleStmtsUtils
    USE UseStmtsUtils
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(CompilerHost), INTENT(IN) :: host
    INTEGER, INTENT(IN) :: max_errors
    CLASS(SourceForm), INTENT(INOUT) :: form
    INTEGER, INTENT(INOUT) :: error_count
    TYPE(DependencyUnit), INTENT(INOUT), ALLOCATABLE :: references(:)
    TYPE(DependencyUnit), INTENT(INOUT), ALLOCATABLE :: defines(:)
    TYPE(String), INTENT(INOUT), ALLOCATABLE :: includes(:)
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !---------------------------------------------------------------------------
    ! Local constants
    
    ! Placeholder for an empty part stack.
    INTEGER, PARAMETER :: no_parts(0) = [INTEGER::]
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Error message list for procedures.
    TYPE(Error), ALLOCATABLE :: sub_err_list(:)
    
    ! A "statement" worth of tokens.
    TYPE(Token), ALLOCATABLE :: tlist(:)
    
    LOGICAL :: is_intrinsic   ! Module is intrinsic flag.
    INTEGER :: ist            ! Statement classification
    
    ! Any statement label associated with tlist.
    TYPE(Label), ALLOCATABLE :: statement_label
    
    ! Name of the program unit being referenced or defined.  Note that if the 
    ! program unit name (or identifier) routines fail to determine the name or 
    ! identifier, then this may not be alloated.
    CHARACTER(:), ALLOCATABLE :: unit_name
    
    ! Source object for the current file.
    CLASS(Source), POINTER :: parent_source
    
    ! Source object for any included file.
    CLASS(Source), POINTER :: inc_source
    
    ! Source form object for any included file.
    CLASS(SourceForm), ALLOCATABLE :: inc_form
    
    !***************************************************************************
    
    ALLOCATE(err_list(0))
    
    !---------------------------------------------------------------------------
    
    ! Loop till we run out of statements.
    DO
      ! Get a statement worth of tokens.
      CALL form%GetStatement(no_parts, sub_err_list, tlist, statement_label)
      IF (Fatal(sub_err_list)) EXIT
      error_count = error_count + SIZE(sub_err_list)
      IF (error_count > max_errors) EXIT
      
      ! stmt%tlist may not be allocated if an error occurred.
      IF (.NOT. ALLOCATED(tlist)) CYCLE
      
      ! No label and no token in a statement means end-of-file.
      IF (.NOT. ALLOCATED(statement_label) .AND. (SIZE(tlist) == 0)) EXIT
      
      ! If the file has a statement label but no tokens, then tlist 
      ! will be zero sized.  No point trying to process the statement 
      ! in that case.
      !
      ! (There will be an error relating to this in sub_err_list from the 
      ! call above, but we are ignoring non-fatal errors.)
      IF (SIZE(tlist) == 0) CYCLE
      
      ! At this point we have something like a statement's worth of tokens.
      
      !-------------------------------------------------------------------------
      ! Include processing.
      
      ! Is the statement actually an INCLUDE line?  We are tolerant to 
      ! a variety of include line related syntax errors here.
      IF (TestInclude(tlist)) THEN
        ! Get a source object, and ultimately a source form, for the 
        ! included file.
        CALL form%GetSource(parent_source)
        CALL host%OpenIncludeFile(  &
            tlist(2),  &
            parent_source,  &
            inc_source,  &
            sub_err_list )
        CALL Add(err_list, sub_err_list)
        IF (Failure(sub_err_list)) CYCLE
        
        ! Add the name of the include file to the list of included files.
        CALL AppendIfUnique(includes, inc_source%name)
        
        ! For now, an included file has the same type of source form as its 
        ! parent.
        CALL form%Clone(inc_source, inc_form)
        
        ! Recursively process the included file.
        !
        ! sub_err_list comes back with dependency related errors.  We track 
        ! those separately to the main err_list to allow location chaining.  
        ! Parsing and other errors are added to stmt_list, if that's required.
        CALL dependency_pass(  &
            host,  &
            max_errors,  &
            inc_form,  &
            error_count,  &
            references,  &
            defines,  &
            includes,  &
            sub_err_list )
        CALL Add(err_list, sub_err_list, LOCATION=QueryLocation(tlist(1)))
        IF (Fatal(sub_err_list)) EXIT
        error_count = error_count + SIZE(sub_err_list)
        IF (error_count > max_errors) EXIT
        CYCLE
      END IF
      
      !-------------------------------------------------------------------------
      ! Statement processing
      
      ! What sort of statement is it?  The classification will not be 
      ! entirely accurate because we don't have context, but we can pick 
      ! the statement types that we need to.
      CALL Classify(tlist, no_parts, ist)
      
      ! See if the statement defines or references a module, or defines some 
      ! other sort of program unit (that we track so far).
      SELECT CASE (ist)
      CASE (istModule)      ! A module definition.
        CALL GetModuleName(tlist, unit_name)
        IF (.NOT. ALLOCATED(unit_name)) CYCLE
        CALL add_definition(iputModule)
        ! Have we seen a reference for this module before?
        CALL check_for_references(iputModule)
        
      CASE (istSubmodule)   ! A submodule definition.
        CALL GetSubmoduleIdentifier(tlist, unit_name)
        IF (.NOT. ALLOCATED(unit_name)) CYCLE
        CALL add_definition(iputSubmodule)
        ! Look for references to this submodule.
        CALL check_for_references(iputSubmodule)
        ! A submodule definition is a module and possible submodule 
        ! reference.  Deal with the module first. 
        CALL GetSubmoduleAncestorName(tlist, unit_name)
        IF (.NOT. ALLOCATED(unit_name)) CYCLE
        CALL add_reference(iputModule)
        ! Deal with the parent submodule.  The parent submodule 
        ! may also be the ancestor module, but we add it anyway 
        ! as a submodule.
        CALL GetSubmoduleParentIdentifier(tlist, unit_name)
        IF (.NOT. ALLOCATED(unit_name)) CYCLE
        CALL add_reference(iputSubmodule)
        
      CASE (istUse)         ! A module reference.
        CALL GetModuleName(tlist, unit_name, is_intrinsic)
        IF (.NOT. ALLOCATED(unit_name)) CYCLE
        IF (is_intrinsic) CYCLE
        CALL add_reference(iputModule)
        
      CASE (istProgram)     ! Main program definition.
        CALL GetProgramName(tlist, unit_name)
        IF (.NOT. ALLOCATED(unit_name)) CYCLE
        CALL add_definition(iputProgram)
        
      END SELECT
      
    END DO
    
    ! Mark the file as closed.
    CALL form%GetSource(inc_source)
    CALL inc_source%Reset()
    
  CONTAINS
    
    !***************************************************************************
    !
    ! Work routine to add a program unit definition.  Before calling 
    ! populate unit_name with the name of the module, submodule or 
    ! main program.
    
    SUBROUTINE add_definition(iput)
      
      !-------------------------------------------------------------------------
      ! Arguments
      
      INTEGER, INTENT(IN) :: iput
      
      !-------------------------------------------------------------------------
      ! Local variables
      
      INTEGER :: iu             ! Program unit index
      
      !*************************************************************************
      
      CALL UpperCase(unit_name)
      
      ! Have we seen a definition for this module before?
      iu = find_progunit(defines, unit_name)
      IF (iu /= 0) THEN
        ! Seen before.  Report an error.
        CALL multiple_def_error(err_list, defines(iu), iput, tlist)
      ELSE
        ! Not seen before.  Add to our list of definitions.
        CALL append_progunit(  &
            defines,  &
            unit_name,  &
            iput,  &
            QueryLocation(tlist(1)) )
      END IF
      
    END SUBROUTINE add_definition
    
    !***************************************************************************
    !
    ! Add the program unit identified by iput and unit name to the list 
    ! of references.
    
    SUBROUTINE add_reference(iput)
      
      !-------------------------------------------------------------------------
      ! Arguments
      
      INTEGER, INTENT(IN) :: iput
      
      !-------------------------------------------------------------------------
      ! Local variables
      
      INTEGER :: iu             ! Program unit index
      
      !*************************************************************************
      
      CALL UpperCase(unit_name)
      CALL append_progunit(  &
          references,  &
          unit_name,  &
          iput,  &
          QueryLocation(tlist(1)) )
      
    END SUBROUTINE add_reference
    
    !***************************************************************************
    !
    ! Check for previous references of the program unit named in unit_name.
    
    SUBROUTINE check_for_references(iput)
      
      !-------------------------------------------------------------------------
      ! Arguments
      
      INTEGER, INTENT(IN) :: iput
      
      !-------------------------------------------------------------------------
      ! Local variables
      
      INTEGER :: iu
      
      !*************************************************************************
      
      CALL UpperCase(unit_name)
      iu = find_progunit(references, unit_name)
      IF (iu /= 0) CALL reference_before_definition(  &
          err_list,  &
          references(iu),  &
          iput,  &
          tlist )
      
    END SUBROUTINE check_for_references
     
  END SUBROUTINE dependency_pass
  
  
  !*****************************************************************************
  !!
  !> Worker routine for SourceDependencyPass_ that adds an appropriate 
  !! multiple definition error to a list of errors.
  !!
  !! @param[in,out] err_list          The error list that the error is being 
  !! added to.
  !!
  !! @param[in]     defined           The program unit information structure.  
  !! The stmt member of this structure should be for the statement that starts 
  !! the first definition of the program unit.
  !!
  !! @param[in]     type              The type of the second definition of 
  !! the program unit (an iput* constant).
  !!
  !! @param[in]     tlist             The tokens for the statement that 
  !! has the second definition of the program unit.  Must not be zero sized.
  
  SUBROUTINE multiple_def_error(err_list, defined, type, tlist)
    
    USE CharUtils
    USE ErrorCodes
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Error), INTENT(INOUT), ALLOCATABLE :: err_list(:)
    TYPE(DependencyUnit), INTENT(IN) :: defined
    INTEGER, INTENT(IN) :: type
    TYPE(Token), INTENT(IN) :: tlist(:)
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! String describing the position of a definition.
    CHARACTER(:), ALLOCATABLE :: pos_str
    
    !***************************************************************************
    
    ! No need for file information to be repeated, so we build the location 
    ! string manually.
    ASSOCIATE(pos => defined%location)
      IF (pos%line /= 0) THEN
        pos_str = 'line ' // ToString(pos%line)
        IF (pos%column /= 0) THEN
          pos_str = pos_str // ', column ' // ToString(pos%column) // ''
        END IF
      END IF
    END ASSOCIATE
    
    ! Multiple definition within file.
    IF (type /= defined%type) THEN
      ! The type of the program unit doesn't match.
      CALL Add( err_list, CODE=errProgramUnitMultipleDefinition,  &
          COMPONENT=comp, LOCATION=QueryLocation(tlist(1)),  &
          MSG=TRIM(putNames(type)) // ' "' // defined%name  &
            // '" previously defined at "'  &
            // pos_str // '" as a ' // putNames(defined%type) // '.' )
    ELSE
      ! Previous definition was of the same type.
      CALL Add( err_list, CODE=errProgramUnitMultipleDefinition,  &
          COMPONENT=comp, LOCATION=QueryLocation(tlist(1)),  &
          MSG=TRIM(putNames(type)) // ' "' // defined%name  &
            // '" previously defined at "' // pos_str // '".' )
    END IF
    
  END SUBROUTINE multiple_def_error
  
  
  !*****************************************************************************
  !!
  !> Worker routine for SourceDependencyPass_ that adds an appropriate warning 
  !! for a reference of program unit prior to its definition within a file.
  !!
  !! @param[in,out] err_list          The error list that the error is being 
  !! added to.
  !!
  !! @param[in]     reference         The program unit information structure 
  !! for the reference.
  !!
  !! @param[in]     tlist             The tokens for the statement that 
  !! starts the definition of the program unit.  Must not be zero sized.
  
  SUBROUTINE reference_before_definition(err_list, reference, type, tlist)
    
    USE CharUtils
    USE ErrorCodes
    USE ErrorLevels
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Error), INTENT(INOUT), ALLOCATABLE :: err_list(:)
    TYPE(DependencyUnit), INTENT(IN) :: reference
    INTEGER, INTENT(IN) :: type
    TYPE(Token), INTENT(IN) :: tlist(:)
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! String describing the position of a reference.
    CHARACTER(:), ALLOCATABLE :: pos_str
    
    !***************************************************************************
    
    ! No need for file information to be repeated, so we build the location 
    ! string manually.
    ASSOCIATE(pos => QueryLocation(tlist(1)))
      IF (pos%line /= 0) THEN
        pos_str = 'line ' // ToString(pos%line)
        IF (pos%column /= 0) THEN
          pos_str = pos_str // ', column ' // ToString(pos%column) // ''
        END IF
      END IF
    END ASSOCIATE
    
    ! Note that we've made this a warning.
    CALL Add( err_list,  &
        CODE=errReferenceBeforeDefinition,  &
        COMPONENT=comp,  &
        LEVEL=errLevelWarning,  &
        LOCATION=reference%location,  &
        MSG=TRIM(putNames(type)) // ' "' // reference%name  &
          // '" referenced before definition at ' // pos_str // '.' )
    
  END SUBROUTINE reference_before_definition
  
  
  !*****************************************************************************
  !!
  !> Adds a single item to the end of the list.
  !!
  !! @param[in,out] list              The list to append to.
  !!
  !! @param[in]     name              The name of the program unit to append.
  !!
  !! @param[in]     type              The type of the program unit to append.
  !!
  !! @param[in]     location          Optional point of declaration or 
  !! reference of the program unit.  No location is assigned to the unit 
  !! if not present.
  
  SUBROUTINE append_progunit(list, name, type, location)
    
    USE CharUtils
    USE CompilerKinds
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(DependencyUnit), INTENT(INOUT), ALLOCATABLE :: list(:)
    CHARACTER(*,KIND=scck), INTENT(IN) :: name
    INTEGER, INTENT(IN) :: type
    TYPE(SourceLocation), INTENT(IN), OPTIONAL :: location
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: i              ! List index.
    
    ! Temporary for size change.
    TYPE(DependencyUnit), ALLOCATABLE :: tmp(:)
    
    !***************************************************************************
    
    IF (.NOT. ALLOCATED(list)) THEN
      ALLOCATE(tmp(1))
    ELSE
      ALLOCATE(tmp(SIZE(list)+1))
      DO i = 1, SIZE(list)
        CALL MOVE_ALLOC(list(i)%name, tmp(i)%name)
        tmp(i)%type = list(i)%type
        CALL MOVE_ALLOC(list(i)%location, tmp(i)%location)
      END DO
    END IF
    tmp(SIZE(tmp))%name = name
    tmp(SIZE(tmp))%type = type
    IF (PRESENT(location)) THEN
      ALLOCATE(tmp(SIZE(tmp))%location, SOURCE=location)
    END IF
    
    CALL MOVE_ALLOC(tmp, list)
    
  END SUBROUTINE append_progunit
  
  
  !*****************************************************************************
  !!
  !> Gets the index in the given list of the program unit with the given 
  !! name.
  !!
  !! @param[in]     list              The list of program units to search.
  !!
  !! @param[in]     name              The name to search for.
  !!
  !! @returns The element index of @a list that corresponds to @a name.
  
  FUNCTION Find_progunit(list, name) RESULT(i)
    
    USE CompilerKinds
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(DependencyUnit), INTENT(IN) :: list(:)
    CHARACTER(*), INTENT(IN) :: name
    
    ! Function result
    INTEGER :: i
    
    !***************************************************************************
    
    DO i = 1, SIZE(list)
      IF (list(i)%name == name) RETURN
    END DO
    i = 0
    
  END FUNCTION Find_progunit
  
END MODULE FileDependencyPass
