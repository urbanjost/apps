! $Id: DependsImplementation.f90 2960 2020-04-17 20:43:28Z ian $
! ff08 source code copyright 2012 M.E.G.M.S.  See LICENCE.txt for licence.
!> @file
!! Defines the DependsImplementation module.


!*******************************************************************************
!!
!> Procedures to implement the FF08Depends program.
!!
!! These have been split out so there's the potential for them to be 
!! tested independently of the main program.
!!
!! Note the use of the word "potential" in that previous sentence.
!!
!! Significant routines:
!! - PrintFileOrder - prints a list of files, ordered by their dependencies.
!! - PrintDependencyTree - prints a tree like structure of files dependencies.
!!
!! @todo Currently all the following routines internally specify the relevant 
!! host object.  This host object probably should be specified externally.
!!
!! @todo Develop weightings for each dependency and a method of taking those 
!! weightings into account when determining file order etc.  For example, 
!! it is probably better to process files that are pre-requisites for lots 
!! of other files (either immediately or in a total chain sense) 

MODULE DependsImplementation
  
  USE FileDependencyPass
  USE Strings
  
  IMPLICIT NONE
  
  PRIVATE
  
  !-----------------------------------------------------------------------------
  ! Expose module procedures and interfaces
  
  PUBLIC :: ExpandSpecsToNames
  PUBLIC :: RemoveDuplicates
  PUBLIC :: PrintFileOrder
  PUBLIC :: PrintDependencyTree
  
  !-----------------------------------------------------------------------------
  ! Expose module procedures
  
  !> Expand a list of file specifications to file names.
  INTERFACE ExpandSpecsToNames
    MODULE PROCEDURE ExpandSpecsToNames_
  END INTERFACE ExpandSpecsToNames
  
  !> Determine and print the order of compilation of a set of source files.
  INTERFACE PrintFileOrder
    MODULE PROCEDURE PrintFileOrder_
  END INTERFACE PrintFileOrder
  
  !> Determine and print the dependency tree for a set of source files.
  INTERFACE PrintDependencyTree
    MODULE PROCEDURE PrintDependencyTree_
  END INTERFACE PrintDependencyTree
  
  !> Remove duplicate file names from a list.
  INTERFACE RemoveDuplicates
    MODULE PROCEDURE RemoveDuplicates_
  END INTERFACE RemoveDuplicates
  
  !-----------------------------------------------------------------------------
  
  !> Element in a vector of include files for a vector of files.
  TYPE include_file
    TYPE(String), ALLOCATABLE :: files(:)
  END TYPE include_file
  
  !> Element in an array of unit references.
  TYPE :: unit_reference_type
    !> Index of the unit being referenced.
    INTEGER :: iunit
    !> Nature of the reference
    INTEGER :: nature
  END TYPE unit_reference_type
  
  ! Type to store dependency info from a file.
  TYPE :: file_info_type
    !> Indicies of the units defined by the file.
    INTEGER, ALLOCATABLE :: defined_units(:)
    !> Units referenced by the file.
    TYPE(unit_reference_type), ALLOCATABLE :: references(:)
    !> Include files referenced by the file.
    TYPE(String), ALLOCATABLE :: includes(:)
  END TYPE file_info_type
    
  !> Type to store dependency info from a unit.
  TYPE, EXTENDS(DependencyUnit) :: unit_info_type
    !> Index of the file that defines the unit.
    INTEGER :: ifile
  END TYPE unit_info_type
  
  !-----------------------------------------------------------------------------
  
  !> Format specification to use for reporting file specific errors.
  CHARACTER(*), PARAMETER :: fmt_file_err = "(A,': ',A)"
  
  !> Format specification to use for reporting general errors, that may 
  !! not be specific to a particular file.
  CHARACTER(*), PARAMETER :: fmt_err = "(A)"
  
  !> All errors reported by this module have the following component.
  CHARACTER(*), PARAMETER :: comp = 'DependsImplementation'
  
CONTAINS
  
  !*****************************************************************************
  !!
  !> Remove duplicate file names from a list of file names (such that only 
  !! one copy of the file name remains in the list).
  !!
  !! @param[in]     host              The compiler host object.  This 
  !! provides the file name comparison procedure used to determine whether 
  !! two filenames are the same.
  !!
  !! @param[in,out] filenames         List of file names.  If duplicate 
  !! file names are present then the size of this array will be reduced 
  !! after this call.
  !!
  !! Note that this procedure should be called on file names, not file 
  !! specifications.
  !!
  !! This procedure is only intended to act on duplicate file names.  Two 
  !! different names may still refer to the same file through the use of 
  !! operating system capability such as links, but if a user is going to 
  !! play those sorts of games then they get what they deserve.
  !!
  !! @todo Consider moving into library.
  
  SUBROUTINE RemoveDuplicates_(host, filenames)
    
    USE Strings
    USE CompilerHosts
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(CompilerHost), INTENT(IN) :: host
    TYPE(String), INTENT(INOUT), ALLOCATABLE :: filenames(:)
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: i1     ! Outer loop file name index
    INTEGER :: i2     ! Inner loop file name index
    
    ! Mask of elements in the input filenames to be retained (.TRUE.) or 
    ! discarded (.FALSE.).
    LOGICAL :: mask(SIZE(filenames))
    
    !***************************************************************************
    
    ! Progressive one way scan approach - for each file name, compare it will 
    ! all other filenames that appear later in the input list.
    mask = .TRUE.
    outer_loop: DO i1 = 1, SIZE(filenames) - 1
      DO i2 = i1 + 1, SIZE(filenames)
        IF (host%CompareFilenames(filenames(i1)%item, filenames(i2)%item)) THEN
          mask(i1) = .FALSE.
          CYCLE outer_loop
        END IF
      END DO
    END DO outer_loop
    
    filenames = PACK(filenames, mask)
    
  END SUBROUTINE RemoveDuplicates_
  
  
  !*****************************************************************************
  !!
  !> Conduct an analysis of the dependencies between a set of Fortran source 
  !! files, and print the results as a suggested order of compilation.
  !!
  !! @param[in]     file_list         List of the file specifications that 
  !! nominate the files to include in the dependency analysis.  The mapping 
  !! from file specifications to file names is defined by the host.
  !!
  !! @param[in]     root_units        List of the root units of interest.  
  !! The dependency analysis will only include the files that define these 
  !! program units and their requirements.  If both this and @a root_files 
  !! are zero size then all files provided in the input will be in the output.
  !!
  !! @param[in]     root_files        List of the root files of interest.  
  !! The dependency analysis will only include these files and their 
  !! requirements.  If both this and @a root_units are zero size then all 
  !! files provided in the input will be in the output.
  !!
  !! @param[in]     forward           Direction of the analysis - if 
  !! .TRUE. the analysis outputs files that are required by root units 
  !! or root files, if .FALSE. the analysis outputs files that depend 
  !! on the root units or root files.
  !!
  !! @param[in]     submodules        If .TRUE., then submodules that 
  !! depend on files that are dependencies of root units or root files 
  !! will also be included in the order.  Only meaningful if 
  !! @a forward is .TRUE..  Usually the dependency tree is such that 
  !! submodules can be later in the order than root units or root files.
  !!
  !! @param[in]     out_unit          Logical unit connected for formatted 
  !! output to use for reporting the results of the dependency analysis 
  !! (the names of the source files listed in the suggested order of 
  !! compilation, one source file per record).
  !!
  !! @param[in]     err_unit          Logical unit connected for formatted 
  !! output to use for reporting any errors.
  !!
  !! @param[in]     stop_code         Error code - non-zero on error with 
  !! a value consistent with the stop codes reported by the FF08Depends 
  !! program.  At this procedural level, we generate a stop code of two 
  !! ("dependency analysis completed, but with errors") for any errors.
  !!
  !! Note that the suggested compilation order may not be unique.
  
  SUBROUTINE PrintFileOrder_( file_list, root_units, root_files, forward,  &
      submodules, out_unit, err_unit, stop_code )
    
    USE CharUtils
    USE DefaultHosts
    USE Errors
    USE ErrorLevels
    USE FileDependencyPass
    USE CompilerHosts
    USE Sources
    USE SourceFiles
    USE Strings
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(String), INTENT(IN) :: file_list(:)
    TYPE(String), INTENT(IN) :: root_units(:)
    TYPE(String), INTENT(IN) :: root_files(:)
    LOGICAL, INTENT(IN) :: forward
    LOGICAL, INTENT(IN) :: submodules
    INTEGER, INTENT(IN) :: out_unit
    INTEGER, INTENT(IN) :: err_unit
    INTEGER, INTENT(OUT) :: stop_code
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! File names
    TYPE(String), ALLOCATABLE :: filenames(:)
    
    ! Root unit names in uppercase
    TYPE(String) :: upper_units(SIZE(root_units))
    
    INTEGER :: i              ! File and error index
    
    ! File includes.  This is returned from the worker procedure, but 
    ! don't use the result.
    TYPE(include_file), ALLOCATABLE :: includes(:)
    
    ! File dependency matrix.  This is calculated by the work procedure, but 
    ! we don't use the result.
    LOGICAL, ALLOCATABLE :: file_matrix(:,:)
    
    ! Array of indices into filenames that indicates the suggested compilation 
    ! order.
    INTEGER, ALLOCATABLE :: order(:)
    
    ! Error object for procedure calls.
    TYPE(Error), ALLOCATABLE :: err_list(:)
    
    ! Compiler host object to define host behaviour.
    TYPE(DefaultHost) :: host
    
    !***************************************************************************
    
    !---------------------------------------------------------------------------
    ! Prepare the list of file names
    
    ! Convert the list of file specifications to a list of file names.  In 
    ! many cases this will be a simple copy.
    CALL ExpandSpecsToNames(host, file_list, filenames, err_list)
    DO i = 1, SIZE(err_list)
      WRITE (err_unit, fmt_err) .Describe. err_list(i)
    END DO
    IF (Failure(err_list)) THEN
      stop_code = 2
      RETURN
    END IF
    
    ! Duplicate file names will confuse the dependency analysis, so get rid 
    ! of them.  The comparison procedure to detect duplicates is defined 
    ! by the host.
    CALL RemoveDuplicates(host, filenames)
    
    !---------------------------------------------------------------------------
    ! Upper case the unit names
    
    DO i = 1, SIZE(root_units)
      upper_units(i) = root_units(i)
      CALL UpperCase(upper_units(i)%item)
    END DO
    
    !---------------------------------------------------------------------------
    
    ! Parse the files, conduct the dependency analysis and get a suggested 
    ! order of compilation.
    
    ALLOCATE(file_matrix(SIZE(filenames), SIZE(filenames)))
    ALLOCATE(includes(SIZE(filenames)))
    
    CALL dependency_worker(  &
        filenames,  &
        host,  &
        upper_units,  &
        root_files,  &
        forward,  &
        submodules,  &
        includes,  &
        file_matrix,  &
        order,  &
        err_list )
    DO i = 1, SIZE(err_list)
      WRITE (err_unit, fmt_err) .Describe. err_list(i)
    END DO
    IF (Failure(err_list)) THEN
      stop_code = 2
      RETURN
    END IF
    
    ! Report the order of compilation.
    DO i = 1, SIZE(order)
      WRITE (out_unit, "(A)") filenames(order(i))%item
    END DO
    
    CALL ReleaseSources
    
    stop_code = 0
    
  END SUBROUTINE PrintFileOrder_
  
  
  !*****************************************************************************
  !!
  !> Conduct an analysis of the dependencies between a set of Fortran source 
  !! files, and print the results as a "tree" like format that is suitable 
  !! for input into many 'make' programs.
  !!
  !! @param[in]     file_list         List of the file specifications that 
  !! nominate the files to include in the dependency analysis.  The mapping 
  !! from file specifications to file names is defined by the host.
  !!
  !! @param[in]     root_units        List of the root units of interest.  
  !! The dependency analysis will only include the files that define these 
  !! program units and their requirements.  If both this and @a root_files 
  !! are zero size then all files provided in the input will be in the output.
  !!
  !! @param[in]     root_files        List of the root files of interest.  
  !! The dependency analysis will only include these files and their 
  !! requirements.  If both this and @a root_units are zero size then all 
  !! files provided in the input will be in the output.
  !!
  !! @param[in]     forward           Direction of the analysis - if 
  !! .TRUE. the analysis outputs files that are required by root units 
  !! or root files, if .FALSE. the analysis outputs files that depend 
  !! on the root units or root files.
  !!
  !! @param[in]     submodules        If .TRUE., then submodules that 
  !! depend on files that are dependencies of root units or root files 
  !! will also be included in the order.  Only meaningful if 
  !! @a forward is .TRUE..  Usually the dependency tree is such that 
  !! submodules can be later in the order than root units or root files.
  !!
  !! @param[in]     out_unit          Logical unit connected for formatted 
  !! output to use for reporting the results of the dependency analysis.
  !!
  !! @param[in]     err_unit          Logical unit connected for formatted 
  !! output to use for reporting any errors.
  !!
  !! @param[in]     output_format     Indicates the output format:
  !! - 1: Make like.
  !! - 2: dot like.
  !!
  !! @param[in]     stop_code         Error code - non-zero on error with 
  !! a value consistent with the stop codes reported by the FF08Depends 
  !! program.
  !!
  !! We define "tree" in a manner that looks nothing like a tree.
  
  SUBROUTINE PrintDependencyTree_( file_list, root_units, root_files,  &
      forward, submodules, out_unit, err_unit, output_format, stop_code )
    
    USE CharUtils
    USE CompilerHosts
    USE DefaultHosts
    USE Errors
    USE ErrorCodes
    USE ErrorLevels
    USE FileDependencyPass
    USE Sources
    USE SourceFiles
    USE Strings
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(String), INTENT(IN) :: file_list(:)
    TYPE(String), INTENT(IN) :: root_units(:)
    TYPE(String), INTENT(IN) :: root_files(:)
    LOGICAL, INTENT(IN) :: forward
    LOGICAL, INTENT(IN) :: submodules
    INTEGER, INTENT(IN) :: out_unit
    INTEGER, INTENT(IN) :: err_unit
    INTEGER, INTENT(IN) :: output_format
    INTEGER, INTENT(OUT) :: stop_code
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Compilation host.
    TYPE(DefaultHost) :: host
    
    ! File names.
    TYPE(String), ALLOCATABLE :: filenames(:)
    
    ! Root unit names in uppercase
    TYPE(String) :: upper_units(SIZE(root_units))
    
    ! File dependency matrix.  Rows supply columns, i.e. if 
    ! file_matrix(1,2) is .TRUE. then file one defines a module that is then 
    ! used in file two (so file one needs to appear before file two in the 
    ! source order).  This is the TRANSPOSE of what the GetPartition procedure 
    ! expects - see comments below.
    LOGICAL, ALLOCATABLE :: file_matrix(:,:)
    
    ! File includes.
    TYPE(include_file), ALLOCATABLE :: includes(:)
    
    ! Array of indices into filenames that indicates the suggested compilation 
    ! order.
    INTEGER, ALLOCATABLE :: order(:)
    
    ! List of errors from procedure calls.
    TYPE(Error), ALLOCATABLE :: err_list(:)
    
    INTEGER :: i            ! Utility index.
    INTEGER :: ie           ! Error index
    
    !***************************************************************************
    
    !---------------------------------------------------------------------------
    ! Prepare the list of file names
    
    ! Convert file specifications to file names.  In many cases this will be 
    ! a simple copy.
    CALL ExpandSpecsToNames(host, file_list, filenames, err_list)
    DO ie = 1, SIZE(err_list)
      WRITE (err_unit, fmt_err) .Describe. err_list(ie)
    END DO
    IF (Failure(err_list)) THEN
      stop_code = 2
      RETURN
    END IF
    
    ! Duplicate file names will confuse the dependency analysis, so get rid 
    ! of them.  The comparison procedure to detect duplicates is defined 
    ! by the host.
    CALL RemoveDuplicates(host, filenames)
    
    !---------------------------------------------------------------------------
    ! Upper case the unit names
    
    DO i = 1, SIZE(root_units)
      upper_units(i) = root_units(i)
      CALL UpperCase(upper_units(i)%item)
    END DO
    
    !---------------------------------------------------------------------------
    ! Get dependency info.
    
    ALLOCATE(file_matrix(SIZE(filenames), SIZE(filenames)))
    ALLOCATE(includes(SIZE(filenames)))
    
    CALL dependency_worker(  &
        filenames,  &
        host,  &
        upper_units,  &
        root_files,  &
        forward,  &
        submodules,  &
        includes,  &
        file_matrix,  &
        order,  &
        err_list )
    DO ie = 1, SIZE(err_list)
      WRITE (err_unit, fmt_err) .Describe. err_list(ie)
    END DO
    IF (Failure(err_list)) THEN
      stop_code = 2
      RETURN
    END IF
    
    !---------------------------------------------------------------------------
    ! Now print the information out.
    
    SELECT CASE (output_format)
    CASE (1)
      CALL tree_output_make(out_unit, filenames, order, file_matrix, includes)
      
    CASE (2)
      CALL tree_output_dot(out_unit, filenames, order, file_matrix, includes)
      
    CASE DEFAULT
      STOP 'Invalid outputformat in DependsImplementation::&
          &PrintDependencyTree_.'
      
    END SELECT
    
    CALL ReleaseSources
    
    stop_code = 0
    
  END SUBROUTINE PrintDependencyTree_
  
  
  !*****************************************************************************
  !!
  !> Expand a list of file specifications into a list of file names. 
  !!
  !! @param[in]     host              The compiler host object.
  !!
  !! @param[in]     specs             List of file specifications.
  !!
  !! @param[out]    names             List of file names.
  !!
  !! @param[out]    err_list          List of errors.
  !!
  !! A file specification may result in zero or more file names.  The nature 
  !! of that mapping is defined by the host.  The naive default mapping is 
  !! one to one (a file spec is a file name).
  
  SUBROUTINE ExpandSpecsToNames_(host, specs, names, err_list)
    
    USE Strings
    USE CompilerHosts
    USE Errors
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(CompilerHost), INTENT(IN) :: host
    TYPE(String), INTENT(IN) :: specs(:)
    TYPE(String), INTENT(OUT), ALLOCATABLE :: names(:)
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: i              ! Spec index.
    
    ! Names from each spec.
    TYPE(String), ALLOCATABLE :: items(:)
    
    ! Error list for procedure calls.
    TYPE(Error), ALLOCATABLE :: sub_err_list(:)
    
    !***************************************************************************
    
    ALLOCATE(err_list(0))
    
    !---------------------------------------------------------------------------
    
    ALLOCATE(names(0))
    DO i = SIZE(specs), 1, -1     ! backwards for historical reasons?
      CALL host%ExpandFileSpec(specs(i)%item, items, sub_err_list)
      CALL Add(err_list, sub_err_list)
      CALL Append(names, items)
    END DO
    
  END SUBROUTINE ExpandSpecsToNames_
  
  
  !*****************************************************************************
  !!
  !> Gets the order that sources (files, usually) need to be compiled based on 
  !! module references and definitions.
  !!
  !! @param[in]     source_files      The source files to test.  Duplicates 
  !! must already have been removed.
  !!
  !! @param[in]     host              Object that describes the 
  !! characteristics of the host platform.
  !!
  !! @param[in]     root_units        List of the root units of interest.  
  !! The dependency analysis will only include the files that define these 
  !! program units and their requirements.  If both this and @a root_files 
  !! are zero size then all files provided in the input will be in the 
  !! output.  Unit names should be in upper case.  Only modules, named 
  !! main programs and submodules can be specified.  Submodule specifications 
  !! should use their identifier.
  !!
  !! @param[in]     root_files        List of the root files of interest.  
  !! The dependency analysis will only include these files and their 
  !! requirements.  If both this and @a root_units are zero size then all 
  !! files provided in the input will be in the output.
  !!
  !! @param[in]     forward           If .TRUE., files listed as 
  !! depoendencies are the files that define modules (or submodules, if 
  !! @a submodules is also .TRUE.) that are required by the root 
  !! files and root units.  If false, files listed as dependencies 
  !! are files that require the modules or submodules defined 
  !! by the root files and root units.  This option does not change the 
  !! ordering of the files, merely which way down the dependency tree 
  !! the analysis is conducted.
  !!
  !! @param[in]     submodules        If .TRUE., then submodules that 
  !! depend on files that are dependencies of root units or root files 
  !! will also be included in the order.  Only meaningful if 
  !! @a forward is .TRUE..
  !!
  !! @param[out]    includes          Include files referenced by 
  !! each file processed.
  !!
  !! @param[out]    file_matrix       File dependency matrix.  Rows supply 
  !! columns, i.e. if file_matrix(1,2) is .TRUE. then file one defines a 
  !! module that is then used in file two (so file one needs to appear before 
  !! file two in the source order).
  !!
  !! @param[out]    order             The indices of the source files, ordered 
  !! in the recommended sequence of compilation.  This needs to have the same 
  !! dimension as @a source_files.
  !!
  !! @param[out]    err_list          Error list for the dependency 
  !! determination.  Note that errors associated with parsing of each file 
  !! are not returned in this list (they are available in @a statements, if 
  !! that is present).
  !!
  !! This is the one stop shop for compilation order.
  
  SUBROUTINE dependency_worker( source_files, host, root_units, root_files,  &
      forward, submodules, includes, file_matrix, order, err_list )
    
    USE CompilerHosts
    USE Debug
    USE DependencyUtils
    USE Errors
    USE ErrorCodes
    USE FileDependencyPass
    USE IntegerLists
    USE Strings
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(String), INTENT(IN) :: source_files(:)
    CLASS(CompilerHost), INTENT(IN) :: host
    TYPE(String), INTENT(IN) :: root_units(:)
    TYPE(String), INTENT(IN) :: root_files(:)
    LOGICAL, INTENT(IN) :: forward
    LOGICAL, INTENT(IN) :: submodules
    TYPE(include_file), INTENT(OUT) :: includes(SIZE(source_files))
    LOGICAL, INTENT(OUT) :: file_matrix(SIZE(source_files),SIZE(source_files))
    INTEGER, INTENT(OUT), ALLOCATABLE :: order(:)
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: if             ! Source file index.
    INTEGER :: i              ! Partition index
    
    ! Units defined by the current file.
    TYPE(DependencyUnit), ALLOCATABLE :: defines(:)
    
    ! Units referenced by the current file.
    TYPE(DependencyUnit), ALLOCATABLE :: references(:)
    
    ! Program units referenced or defined by each source file.
    TYPE(file_info_type) :: file_info(SIZE(source_files))
    
    ! All program units referenced or defined.  The location component is 
    ! used to track the location of the definition of the program unit.
    TYPE(unit_info_type), ALLOCATABLE :: unit_info(:)
    
    ! Error list for procedure calls
    TYPE(Error), ALLOCATABLE :: sub_err_list(:)
    
    ! Indices (into source_files) of the files of interest.
    INTEGER, ALLOCATABLE :: root_indices(:)
    
    ! Flag to indicate that an element in root_files was matched to a 
    ! source_file.
    LOGICAL :: file_found(SIZE(root_files))
    
    ! Flag to indicate that an element in root_units was matched to a 
    ! source file.
    LOGICAL :: unit_found(SIZE(root_units))
    
    ! Partitions
    TYPE(IntegerList), ALLOCATABLE :: part(:)
    
    ! For constructing error messages
    CHARACTER(:), ALLOCATABLE :: msg
    
    ! Has the list of root files changed as a result of considering 
    ! submodules?
    LOGICAL :: changed
    
    !***************************************************************************
    
    ALLOCATE(err_list(0))
    
    !---------------------------------------------------------------------------
    
    ALLOCATE(unit_info(0))
    ALLOCATE(root_indices(0))
    
    ! Set root* found arrays to "not found".
    file_found = .FALSE.
    unit_found = .FALSE.
    
    file_loop: DO if = 1, SIZE(source_files)
      
      CALL SourceDependencyPass(  &
          host,  &
          source_files(if)%item,  &
          references,  &
          defines,  &
          includes(if)%files,  &
          sub_err_list )
      
      ! Accumulate dependency related errors.
      CALL Add(err_list, sub_err_list)
      ! Some errors may prevent any useful information from being returned.
      IF (.NOT. ALLOCATED(references)) CYCLE
      
      ! Updates root indices, file_found and unit_found.
      CALL check_file_against_roots
      
      ! Map unit names in the definition and reference lists across to unit 
      ! indices, build the unit_info and file_info arrays.
      CALL merge_unit_info(  &
          if,  &
          defines,  &
          references,  &
          unit_info,  &
          file_info(if),  &
          sub_err_list )
      CALL Add(err_list, sub_err_list)
      
    END DO file_loop
    
    ! if root files or root units were specified, check that we've located 
    ! them all.  Puts a failure in err_list if not found.
    CALL check_all_roots_found
    
    ! If dependency related errors have been raised, then there's no point 
    ! proceding further.
    IF (Failure(err_list)) RETURN
    
    !---------------------------------------------------------------------------
    ! Build the file dependency matrix by combining the program unit 
    ! reference matrix (ref_by_file) with the program unit definition vector 
    ! (def_file) - essentially we eliminate program units as a concept here.
    !
    ! This is the TRANSPOSE of what the GetPartition procedure expects - see 
    ! comments below.
    CALL get_file_dependency_matrix(unit_info, file_info, file_matrix)
    IF (DbgFlag) CALL dump_matrix
    
    sub_module_loop: DO
      changed = .FALSE.
      
      !-------------------------------------------------------------------------
      ! Partition the file dependency matrix.  Partitions are ordered 
      ! based on their dependencies.  A partition of size greater than one 
      ! is generated for each "recycle" between files, which indicates 
      ! non-conforming source.
      
      IF (forward) THEN
        CALL GetPartitions(file_matrix, root_indices, part)
      ELSE
        ! The transpose here reverse the direction of the dependency analysis.  
        ! Further below, we also reverse the ordering of the results to 
        ! compensate.
        CALL GetPartitions(TRANSPOSE(file_matrix), root_indices, part)
      END IF
      
      !-------------------------------------------------------------------------
      ! Check for partitions with size greater than one, reversing the 
      ! partition to our final order as we go.
      
      ALLOCATE(order(0))
      DO i = SIZE(part), 1, -1
        IF (SIZE(part(i)%items) > 1) THEN
          msg = '"' // source_files(part(i)%items(1))%item // '"'
          DO if = 2, SIZE(part(i)%items) - 1
            msg = msg // ', "' // source_files(part(i)%items(if))%item  &
                // '"'
          END DO
          msg = msg // ' and "' // source_files(part(i)%items(if))%item  &
              // '".'
        
          ! At the moment this is an error.  From the language point of view 
          ! the error is really if there is a cyclic dependency between the 
          ! public parts of the various program units.  We allow for that in 
          ! the warning level given for errReferenceBeforeDefinition.  Given 
          ! that the user has asked for a source file order, and such an order 
          ! cannot be specified in the presence of cycles, we'll stick with 
          ! the error here for now.
          !
          ! This error also covers the case where there is a cyclic dependency 
          ! between modules spread across different files, because such a 
          ! cycle cannot exist without there being a cyclic file dependency 
          ! too.
          !
          ! Note that we don't explain why there is a cyclic dependency.  That 
          ! would take a little more work.
          CALL Add( err_list,  &
              CODE=errCyclicFileDependency,  &
              MSG='A cyclic dependency exists between the files ' // msg )
        END IF
        IF (forward) THEN
          order = [order, part(i)%items]
        ELSE
          order = [part(i)%items, order]
        END IF
      END DO
      
      !-------------------------------------------------------------------------
      ! Now deal with submodules.  This only has meaning in the forward sense.
      
      IF (forward .AND. submodules) THEN
        ! Look for submodule references to files that are in the list of 
        ! ordered files so far.
        DO if = 1, SIZE(file_info)
          IF (ANY(if == order)) CYCLE
          
          DO i = 1, SIZE(file_info(if)%references)
            IF (file_info(if)%references(i)%nature /= iputSubmodule) CYCLE
            IF (ANY( unit_info(file_info(if)%references(i)%iunit)%ifile  &
                == order )) THEN
              ! The submodule that references the file in the current list 
              ! of ordered files need to be added to the list root files.
              IF (.NOT. ANY(root_indices == if)) THEN
                root_indices = [root_indices, if]
                changed = .TRUE.
              END IF
            END IF
          END DO
        END DO
      END IF
      
      IF(.NOT. changed) EXIT
      DEALLOCATE(order)
    END DO sub_module_loop
    
  CONTAINS
    
    !***************************************************************************
    !
    ! Check the current (index if) file against the root files and root 
    ! units.  If the file is a root file, then add its index to the root 
    ! indices list, and flag the file or unit as found.
    !
    ! In an internal procedure for code clarity.
    
    SUBROUTINE check_file_against_roots
      
      INTEGER :: i              ! root* array index
      
      ! Flag to indicate that the file has been added to the list.
      LOGICAL :: added
      
      !*************************************************************************
      
      added = .FALSE.
      
      ! Is this source file a root file?
      file_loop: DO i = 1, SIZE(root_files)
        IF (file_found(i)) CYCLE
        IF (host%CompareFilenames(  &
            root_files(i)%item,  &
            source_files(if)%item )) THEN
          ! File is a root file.
          file_found(i) = .TRUE.
          root_indices = [root_indices, if]
          added = .TRUE.    ! So we don't add it again below.
          EXIT file_loop    ! Go onto unit checking.
        END IF
      END DO file_loop
      
      ! Did this source file define a root unit?
      unit_loop: DO i = 1, SIZE(root_units)
        ! If the unit has been defined in multiple files then this can be 
        ! true.  This is an error that is caught in MergeDefines.
        IF (unit_found(i)) CYCLE
        IF (Find(defines, root_units(i)%item) /= 0) THEN
          ! File defines a root unit.
          unit_found(i) = .TRUE.
          IF (.NOT. added) THEN
            root_indices = [root_indices, if]
            added = .TRUE.      ! So we don't add it again.
          END IF
        END IF
      END DO unit_loop
      
    END SUBROUTINE check_file_against_roots
    
    !***************************************************************************
    !
    ! Check the file_found and unit_found arrays to see if any root 
    ! specifications have been missed.
    !
    ! In an internal procedure for code clarity.
    
    SUBROUTINE check_all_roots_found
      
      INTEGER :: i            ! root* index.
      
      !*************************************************************************
      
      file_loop: DO i = 1, SIZE(root_files)
        IF (.NOT. file_found(i)) THEN
          CALL Add( err_list,  &
              CODE=errRootFileNotFound,  &
              COMPONENT=comp,  &
              MSG='The root file "' // root_files(i)%item  &
                // '" was not in the source files to examine.' )
        END IF
      END DO file_loop
      
      unit_loop: DO i = 1, SIZE(root_units)
        IF (.NOT. unit_found(i)) THEN
          CALL Add( err_list,  &
              CODE=errRootUnitNotFound,  &
              COMPONENT=comp,  &
              MSG='The root unit "' // root_units(i)%item  &
                // '" was not defined in the source files examined.' )
        END IF
      END DO unit_loop
      
    END SUBROUTINE check_all_roots_found
    
    
    !*************************************************************************
    !
    ! Write a debugging representation of the file dependency matrix to 
    ! the debug unit.
    
    SUBROUTINE dump_matrix
      
      ! Maximum length of a filename.
      INTEGER :: max_filename_length
      
      ! Number of characters required for file number.
      INTEGER :: number_length
      
      ! Buffer for output format.
      CHARACTER(30) :: fmt
      
      INTEGER :: if           ! Source file index.
      INTEGER :: i            ! Line number.
      
      ! Output item index (spurious warning workaround)
      INTEGER :: i2
      
      ! Lines to write.
      CHARACTER(:), ALLOCATABLE :: lines(:)
      
      !*************************************************************************
      
      WRITE (DbgUnit, "('file dependency matrix:')")
      
      max_filename_length = MAXVAL(Len(source_files))
      number_length = MAX(1,INT(LOG10(REAL(SIZE(source_files)))) + 1)
      
      ALLOCATE(CHARACTER(number_length+1) :: lines(SIZE(source_files)))
      WRITE (fmt, "('(I',I0,')')") number_length
      DO if = 1, SIZE(source_files)
        WRITE (lines(if), fmt) if
      END DO
      
      WRITE (fmt, "('(',I0,'X,1X,',I0,'X,',I0,'(1X,A1))')")  &
          number_length, max_filename_length, SIZE(source_files)
      DO i = 1, LEN(lines)
        WRITE (DbgUnit, fmt)  &
            (lines(i2)(i:i), i2=1, SIZE(lines))
      END DO
      
      WRITE (fmt, "('(I',I0,',1X,A',I0,',',I0,'(1X,L1))')")  &
          number_length, max_filename_length, SIZE(source_files)
      DO if = 1, SIZE(source_files)
        WRITE (DbgUnit, fmt)  &
            if,  &
            source_files(if)%item,  &
            (file_matrix(if,i2), i2=1, SIZE(file_matrix,2))
      END DO
      
    END SUBROUTINE dump_matrix
    
  END SUBROUTINE dependency_worker
  
  
  !*****************************************************************************
  !!
  !> Merge program unit definitions produced by the SourceDependencyPass 
  !! together into a common set of definitions.
  !!
  !! @param[in]     ifile             The index of the file being 
  !! considered.
  !!
  !! @param[in]     defines           The definitions for a particular 
  !! source file to be merged.
  !!
  !! @param[in,out] all_units         Information about all the program 
  !! units referenced or defined by the source files processed to date.  
  !! The location component of this argument is used to record the 
  !! location of the definition of the relevant program unit.  If it is 
  !! not allocated then the unit has been referenced, but not defined.  
  !! Dimension (num_units).
  !!
  !! @param[in,out] ref_by_file       Program unit references by file, 
  !! dimension (num_files, num_units).
  !!
  !! @param[in,out] def_file          Index of the file that defined 
  !! each program unit, or zero if no definition has been seen (dimension 
  !! num_units).
  !!
  !! @param[out]    units_def_by_file  Indicies into all_units of the 
  !! units defined by the file.
  !!
  !! @param[out]    err_list          List of errors.
  !!
  !! The defines type includes information about the file that each 
  !! program unit was defined in.
  
  SUBROUTINE merge_unit_info( ifile, defines, references,  &
      unit_info, file_info, err_list )
    
    USE Errors
    USE ErrorCodes
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    INTEGER, INTENT(IN) :: ifile
    TYPE(DependencyUnit), INTENT(IN) :: defines(:)
    TYPE(DependencyUnit), INTENT(IN) :: references(:)
    TYPE(unit_info_type), INTENT(INOUT), ALLOCATABLE :: unit_info(:)
    TYPE(file_info_type), INTENT(INOUT) :: file_info
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: id             ! Defines array index.
    INTEGER :: ir             ! References index.
    INTEGER :: iu             ! unit_info index
    
    !***************************************************************************
    
    ALLOCATE(err_list(0))
    
    !---------------------------------------------------------------------------
    ! Merege in the definitions.
    
    IF (.NOT. ALLOCATED(file_info%defined_units))  &
        ALLOCATE(file_info%defined_units(0))
    
    def_loop: DO id = 1, SIZE(defines)
      iu = Find(unit_info%DependencyUnit, defines(id)%name)
      IF (iu /= 0) THEN
        ! We've seen this unit before.  Have we seen a definition for it?
        IF (unit_info(iu)%ifile /= 0) THEN
          ! Unit defined in multiple files.  The location in all_units 
          ! has the location of the definition that we've seen so far.  
          ! We don't give these error messages explicit location information 
          ! because the whole point is that there are multiple locations.
          IF (defines(id)%type /= unit_info(iu)%type) THEN
            CALL Add( err_list,  &
                CODE=errProgramUnitMultipleDefinition,  &
                MSG=TRIM(putNames(defines(id)%type)) // ' "'  &
                  // defines(id)%name // '" is defined in "'  &
                  // defines(id)%location%ToString() // '" and also in "'  &
                  // unit_info(iu)%location%ToString() // '" as a '  &
                  // TRIM(putNames(unit_info(iu)%type)) // '.' )
          ELSE
            CALL Add( err_list,  &
                CODE=errProgramUnitMultipleDefinition,  &
                MSG=TRIM(putNames(defines(id)%type)) // ' "'  &
                  // defines(id)%name // '" is defined in "'  &
                  // defines(id)%location%ToString() // '" and also in "'  &
                  // unit_info(iu)%location%ToString() // '".' )
          END IF
        ELSE
          ! Remember where we saw the definition - both as a simple file 
          ! index and as the full source location component in the all 
          ! units array (useful for error reporting like the above).
          unit_info(iu)%ifile = ifile
          ALLOCATE(unit_info(iu)%location, SOURCE=defines(id)%location)
        END IF
      ELSE
        ! Haven't seen this unit yet.  Grow all the arrays with a unit 
        ! dimension.
        CALL append_unit_info(  &
            unit_info,  &
            ifile,  &
            defines(id)%name,  &
            defines(id)%type,  &
            defines(id)%location )
        iu = SIZE(unit_info)
      END IF
      file_info%defined_units = [file_info%defined_units, iu]
    END DO def_loop
    
    !---------------------------------------------------------------------------
    ! Merge in the references.
    
    IF (.NOT. ALLOCATED(file_info%references))  &
        ALLOCATE(file_info%references(0))
    
    ref_loop: DO ir = 1, SIZE(references)
      iu = Find(unit_info%DependencyUnit, references(ir)%name)
      IF (iu == 0) THEN
        ! We've not seen this unit before - add it.
        
        ! No location information in all_units.
        CALL append_unit_info(  &
            unit_info,  &
            0,  &
            references(ir)%name,  &
            references(ir)%type )
        iu = SIZE(unit_info)
      END IF
      file_info%references = [  &
          file_info%references,  &
          unit_reference_type(iu, references(ir)%type) ]
    END DO ref_loop
    
  END SUBROUTINE merge_unit_info
  
  
  !*****************************************************************************
  !!
  !> Get the file dependency matrix.
  !!
  !! @param[in]     unit_info         Unit information.
  !!
  !! @param[in]     file_info         File information.
  !!
  !! @param[out]    file_matrix       File dependency matrix.  Rows supply 
  !! columns, i.e. if file_matrix(1,2) is .TRUE. then file one defines a 
  !! module that is then used in file two (so file one needs to appear 
  !! before file two in the source order).  Dimension (num_files, num_files).
  
  SUBROUTINE get_file_dependency_matrix(unit_info, file_info, file_matrix)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(unit_info_type), INTENT(IN) :: unit_info(:)
    TYPE(file_info_type), INTENT(IN) :: file_info(:)
    LOGICAL, INTENT(OUT) :: file_matrix(:,:)
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: if             ! File index
    INTEGER :: ir             ! Reference index.
    
    !***************************************************************************
    
    file_matrix = .FALSE.
    
    DO if = 1, SIZE(file_info)
      ASSOCIATE(file => file_info(if))
        DO ir = 1, SIZE(file%references)
          IF (unit_info(file%references(ir)%iunit)%ifile /= 0) THEN
            file_matrix(unit_info(file%references(ir)%iunit)%ifile,if)  &
                = .TRUE.
          END IF
        END DO
      END ASSOCIATE
    END DO
    
  END SUBROUTINE get_file_dependency_matrix
  
  
  !*****************************************************************************
  !!
  !> Adds a single item to the end of the list.
  !!
  !! @param[in,out] list              The list to append to.
  !!
  !! @param]in]     ifile             File index to append.
  !!
  !! @param[in]     name              The name of the program unit to append.
  !!
  !! @param[in]     type              The type of the program unit to append.
  !!
  !! @param[in]     position          Point of declaration or reference of 
  !! the program unit.
  
  SUBROUTINE append_unit_info(list, ifile, name, type, location)
    
    USE CharUtils
    USE CompilerKinds
    USE SourceLocations
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(unit_info_type), INTENT(INOUT), ALLOCATABLE :: list(:)
    INTEGER, INTENT(IN) :: ifile
    CHARACTER(*,KIND=scck), INTENT(IN) :: name
    INTEGER, INTENT(IN) :: type
    TYPE(SourceLocation), INTENT(IN), OPTIONAL :: location
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Temporary for size change.
    TYPE(unit_info_type), ALLOCATABLE :: tmp(:)
    
    !***************************************************************************
    
    IF (.NOT. ALLOCATED(list)) THEN
      ALLOCATE(tmp(1))
    ELSE
      ALLOCATE(tmp(SIZE(list)+1))
      tmp(:SIZE(list)) = list
    END IF
    tmp(SIZE(tmp))%ifile = ifile
    tmp(SIZE(tmp))%name = name
    tmp(SIZE(tmp))%type = type
    IF (PRESENT(location)) THEN
      ALLOCATE(tmp(SIZE(tmp))%location, SOURCE=location)
    END IF
    
    CALL MOVE_ALLOC(tmp, list)
    
  END SUBROUTINE append_unit_info
  
  
  !*****************************************************************************
  !!
  !> Print the dependency tree in make format.
  !!
  !! @param[in]     out_unit          Logical unit connected for formatted 
  !! output to use for reporting the results of the dependency analysis 
  !! (the names of the source files listed in the suggested order of 
  !! compilation, one source file per record).
  !!
  !! @param[in]     filenames         Names of the files in the analysis.
  !!
  !! @param[in]     order             Suggested compilation order.
  !!
  !! @param[in]     file_matrix       Dependency matrix.  Columns and rows 
  !! correspond to @a filenames.  See the comment in the caller of this 
  !! procedure for what rows and columns mean - I can never remember.
  !!
  !! @param[in]     includes          List of include files referenced per 
  !! file.
  
  SUBROUTINE tree_output_make( out_unit, filenames, order, file_matrix,  &
      includes )
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    INTEGER, INTENT(IN) :: out_unit
    TYPE(String), INTENT(IN) :: filenames(:)
    INTEGER, INTENT(IN) :: order(:)
    LOGICAL, INTENT(IN) :: file_matrix(:,:)
    TYPE(include_file), INTENT(IN) :: includes(:)
    
    !---------------------------------------------------------------------------
    ! Local constants
    
    ! Format specification to use for the "target" part of the output (the 
    ! name of the file that has dependencies.
    CHARACTER(*), PARAMETER :: fmt_make_first = "(A,': ')"
    
    ! Format specifcation to use for a file that defines a module that is 
    ! referenced (USE'd) by a program unit in the target file.
    CHARACTER(*), PARAMETER :: fmt_make_ref = "(2X,A)"
    
    ! Format specification to use for a file that is INCLUDE'd by the 
    ! target file.
    CHARACTER(*), PARAMETER :: fmt_make_inc = "(4X,A)"
    
    ! Format specification to use to indicate that the tree information for 
    ! a particular target continues in the next record.
    CHARACTER(*), PARAMETER :: fmt_make_cont = "(' \')"
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: if           ! File index
    INTEGER :: io           ! Order index.
    
    !***************************************************************************
    
    DO io = 1, SIZE(order)
      ! Arranged such that after each item we do not terminate the record 
      ! (so each "thing" other than the first should start with termination 
      ! of the previous record).  This is to make it easier to do line 
      ! continuation (terminate each previous record with the line 
      ! continuation format spec).
      
      ! The thing being described.
      WRITE (out_unit, fmt_make_first, ADVANCE='NO') filenames(order(io))%item
      
      ! Dependencies due to modules, et. al.
      DO if = 1, SIZE(file_matrix,1)
        IF (file_matrix(if,order(io))) THEN
          WRITE (out_unit, fmt_make_cont)
          WRITE (out_unit, fmt_make_ref, ADVANCE='NO') filenames(if)%item
        END IF
      END DO
      
      ! Dependencies due to INCLUDE.
      DO if = 1, SIZE(includes(order(io))%files)
        WRITE (out_unit, fmt_make_cont)
        WRITE (out_unit, fmt_make_inc, ADVANCE='NO')  &
            includes(order(io))%files(if)%item
      END DO
      
      ! Terminate final record of the list for this thing.
      WRITE (out_unit, "()")
    END DO
    
  END SUBROUTINE tree_output_make
  
  
  !*****************************************************************************
  !!
  !> Print the dependency tree in dot format.
  !!
  !! @param[in]     out_unit          Logical unit connected for formatted 
  !! output to use for reporting the results of the dependency analysis 
  !! (the names of the source files listed in the suggested order of 
  !! compilation, one source file per record).
  !!
  !! @param[in]     filenames         Names of the files in the analysis.
  !!
  !! @param[in]     order             Suggested compilation order.
  !!
  !! @param[in]     file_matrix       Dependency matrix.  Columns and rows 
  !! correspond to @a filenames.  See the comment in the caller of this 
  !! procedure for what rows and columns mean - I can never remember.
  !!
  !! @param[in]     includes          List of include files referenced per 
  !! file.
  !!
  !! @todo Consider better handling of the include files.
  
  SUBROUTINE tree_output_dot(out_unit, filenames,order,  file_matrix, includes)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    INTEGER, INTENT(IN) :: out_unit
    TYPE(String), INTENT(IN) :: filenames(:)
    INTEGER, INTENT(IN) :: order(:)
    LOGICAL, INTENT(IN) :: file_matrix(:,:)
    TYPE(include_file), INTENT(IN) :: includes(:)
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: io           ! Order index.
    INTEGER :: i            ! Secondary index.
    
    ! If true, then the directed graph points towards the files that must be 
    ! compiled before the subject node.  Otherwise the arrow points at the 
    ! files that may be compiled after the subject node.
    LOGICAL, PARAMETER :: compiled_before = .FALSE.
    
    !***************************************************************************
    
    WRITE (out_unit, "('digraph FF08Depends {')")
    DO io = 1, SIZE(order)
      IF (COUNT(file_matrix(:,order(io))) == 0) THEN
        WRITE (out_unit, "(2X,'F',I0,1X,'[label=""',A,'"" color=gray &
            &style=filled];')")  &
            order(io), safe_dot_name(filenames(order(io))%item)
      ELSE
        WRITE (out_unit, "(2X,'F',I0,1X,'[label=""',A,'""];')")  &
            order(io), safe_dot_name(filenames(order(io))%item)
      END IF
      
      DO i = 1, SIZE(file_matrix, 1)
        IF (file_matrix(i,order(io))) THEN
          IF (compiled_before) THEN
            WRITE (out_unit, "(2X,'F',I0,' -> F',I0,';')") order(io), i
          ELSE
            WRITE (out_unit, "(2X,'F',I0,' -> F',I0,';')") i, order(io)
          END IF
        END IF
      END DO
      
      ! Perhaps?
      DO i = 1, SIZE(includes(order(io))%files)
        WRITE (out_unit, "(2X,'F',I0,'_I',I0,1X,'[label=""',A,'""];')")  &
            order(io), safe_dot_name(includes(order(io))%files(i)%item)
        WRITE (out_unit, "(2X,'F',I0,' -> F',I0,'_I',I0,';')")  &
            order(io), order(io), i
      END DO
    END DO
    WRITE (out_unit, "('}')")
    
  END SUBROUTINE tree_output_dot
  
  
  !*****************************************************************************
  !!
  !> Remove dodgy characters from a filename so that it can be used as a 
  !! label in a dot graph.
  !!
  !! @param[in]     name              The unsafe variant of the name.
  !!
  !! @returns A sterilised variant of name.  We chop out things like 
  !! newlines and double quotes (not that they could necessarily be in the 
  !! name in the first place, but you can never be too paranoid...
  !!
  !! This is pretty half-hearted.  @todo Rethink.
  
  PURE FUNCTION safe_dot_name(name) RESULT(safe)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CHARACTER(*), INTENT(IN) :: name
    
    CHARACTER(:), ALLOCATABLE :: safe
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: i              ! Character position.
    
    !---------------------------------------------------------------------------
    ! Local constants
    
    ! Characters that are just dropped.
    CHARACTER(*), PARAMETER :: bodgy_chars = ACHAR(0) // ACHAR(10)  &
        // ACHAR(13) // '"'
    
    !***************************************************************************
    
    safe = ''
    DO i = 1, LEN(name)
      ! Check characters to skip.
      IF (SCAN(name(i:i), bodgy_chars) /=0) CONTINUE
      
      ! Character is ok (apparently) - append it.
      safe = safe // name(i:i)
    END DO
    
  END FUNCTION safe_dot_name
  
END MODULE DependsImplementation
