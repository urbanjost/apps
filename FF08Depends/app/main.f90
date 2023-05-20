! $Id: FF08Depends.f90 2960 2020-04-17 20:43:28Z ian $
! ff08 source code copyright 2012 M.E.G.M.S.  See LICENCE.txt for licence.
!> @file
!! Defines the ff08-depends program


!*******************************************************************************
!!
!> Main program for FF08Depends.
!!
!! Program manual stuff
!! ====================
!!
!! Print dependency information for Fortran sources.
!!
!! Usage:
!!  FF08Depends [options] source-file-specs ...
!!
!! Command line options:
!! - -? or --help     Display command line and program usage help, then exit.
!! - --version        Display program version information, then exit.
!! - -t or --tree     Display the dependency tree rather than the dependency 
!!                    order.  The optional argument specifies the format for 
!!                    the tree.  The default format is "make", which is a 
!!                    format that is suitable for many 'make' programs.  The 
!!                    other available format is "dot", which specifies a 
!!                    format suitable as input into the dot graph tool.
!! - --list[:] file   Specify a list file - each line from the given file will 
!!                    be added (in addition to any source file specifications 
!!                    provided on the command line) to the list of source file 
!!                    specifications to be processed.  May be specified 
!!                    multiple times.
!! - --root-unit[:] unit    Specify the name of a main program, module or 
!!                    submodule program unit, the file that contains the 
!!                    definition of the unit will be a root node of the 
!!                    dependency analysis.  To nominate a submodule the name 
!!                    is in the form of a parent-identifier - the ancestor 
!!                    module of the submodule, followed by a colon, followed 
!!                    by the name of the submodule.  May be specified multiple 
!!                    times to have multiple dependency roots.
!! - --root-file[:] name    Specify the name of a file (not a file spec) that 
!!                    will be a root node of the dependency analysis.  May be 
!!                    specified multiple times to have multiple dependency 
!!                    roots.
!! - --reverse        Reverse the meaning of a root unit or root file from 
!!                    the default, such that files listed as a consequence 
!!                    of a root file or unit are the files that consume 
!!                    modules defined by the root file or unit.
!!   --forward        Use the default meaning of a root unit or root file, 
!!                    such that files listed as a consequence of a root unit 
!!                    or root file are the files that define modules 
!!                    that are ultimately used by the root unit or file.
!! - --submodules     Include files containing submodules that are descendants 
!!                    of modules in the dependency chain.  Applicable only if 
!!                    the default meaning of a root unit or root file is in 
!!                    effect (as implied by --forward).
!!
!! (Internal options:)
!! - --dbg            Enables debugging output, written to the file 
!!                    FF08Depends.log in the current directory.
!!
!! Note that the --reverse option does not change the order in which 
!! dependencies are output (for example the compilation order is 
!! still the order that files will need to be compiled in), instead it 
!! changes the subset of files output by the analysis.  Using --reverse 
!! without any root units or root files may not be usefully different 
!! from output without --reverse.
!!
!! A source-file-spec of '-' directs that source file specifications will also 
!! be read from the console, one file per line.  A line that is zero length 
!! or all blanks terminates the input.  '-' may only be specified once on the 
!! command line.  This feature is useful with operating system shell provided 
!! pipes between programs.
!!
!! For most platforms a source file specification is the same as the name 
!! of a source file.  Some platforms may offer the ability for a source file 
!! specification to contain wildcards that may expand out to zero or more 
!! source file names.  This requires processor capability that is not 
!! described in the F2003 standard and which is not included in default 
!! builds.
!!
!! STOP codes (which may also be the program's exit code):
!! - no code:         Program completed successfully.  This will be reported 
!!                    by most processors back to the operating system as an 
!!                    exit code of zero.  (I did what you asked.)
!! - 2:               Dependency analysis commenced, but with errors.
!!                    (I know what you want me to do, but I couldn't do it.)
!! - 3:               There was a problem with the command line arguments.  
!!                    (I haven't got a clue what you want to do.)
!!
!! The dependency analysis is based on the definition and subsequent use 
!! of modules and the parent-child relationship between modules and 
!! submodules.  Files that contain a program unit that USE's a module or 
!! that defines a submodule of a module depend on the file that defines 
!! the module.
!!
!! The dependency analysis does not consider external procedures.
!!
!! Dependency analysis errors that may be reported include circular module 
!! or file references.  An example of the first case is where module A 
!! depends on (USE's) module B which depends on module C which depends back 
!! on module A.  An example of the second is where module A depends on module 
!! B which depends on module C (linear), but the definitions of modules A and 
!! C are in the same file.
!!
!! The analysis will accommodate some syntactical errors in the Fortran source 
!! but some syntax issues, in particular with any module statements, 
!! submodule statements or use statements, may prevent an accurate depiction 
!! of the dependencies.
!!
!! Error messages are written the error unit.  Dependency analysis results 
!! are written to the console.
!!
!! Examples (including interaction with OS shell)
!! ----------------------------------------------
!!
!! Perform dependency analysis of f90 files in the current directory:
!!
!! ~~~~
!! dir /b *.f90 | FF08Depends -
!! ls *.f90 | FF08Depends -
!! ~~~~
!!
!! Program unit notes
!! ==================
!!
!! The main program slices and dices command line options, reports obvious 
!! command line errors (giving rise to a stop code of three), then calls 
!! appropriate worker routines in the DependsImplementation module to do the 
!! useful work.  "Useful work" includes reading input, performining analysis 
!! and reporting output (including errors).
!!
!! @todo --host xxx.
!!
!! @todo We should probably send error messages relating to command line 
!! parsing to the error unit.
!!
!! --forward does absolutely nothing.  --reverse does something, but it might 
!! not be all that useful.

PROGRAM FF08Depends
  
  USE Strings
  USE CmdLine
  USE Debug
  USE DependsImplementation
  USE Errors
  USE FileUtilities
  USE UnitUtilities
  USE Version
  
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: OUTPUT_UNIT, ERROR_UNIT
  
  IMPLICIT NONE
  
  !-----------------------------------------------------------------------------
  ! Local constants
  
  ! Name of this program - for the --version option.
  CHARACTER(*), PARAMETER :: program_name = 'FF08Depends'
  
  !-----------------------------------------------------------------------------
  ! Local variables
  
  ! Command line option description and parsing results.
  !
  ! In order: 
  ! 1:  --help                2:  --version 
  ! 3:  --tree                4:  --list 
  ! 5:  --dbg                 6:  --root-unit
  ! 7:  --root-file           8:  --reverse
  ! 9:  --forward             10: --submodules
  TYPE(CmdLineOption) :: cmd_line_options(10)
  
  ! List of non-option arguments.
  TYPE(String), ALLOCATABLE :: arguments(:)
  
  ! List of file specifications
  TYPE(String), ALLOCATABLE :: file_specs(:)
  
  ! List of root units.  Zero size means that there were no root units 
  ! specified.
  TYPE(String), ALLOCATABLE :: root_units(:)
  
  ! List of root files.  Zero size means that there were no root files 
  ! specified.
  TYPE(String), ALLOCATABLE :: root_files(:)
  
  ! Error message
  CHARACTER(:), ALLOCATABLE :: msg
  
  INTEGER :: stat             ! Error code for ParseCmdLine call.
  INTEGER :: stop_code        ! Stop code from procedure calls
  INTEGER :: i                ! Loop index
  
  ! Set to .TRUE. if a dash appears as an argument.
  LOGICAL :: got_dash
  
  ! Set to .TRUE. if the dependency direction is forward.
  LOGICAL :: forward
  
  ! Set to .TRUE. if submodules are to be included.
  LOGICAL :: sub_modules
  
  ! Tree like format.
  CHARACTER(:), ALLOCATABLE :: tree_format
  
  !*****************************************************************************
  
  !-----------------------------------------------------------------------------
  ! Set up and process command line options.
  
  ! Keep descriptions consistent with the lead-in comments to this program 
  ! unit (hence the indent).  Line arrangements for the DESCRIPTION argument 
  ! are approximately as they'll be seen for 80 column output, by chance.
  
  cmd_line_options(1) = CmdLineOption(  &
      LONG_NAME='help',  &
      CHAR_NAME='?',  &
      DESCRIPTION='Display command line and program usage help, then exit.' )
  
  cmd_line_options(2) = CmdLineOption(  &
      LONG_NAME='version',  &
      DESCRIPTION='Display program version information, then exit.' )
  
  cmd_line_options(3) = CmdLineOption(  &
      LONG_NAME='tree',  &
      CHAR_NAME='t',  &
      DESCRIPTION='Display the dependency tree rather than the dependency &
                  &order.  The optional argument specifies the format for &
                  &the tree.  The default format is "make", which is a &
                  &format that is suitable for many ''make'' programs.  The &
                  &other available format is "dot", which specifies a &
                  &format suitable for input into the dot graph tool.',  &
      OPTION_TYPE=iOptArgOpt )
  
  cmd_line_options(4) = CmdLineOption(  &
      LONG_NAME='list',  &
      DESCRIPTION='Specify a list file - each record from the given file will &
                  &be added (in addition to any source file specifications &
                  &provided on the command line) to the list of source file &
                  &specifications to be processed.  May be specified &
                  &multiple times.',  &
      OPTION_TYPE=ioptArg,  &
      CAN_REPEAT=.TRUE. )
  cmd_line_options(6) = CmdLineOption(  &
      LONG_NAME='root-unit',  &
      DESCRIPTION='Specify the name of a main program, module or &
                  &submodule program unit, the file that contains the &
                  &definition of the unit will be a root node of the &
                  &dependency analysis.  To nominate a submodule the name &
                  &is in the form of a parent-identifier - the ancestor &
                  &module of the submodule, followed by a colon, followed &
                  &by the name of the submodule.  May be specified multiple &
                  &times to have multiple dependency roots.',  &
      OPTION_TYPE=iOptArg,  &
      CAN_REPEAT=.TRUE. )
  cmd_line_options(7) = CmdLineOption(  &
      LONG_NAME='root-file',  &
      DESCRIPTION='Specify the name of a file (not a file spec) that &
                  &will be a root node of the dependency analysis.  May be &
                  &specified multiple times to have multiple dependency &
                  &roots.',  &
      OPTION_TYPE=iOptArg,  &
      CAN_REPEAT=.TRUE. )
  cmd_line_options(8) = CmdLineOption(  &
      LONG_NAME='reverse',  &
      DESCRIPTION='Reverse the meaning of a root unit or root file from &
                  &the default, such that files listed as a consequence &
                  &of a root file or unit are the files that consume &
                  &modules defined by the root file or unit.' )
  cmd_line_options(9) = CmdLineOption(  &
      LONG_NAME='forward',  &
      DESCRIPTION='Use the default meaning of a root unit or root file, &
                  &such that files listed as a consequence of a root unit &
                  &or root file are the files that define modules &
                  &that are ultimately used by the root unit or file.' )
  cmd_line_options(10) = CmdLineOption(  &
      LONG_NAME='submodules',  &
      DESCRIPTION='Include files containing submodules that are descendants &
                  &of modules in the dependency chain.  Applicable only if &
                  &the default meaning of a root unit or root file is in &
                  &effect (as implied by --forward).' )
  
  ! Internal options:
  cmd_line_options(5) = CmdLineOption('dbg')
  
  CALL ParseCmdLine(cmd_line_options, arguments, stat, msg)
  
  IF (stat /= 0) THEN
    WRITE (ERROR_UNIT, "('Error parsing command line:',A)") msg
    CALL usage_message(OUTPUT_UNIT, cmd_line_options)
    STOP 3
  END IF
  
  ! Check for the --help (or -?) option in priority to all others.
  !
  ! If no arguments or options are provided (i.e. no command line 
  ! arguments) then we have nothing to do.  Interpret that as 
  ! a request for help as well.
  IF ( (cmd_line_options(1)%PresentCount > 0)  &
      .OR. (COMMAND_ARGUMENT_COUNT() == 0) ) THEN
    CALL usage_message(OUTPUT_UNIT, cmd_line_options)
    STOP
  END IF
  
  ! Check for the --version option.
  IF (cmd_line_options(2)%PresentCount > 0) THEN
    CALL VersionMessage(program_name)
    STOP
  END IF
  
  ! Check for the --dbg option.
  IF (cmd_line_options(5)%PresentCount > 0) THEN
    ! The Dbg* variables are global variables from the Debug module.  
    ! Setting the following flag enables debug output for all procedures 
    ! that have such output.  It is also what we use to remember that 
    ! we need to close the debugging log file.
    DbgFlag = .TRUE.
    CALL GetUnit(DbgUnit)
    OPEN( UNIT=DbgUnit,  &
        FILE=program_name //  DbgExtension,  &
        STATUS='REPLACE',  &
        ACTION='WRITE' )
    WRITE (ERROR_UNIT, "('DbgFlag set')")
  ELSE
    DbgFlag = .FALSE.
  END IF
  
  ! If - is specified on the command line then read file specs from the 
  ! input unit.  Copy other arguments over to the list of file specs.
  got_dash = .FALSE.
  DO i = 1, SIZE(arguments)
    IF (arguments(i)%item == '-') THEN
      IF (got_dash) THEN
        WRITE ( ERROR_UNIT,  &
            "('''-'' appears more than once on the command line.')" )
        STOP 3
      END IF
      got_dash = .TRUE.
      CALL AddFromConsole(file_specs, ERROR_UNIT, stop_code)
      IF (stop_code /= 0) CALL do_stop
    ELSE
      CALL Append(file_specs, arguments(i)%item)
    END IF
  END DO
  
  ! Root units.
  CALL MOVE_ALLOC(cmd_line_options(6)%Argument, root_units)
  IF (.NOT. ALLOCATED(root_units)) ALLOCATE(root_units(0))
  
  ! Root files.
  CALL MOVE_ALLOC(cmd_line_options(7)%Argument, root_files)
  IF (.NOT. ALLOCATED(root_files)) ALLOCATE(root_files(0))
  
  ! Output submodule files?
  sub_modules = cmd_line_options(10)%PresentCount /= 0
  
  ! Dependency direction.
  forward = .TRUE.
  IF (cmd_line_options(8)%PresentCount /= 0) THEN
    IF (cmd_line_options(9)%PresentCount /= 0) THEN
      WRITE ( ERROR_UNIT,  &
          "('Both --forward and --reverse have been specified on &
          &the command line.')" )
      STOP 3
    END IF
    IF (sub_modules) THEN
      WRITE ( ERROR_UNIT,  &
          "('--submodules has no meaning if --reverse is specified on &
          &the command line.')" )
      STOP 3
    END IF
    forward = .FALSE.
  END IF
  
  ! Calls to do_stop after this point bypass debug file cleanup.
  
  ! If there are any list files specified then expand the list of files 
  ! to accomodate their contents.
  DO i = 1, cmd_line_options(4)%PresentCount
    CALL AddListFile(  &
        file_specs,  &
        cmd_line_options(4)%Argument(i)%item,  &
        ERROR_UNIT,  &
        stop_code )
    IF (stop_code /= 0) CALL do_stop
  END DO
  
  !-----------------------------------------------------------------------------
  ! Parse the files, analyse the dependencies and print the results.
  
  ! The type of output is determined by the presence or otherwise of the 
  ! --tree (or -t) option.
  !
  ! Both branches of the following set stop_code, hopefully (from the users 
  ! point of view) to zero, if they complete.
  IF (cmd_line_options(3)%PresentCount == 0) THEN
    ! Just a simple list of files.
    CALL PrintFileOrder(  &
        file_specs,  &
        root_units,  &
        root_files,  &
        forward,  &
        sub_modules,  &
        OUTPUT_UNIT,  &
        ERROR_UNIT,  &
        stop_code )
  ELSE
    IF (ALLOCATED(cmd_line_options(3)%Argument)) THEN
      tree_format = cmd_line_options(3)%Argument(1)%item
    ELSE
      tree_format = 'make'
    END IF
    
    SELECT CASE (tree_format)
    CASE ('make')
      ! The make-like results.
      CALL PrintDependencyTree(  &
          file_specs,  &
          root_units,  &
          root_files,  &
          forward,  &
          sub_modules,  &
          OUTPUT_UNIT,  &
          ERROR_UNIT,  &
          1,  &
          stop_code )
    CASE ('dot')
      CALL PrintDependencyTree(  &
          file_specs,  &
          root_units,  &
          root_files,  &
          forward,  &
          sub_modules,  &
          OUTPUT_UNIT,  &
          ERROR_UNIT,  &
          2,  &
          stop_code )
    CASE DEFAULT
      WRITE ( ERROR_UNIT,  &
            "('Invalid argument ""',A,'"" for the --tree or -t option.')" )  &
          tree_format
      WRITE ( ERROR_UNIT,  &
          "('Valid options are ""make"" (default) or ""dot"".')" )
      STOP 3
    END SELECT
  END IF
  
  ! A stop code of three indicates a problem with command line input, so be 
  ! helpful and tell the user what the command line should look like.  This 
  ! also saves us from having to pass the help output procedure along as 
  ! an actual argument (which you can't do in F2003 when it is an internal 
  ! procedure, and we don't want to have to make it a module procedure).
  IF (stop_code == 3) CALL usage_message(OUTPUT_UNIT, cmd_line_options)
  
  !-----------------------------------------------------------------------------
  ! Cleanup and shut down.
  
  IF (DbgFlag) THEN
    CLOSE(DbgUnit)
    CALL ReleaseUnit(DbgUnit)
  END IF
  
  CALL do_stop
  
CONTAINS
  
  ! F2003 doesn't allow an expression as a stop-code (F2008 relaxes this 
  ! to a constant expression, which is still frustrating) so this is 
  ! a work around.
  SUBROUTINE do_stop
    SELECT CASE (stop_code)
    CASE (0)   ! Fall through to end statement.
    CASE (1) ; STOP 1
    CASE (2) ; STOP 2
    CASE DEFAULT;  STOP 3
    END SELECT
  END SUBROUTINE do_stop
  
  
  !*****************************************************************************
  !!
  !> Prints the command line usage message for the program.  This is 
  !! intended as the handler for '--help' or similar.
  !!
  !! @param[in]     out_unit          Logical unit connected for formatted 
  !! output to receieve the usage message.
  !!
  !! @param[in]     cmd_line_options  Command line options defined for the 
  !! program.
  !!
  !! Try and keep this consistent with the description in the lead-in 
  !! comments for the main program, or users and (more importantly) 
  !! programmers will get confused.
  
  SUBROUTINE usage_message(out_unit, cmd_line_options)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    INTEGER, INTENT(IN) :: out_unit
    TYPE(CmdLineOption), INTENT(IN) :: cmd_line_options(:)
    
    !---------------------------------------------------------------------------
    ! Local constants
    
    ! Format specification for a lines worth of message.
    CHARACTER(*), PARAMETER :: fmt_a = "(A)"
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! First argument on command line, which is normally the program name.
    CHARACTER(:), ALLOCATABLE :: local_program_name
    
    INTEGER :: stat           ! Error code for command line argument query.
    
    !***************************************************************************
    
    WRITE (out_unit, fmt_a) 'Print dependency information for Fortran sources.'
    WRITE (out_unit, "()")
    
    ! Use actual program name in description, in case we've been renamed.
    CALL GetCmdLineArgument(0, local_program_name, stat)
    IF (stat /= 0) local_program_name = program_name
    
    WRITE (out_unit, fmt_a) 'Usage: ' // local_program_name  &
        // ' [options] source-file-specs...'
    WRITE (out_unit, "()")
    WRITE (out_unit, fmt_a) 'Valid options are:'
    WRITE (out_unit, "()")
    
    CALL WriteUsageMessage(cmd_line_options, out_unit)
    
    ! Short and simple - we won't elaborate on names versus spec's here.  We 
    ! rely on format reversion to do multiple records for each 80 character 
    ! (approx) line.
    WRITE (out_unit, fmt_a) 'Note that the --reverse option does not change &
        &the order in which dependencies ','are output (for example the &
        &compilation order is still the order that files ','will need to &
        &be compiled in), instead its changes the subset of files output ','&
        &by the analysis.  Using --reverse without any root units or root &
        &files may ','not be usefully different from output without --reverse.'
    WRITE (out_unit, "()")
    WRITE (out_unit, fmt_a) 'source-file-specs is a list of the names of &
        &the Fortran source files to ', 'parse.  If there is a file named - &
        &then additional file names will be read ', 'from the console (one &
        &per record).'
    
  END SUBROUTINE usage_message
  
END PROGRAM FF08Depends
