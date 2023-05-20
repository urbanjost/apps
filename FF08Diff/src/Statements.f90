! $Id: Statements.f90 2874 2019-04-03 20:44:22Z ian $
! ff08 source code copyright 2012 M.E.G.M.S.  See LICENCE.txt for licence.
!> @file
!! Defines the Statements module.


!*******************************************************************************
!!
!> Symbolic constants for each type of statement that the parser considers 
!! and some constants and procedures for related statement groups.
!!
!! The ist* constants correspond to the *-stmt syntactic classes from the 
!! standard.  Values for each constant correspond to the restriction number 
!! that defines the syntax rule for the statement.  This number is specific 
!! to F2008.
!!
!! For each ist* constant we provide the syntax term from the standard 
!! document as a text string.
!!
!! The isg* constants represent various statement groups.  Their values are 
!! always negative to distinguish them from the ist values.  Which statements 
!! are in each group is given by the associated stlist_* arrays, which are 
!! processed by the StMatches generic.
!!
!! Not every isg* code has a corresponding list - membership of some groups 
!! are determined by program logic rather than look-up.
!!
!! If a revision of the standard changes the rule numbers or adds a 'B' 
!! rule in between existing rules then we would need to revise the numbers 
!! chosen below, but that change should be local to this module as client 
!! code should be using the symbolic constants and not the rule number.
!!
!! (Adding a B rule for a new statement is not likely, which is lucky as it 
!! would probably cause some distress here, or at least a selection of a 
!! silly constant value.)
!!
!! This module also provides the GetStName procedure to get a text 
!! representation of the statement name, using the syntax rule form in 
!! the standard.

MODULE Statements
  
  IMPLICIT NONE
  
  ! Entities public unless specified otherwise.
  
  !-----------------------------------------------------------------------------
  
  ! Maximum length of a syntax rule term for a statement (sans the -stmt).
  INTEGER, PARAMETER, PRIVATE :: st_name_length = 24
  
  !> Map statement numbers to statement names.
  TYPE, PRIVATE :: sn
    !> ist* number.
    INTEGER :: number
    !> Fixed length so we can use it as a parameter.
    CHARACTER(st_name_length) :: name
  END TYPE sn
  
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  ! Statement type indices
  
  ! Things that are resolved as part of our initial classification.
  !
  ! Ordered by the order of definition.
  
  !-----------------------------------------------------------------------------
  ! Internal definitions - not part of the standard
  
  !> We couldn't work out what sort of statement it was.  This is an error.
  INTEGER, PARAMETER :: istUnclassified = 0
  
  !> Generic END statement (1.3.66).
  !!
  !! end-program-stmt, end-function-stmt, end-subroutine-stmt, 
  !! end-module-stmt, end-submodule-stmt, end-blockdata-stmt and 
  !! end-mp-subprogram-stmt are potentially ambiguous if the keyword 
  !! following the END is left off.
  INTEGER, PARAMETER :: istEnd = 1
  
  !> Generic first statement
  !!
  INTEGER, PARAMETER :: istFirst = 2
  
  TYPE(sn), PARAMETER :: clause_0_names(3) = [  &
      !                            123456789012345678901234
      sn(istUnclassified,         'unclassified            '),  &
      sn(istEnd,                  'unclassified-end        '),  &
      sn(istFirst,                'unclassified-first      ') ]
  
  !-----------------------------------------------------------------------------
  ! Clause 2
  
  !-----------------------------------------------------------------------------
  ! Clause 4 - type definitions
  
  !> /A derived-type-stmt/: "TYPE[[, attrs] ::] name[(params)]" (R426).
  INTEGER, PARAMETER :: istDerivedType = 426
  
  !> An /end-type-stmt/: "END TYPE [name]" (R429).
  INTEGER, PARAMETER :: istEndType = 429
  
  !> A /sequence-stmt/: "SEQUENCE" (R430).
  INTEGER, PARAMETER :: istSequence = 430
  
  !> A /type-param-def-stmt/, which looks like a type declaration but with 
  !! a KIND or LEN attribute (R431).
  INTEGER, PARAMETER :: istTypeParamDef = 431
  
  !> A /data-component-def-stmt/, which looks like a type declaration, but
  !! it is inside a TYPE construct (R436).
  INTEGER, PARAMETER :: istDataComponentDef = 436
  
  !> A /proc-component-def-stmt/ (R440).
  INTEGER, PARAMETER :: istProcComponentDef = 440
  
  !> A /private-components-stmt/ "PRIVATE" (R444).
  INTEGER, PARAMETER :: istPrivateComponents = 444
  
  !> A /binding-private-stmt/ "PRIVATE" (R446).
  INTEGER, PARAMETER :: istBindingPrivate = 446
  
  !> A /type-bound-procedure-stmt/ (R448).
  INTEGER, PARAMETER :: istTypeBoundProcedure = 448
  
  !> A /type-bound-generic-stmt/ (R450).
  INTEGER, PARAMETER :: istTypeBoundGeneric = 450
  
  !> A /final-procedure-stmt/ (R452).
  INTEGER, PARAMETER :: istFinalProcedure = 452
  
  !> An /enum-def-stmt/: "ENUM, BIND(C)" (R459).
  INTEGER, PARAMETER :: istEnumDef = 459
  
  !> An /enumerator-def-stmt/: "ENUMERATOR [::] list" (R460).
  INTEGER, PARAMETER :: istEnumeratorDef = 460
  
  !> An /end-enum-stmt/: "END ENUM" (R462).
  INTEGER, PARAMETER :: istEndEnum = 462
  
  !> Syntax rule terms for statements defined in clause 4.
  TYPE(sn), PARAMETER, PRIVATE :: clause_4_names(14) = [  &
      !                            123456789012345678901234
      sn(istDerivedType,          'derived-type            '),  &
      sn(istEndType,              'end-type                '),  &
      sn(istSequence,             'sequence                '),  &
      sn(istTypeParamDef,         'type-param-def          '),  &
      sn(istDataComponentDef,     'data-component-def      '),  &
      sn(istProcComponentDef,     'proc-component-def      '),  &
      sn(istPrivateComponents,    'private-components      '),  &
      sn(istBindingPrivate,       'binding-private         '),  &
      sn(istTypeBoundProcedure,   'type-bound-procedure    '),  &
      sn(istTypeBoundGeneric,     'type-bound-generic      '),  &
      sn(istFinalProcedure,       'final-procedure         '),  &
      sn(istEnumDef,              'enum-def                '),  &
      sn(istEnumeratorDef,        'enumerator-def          '),  &
      sn(istEndEnum,              'end-enum                ') ]
  
  !-----------------------------------------------------------------------------
  ! Clause 5 - type and attribute declarations.
  
  !> A /type-declaration-stmt/: "type[[, attrs] ::] list" (R501).
  !!
  !! This is the "usual" variable (or parameter or function) declaration.
  INTEGER, PARAMETER :: istTypeDeclaration = 501
  
  !> An /access-stmt/: "access-spec [[::] access-id-list]" (R524).
  !!
  !! /access-spec/ can be PUBLIC or PRIVATE.
  INTEGER, PARAMETER :: istAccess = 524
  
  !> An /allocatable-stmt/: "ALLOCATABLE [::] allocatable-decl-list" (R526).
  INTEGER, PARAMETER :: istAllocatable = 526
  
  !> An /asynchronous-stmt/: "ASYNCHRONOUS [::] object-name-list" (R528).
  INTEGER, PARAMETER :: istAsynchronous = 528
  
  !> A /bind-stmt/: "BIND(xxxx) [::] binding-entity-list" (R529).
  INTEGER, PARAMETER :: istBind = 529
  
  !> A /codimension-stmt/: "CODIMENSION [::] codimension-decl-list" (R531).
  INTEGER, PARAMETER :: istCodimension = 531
  
  !> A /contiguous-stmt/: "CONTIGUOUS [::] list" (R533).
  INTEGER, PARAMETER :: istContiguous = 533
  
  !> A /data-stmt/: "DATA data-list (R534).
  INTEGER, PARAMETER :: istData = 534
  
  !> A /dimension-stmt/: "DIMENSION [::] xxx" (R545).
  INTEGER, PARAMETER :: istDimension = 545
  
  !> An /intent-stmt/: "INTENT(xxx) :: name-list" (R546).
  INTEGER, PARAMETER :: istIntent = 546
  
  !> An /optional-stmt/: "OPTIONAL [::] list" (R547).
  INTEGER, PARAMETER :: istOptional = 547
  
  !> A /parameter-stmt/: "PARAMETER (named-constant=const-expr, ...)" (R548).
  INTEGER, PARAMETER :: istParameter = 548
  
  !> A /pointer-stmt/: "POINTER [::] pointer-decl-list" (R550).
  INTEGER, PARAMETER :: istPointer = 550
  
  !> A /protected-stmt/: "PROTECTED [::] name-list" (R552).
  INTEGER, PARAMETER :: istProtected = 552
  
  !> A /save-stmt/: "SAVE [[::] saved-entity-list]" (R553).
  INTEGER, PARAMETER :: istSave = 553
  
  !> A /target-stmt/: "TARGET [::] list" (R556).
  INTEGER, PARAMETER :: istTarget = 556
  
  !> A /value-stmt/: "VALUE [::] dummy-arg-name-list" (R558).
  INTEGER, PARAMETER :: istValue = 558
  
  !> A /volatile-stmt/: "VOLATILE [::] list (R559).
  INTEGER, PARAMETER :: istVolatile = 559
  
  !> An /implicit-stmt/: "IMPLICIT NONE|implicit-spec-list" (R560).
  INTEGER, PARAMETER :: istImplicit = 560
  
  !> A /namelist-stmt/: "NAMELIST /name/ list, ..." (R563).
  INTEGER, PARAMETER :: istNamelist = 563
  
  !> An /equivalence-stmt/: "EQUIVALENCE (xxx), ..." (R565).
  INTEGER, PARAMETER :: istEquivalence = 565
  
  !> A /common-stmt/: "COMMON [/[name]/] list" (R568).
  INTEGER, PARAMETER :: istCommon = 568
  
  !> Syntax rule terms for statements defined in clause 5.
  TYPE(sn), PARAMETER, PRIVATE :: clause_5_names(22) = [  &
      !                            123456789012345678901234
      sn(istTypeDeclaration,      'type-declaration        '),  &
      sn(istAccess,               'access                  '),  &
      sn(istAllocatable,          'allocatable             '),  &
      sn(istAsynchronous,         'asynchronous            '),  &
      sn(istBind,                 'bind                    '),  &
      sn(istCodimension,          'codimension             '),  &
      sn(istContiguous,           'contiguous              '),  &
      sn(istData,                 'data                    '),  &
      sn(istDimension,            'dimension               '),  &
      sn(istIntent,               'intent                  '),  &
      sn(istOptional,             'optional                '),  &
      sn(istParameter,            'parameter               '),  &
      sn(istPointer,              'pointer                 '),  &
      sn(istProtected,            'protected               '),  &
      sn(istSave,                 'save                    '),  &
      sn(istTarget,               'target                  '),  &
      sn(istValue,                'value                   '),  &
      sn(istVolatile,             'volatile                '),  &
      sn(istImplicit,             'implicit                '),  &
      sn(istNamelist,             'namelist                '),  &
      sn(istEquivalence,          'equivalence             '),  &
      sn(istCommon,               'common                  ') ]
  
  !-----------------------------------------------------------------------------
  ! Clause 6 - Use of data objects.
  
  !> An /allocate-stmt/: "ALLOCATE([typespec ::] ...)" (R626).
  INTEGER, PARAMETER :: istAllocate = 626
  
  !> A /nullify-stmt/: "NULLIFY(list)" (R638).
  INTEGER, PARAMETER :: istNullify = 638
  
  !> A /deallocate-stmt/: "DEALLOCATE(xxx)" (R640).
  INTEGER, PARAMETER :: istDeallocate = 640
  
  !> Syntax rule terms for statements defined in clause 6.
  TYPE(sn), PARAMETER, PRIVATE :: clause_6_names(3) = [  &
      !                            123456789012345678901234
      sn(istAllocate,             'allocate                '),  &
      sn(istNullify,              'nullify                 '),  &
      sn(istDeallocate,           'deallocate              ') ]
  
  !-----------------------------------------------------------------------------
  ! Clause 7 - Expressions and assignment
  
  !> An /assignment-stmt/: "variable = expr" (R732).
  INTEGER, PARAMETER :: istAssignment = 732
  
  !> A /pointer-assignment-stmt/: "pointer-object => target" (R733).
  INTEGER, PARAMETER :: istPointerAssignment = 733
  
  !> A /where-stmt/: "WHERE (mask) where-assignment-stmt  " (R741).
  INTEGER, PARAMETER :: istWhere = 741
  
  !> A /where-construct-stmt/: "[name:] WHERE (expr)" (R743).
  INTEGER, PARAMETER :: istWhereConstruct = 743
  
  !> A /masked-elsewhere-stmt/: "ELSEWHERE (expr) [name]" (R747).
  INTEGER, PARAMETER :: istMaskedElsewhere = 747
  
  !> An /elsewhere-stmt/: "ELSEWHERE [name]" (R748).
  INTEGER, PARAMETER :: istElsewhere = 748
  
  !> An /end-where-stmt/: "END WHERE [name]" (R749).
  INTEGER, PARAMETER :: istEndWhere = 749
  
  !> A /forall-construct-stmt/: "[name:] FORALL (...)" (R751).
  INTEGER, PARAMETER :: istForallConstruct = 751
  
  !> An /end-forall-stmt/: "END FORALL [name]" (R758).
  INTEGER, PARAMETER :: istEndForall = 758
  
  !> A /forall-stmt/: "FORALL (expr) stmt" (R759).
  INTEGER, PARAMETER :: istForall = 759
  
  !> Syntax rule terms for statements defined in clause 7.
  TYPE(sn), PARAMETER, PRIVATE :: clause_7_names(10) = [  &
      !                            123456789012345678901234
      sn(istAssignment,           'assignment              '),  &
      sn(istPointerAssignment,    'pointer-assignment      '),  &
      sn(istWhere,                'where                   '),  &
      sn(istWhereConstruct,       'where-construct         '),  &
      sn(istMaskedElsewhere,      'masked-elsewhere        '),  &
      sn(istElsewhere,            'elsewhere               '),  &
      sn(istEndWhere,             'end-where               '),  &
      sn(istForallConstruct,      'forall-construct        '),  &
      sn(istEndForall,            'end-forall              '),  &
      sn(istForall,               'forall                  ') ]
  
  !-----------------------------------------------------------------------------
  ! Clause 8 - Execution control
  
  !> An /associate-stmt/: "[name:] ASSOCIATE(assoc-list)" (R803).
  INTEGER, PARAMETER :: istAssociate = 803
  
  !> An /end-associate-stmt/: "END ASSOCIATE [name]" (R806).
  INTEGER, PARAMETER :: istEndAssociate = 806
  
  !> A /block-stmt/: "[name:] BLOCK" (R808).
  INTEGER, PARAMETER :: istBlock = 808
  
  !> An /end-block-stmt/: "END BLOCK [name]" (R809).
  INTEGER, PARAMETER :: istEndBlock = 809
  
  !> A /critical-stmt/: "[name:] CRITICAL" (R811).
  INTEGER, PARAMETER :: istCritical = 811
  
  !> An /end-critical-stmt/: "END CRITICAL [name]" (R812).
  INTEGER, PARAMETER :: istEndCritical = 812
  
  !> A /label-do-stmt/: "DO ddd [loop-control]" (R816).
  !!
  !! loop-control can be "[,] var = expr, expr[, expr]" or 
  !! "[,] WHILE(expr)" or "[,] CONCURRENT forall-header".
  INTEGER, PARAMETER :: istLabelDo = 816
  
  !> A /nonlabel-do-stmt/: "[name:] DO [loop-control]" (R817).
  !!
  !! See comments for istLabelDo for the definition of loop-control.
  INTEGER, PARAMETER :: istNonlabelDo = 817
  
  !> An /end-do-stmt/: "END DO [name]" (R822).
  INTEGER, PARAMETER :: istEndDo = 822
  
  !> A /cycle-stmt/: "CYCLE" (R831).
  INTEGER, PARAMETER :: istCycle = 831
  
  !> An /if-then-stmt/: "[name:] IF (expr) THEN" (R833).
  INTEGER, PARAMETER :: istIfThen = 833
  
  !> An /else-if-stmt/: "ELSE IF (expr) THEN [name]" (R834).
  INTEGER, PARAMETER :: istElseIf = 834
  
  !> An /else-stmt/: "ELSE [name]" (R835).
  INTEGER, PARAMETER :: istElse = 835
  
  !> An /end-if-stmt/: "END IF [name]" (R836).
  INTEGER, PARAMETER :: istEndIf = 836
  
  !> An /if-stmt/: "IF (expr) action-stmt" (R837).
  INTEGER, PARAMETER :: istIf = 837
  
  !> A /select-case-stmt/: "[name:] SELECT CASE (expr)" (R839).
  INTEGER, PARAMETER :: istSelectCase = 839
  
  !> A /case-stmt/: "CASE (...)|DEFAULT [name]" (R840).
  INTEGER, PARAMETER :: istCase = 840
  
  !> An /end-select-stmt/: "END SELECT [name]" (R841).
  INTEGER, PARAMETER :: istEndSelect = 841
  
  !> A /select-type-stmt/: "[name:] SELECT TYPE ([sel=>]expr)" (R847).
  INTEGER, PARAMETER :: istSelectType = 847
  
  !> A /type-guard-stmt/: "TYPE|CLASS IS(type-spec)|DEFAULT [name]" (R848).
  INTEGER, PARAMETER :: istTypeGuard = 848
  
  ! An /end-select-type-stmt/: "END SELECT [name]" (R849) cannot be 
  ! distinguished from an end-select-stmt (R841) except by context.
  INTEGER, PARAMETER :: istEndSelectType = 849
  
  !> An /exit-stmt/: "EXIT xxx" (R850).
  INTEGER, PARAMETER :: istExit = 850
  
  !> A /goto-stmt/: "GO TO label" (R851).
  INTEGER, PARAMETER :: istGoto = 851
  
  !> A /computed-goto-stmt/: "GO TO (label-list)[,] expr" (R852).
  INTEGER, PARAMETER :: istComputedGoto = 852
  
  !> An /arithmetic-if-stmt/; "IF (scalar-numeric-expr) label, ..." (R853).
  INTEGER, PARAMETER :: istArithmeticIf = 853
  
  !> A /continue-stmt/: "CONTINUE" (R854).
  INTEGER, PARAMETER :: istContinue = 854
  
  !> A /stop-stmt/: "STOP [stop-code]" (R855).
  INTEGER, PARAMETER :: istStop = 855
  
  !> An /error-stop-stmt/: "ERROR STOP [code]" (R856).
  INTEGER, PARAMETER :: istErrorStop = 856
  
  !> A /sync-all-stmt/: "SYNC ALL (xxx)" (R858).
  INTEGER, PARAMETER :: istSyncAll = 858
  
  !> A /sync-images-stmt/: "SYNC IMAGES (xxx)" (R860).
  INTEGER, PARAMETER :: istSyncImages = 860
  
  !> A /sync-memory-stmt/: "SYNC MEMORY [ ( [ stat-list ] ) ]" (R862).
  INTEGER, PARAMETER :: istSyncMemory = 862
  
  !> A /lock-stmt/: "LOCK(xxx)" (R863).
  INTEGER, PARAMETER :: istLock = 863
  
  !> An /unlock-stmt/: "UNLOCK(xxx)" (R865).
  INTEGER, PARAMETER :: istUnlock = 865
  
  !> Syntax rule terms for statements defined in clause 8.
  TYPE(sn), PARAMETER, PRIVATE :: clause_8_names(33) = [  &
      !                            123456789012345678901234
      sn(istAssociate,            'associate               '),  &
      sn(istEndAssociate,         'end-associate           '),  &
      sn(istBlock,                'block                   '),  &
      sn(istEndBlock,             'end-block               '),  &
      sn(istCritical,             'critical                '),  &
      sn(istEndCritical,          'end-critical            '),  &
      sn(istLabelDo,              'label-do                '),  &
      sn(istNonlabelDo,           'nonlabel-do             '),  &
      sn(istEndDo,                'end-do                  '),  &
      sn(istCycle,                'cycle                   '),  &
      sn(istIfThen,               'if-then                 '),  &
      sn(istElseIf,               'else-if                 '),  &
      sn(istElse,                 'else                    '),  &
      sn(istEndIf,                'end-if                  '),  &
      sn(istIf,                   'if                      '),  &
      sn(istSelectCase,           'select-case             '),  &
      sn(istCase,                 'case                    '),  &
      sn(istEndSelect,            'end-select              '),  &
      sn(istSelectType,           'select-type             '),  &
      sn(istTypeGuard,            'type-guard              '),  &
      sn(istEndSelectType,        'end-select-type         '),  &
      sn(istExit,                 'exit                    '),  &
      sn(istGoto,                 'goto                    '),  &
      sn(istComputedGoto,         'computed-goto           '),  &
      sn(istArithmeticIf,         'arithmetic-if           '),  &
      sn(istContinue,             'continue                '),  &
      sn(istStop,                 'stop                    '),  &
      sn(istErrorStop,            'error-stop              '),  &
      sn(istSyncMemory,           'sync-memory             '),  &
      sn(istLock,                 'lock                    '),  &
      sn(istUnlock,               'unlock                  '),  &
      sn(istSyncAll,              'sync-all                '),  &
      sn(istSyncImages,           'sync-images             ') ]
  
  !-----------------------------------------------------------------------------
  ! Clause 9 - Input/output statements.
  
  !> An /open-stmt/: "OPEN(xxx)" (R904).
  INTEGER, PARAMETER :: istOpen = 904
  
  !> A /close-stmt/: "CLOSE(xxx)" (R908).
  INTEGER, PARAMETER :: istClose = 908
  
  !> A /read-stmt/: "READ (xxx) list" or READ x, ..." (R910).
  INTEGER, PARAMETER :: istRead = 910
  
  !> A /write-stmt/: "WRITE (xxx) list" (R911).
  INTEGER, PARAMETER :: istWrite = 911
  
  !> A /print-stmt/: "PRINT fmt, list" (R912).
  INTEGER, PARAMETER :: istPrint = 912
  
  !> A /wait-stmt/: "WAIT (spec-list)" (R922).
  INTEGER, PARAMETER :: istWait = 922
  
  !> A /backspace-stmt/: "BACKSPACE (xxx)" (R924).
  INTEGER, PARAMETER :: istBackspace = 924
  
  !> An /endfile-stmt/: "ENDFILE(xxx)" (R925).
  INTEGER, PARAMETER :: istEndfile = 925
  
  !> A /rewind-stmt/: "REWIND xxx" or "REWIND(xxx)" (R926).
  INTEGER, PARAMETER :: istRewind = 926
  
  !> A /flush-stmt/: "FLUSH xxx" or "FLUSH(xxx)" (R928).
  INTEGER, PARAMETER :: istFlush = 928
  
  !> An /inquire-stmt/: "INQUIRE(xxx)" (R930).
  INTEGER, PARAMETER :: istInquire = 930
  
  !> Syntax rule terms for statements defined in clause 9.
  TYPE(sn), PARAMETER, PRIVATE :: clause_9_names(11) = [  &
      !                            123456789012345678901234
      sn(istOpen,                 'open                    '),  &
      sn(istClose,                'close                   '),  &
      sn(istRead,                 'read                    '),  &
      sn(istWrite,                'write                   '),  &
      sn(istPrint,                'print                   '),  &
      sn(istWait,                 'wait                    '),  &
      sn(istBackspace,            'backspace               '),  &
      sn(istEndfile,              'endfile                 '),  &
      sn(istRewind,               'rewind                  '),  &
      sn(istFlush,                'flush                   '),  &
      sn(istInquire,              'inquire                 ') ]
  
  !-----------------------------------------------------------------------------
  ! Clause 10 - Input/output editing.
  
  !> A /format-stmt/: "FORMAT(....)" (R1001)
  INTEGER, PARAMETER :: istFormat = 1001
  
  !> Syntax rule terms for statements defined in clause 10.
  TYPE(sn), PARAMETER, PRIVATE :: clause_10_names(1) = [  &
      sn(istFormat,               'format                  ') ]
  
  !-----------------------------------------------------------------------------
  ! Clause 11 - program units
  
  !> A /program-stmt/: "PROGRAM name" (R1102).
  INTEGER, PARAMETER :: istProgram = 1102
  
  !> An /end-program-stmt/: "END [PROGRAM [name]]" (R1103).
  !!
  !! See also istEnd.
  INTEGER, PARAMETER :: istEndProgram = 1103
  
  !> A /module-stmt/: "MODULE name" (R1105).
  INTEGER, PARAMETER :: istModule = 1105
  
  !> An /end-module-stmt/: "END [MODULE [name]]" (R1106).
  !!
  !! See also istEnd.
  INTEGER, PARAMETER :: istEndModule = 1106
  
  !> A /use-stmt/: "USE[[, nature] ::] name[, ...]" (R1109).
  INTEGER, PARAMETER :: istUse = 1109
  
  !> A /submodule-stmt/: "SUBMODULE(parent) name" (R1117).
  INTEGER, PARAMETER :: istSubmodule = 1117
  
  !> An /end-submodule-stmt/: "END [SUBMODULE [name]] (R1119).
  !!
  !! See also istEnd.
  INTEGER, PARAMETER :: istEndSubmodule = 1119
  
  !> A /block-data-stmt/: "BLOCK DATA [name]" (R1121).
  INTEGER, PARAMETER :: istBlockData = 1121
  
  !> An /end-block-data-stmt/: "END BLOCK DATA [name]" (R1122).
  !!
  !! See also istEnd.
  INTEGER, PARAMETER :: istEndBlockData = 1122
  
  !> Syntax rule terms for statements defined in clause 11.
  TYPE(sn), PARAMETER, PRIVATE :: clause_11_names(9) = [  &
      !                            123456789012345678901234
      sn(istProgram,              'program                 '),  &
      sn(istEndProgram,           'end-program             '),  &
      sn(istModule,               'module                  '),  &
      sn(istEndModule,            'end-module              '),  &
      sn(istUse,                  'use                     '),  &
      sn(istSubmodule,            'submodule               '),  &
      sn(istEndSubmodule,         'end-submodule           '),  &
      sn(istBlockData,            'block-data              '),  &
      sn(istEndBlockData,         'end-block-data          ') ]
  
  !-----------------------------------------------------------------------------
  ! Clause 12 - procedures
  
  !> An /interface-stmt/: "INTERFACE [gen-spec]" (R1203).
  INTEGER, PARAMETER :: istInterface = 1203
  
  !> An /end-interface-stmt/: "END INTERFACE [gen-spec]" (R1204).
  INTEGER, PARAMETER :: istEndInterface = 1204
  
  !> A /procedure-stmt/: "[MODULE] PROCEDURE [::] list" (R1206).
  INTEGER, PARAMETER :: istProcedure = 1206
  
  !> An /import-stmt/: "IMPORT [[::] name-list]" (R1209).
  INTEGER, PARAMETER :: istImport = 1209
  
  !> An /external-stmt/: "EXTERNAL [::] name-list" (R1210).
  INTEGER, PARAMETER :: istExternal = 1210
  
  !> A /procedure-declaration-stmt/: "PROCEDURE([intf])[[, attrs] ::] list"
  !! (R1211)
  INTEGER, PARAMETER :: istProcedureDeclaration = 1211
  
  !> An /intrinsic-stmt/: "INTRINSIC (xxx)" (R1218).
  INTEGER, PARAMETER :: istIntrinsic = 1218
  
  !> A /call-stmt/: "CALL procedure-designator [(args)]" (R1220).
  INTEGER, PARAMETER :: istCall = 1220
  
  !> A /function-stmt/: "[prefix] FUNCTION name([args]) [suffix]" (R1228).
  INTEGER, PARAMETER :: istFunction = 1228
  
  !> An /end-function-stmt/: "END [FUNCTION [name]]" (R1232).
  !!
  !! See also istEnd.
  INTEGER, PARAMETER :: istEndFunction = 1232
  
  !> A /subroutine-stmt/: "[prefix] SUBROUTINE name[([args])] [binding]"
  !! (R1234).
  INTEGER, PARAMETER :: istSubroutine = 1234
  
  !> An /end-subroutine-stmt/: "END [SUBROUTINE [name]]" (R1236).
  !!
  !! See also istEnd.
  INTEGER, PARAMETER :: istEndSubroutine = 1236
  
  !> A /mp-subprogram-stmt/: "MODULE PROCEDURE name" (R1238).
  !!
  !! Note that this is dependent on context - the same series of tokens could 
  !! be a procedure-stmt.  procedure-stmt's only appear in INTERFACE blocks, 
  !! mp-subprogram-stmt's only appear in the module-subprogram part of a 
  !! module (the bit after CONTAINS).
  INTEGER, PARAMETER :: istMpSubprogram = 1238
  
  !> An /end-mp-subprogram-stmt/: "END [PROCEDURE [name]]" (R1239).
  !!
  !! See also istEnd.
  INTEGER, PARAMETER :: istEndMpSubprogram = 1239
  
  !> An /entry-stmt/: "ENTRY name [(arg-list)] suffix" (R1240).
  INTEGER, PARAMETER :: istEntry = 1240
  
  !> A /return stmt/: "RETURN [ ddd ]" (R1241).
  INTEGER, PARAMETER :: istReturn = 1241
  
  !> A /contains-stmt/: "CONTAINS" (R1242).
  INTEGER, PARAMETER :: istContains = 1242
  
  !> A stmt-function-stmt: "name(dummy-arg-list) = expr" (R1243).
  !!
  !! Deprecated.  Ambiguous with an assignment statement that assigns 
  !! to a array element.  This is the only statement that is sensitive 
  !! to previous declarations for names.
  INTEGER, PARAMETER :: istStmtFunction = 1243
  
  !> Syntax rule terms for statements defined in clause 12.
  TYPE(sn), PARAMETER, PRIVATE :: clause_12_names(18) = [  &
      !                            123456789012345678901234
      sn(istInterface,            'interface               '),  &
      sn(istEndInterface,         'end-interface           '),  &
      sn(istProcedure,            'procedure               '),  &
      sn(istImport,               'import                  '),  &
      sn(istExternal,             'external                '),  &
      sn(istProcedureDeclaration, 'procedure-declaration   '),  &
      sn(istIntrinsic,            'intrinsic               '),  &
      sn(istCall,                 'call                    '),  &
      sn(istFunction,             'function                '),  &
      sn(istEndFunction,          'end-function            '),  &
      sn(istSubroutine,           'subroutine              '),  &
      sn(istEndSubroutine,        'end-subroutine          '),  &
      sn(istMpSubprogram,         'mp-subprogram           '),  &
      sn(istEndMpSubprogram,      'end-mp-subprogram       '),  &
      sn(istEntry,                'entry                   '),  &
      sn(istReturn,               'return                  '),  &
      sn(istContains,             'contains                '),  &
      sn(istStmtFunction,         'stmt-function           ') ]
  
  !-----------------------------------------------------------------------------
  ! Statement groups and the lists for each group.
  
  !> Any statement
  INTEGER, PARAMETER :: isgAny = -1
  
  !> A statement that has a particular matching label.
  !!
  !! This is used in the Contexter in the statement field for the context map 
  !! for things like the statement that terminates a label-do block.
  INTEGER, PARAMETER :: isgMatch = -2
  
  !> An implicit-part-stmt (R206).
  INTEGER, PARAMETER :: isgImplicitPart = -206
  
  !> Statements listed under /other-specification-stmt/ (R212).
  INTEGER, PARAMETER :: isgOtherSpecification = -212
  
  !> /other-specification-stmt/.
  !!
  !! istContiguous added on the assumption that its absence was an oversight 
  !! in the standard.
  INTEGER, PARAMETER :: stlist_OtherSpecification(21) = [  &
      istAccess,          istAllocatable,     istAsynchronous,  &
      istBind,            istCodimension,     istContiguous,  &
      istCommon,          istData,            istDimension,  &
      istEquivalence,     istExternal,        istIntent,  &
      istIntrinsic,       istNamelist,        istOptional,  &
      istPointer,         istProtected,       istSave,  &
      istTarget,          istVolatile,        istValue ]
  
  !> action-stmt's.
  INTEGER, PARAMETER :: isgAction = -214
  
  !> List of statements that are action-stmt's.
  !!
  !! This excludes action-stmt's that may not be executable.
  INTEGER, PARAMETER :: stlist_Action(34) = [  &
      istAllocate,        istAssignment,        istBackspace,  &
      istCall,            istClose,             istContinue,  &
      istCycle,           istDeallocate,        istEndFile,  &
      istErrorStop,       istExit,              istFlush,  &
      istForall,          istGoto,              istIf,  &
      istInquire,         istLock,              istNullify,  &
      istOpen,            istPointerAssignment, istPrint,  &
      istRead,            istReturn,            istRewind,  &
      istStop,            istSyncAll,           istSyncImages,  &
      istSyncMemory,      istUnlock,            istWait,  &
      istWhere,           istWrite,             istArithmeticIf,  &
      istComputedGoto ]
  
  !> The action-stmt's prohibited in an execution-part by C201.
  INTEGER, PARAMETER :: stlist_ActionStruct(4) = [  &
      istEndFunction,     istEndMpSubprogram,   istEndProgram,  &
      istEndSubroutine ]
  
  !> Things that can appear in a forall-construct
  INTEGER, PARAMETER :: isgForallAssignment = -757
  
  !> See the list in (R756) and (R757).
  INTEGER, PARAMETER :: stlist_ForallAssignment(2) = [  &
      istAssignment,      istPointerAssignment ]
  
  !> do-stmt's
  INTEGER, PARAMETER :: isgDo = -815
  
  !> Any sort of construct END statement, including things that cannot just be 
  !! abbreviated to "END"
  INTEGER, PARAMETER :: isgGeneralEnd = -5
  
  !> Any sort of construct middle statement, such as an else-if-stmt, that 
  !! cannot be out on their own.
  INTEGER, PARAMETER :: isgGeneralMiddle = -6
  
  !> The statements that we consider general END statements.
  INTEGER, PARAMETER :: stlist_GeneralEnd(19) = [  &
      istEnd,             istEndProgram,      istEndSubroutine,  &
      istEndFunction,     istEndBlockData,    istEndModule,  &
      istEndSubmodule,    istEndDo,           istEndIf, &
      istEndSelect,       istEndAssociate,    istEndType,  &
      istEndInterface,    istEndMPSubprogram, istEndEnum,  &
      istEndBlock,        istEndSelectType,   istEndWhere,  &
      istEndForall ]
  
  !> The statements that we consider general middle statements.
  INTEGER, PARAMETER :: stlist_GeneralMiddle(5) = [  &
      istElseIf,          istElse,            istElsewhere,  &
      istCase,            istTypeGuard ]
  
  !> Anything that is a "program unit END statements", as per 1.3.66.
  INTEGER, PARAMETER :: isgProgramUnitEnd = -7
  
  !> The statements listed in 1.3.66.  We note that this list includes 
  !! /end-mp-subprogram-stmt/, even though a mp-subprogram is never a 
  !! program unit.
  INTEGER, PARAMETER :: stlist_ProgramUnitEnd(8) = [  &
      istEnd,             istEndBlockData,    istEndFunction,  &
      istEndModule,       istEndMPSubprogram, istEndProgram,  &
      istEndSubmodule,    istEndSubroutine ]
  
  !-----------------------------------------------------------------------------
  
  !> start index of each section
  INTEGER, PARAMETER, PRIVATE :: name_index(13) = [  &
      1,  &                                                             !  0xx
      1,  &                                                             !  1xx
      1,  &                                                             !  2xx
      1,  &                                                             !  3xx
      1 + SIZE(clause_0_names),  &                                      !  4xx
      1 + SIZE(clause_0_names) + SIZE(clause_4_names),  &               !  5xx
      1 + SIZE(clause_0_names) + SIZE(clause_4_names)  &
        + SIZE(clause_5_names),  &                                      !  6xx
      1 + SIZE(clause_0_names) + SIZE(clause_4_names)  &
        + SIZE(clause_5_names) + SIZE(clause_6_names),  &               !  7xx
      1 + SIZE(clause_0_names) + SIZE(clause_4_names)  &
        + SIZE(clause_5_names) + SIZE(clause_6_names)  &
        + SIZE(clause_7_names),  &                                      !  8xx
      1 + SIZE(clause_0_names) + SIZE(clause_4_names)  &
        + SIZE(clause_5_names) + SIZE(clause_6_names)  &
        + SIZE(clause_7_names) + SIZE(clause_8_names),  &               !  9xx
      1 + SIZE(clause_0_names) + SIZE(clause_4_names)  &
        + SIZE(clause_5_names) + SIZE(clause_6_names)  &
        + SIZE(clause_7_names) + SIZE(clause_8_names)  &
        + SIZE(clause_9_names),  &                                      ! 10xx
      1 + SIZE(clause_0_names) + SIZE(clause_4_names)  &
        + SIZE(clause_5_names) + SIZE(clause_6_names)  &
        + SIZE(clause_7_names) + SIZE(clause_8_names)  &
        + SIZE(clause_9_names) + SIZE(clause_10_names),  &              ! 11xx
      1 + SIZE(clause_0_names) + SIZE(clause_4_names)  &
        + SIZE(clause_5_names) + SIZE(clause_6_names)  &
        + SIZE(clause_7_names) + SIZE(clause_8_names)  &
        + SIZE(clause_9_names) + SIZE(clause_10_names)  &
        + SIZE(clause_11_names) ]                                        ! 12xx
  
  INTEGER, PARAMETER :: name_size  &
      = SIZE(clause_0_names)  &
      + SIZE(clause_4_names)  &
      + SIZE(clause_5_names)  &
      + SIZE(clause_6_names)  &
      + SIZE(clause_7_names)  &
      + SIZE(clause_8_names)  &
      + SIZE(clause_9_names)  &
      + SIZE(clause_10_names)  &
      + SIZE(clause_11_names)  &
      + SIZE(clause_12_names)
  
  !> All the statements names in one big list.
  TYPE(sn), PARAMETER, PRIVATE :: statement_names(name_size) = [  &
      clause_0_names,  &
      clause_4_names,  &
      clause_5_names,  &
      clause_6_names,  &
      clause_7_names,  &
      clause_8_names,  &
      clause_9_names,  &
      clause_10_names,  &
      clause_11_names,  &
      clause_12_names ]
  
  !-----------------------------------------------------------------------------
  
  ! Because the default accessibility for this module is public we explicitly 
  ! set the accessbility for the procedures to PRIVATE, so only the associated 
  ! interfaces are visible.
  
  PRIVATE :: statement_matches
  PRIVATE :: is_in_list
  PRIVATE :: GetStName_
  
  !> Test whether a statement matches another statement or group.
  INTERFACE StMatches
    MODULE PROCEDURE statement_matches
  END INTERFACE StMatches
  
  !> Get the syntax rule name of a statement.
  INTERFACE GetStName
    MODULE PROCEDURE GetStName_
  END INTERFACE GetStName
  
CONTAINS
  
  !*****************************************************************************
  !!
  !> Test whether a particular statement matches a statement or group.
  !!
  !! @param[in]     ist               Statement code to test.
  !!
  !! @param[in]     stmt_or_group     Statement or group code to test against.
  !!
  !! @returns .TRUE. if @a ist matches @a stmt_or_group, .FALSE. otherwise.
  !!
  !! This is recursive, because we call this to resolve each item in a 
  !! statement-group list.
  
  RECURSIVE PURE FUNCTION statement_matches(ist, stmt_or_group) RESULT(b)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    INTEGER, INTENT(IN) :: ist
    INTEGER, INTENT(IN) :: stmt_or_group
    
    ! Function result
    LOGICAL :: b
    
    !***************************************************************************
    
    IF (stmt_or_group >= 0) THEN
      b = ist == stmt_or_group
    ELSE
      ! We have a group of some sort.
      SELECT CASE (stmt_or_group)
      CASE (isgAny)
        b = (ist /= istUnclassified) .AND. (ist /= istFirst)
      CASE (isgForallAssignment)
        b = is_in_list(ist, stlist_ForallAssignment)
      CASE (isgAction)
        b = is_in_list(ist, stlist_Action)
      CASE (isgOtherSpecification)
        b = is_in_list(ist, stlist_OtherSpecification)
      CASE (isgGeneralEnd)
        b = is_in_list(ist, stlist_GeneralEnd)
      CASE (isgGeneralMiddle)
        b = is_in_list(ist, stlist_GeneralMiddle)
      CASE (isgProgramUnitEnd)
        b = is_in_list(ist, stlist_ProgramUnitEnd)
      CASE (isgMatch)
        ! Match based on statement label - not relevant here.
        b = .FALSE.
      CASE DEFAULT
        ! Actually an internal error, but PURE prevents us reporting it.
        b = .FALSE.
      END SELECT
    END IF
    
  END FUNCTION statement_matches
  
  
  !*****************************************************************************
  !!
  !> Test whether a statement code matches an array of statement or group 
  !! codes.
  !!
  !! @param[in]     ist               The statement code to test.
  !!
  !! @param[in]     list              The list (array) of statement or 
  !! group codes to test  against.
  !!
  !! @returns .TRUE. if @a ist matches any particular code or group in 
  !! @a list, .FALSE. otherwise.
  !!
  !! Marked recursive because group codes may require checks of further 
  !! lists.
  
  RECURSIVE PURE FUNCTION is_in_list(ist, list) RESULT(b)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    INTEGER, INTENT(IN) :: ist
    INTEGER, INTENT(IN) :: list(:)
    
    ! Function result
    LOGICAL :: b
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    INTEGER :: i              ! Index into the list.
    
    !***************************************************************************
    
    DO i = 1, SIZE(list)
      IF (statement_matches(ist, list(i))) THEN
        b = .TRUE.
        RETURN
      END IF
    END DO
    b = .FALSE.
    
  END FUNCTION is_in_list
  
  
  !*****************************************************************************
  !!
  !> Get the syntax rule name of a statement based on its integer identifier.
  !!
  !! @param[in]     ist               The statement identifier.
  !!
  !! @returns The syntax rule term for the statement (xxx-stmt).
  
  PURE FUNCTION GetStName_(ist) RESULT(name)
    
    USE CharUtils
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    INTEGER, INTENT(IN) :: ist
    
    ! Function result
    CHARACTER(:), ALLOCATABLE :: name
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: start          ! Starting index for the search.
    INTEGER :: i              ! Index into statement_names.
    
    !***************************************************************************
    
    start = ist / 100   ! integer division required
    IF (start >= 1 .AND. start <= SIZE(name_index)) THEN
      start = name_index(start)
    ELSE
      start = 1
    END IF
    
    ! Inefficient, but it will do for now.
    DO i = start, SIZE(statement_names)
      IF (statement_names(i)%number == ist) THEN
        name = TRIM(statement_names(i)%name) // '-stmt'
        RETURN
      END IF
    END DO
    
    name = ToString(ist) // '-stmt'
    
  END FUNCTION GetStName_
  
END MODULE Statements
