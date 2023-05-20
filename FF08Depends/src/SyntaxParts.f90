! $Id: SyntaxParts.f90 1864 2015-09-15 00:24:21Z ian $
! ff08 source code copyright 2012 M.E.G.M.S.  See LICENCE.txt for licence.
!> @file
!! Defines the SyntaxParts module.


!*******************************************************************************
!!
!> Identifiers for syntax parts.
!!
!! "Parts" are parts of the fortran syntax at a coarser level than statements.
!!
!! Name selected because many of the parts have a syntax term that ends 
!! in -part (but not all).
!!
!! Each symbolic constant that represents a part is given a value that 
!! corresponds to the syntax rule for the part from F2008.  If new syntax 
!! rules are added by interp then we ... will think about what to do at 
!! that time.
!!
!! Old code.  Comments could do with a clean-up.

MODULE SyntaxParts
  
  IMPLICIT NONE
  
  !-----------------------------------------------------------------------------
  
  !> Length of the character variable that stores the  syntax rule name for 
  !! each part.
  INTEGER, PARAMETER, PRIVATE :: part_name_length = 30
  
  !> Utility container for the list of parts.
  TYPE, PRIVATE :: pn
    !> Part syntax rule number.
    INTEGER :: number
    !> Part name (syntax rule).
    CHARACTER(part_name_length) :: name
  END TYPE pn
  
  !> No part - we use a part number of zero to mean no part.
  INTEGER, PARAMETER :: iptNone = 0
  
  !-----------------------------------------------------------------------------
  ! Clause 2 - Fortran concepts
  
  !> Syntax element /program/ (R201).  This is the top level syntax 
  !! rule.
  INTEGER, PARAMETER :: iptProgram = 201
  
  !> The specification part of a program unit.  See (R204).
  INTEGER, PARAMETER :: iptSpecificationPart = 204
  
  !> The specification section part immediately after the USE and IMPORT 
  !! statements (R205).
  !!
  !! An implicit statement concludes this section.  A /declaration-construct/ 
  !! belongs to the parent /specification-part/.
  INTEGER, PARAMETER :: iptImplicitPart = 205
  
  !> We are in the execution part of a program unit.
  !!
  !! This could change the type of statement returned for something that 
  !! looks like a statement function.
  INTEGER, PARAMETER :: iptExecutionPart = 208
  
  !> We are in the internal subprogram-part
  INTEGER, PARAMETER :: iptInternalSubprogramPart = 210
  
  !> Syntax rule terms for contexts relevant to clause 2.
  TYPE(pn), PARAMETER, PRIVATE :: clause_2_names(5) = [  &  
      !                                 123456789012345678901234567890
      pn(iptProgram,                   'program                       '),  &
      pn(iptSpecificationPart,         'specification-part            '),  &
      pn(iptImplicitPart,              'implicit-part                 '),  &
      pn(iptExecutionPart,             'execution-part                '),  &
      pn(iptInternalSubprogramPart,    'internal-subprogram-part      ')  ]  
  
  !-----------------------------------------------------------------------------
  ! Clause 4 - Types
  
  !> A derived type definition (R425).
  INTEGER, PARAMETER :: iptDerivedTypeDef = 425
  
  !> The /private-or-sequence/ part.  This isn't really a part (it is a 
  !! single statement and we can never be sure that we are in this part 
  !! until after we are out of it), so it should probably be deleted.
  INTEGER, PARAMETER :: iptPrivateOrSequence = 428
  
  !> The components of a derived type (after parameters and before the 
  !! CONTAINS).  Similarly - the validity of this is as a useful 
  !! syntax part (from the point of view of this processor) is 
  !! questionable and it should probably be deleted.
  INTEGER, PARAMETER :: iptComponentPart = 434
  
  !> The part that starts with CONTAINS for a 
  !! /derived-type-def/ (the /type-bound-procedure-part/).
  !!
  !! This is used by the Classifier to decide whether a PRIVATE 
  !! statement is a /private-components-stmt/ (appears in the parent 
  !! /derived-type-def/) or a /binding-private-stmt/ (appears in the 
  !! /type-bound-procedure-part/).
  INTEGER, PARAMETER :: iptTypeBoundProcedurePart = 445
  
  !> We are in an enum-def.
  INTEGER, PARAMETER :: iptEnumDef = 458
  
  !> Syntax rule terms for parts relevant to clause 4.
  TYPE(pn), PARAMETER, PRIVATE :: clause_4_names(5) = [  &
      !                                 123456789012345678901234567890
      pn(iptDerivedTypeDef,            'derived-type-def              '),  &
      pn(iptPrivateOrSequence,         'private-or-sequence           '),  &
      pn(iptComponentPart,             'component-part                '),  &
      pn(iptTypeBoundProcedurePart,    'type-bound-procedure-part     '),  &
      pn(iptEnumDef,                   'enum-def                      ') ]
  
  !-----------------------------------------------------------------------------
  ! Clause 7 - Expressions and assignment
  
  !> A /where-construct/ (R742).
  INTEGER, PARAMETER :: iptWhereConstruct = 742
  
  !> A /forall-construct/ (R750).
  INTEGER, PARAMETER :: iptForallConstruct = 750
  
  !> Syntax rule terms for parts relevant to clause 7.
  TYPE(pn), PARAMETER, PRIVATE :: clause_7_names(2) = [  &
      !                                 123456789012345678901234567890
      pn(iptWhereConstruct,            'where-construct               '),  &
      pn(iptForallConstruct,           'forall-construct              ') ]
  
  !-----------------------------------------------------------------------------
  ! Clause 8 - Execution control
  
  !> A /block/ (R801).
  INTEGER, PARAMETER :: iptBlock = 801
  
  !> An /associate-construct/ (R802).
  INTEGER, PARAMETER :: iptAssociateConstruct = 802
  
  !> A /block-construct/ (R807).
  INTEGER, PARAMETER :: iptBlockConstruct = 807
  
  !> A /critical-construct/ (R810).
  INTEGER, PARAMETER :: iptCriticalConstruct = 810
  
  !> A /do-construct/ (R813).
  INTEGER, PARAMETER :: iptDoConstruct = 813
  
  !> A /block-do-construct/ (R814).
  !!
  !! Not necessarily used, but here for completeness.
  INTEGER, PARAMETER :: iptBlockDoConstruct = 814
  
  !> A /nonblock-do-construct/ (R823).
  !!
  !! Not necessarily used, but here for completeness.
  INTEGER, PARAMETER :: iptNonblockDoConstruct = 823
  
  !> An /if-construct/.
  INTEGER, PARAMETER :: iptIfConstruct = 832
  
  !> A /case-construct/.
  INTEGER, PARAMETER :: iptCaseConstruct = 838
  
  !> A /select-type-construct/.
  INTEGER, PARAMETER :: iptSelectTypeConstruct = 846
  
  !> Syntax rule terms for contexts relevant to clause 8.
  TYPE(pn), PARAMETER, PRIVATE :: clause_8_names(10) = [  &
      !                                 123456789012345678901234567890
      pn(iptBlock,                     'block                         '),  &
      pn(iptAssociateConstruct,        'associate-construct           '),  &
      pn(iptBlockConstruct,            'block-construct               '),  &
      pn(iptCriticalConstruct,         'critical-construct            '),  &
      pn(iptDoConstruct,               'do-construct                  '),  &
      pn(iptBlockDoConstruct,          'block-do-construct            '),  &
      pn(iptNonblockDoConstruct,       'nonblock-do-construct         '),  &
      pn(iptIfConstruct,               'if-construct                  '),  &
      pn(iptCaseConstruct,             'case-construct                '),  &
      pn(iptSelectTypeConstruct,       'select-type-construct         ') ]
  
  !-----------------------------------------------------------------------------
  ! Clause 11 - Program units
  
  !> The main program.
  INTEGER, PARAMETER :: iptMainProgram = 1101
  
  !> A MODULE program unit (R1104).
  INTEGER, PARAMETER :: iptModule = 1104
  
  !> The part that starts with CONTAINS in a module (R1107).
  INTEGER, PARAMETER :: iptModuleSubprogramPart = 1107
  
  !> A SUBMODULE program unit (R1116).
  INTEGER, PARAMETER :: iptSubmodule = 1116
  
  !> A BLOCK DATA program unit (R1120).
  INTEGER, PARAMETER :: iptBlockData = 1120
  
  !> Syntax rule terms for contexts relevant to clause 8.
  TYPE(pn), PARAMETER, PRIVATE :: clause_11_names(5) = [  &
      !                                 123456789012345678901234567890
      pn(iptMainProgram,               'main-program                  '),  &
      pn(iptModule,                    'module                        '),  &
      pn(iptModuleSubprogramPart,      'module-subprogram-part        '),  &
      pn(iptSubmodule,                 'submodule                     '),  &
      pn(iptBlockData,                 'block-data                    ') ]
  
  !-----------------------------------------------------------------------------
  ! Clause 12 - Procedures
  
  !> An interface block.
  INTEGER, PARAMETER :: iptInterfaceBlock = 1201
  
  !> An interface body.
  INTEGER, PARAMETER :: iptInterfaceBody = 1205
  
  !> A function program unit
  INTEGER, PARAMETER :: iptFunctionSubprogram = 1227
  
  !> A subroutine program unit.
  INTEGER, PARAMETER :: iptSubroutineSubprogram = 1233
  
  !> A separate module subprogram (R1237).
  INTEGER, PARAMETER :: iptSeparateModuleSubprogram = 1237
  
  !> Syntax rule terms for contexts relevant to clause 8.
  TYPE(pn), PARAMETER, PRIVATE :: clause_12_names(5) = [  &
      !                                 123456789012345678901234567890
      pn(iptInterfaceBlock,            'interface-block               '),  &
      pn(iptInterfaceBody,             'interface-body                '),  &
      pn(iptFunctionSubprogram,        'function-subprogram           '),  &
      pn(iptSubroutinesubprogram,      'subroutine-subprogram         '),  &
      pn(iptSeparateModuleSubprogram,  'separate-module-subprogram    ') ]
  
  !-----------------------------------------------------------------------------
  
  INTEGER, PARAMETER :: pn_size  &
      = SIZE(clause_2_names)  &
      + SIZE(clause_4_names)  &
      + SIZE(clause_7_names)  &
      + SIZE(clause_8_names)  &
      + SIZE(clause_11_names)  &
      + SIZE(clause_12_names)
  
  TYPE(pn), PARAMETER :: part_names(pn_size) = [  &
      clause_2_names,  &
      clause_4_names,  &
      clause_7_names,  &
      clause_8_names,  &
      clause_11_names,  &
      clause_12_names ]
  
  PRIVATE :: GetPartName_
  
  !> Get the syntax rule name of a statement.
  INTERFACE GetPartName
    MODULE PROCEDURE GetPartName_
  END INTERFACE GetPartName
  
CONTAINS
  
  !*****************************************************************************
  !!
  !> Get the syntax rule terms of a part based on its integer identifier.
  !!
  !! @param[in]     ipt               The part identifier.
  !!
  !! @returns The syntax rule term for the part.
  
  PURE FUNCTION GetPartName_(ipt) RESULT(name)
    
    USE CharUtils
    USE Statements
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    INTEGER, INTENT(IN) :: ipt
    
    ! Function result
    CHARACTER(:), ALLOCATABLE :: name
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: i              ! Part name index.
    
    !***************************************************************************
    
    ! Inefficient, but it will do for now.
    DO i = 1, SIZE(part_names)
      IF (part_names(i)%number == ipt) THEN
        name = TRIM(part_names(i)%name)
        RETURN
      END IF
    END DO
    
    ! Assume it is a statement.
    name = GetStName(ipt)
    
  END FUNCTION GetPartName_
  
END MODULE SyntaxParts
