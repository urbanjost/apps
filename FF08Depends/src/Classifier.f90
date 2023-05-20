! $Id: Classifier.f90 2877 2019-04-05 21:27:32Z ian $
! ff08 source code copyright 2012 M.E.G.M.S.  See LICENCE.txt for licence.
!> @file
!! Defines the Classifier module.


!*******************************************************************************
!!
!> Procedures and parameters to classify statements.
!!
!! The objective of classification is to determine the statement type 
!! to a good-enough degree of accuracy, that passes that depend on the 
!! statement type can do their job effectively.
!!
!! Some statement types depend on their context.  Classification is 
!! designed that a reasonable guess at the statement type will be returned 
!! even in the absence of context.  Note though, that if context is 
!! provided then a different statement type might be determined.
!!
!! Even if perfect context is provided, it is not possible to discern 
!! statement function statements from assignment statements - higher 
!! level passes (that have access to entity declarations) need to make 
!! that call.
!!
!! - is_*:    Functions that return a boolean that might be useful in 
!!   IF (...) statements.
!! - test_*:  Subroutines that test the statement type of a token line 
!!   against a restricted set.  The second argument will be the statement 
!!   type.
!! - match_*: Subroutines that check some sort of condition.  The 
!!   second argument will be the token past the matched sequence.
!!
!! This module uses the StmtFunctionStatementUtils module, so it is important 
!! that the dependencies of that module don't create a cyclic loop.

MODULE Classifier
  
  USE CompilerKinds
  USE Tokens
  
  IMPLICIT NONE
  
  !-----------------------------------------------------------------------------
  ! Interfaces
  
  !> Classify a sequence of tokens in (or into) a statement.
  INTERFACE Classify
    MODULE PROCEDURE Classify_
  END INTERFACE Classify
  
  INTERFACE is_breakable
    MODULE PROCEDURE is_breakable_2
    MODULE PROCEDURE is_breakable_3
  END INTERFACE is_breakable
  
  INTERFACE is_keywords
    MODULE PROCEDURE is_keywords_2
  END INTERFACE is_keywords
  
CONTAINS
  
  !*****************************************************************************
  !!
  !> Determines the type of statement of a sequence of tokens.
  !!
  !! @param[in]     tlist             The sequence of tokens,  with 
  !! keywords and names uppercased.
  !!
  !! @param[in]     part_stack        The current syntax part stack.  This 
  !! allows disambiguation between different statements that have the same 
  !! keywords.
  !!
  !! @param[out]    ist               Statement number - an ist* constant.
  
  SUBROUTINE Classify_(tlist, part_stack, ist)
    
    USE Errors
    USE CharUtils
    USE Tokens
    USE MatchUtils
    USE Statements
    USE StmtFunctionStmtsUtils
    USE SyntaxParts
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Token), INTENT(IN) :: tlist(:)
    INTEGER, INTENT(IN) :: part_stack(:)
    INTEGER, INTENT(OUT) :: ist
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    LOGICAL :: has_name       ! Construct name flag.
    
    !***************************************************************************
    
    ! Initially we split into assignment-like (assignment and pointer 
    ! assignment) and "other".  Whether something is an assignment-like 
    ! statement is determined based on the presence of the = or => token.
    ! But initialisers for various declaration statements and do-stmt's 
    ! can confuse this.
    !
    ! However anything with an initialiser also has a double colon, so 
    ! test for a bare one of those first.
    IF (HasBare(tlist, ittDeclare, scck_'::')) THEN
      ! Could be a use-stmt or a type-declaration-stmt or an attribute 
      ! stmt, definitely not pointer assignment or assignment.
      CALL classify_not_assignment(tlist, part_stack, ist, has_name)
      
    ! do-stmt's that have a '=' token will also have a bare comma.
    !
    ! DO i = 1, 2
    !
    ! This will also catch older style type declaration-like stmt's where 
    ! there is no initialiser, no attributes and more than one variable 
    ! being declared (INTEGER a, b, c).
    ELSE IF (HasBare(tlist, ittComma, scck_',')) THEN
      ! Anything with a bare list, which includes type-declaration-stmts, 
      ! do-stmts, use statements (with rename clause).  Isn't an assignment 
      ! statement.
      CALL classify_not_assignment(tlist, part_stack, ist, has_name)
      
    ! From this point on, anything with a bare assignment token has an 
    ! assignment statement somewhere in it or is a statement function 
    ! statement, and anything with a bare associate token has a pointer 
    ! assignment statement somewhere in it.  However, we still need 
    ! to differentiate single statement forms of some constructs - the 
    ! assignment or pointer assignment statement might be a child 
    ! statement of a FORALL or similar.
    ELSE IF (is_assignment(tlist)) THEN
      
      ! Could also be a stmt-function-stmt.
      IF ( stack_top_match(part_stack, iptSpecificationPart)  &
          .OR. stack_top_match(part_stack, iptImplicitPart) ) THEN
        IF (IsMaybeStmtFunction(tlist)) THEN
          ist = istStmtFunction
          RETURN
        END IF
      END IF
      
      ist = istAssignment
      
    ELSE IF (is_pointer_assignment(tlist)) THEN
      
      ist = istPointerAssignment
      
    ! Left-overs include older style type declarations (no ::) with 
    ! only one variable (INTEGER a) and lots of code structure type 
    ! statements (MODULE xxx, PROGRAM yyy, INTERFACE, FUNCTION, etc)
    ELSE
      
      CALL classify_not_assignment(tlist, part_stack, ist, has_name)
      
    END IF
    
  END SUBROUTINE Classify_
  
  
  !*****************************************************************************
  !!
  !> Classification of tokens into a statement, where we know that the 
  !! tokens are not part of an assignment-like statement.
  !!
  !! @param[in]     tlist             The sequence of tokens to be 
  !! classified, with keywords and names uppercased.
  !!
  !! @param[in]     part_stack        The current syntax part stack.
  !!
  !! @param[out]    ist               The statement index.
  !!
  !! @param[out]    has_name          Flag to indicate whether the statement 
  !! had a construct name.
  !!
  !! Because this isn't an assignment-like statement we know that the first 
  !! tokens in tlist are not a variable designator.
  !!
  !! (not assignment-like means that the statement is definitely not 
  !! an assignment-stmt or an pointer-assignment-stmt.)
  !!
  !! @todo Sort into executable, specification and other, start search 
  !! in the appropriate group based on the top of @a part_stack.  Within 
  !! each group sort alphabetically and start search based on first 
  !! character of the token.
  !!
  !! @todo Check that we have every type of statement available.
  
  SUBROUTINE classify_not_assignment(tlist, part_stack, ist, has_name)
    
    USE SyntaxParts
    USE Errors
    USE Tokens
    USE Statements
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Token), INTENT(IN) :: tlist(:)
    INTEGER, INTENT(IN) :: part_stack(:)
    INTEGER, INTENT(OUT) :: ist
    LOGICAL, INTENT(OUT) :: has_name
    
    !---------------------------------------------------------------------------    
    ! Local variables
    
    INTEGER :: it             ! Index of the token after a construct name.
    
    !***************************************************************************
    
    ! Do we have a construct name at the start?
    CALL match_construct_name(tlist, it)
    IF (it /= 0) THEN
      ! Yes - classify the bit sans the construct name.
      ! stmt%has_construct = .TRUE.
      has_name = .TRUE.
      CALL classify_not_assign_no_name(tlist(it:), part_stack, ist)
    ELSE
      ! No - classify the whole token sequence.
      CALL classify_not_assign_no_name(tlist, part_stack, ist)
    END IF
    
  END SUBROUTINE classify_not_assignment
  
  
  !*****************************************************************************
  !!
  !> Classification of tokens into a statement, where we know that the 
  !! tokens are not part of an assignment statement and that there isn't 
  !! a leading construct name.
  !!
  !! @param[in]     tlist             The tokens to be classified, with 
  !! keywords and names uppercased.
  !!
  !! @param[in]     part_stack        The syntax part stack.
  !!
  !! @param[out]    ist               The statement index.
  !!
  !! Because this isn't an assignment-like statement we know that the first 
  !! tokens in tlist are not a variable designator.
  !!
  !! We can test the first token to find out what sort of statement this is 
  !! (in most cases - some statements can have prefix tokens before you 
  !! get to the signficant token).
  
  SUBROUTINE classify_not_assign_no_name(tlist, part_stack, ist)
    
    USE Errors
    USE Tokens
    USE Statements
    USE SyntaxParts
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Token), INTENT(IN) :: tlist(:)
    INTEGER, INTENT(IN) :: part_stack(:)
    INTEGER, INTENT(OUT) :: ist
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: it             ! Token index.
    
    ! Value of the first token.
    CHARACTER(:,KIND=scck), ALLOCATABLE :: value
    
    !***************************************************************************
    
    ! Assume failure
    ist = istUnclassified
    
    ! Deal with the easy cases first.
    IF (SIZE(tlist) < 1) RETURN
    IF (tlist(1) /= ittName) RETURN
    value = QueryValue(tlist(1))
    IF (LEN(value) == 0) RETURN
    
    ! Group together alphabetically as this might make things a little more 
    ! efficient.  Any path through here that doesn't assign something to 
    ! ist results in istUnclassified.
    SELECT CASE (value(1:1))
    CASE (scck_'A')
      IF (value == scck_'ABSTRACT') THEN
        ist = istInterface
      ELSE IF (value == scck_'ALLOCATABLE') THEN
        ist = istAllocatable
      ELSE IF (value == scck_'ALLOCATE') THEN
        ist = istAllocate
      ELSE IF (value == scck_'ASSOCIATE') THEN
        ist = istAssociate
      ELSE IF (value == scck_'ASYNCHRONOUS') THEN
        ist = istAsynchronous
      END IF
      
    CASE (scck_'B')
      IF (value == scck_'BACKSPACE') THEN
        ist = istBackspace
      ELSE IF (value == scck_'BIND') THEN
        ist = istBind
      ELSE IF (is_breakable(tlist, scck_'BLOCK', scck_'DATA')) THEN
        ist = istBlockData
      ELSE IF (value == scck_'BLOCK') THEN
        ist = istBlock
      END IF
      
    CASE (scck_'C')
      IF (value == scck_'CASE') THEN
        ist = istCase
      ELSE IF (value == scck_'CALL') THEN
        ist = istCall
      ELSE IF (value == scck_'CHARACTER') THEN
        CALL test_function(tlist, ist)
        IF (ist == istUnclassified) CALL test_dtcomponent(part_stack, ist)
      ELSE IF (value == scck_'CLASS') THEN
        IF (SIZE(tlist) > 1) THEN
          IF (IsKeyword(tlist(2), scck_'IS')) THEN
            ist = istTypeGuard    ! CLASS IS (xxx) [construct-name]
          ELSE IF (IsKeyword(tlist(2), scck_'DEFAULT')) THEN
            ist = istTypeGuard    ! CLASS DEFAULT [construct-name]
          ELSE
            CALL test_function(tlist, ist)
            IF (ist == istUnclassified)  &
                CALL test_dtcomponent(part_stack, ist)
          END IF
        END IF
      ELSE IF (value == scck_'CLOSE') THEN
        ist = istClose
      ELSE IF (value == scck_'CODIMENSION') THEN
        ist = istCodimension
      ELSE IF (value == scck_'COMMON') THEN
        ist = istCommon
      ELSE IF (value == scck_'COMPLEX') THEN
        CALL test_function(tlist, ist)
        IF (ist == istUnclassified) CALL test_dtcomponent(part_stack, ist)
      ELSE IF (value == scck_'CONTAINS') THEN
        ist = istContains
      ELSE IF (value == scck_'CONTIGUOUS') THEN
        ist = istContiguous
      ELSE IF (value == scck_'CONTINUE') THEN
        ist = istContinue
      ELSE IF (value == scck_'CRITICAL') THEN
        ist = istCritical
      ELSE IF (value == scck_'CYCLE') THEN
        ist = istCycle
      END IF
      
    CASE (scck_'D')
      IF (value == scck_'DATA') THEN
        ist = istData
      ELSE IF (value == scck_'DEALLOCATE') THEN 
        ist = istDeallocate
      ELSE IF (value == scck_'DIMENSION') THEN
        ist = istDimension
      ELSE IF (value == scck_'DO') THEN
        IF (SIZE(tlist) > 1) THEN
          IF (tlist(2) == ittIntegerLiteral) THEN
            ist = istLabelDo
          ELSE
            ist = istNonlabelDo
          END IF
        ELSE
          ist = istNonlabelDo
        END IF
      ELSE IF (is_breakable(tlist, scck_'DOUBLE', scck_'PRECISION')) THEN
        CALL test_function(tlist, ist)
        IF (ist == istUnclassified) CALL test_dtcomponent(part_stack, ist)
      END IF
      
    CASE (scck_'E')
      IF (value == scck_'ELSE') THEN
        IF (SIZE(tlist) > 1) THEN
          IF (tlist(2) == scck_'IF') THEN
            ist = istElseIf
          ELSE IF (tlist(2) == scck_'WHERE') THEN
            ! Decide between istMaskedElsewhere and istElsewhere based on 
            ! the presence of parentheses.
            CALL test_elsewhere(tlist, 3, ist)
          ELSE
            ist = istElse
          END IF
        ELSE
          ist = istElse
        END IF
      ELSE IF (value == scck_'ELSEIF') THEN
        ist = istElseIf
      ELSE IF (value == scck_'ELSEWHERE') THEN
        CALL test_elsewhere(tlist, 2, ist)
      ELSE IF (is_breakable(tlist, scck_'END', scck_'PROGRAM')) THEN
        ist = istEndProgram
      ELSE IF (is_breakable(tlist, scck_'END', scck_'FUNCTION')) THEN
        ist = istEndFunction
      ELSE IF (is_breakable(tlist, scck_'END', scck_'SUBROUTINE')) THEN
        ist = istEndSubroutine
      ELSE IF (is_breakable(tlist, scck_'END', scck_'MODULE')) THEN
        ist = istEndModule
      ELSE IF (is_breakable(tlist, scck_'END', scck_'SUBMODULE')) THEN
        ist = istEndSubmodule
      ELSE IF (is_breakable(tlist, scck_'END', scck_'PROCEDURE')) THEN
        ist = istEndMPSubprogram
      ELSE IF (is_breakable(tlist, scck_'END', scck_'TYPE')) THEN
        ist = istEndType
      ELSE IF (is_breakable(tlist, scck_'END', scck_'INTERFACE')) THEN
        ist = istEndInterface
      ELSE IF (is_breakable(tlist, scck_'END', scck_'ASSOCIATE')) THEN
        ist = istEndAssociate
      ELSE IF (is_breakable(tlist, scck_'END', scck_'BLOCK', scck_'DATA')) THEN
        ! /end-block-data/ or /end-block/ with a construct name of DATA?  
        ! We assume /end-block/ unless we are in a block-data program unit.
        IF (part_depth(part_stack, iptBlockData) /= 0) THEN
          ist = istEndBlockData
        ELSE
          ist = istEndBlock
        END IF
      ELSE IF (is_breakable(tlist, scck_'END', scck_'BLOCK')) THEN
        ist = istEndBlock
      ELSE IF (is_breakable(tlist, scck_'END', scck_'SELECT')) THEN
        IF ( part_depth(part_stack, iptSelectTypeConstruct)  &
            > part_depth(part_stack, iptCaseConstruct) ) THEN
          ist = istEndSelectType
        ELSE
          ist = istEndSelect
        END IF
      ELSE IF (is_breakable(tlist, scck_'END', scck_'CRITICAL')) THEN
        ist = istEndCritical
      ELSE IF (is_breakable(tlist, scck_'END', scck_'DO')) THEN
        ist = istEndDo
      ELSE IF (is_breakable(tlist, scck_'END', scck_'IF')) THEN
        ist = istEndIf
      ELSE IF (is_breakable(tlist, scck_'END', scck_'ENUM')) THEN
        ist = istEndEnum
      ELSE IF (is_breakable(tlist, scck_'END', scck_'FILE')) THEN
        ist = istEndFile
      ELSE IF (is_breakable(tlist, scck_'END', scck_'WHERE')) THEN
        ist = istEndWhere
      ELSE IF (is_breakable(tlist, scck_'END', scck_'FORALL')) THEN
        ist = istEndForall
      ! This must appear after all the other tests for END xxx type
      ! statements.
      ELSE IF (value == scck_'END') THEN
        CALL test_end(part_stack, ist)
      ELSE IF (value == scck_'ENTRY') THEN
        ist = istEntry
      ELSE IF (value == scck_'ENUM') THEN
        ist = istEnumDef
      ELSE IF (value == scck_'ENUMERATOR') THEN
        ist = istEnumeratorDef
      ELSE IF (value == scck_'EQUIVALENCE') THEN
        ist = istEquivalence
      ELSE IF (is_keywords(tlist, scck_'ERROR', scck_'STOP')) THEN
        ist = istErrorStop
      ELSE IF (value == scck_'EXIT') THEN
        ist = istExit
      ELSE IF (value == scck_'EXTERNAL') THEN
        ist = istExternal
      END IF
      
    CASE (scck_'F')
      IF (value == scck_'FINAL') THEN
        ist = istFinalProcedure
      ELSE IF (value == scck_'FLUSH') THEN
        ist = istFlush
      ELSE IF (value == scck_'FORALL') THEN
        CALL get_token_after_parens(tlist(2:), it)
        IF (it == 0) THEN
          ist = istForallConstruct
        ELSE
          ist = istForAll
        END IF
      ELSE IF (value == scck_'FORMAT') THEN
        ist = istFormat
      ELSE IF (value == scck_'FUNCTION') THEN
        ist = istFunction
      END IF
      
    CASE (scck_'G')
      IF (value == scck_'GENERIC') THEN
        ist = istTypeBoundGeneric
      ELSE IF (value == scck_'GOTO') THEN
        ! Decide between computed-goto-stmt (has parentheses) and goto-stmt.
        CALL test_goto(tlist, 2, ist)
      ELSE IF (is_keywords(tlist, scck_'GO', scck_'TO')) THEN
        ! Decide between computed-goto-stmt (has parentheses) and goto-stmt.
        CALL test_goto(tlist, 3, ist)
      END IF
      
    CASE (scck_'I')
      IF (value == scck_'IF') THEN
        ! Decide between /if-then-stmt/ (has THEN following parentheses), 
        ! an /arithmetic-if-stmt/ (has a label following the parentheses) 
        ! or an /if-stmt/ (another statement following parentheses).
        CALL get_token_after_parens(tlist(2:), it)
        IF (it == 0) THEN
          ist = istUnclassified
        ELSE IF ( IsKeyword(tlist(it+1), scck_'THEN')  &
            .AND. (SIZE(tlist) == it + 1) ) THEN
          ist = istIfThen
        ELSE IF (tlist(it+1) == ittIntegerLiteral) THEN
          ist = istArithmeticIf
        ELSE
          ist = istIf
        END IF
      ELSE IF (value == scck_'IMPLICIT') THEN
        ist = istImplicit
      ELSE IF (value == scck_'IMPORT') THEN
        ist = istImport
      ELSE IF (value == scck_'INQUIRE') THEN
        ist = istInquire
      ELSE IF (value == scck_'INTEGER') THEN
        CALL test_function(tlist, ist)
        IF (ist == istUnclassified)  &
            CALL test_dtcomponent_typeparam(tlist, part_stack, ist)
      ELSE IF (value == scck_'INTENT') THEN
        ist = istIntent
      ELSE IF (value == scck_'INTERFACE') THEN
        ist = istInterface
      ELSE IF (value == scck_'INTRINSIC') THEN
        ist = istIntrinsic
      END IF
      
    CASE (scck_'L')
      IF (value == scck_'LOCK') THEN
        ist = istLock
      ELSE IF (value == scck_'LOGICAL') THEN
        CALL test_function(tlist, ist)
        IF (ist == istUnclassified)  &
            CALL test_dtcomponent(part_stack, ist)
      END IF
      
    CASE (scck_'M')
      IF (value == scck_'MODULE') THEN
        ! module-stmt (only in program): 
        !   MODULE name
        ! procedure-stmt (only in interface-block):
        !   [MODULE] PROCEDURE [::] list
        ! function-stmt (program or module-subprogram-part or 
        ! internal-subprogram-part):
        !   MODULE other-prefixes FUNCTION name([arg-list]) [suffix]
        ! subroutine-stmt (program or module-subprogram-part or 
        ! internal-subprogram-part):
        !   MODULE other-prefixes SUBROUTINE name[([arg-list]) [suffix]]
        ! mp-subprogram-stmt (only in module-subprogram-part):
        !   MODULE PROCEDURE name
        IF (SIZE(tlist) > 2) THEN
          IF (tlist(2) == scck_'PROCEDURE') THEN
            ! MODULE PROCEDURE xxx
            ! Only test the top level for the procedure-stmt form.
            IF (stack_top_match(part_stack, iptInterfaceBlock)) THEN
              ist = istProcedure
            ELSE
              ist = istMpSubprogram
            END IF
          ELSE
            CALL test_subprogram(tlist, ist)
            ! We might get istUnclassified back here, which we just leave. 
          END IF
        ELSE
          ist = istModule
        END IF
      END IF
      
    CASE (scck_'N')
      IF (value == scck_'NAMELIST') THEN
        ist = istNamelist
      ELSE IF (value == scck_'NULLIFY') THEN
        ist = istNullify
      END IF
      
    CASE (scck_'O')
      IF (value == scck_'OPEN') THEN
        ist = istOpen
      ELSE IF (value == scck_'OPTIONAL') THEN
        ist = istOptional
      END IF
      
    CASE (scck_'P')
      IF (value == scck_'PARAMETER') THEN
        ist = istParameter
      ELSE IF (value == scck_'POINTER') THEN
        ist = istPointer
      ELSE IF (value == scck_'PRINT') THEN
        ist = istPrint
      ELSE IF (value == scck_'PRIVATE') THEN
        ! Order is important.
        IF (ANY(part_stack == iptTypeBoundProcedurePart)) THEN
          ist = istBindingPrivate
        ELSE IF (ANY(part_stack == iptDerivedTypeDef)) THEN
          ist = istPrivateComponents
        ELSE
          ist = istAccess
        END IF
      ELSE IF (value == scck_'PROCEDURE') THEN
        ! procedure-declaration-stmt (only in specification-part): 
        !   PROCEDURE( [proc-interface])[[,attrs] :: ] list
        ! procedure-stmt (only in interface-block):
        !   [MODULE] PROCEDURE [::] list
        ! type-bound-procedure-stmt (only in type-bound-procedure-part):
        !   PROCEDURE [[,attrs] :: ] list
        !   PROCEDURE(name), attrs :: name
        ! proc-component-def-stmt (only in component-part):
        !   PROCEDURE([name]), attrs :: list
        IF (SIZE(tlist) > 1) THEN
          IF (tlist(2) == scck_'(') THEN
            ! Order of these two tests is important as the 
            ! type-bound-procedure-part is a child of a derived-type-def.
            IF (ANY(part_stack == iptTypeBoundProcedurePart)) THEN
              ist = istTypeBoundProcedure
            ELSE IF (ANY(part_stack == iptDerivedTypeDef)) THEN 
              ist = istProcComponentDef
            ELSE ! Any other context.
              ist = istProcedureDeclaration
            END IF
          ELSE
            IF (ANY(part_stack == iptTypeBoundProcedurePart)) THEN
              ist = istTypeBoundProcedure
            ELSE  ! Should only happen in interface-blocks
              ist = istProcedure
            END IF
          END IF
        ELSE
          ist = istUnclassified
        END IF
      ELSE IF (value == scck_'PROGRAM') THEN
        ist = istProgram
      ELSE IF (value == scck_'PROTECTED') THEN
        ist = istProtected
      ELSE IF (value == scck_'PUBLIC') THEN
        ist = istAccess
      END IF
      
    CASE (scck_'R')
      IF (value == scck_'READ') THEN
        ist = istRead
      ELSE IF (value == scck_'REAL') THEN
        CALL test_function(tlist, ist)
        IF (ist == istUnclassified) CALL test_dtcomponent(part_stack, ist)
      ELSE IF (value == scck_'RETURN') THEN
        ist = istReturn
      ELSE IF (value == scck_'REWIND') THEN
        ist = istRewind
      END IF
      
    CASE (scck_'S')
      IF (value == scck_'SAVE') THEN
        ist = istSave
      ELSE IF (is_breakable(tlist, scck_'SELECT', scck_'CASE')) THEN
        ist = istSelectCase
      ELSE IF (is_breakable(tlist, scck_'SELECT', scck_'TYPE')) THEN
        ist = istSelectType
      ELSE IF (value == scck_'SEQUENCE') THEN
        ist = istSequence
      ELSE IF (value == scck_'STOP') THEN
        ist = istStop
      ELSE IF (value == scck_'SUBMODULE') THEN
        ist = istSubmodule
      ELSE IF (value == scck_'SUBROUTINE') THEN
        ist = istSubroutine
      ELSE IF (is_keywords(tlist, scck_'SYNC', scck_'ALL')) THEN
        ist = istSyncAll
      ELSE IF (is_keywords(tlist, scck_'SYNC', scck_'IMAGES')) THEN
        ist = istSyncImages
      ELSE IF (is_keywords(tlist, scck_'SYNC', scck_'MEMORY')) THEN
        ist = istSyncMemory
      END IF
      
    CASE(scck_'T')
      IF (value == scck_'TARGET') THEN
        ist = istTarget
      ELSE IF (value == scck_'TYPE') THEN
        IF (SIZE(tlist) > 1) THEN
          IF (IsKeyword(tlist(2), scck_'IS')) THEN
            ist = istTypeGuard      ! TYPE IS (xxx) [construct-name]
          ELSE IF (IsToken(tlist(2), ittDelimiter, scck_'(')) THEN
            CALL test_function(tlist, ist)
            IF (ist == istUnclassified)  &
                CALL test_dtcomponent(part_stack, ist)
          ELSE
            ist = istDerivedType
          END IF
        ELSE
          ist = istUnclassified
        END IF
      END IF
      
    CASE (scck_'U')
      IF (value == scck_'UNLOCK') THEN
        ist = istUnlock
      ELSE IF (value == scck_'USE') THEN
        ist = istUse
      END IF
      
    CASE (scck_'V')
      IF (value == scck_'VALUE') THEN
        ist = istValue
      ELSE IF (value == scck_'VOLATILE') THEN
        ist = istVolatile
      END IF
      
    CASE (scck_'W')
      IF (value == scck_'WAIT') THEN
        ist = istWait
      ELSE IF (value == scck_'WHERE') THEN
        CALL get_token_after_parens(tlist(2:), it)
        IF (it == 0) THEN
          ist = istWhereConstruct
        ELSE
          ist = istWhere
        END IF
      ELSE IF (value == scck_'WRITE') THEN
        ist = istWrite
      END IF
      
    END SELECT
    
    IF (ist /= istUnclassified) RETURN
    
    IF (is_procedure_prefix(tlist(1))) CALL test_subprogram(tlist, ist)
    
  END SUBROUTINE classify_not_assign_no_name
  
  
  !*****************************************************************************
  !!
  !> Tests whether something that looks like a construct name is at the 
  !! front of the token list.
  !!
  !! @param[in]     tlist             The list of tokens.
  !!
  !! @param[out]    it                Index of the token after the colon of 
  !! the construct name, or zero if there was no construct name.
  !!
  !! A *-construct-name uses two tokens - the name itself and then 
  !! the trailing ':'
  
  SUBROUTINE match_construct_name(tlist, it)
    
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(Token), INTENT(IN) :: tlist(:)
    INTEGER, INTENT(OUT) :: it
    
    !***************************************************************************
    
    ! Assume failure.
    it = 0
    
    ! In addition to the two tokens that make up the construct name we also 
    ! need something to be the construct - hence three tokens minimum.
    IF (SIZE(tlist) < 3) RETURN
    
    IF ((tlist(1) == ittName) .AND. (tlist(2) == ittColon)) it = 3
    
  END SUBROUTINE match_construct_name
  
  
  !*****************************************************************************
  !!
  !> Gets the next token after a parenthesis pair.
  !!
  !! @param[in]     tlist             The list of tokens.
  !!
  !! @param[out]    it                Index of the first token after the 
  !! closing parenthesis of the parenthesis pair, or 0 if no such token 
  !! exists.
  !!
  !! The first token should be a '('.  For the example "(a b c) d" this 
  !! procedure determines the index of d.
  
  SUBROUTINE get_token_after_parens(tlist, it)
    
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Token), INTENT(IN) :: tlist(:)
    INTEGER, INTENT(OUT) :: it
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    LOGICAL :: got_pair
    
    INTEGER :: nest
    
    !***************************************************************************
    
    nest = 0
    got_pair = .FALSE.
    
    DO it = 1, SIZE(tlist)
      IF (got_pair) RETURN
      IF (tlist(it) == ittDelimiter) THEN
        IF (tlist(it) == scck_'(') THEN
          nest = nest + 1
        ELSE IF (tlist(it) == scck_')') THEN
          nest = nest - 1
          IF (nest == 0) got_pair = .TRUE.
        END IF
      END IF
    END DO
    
    ! No pair of parentheses found, or no token after pair of parentheses.
    it = 0
    
  END SUBROUTINE get_token_after_parens
  
  
  !*****************************************************************************
  !!
  !> Tests whether we have a sequence of keywords.
  !!
  !! @param[in]     tlist             The token sequence.  Testing starts 
  !! from the beginning of this sequence.
  !!
  !! @param[in]     word1             The first keyword.  Should be in 
  !! upper case.
  !!
  !! @param[in]     word2             The second keyword.  Should be in 
  !! upper case.
  !!
  !! @returns True if the first two tokens in @a tlist are name tokens 
  !! with values @a word1 and @a word2 respectively, false otherwise.
  
  FUNCTION is_keywords_2(tlist, word1, word2) RESULT(match)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(Token), INTENT(IN) :: tlist(:)
    CHARACTER(*, KIND=scck), INTENT(IN) :: word1
    CHARACTER(*, KIND=scck), INTENT(IN) :: word2
    
    ! Function result
    LOGICAL :: match
    
    !***************************************************************************
    
    match = .FALSE.
    
    ! Need to have at least two tokens.
    IF (SIZE(tlist) < 2) RETURN
    ! Both tokens must be name tokens.
    IF ((tlist(1) /= ittName) .OR. (tlist(2) /= ittName)) RETURN
    ! The values of both tokens must match.
    IF ((tlist(1) /= word1) .OR. (tlist(2) /= word2)) RETURN
    
    match = .TRUE.
    
  END FUNCTION is_keywords_2
  
  
  !*****************************************************************************
  !!
  !> Tests whether a token sequence matches a keyword that can have an 
  !! optional space breaking the two words inside it.
  !!
  !! @param[in]     tlist             The sequence of tokens, with names and 
  !! keywords in upper case.
  !!
  !! @param[in]     word1             First word.  Should be in upper case.
  !!
  !! @param[in]     word2             Second word.  Should be in upper case.
  !!
  !! @returns .TRUE. if the start of tlist could be the combination of 
  !! @a word1 and @a word2 (with or without an intervening space).
  
  FUNCTION is_breakable_2(tlist, word1, word2) RESULT(b)
    
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(Token), INTENT(IN) :: tlist(:)
    CHARACTER(*, KIND=scck), INTENT(IN) :: word1
    CHARACTER(*, KIND=scck), INTENT(IN) :: word2
    
    ! Function result
    LOGICAL :: b
    
    !***************************************************************************
    
    b = .FALSE.
    
    IF (SIZE(tlist) == 0) RETURN
    
    IF (tlist(1) /= ittName) RETURN
    
    IF (tlist(1) == word1 // word2) THEN
      b = .TRUE.
      RETURN
    END IF
    
    IF (SIZE(tlist) == 1) RETURN
      
    IF (tlist(2) /= ittName) RETURN
    
    IF ( (tlist(1) == word1)  &
        .AND. (tlist(2) == word2) ) THEN
      b = .TRUE.
      RETURN
    END IF
    
  END FUNCTION is_breakable_2
  
  
  !*****************************************************************************
  !!
  !> Tests whether a token sequence matches a keyword that can have an 
  !! optional space breaking the three words inside it.
  !!
  !! @param[in]     tlist             The sequence of tokens, with names and 
  !! keywords in upper case.
  !!
  !! @param[in]     word1             First word.  Should be in upper case.
  !!
  !! @param[in]     word2             Second word.  Should be in upper case.
  !!
  !! @param[in]     word3             Third word.  Should be in upper case.
  !!
  !! @returns .TRUE. if the start of tlist could be the combination of 
  !! @a word1, @a word2 and @a word3 (with or without an intervening space).
  
  FUNCTION is_breakable_3(tlist, word1, word2, word3) RESULT(b)
    
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(Token), INTENT(IN) :: tlist(:)
    CHARACTER(*, KIND=scck), INTENT(IN) :: word1
    CHARACTER(*, KIND=scck), INTENT(IN) :: word2
    CHARACTER(*, KIND=scck), INTENT(IN) :: word3
    
    ! Function result
    LOGICAL :: b
    
    !***************************************************************************
    
    b = .FALSE.
    
    IF (SIZE(tlist) == 0) RETURN
    
    IF (tlist(1) /= ittName) RETURN
    
    IF (tlist(1) == word1 // word2 // word3) THEN
      b = .TRUE.
      RETURN
    END IF
    
    IF (SIZE(tlist) == 1) RETURN
      
    IF (tlist(2) /= ittName) RETURN
    
    IF ( ( (tlist(1) == word1)  &
          .AND. (tlist(2) == word2 // word3) )  &
        .OR. ( (tlist(1) == word1 // word2)  &
          .AND. (tlist(2) == word3) ) ) THEN
      b = .TRUE.
      RETURN
    END IF
    
    IF (SIZE(tlist) == 2) RETURN
      
    IF (tlist(3) /= ittName) RETURN
    
    IF ( (tlist(1) == word1)  &
        .AND. (tlist(2) == word2)  &
        .AND. (tlist(3) == word3) ) THEN
      b = .TRUE.
      RETURN
    END IF
    
  END FUNCTION is_breakable_3
  
  
  !*****************************************************************************
  !!
  !> Tests which particular end-stmt a statement starting with END could be.
  !!
  !! It is assumed that an END keyword has been encountered - call this 
  !! function to determine the type of end statement based on the current 
  !! syntax part.
  !!
  !! @param[in]     part_stack        Current part stack.
  !!
  !! @param[out]    ist               Statement number - will be an istEnd* 
  !! value.
  
  SUBROUTINE test_end(part_stack, ist)
    
    USE Statements
    USE SyntaxParts
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    INTEGER, INTENT(IN) :: part_stack(:)
    INTEGER, INTENT(OUT) :: ist
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! One for each type of program unit or thing that might finish with an 
    ! END statement, in order:
    ! - program (empty main program)
    ! - main program
    ! - function subprogram
    ! - subroutine subprogram
    ! - separate module subprogram
    ! - module
    ! - submodule
    ! - block data
    ! - interface body
    INTEGER :: depths(9)
    
    !***************************************************************************
    
    depths(1) = part_depth(part_stack, iptProgram)
    depths(2) = part_depth(part_stack, iptMainProgram)
    depths(3) = part_depth(part_stack, iptFunctionSubprogram)
    depths(4) = part_depth(part_stack, iptSubroutineSubprogram)
    depths(5) = part_depth(part_stack, iptSeparateModuleSubprogram)
    depths(6) = part_depth(part_stack, iptModule)
    depths(7) = part_depth(part_stack, iptSubmodule)
    depths(8) = part_depth(part_stack, iptBlockData)
    depths(9) = part_depth(part_stack, iptInterfaceBody)
    
    IF (ALL(depths == 0)) THEN
      ! May occur if we are not given context.
      ist = istEnd
    ELSE
      SELECT CASE (MAXLOC(depths, DIM=1))
      CASE (1,2)  ! main program
        ist = istEndProgram
      CASE (3)  ! function subprogram
        ist = istEndFunction
      CASE (4)  ! subroutine subprogram
        ist = istEndSubroutine
      CASE (5)  ! separate module subprogram
        ist = istEndMPSubprogram
      CASE (6)  ! module
        ist = istEndModule
      CASE (7)  ! submodule
        ist = istEndSubmodule
      CASE (8)  ! block data
        ist = istEndBlockData
      CASE (9)  ! interface body.
        ! Unfortunately, the way the syntax rules are written there is 
        ! no specific syntax rules for the function interface body or 
        ! the subroutine interface body (they both sit directly as 
        ! options under /interface-body/.  The contexter (if used) knows the 
        ! difference, but that doesn't get transferred across to the 
        ! part stack.
        ist = istEnd
      END SELECT
    END IF
    
  END SUBROUTINE test_end
  
  
  !*****************************************************************************
  !!
  !> Tests whether the series of tokens looks like a function-stmt.
  !!
  !! @param[in]     tlist             The sequence of tokens, with names in 
  !! upper case.
  !!
  !! @param[out]    ist               istFunction if the tokens match a 
  !! /function-stmt/, otherwise istUnclassified.
  
  SUBROUTINE test_function(tlist, ist)
    
    USE Errors
    USE MatchUtils
    USE Tokens
    USE Statements
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Token), INTENT(IN) :: tlist(:)
    INTEGER, INTENT(OUT) :: ist
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: i              ! Current token index.
    INTEGER :: it             ! Index past any declaration type spec.
    INTEGER :: finish         ! Index of the closing parenthesis.
    
    !***************************************************************************
    
    ! Need to check for prefix keywords and type declaration!
    
    ! Keep looping until something makes us stop.  We don't worry about 
    ! inconsistent or repeated prefixes.
    i = 0
    DO
      i = i + 1
      IF (i > SIZE(tlist)) RETURN
      IF (tlist(i) /= ittName) EXIT
      IF (is_procedure_prefix(tlist(i))) CYCLE
      IF (is_intrinsic_type(tlist(i))) THEN
        ! Do we have a kind specification?
        IF (i < SIZE(tlist)) THEN
          IF (IsToken(tlist(i+1), ittDelimiter, scck_'(')) THEN
            ! Maybe looks like integer(xxx) - advance i to the closing ).
            finish = FindClosingParens(tlist(i+1:))
            IF (finish /= 0) i = i + finish
            ! If we didn't find a closing token, then there's a syntax error - 
            ! just fumble on and see what we come up with.
          END IF
        END IF
        CYCLE
      END IF
      
      CALL match_declaration_type_spec(tlist(i:), it)
      IF (it /= 0) THEN
        i = i + it - 2
        CYCLE
      END IF
      
      IF (tlist(i) == scck_'FUNCTION') THEN
        ist = istFunction
        RETURN
      END IF
    END DO
    
    ist = istUnclassified
    
  END SUBROUTINE test_function
  
  
  !*****************************************************************************
  !!
  !> Tests whether the series of tokens looks like a subroutine-stmt.
  !!
  !! @param[in]     tlist             The sequence of tokens, with names in 
  !! upper case.
  !!
  !! @param[out]    ist               istSubroutine if the tokens match a 
  !! /subroutine-stmt/, otherwise istUnclassified.
  
  SUBROUTINE test_subroutine(tlist, ist)
    
    USE Errors
    USE Tokens
    USE Statements
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Token), INTENT(IN) :: tlist(:)
    INTEGER, INTENT(OUT) :: ist
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: i              ! Current token index.
    
    !***************************************************************************      
    
    ! Need to check for prefix keywords.
    
    ! Keep looping until something makes us stop.  We don't worry about 
    ! inconsistent or repeated prefixes.
    i = 0
    DO
      i = i + 1
      IF (i > SIZE(tlist)) RETURN
      IF (tlist(i) /= ittName) EXIT
      IF (is_procedure_prefix(tlist(i))) CYCLE
      
      IF (tlist(i) == scck_'SUBROUTINE') THEN
        ist = istSubroutine
        RETURN
      END IF
    END DO
    
    ist = istUnclassified
    
  END SUBROUTINE test_subroutine
  
  
  !*****************************************************************************
  !!
  !> Tests whether the sequence of tokens looks like a function-stmt or a 
  !! subroutine-stmt.
  !!
  !! @param[in]     tlist             The sequence of tokens, with names in 
  !! upper case.
  !!
  !! @param[out]    ist               istFunction if the tokens look like a 
  !! /function-stmt/, istSubroutine if the tokens look like a 
  !! /subroutine-stmt/, otherwise istUnclassified.
  !!
  !! We trundle through potential prefix items until we get to a 
  !! SUBROUTINE or FUNCTION keyword.  This includes trundling past 
  !! type specifications, which clearly cannot be part of a prefix 
  !! for a subroutine.  In this case we return istUnclassified.
  
  SUBROUTINE test_subprogram(tlist, ist)
    
    USE Errors
    USE Tokens
    USE MatchUtils
    USE Statements
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Token), INTENT(IN) :: tlist(:)
    INTEGER, INTENT(OUT) :: ist
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: i              ! Current token index.
    INTEGER :: it             ! Index past any declaration type spec.
    LOGICAL :: fn_flag        ! Flag set when a type spec is found.
    INTEGER :: finish         ! Index of the closing parenthesis.
    
    !***************************************************************************
    
    ! Need to check for prefix keywords and type declaration!
    
    ! Keep looping until something makes us stop.  We don't worry about 
    ! inconsistent or repeated prefixes.
    i = 0
    fn_flag = .FALSE.
    DO
      i = i + 1
      IF (i > SIZE(tlist)) RETURN
      IF (tlist(i) /= ittName) EXIT
      IF (is_procedure_prefix(tlist(i))) CYCLE
      IF (is_intrinsic_type(tlist(i))) THEN
        ! Do we have a kind specification?
        IF (i < SIZE(tlist)) THEN
          IF (IsToken(tlist(i+1), ittDelimiter, scck_'(')) THEN
            ! Maybe looks like integer(xxx) - advance i to the closing ).
            finish = FindClosingParens(tlist(i+1:))
            IF (finish /= 0) i = i + finish
            ! If we didn't find a closing token, then there's a syntax error - 
            ! just fumble on and see what we come up with.
          END IF
        END IF
        fn_flag = .TRUE.
        CYCLE
      END IF
      
      CALL match_declaration_type_spec(tlist(i:), it)
      IF (it /= 0) THEN
        fn_flag = .TRUE.
        i = i + it - 2
        CYCLE
      END IF
      
      IF (tlist(i) == scck_'SUBROUTINE') THEN
        IF (fn_flag) THEN
          ist = istUnclassified
        ELSE
          ist = istSubroutine
        END IF
        RETURN
      END IF
      
      IF (tlist(i) == scck_'FUNCTION') THEN
        ist = istFunction
        RETURN
      END IF
    END DO
    
    ist = istUnclassified
    
  END SUBROUTINE test_subprogram
  
  
  !*****************************************************************************
  !!
  !> Tests whether a token could be a prefix keyword for a function or
  !! subroutine statement.
  !!
  !! @param[in]     tok               The token to test.
  !!
  !! @returns True if the given token could be a prefix - it is a name token 
  !! with a value that matches one of the keywords from R1226.  False 
  !! otherwise.
  
  FUNCTION is_procedure_prefix(tok) RESULT(b)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(Token), INTENT(IN) :: tok
    
    ! Function result
    LOGICAL :: b
    
    !***************************************************************************
    
    IF (tok == ittName) THEN
      b = tok == scck_'PURE'  &
          .OR. tok == scck_'RECURSIVE'  &
          .OR. tok == scck_'ELEMENTAL'  &
          .OR. tok == scck_'IMPURE'  &
          .OR. tok == scck_'MODULE'
      RETURN
    END IF
    b = .FALSE.
    
  END FUNCTION is_procedure_prefix
  
  
  !*****************************************************************************
  !!
  !> Test whether the series of tokens at the start of tlist matches a 
  !! declaration-type-spec (R403).
  !!
  !! @param[in]     tlist             The sequence of tokens, with names and 
  !! keywords upper cased.
  !!
  !! @param[out]    it                The index of the token after the 
  !! declaration-type-spec if one is found or zero if not.  (This should 
  !! never come back as 1.)
  !!
  !! A declaration type spec could be of the form:
  !! - INTEGER
  !! - INTEGER(kind)
  !! - DOUBLE PRECISION
  !! - TYPE(type)
  !! - TYPE(type(kinds))
  !!
  !! (substitute REAL, LOGICAL or CHARACTER for INTEGER).
  
  SUBROUTINE match_declaration_type_spec(tlist, it)
    
    USE MatchUtils
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Token), INTENT(IN) :: tlist(:)
    INTEGER, INTENT(OUT) :: it
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: finish         ! Index of the closing parenthesis.
    
    !***************************************************************************
    
    ! Assume failure
    it = 0
    
    IF (SIZE(tlist) < 1) RETURN
    
    IF (tlist(1) /= ittName) RETURN
    
    IF (is_intrinsic_type(tlist(1))) THEN
      ! Once we've seen an intrinsic type keyword we are convinced that 
      ! it is a declaration-type-specifier (so don't pass something 
      ! like 'REAL=' into this test).  But we need to see if there was a 
      ! kind specifier.
      IF (SIZE(tlist) > 1) THEN
        IF (tlist(2) == scck_'(') THEN
          finish = FindClosingParens(tlist(2:))
          IF (finish /= 0) THEN
            ! One for the TYPE token at the start, one to move past the
            ! closing token.
            it = finish + 2
            RETURN
          END IF
        END IF
      END IF
      it = 2
      RETURN
    END IF
    
    ! Double precision as one token (allowed in free form).
    IF (tlist(1) == scck_'DOUBLEPRECISION') THEN
      ! No need to test for kind - that's implicit in the DOUBLE PRECISION
      ! type specifier.
      it = 2
      RETURN
    END IF
    
    IF (SIZE(tlist) < 2) RETURN
    
    IF (tlist(1) == scck_'DOUBLE') THEN
      IF (tlist(2) == ittName) THEN
        IF (tlist(2) == scck_'PRECISION') THEN
          ! No need to check for kind.
          it = 3
          RETURN
        END IF
      END IF
    END IF
    
    IF ( (tlist(1) == scck_'TYPE')  &
        .OR. (tlist(1) == scck_'CLASS') ) THEN
      IF (tlist(2) == scck_'(') THEN
        finish = FindClosingParens(tlist(2:))
        IF (finish /= 0) THEN
          ! One for the TYPE token at the start, one to move past the
          ! closing token.
          it = finish + 2
          RETURN
        END IF
      END IF
    END IF
    
    ! It wasn't a declaration-type-spec
    
  END SUBROUTINE match_declaration_type_spec
  
  
  !*****************************************************************************
  !!
  !> Test whether a token matches an intrinsic type name
  !!
  !! @param[in]     tok               The token to test.  Should be of type
  !! ittKeyword or ittName.
  !!
  !! @returns .TRUE. if the token matches on of the intrinsic types REAL, 
  !! INTEGER, CHARACTER, COMPLEX or LOGICAL, .FALSE. otherwise.
  !!
  !! DOUBLE PRECISION is an intrinsic type specifier but not a type name 
  !! per-se.
  
  FUNCTION is_intrinsic_type(tok) RESULT(b)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(Token), INTENT(IN) :: tok
    
    ! Function result
    LOGICAL :: b
    
    !***************************************************************************
    
    b = (tok == scck_'INTEGER')  &
        .OR. (tok == scck_'LOGICAL')  &
        .OR. (tok == scck_'CHARACTER')  &
        .OR. (tok == scck_'REAL')  &
        .OR. (tok == scck_'COMPLEX')
    
  END FUNCTION is_intrinsic_type
  
  
  !*****************************************************************************
  !!
  !> Test whether a token sequence, already subject to significant filtering, 
  !! is an assignment statement.
  !!
  !! @param[in]     tlist             The sequence of tokens.
  !!
  !! @returns true if the token sequence is an assignment or statement 
  !! function statement, false otherwise.
  
  FUNCTION is_assignment(tlist) RESULT(b)
    
    USE MatchUtils
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(Token), INTENT(IN) :: tlist(:)
    
    ! Function result.
    LOGICAL :: b
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: it             ! Index of the '=' token.
    
    !***************************************************************************
    
    it = FindBare(tlist, ittAssign, scck_'=')
    b = it /= 0
    IF (.NOT. b) RETURN
    
    ! we have an assignment token.
    b = .NOT. is_single_statement_construct(tlist(:it-1))
    
  END FUNCTION is_assignment
  
  
  !*****************************************************************************
  !!
  !> Test whether a token sequence, already subject to significant filtering, 
  !! is a pointer assignment statement.
  !!
  !! @param[in]     tlist             The sequence of tokens.
  !!
  !! @returns true if the token sequence is a pointer assignment statement, 
  !! false otherwise.
  
  FUNCTION is_pointer_assignment(tlist) RESULT(b)
    
    USE MatchUtils
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(Token), INTENT(IN) :: tlist(:)
    
    ! Function result.
    LOGICAL :: b
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: it             ! Index of the '=>' token.
    
    !***************************************************************************
    
    it = FindBare(tlist, ittAssociate, scck_'=>')
    b = it /= 0
    IF (.NOT. b) RETURN
    
    ! we have a pointer assignment token.
    b = .NOT. is_single_statement_construct(tlist(:it-1))
    
  END FUNCTION is_pointer_assignment
  
  
  !*****************************************************************************
  !!
  !> Test whether a token sequence, already subject to significant filtering, 
  !! is a single statement form of a construct.
  !!
  !! @param[in]     tlist             The sequence of tokens, truncated back 
  !! to the first bare '=" or '=>' token.
  !!
  !! What we look for is a parenthesis pair that is not followed by an 
  !! operator or the end of the token sequence.
  
  FUNCTION is_single_statement_construct(tlist) RESULT(b)
    
    USE MatchUtils
    
    !---------------------------------------------------------------------------
    ! Characteristics 
    
    TYPE(Token), INTENT(IN) :: tlist(:)
    
    ! Function result.
    LOGICAL :: b
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: start          ! Index of token immediately after '('.
    INTEGER :: finish         ! Index of token immediately before ')'.
    
    !***************************************************************************
    
    CALL GetNextParensRange(tlist, start, finish)
    
    ! zero indicates no or no matching parentheses, in which case this is 
    ! not a single statement form of construct.
    b = start /= 0
    IF (.NOT. b) RETURN
    
    ! finish must be less than SIZE(tlist), otherwise there's no room for 
    ! the closing parens.  If the closing parens is last, then this is 
    ! not a single statement form of construct.
    b = finish + 1 /= SIZE(tlist)
    IF (.NOT. b) RETURN
    
    ! finish must be less than SIZE(tlist) + 1 due to the previous test.  
    ! If the thing following the closing parenthesis is a name, then we 
    ! have the single statement form of construct.
    b = tlist(finish + 2) == ittName
    
    ! F2008 as originally published had the ambiguity that:
    !   IF (XYZ) .OP. ABC = DEF
    ! could be an assignment statement, with the left hand side comprising 
    ! a reference to a function called IF and a binary user defined operator 
    ! called OP; OR an /if-stmt/.  This was corrected with corrigendum three 
    ! as a result of interpretation f08/0075.
    
  END FUNCTION is_single_statement_construct
  
  
  !*****************************************************************************
  !!
  !> Decide between a masked-elsewhere-stmt and an elsewhere-stmt.
  !! 
  !! @param[in]     tlist             The sequence of tokens, with names and 
  !! keywords upper cased.
  !!
  !! @param[in]     inext             Index of the token following the 
  !! keywords.
  !!
  !! @param[out]    ist               Either istMaskedElsewhere or 
  !! istElsewhere.
  !!
  !! The masked-elsewhere-stmt has a set of parentheses (with the mask 
  !! expression inside) following the ELSE WHERE or ELSEWHERE keyword, while
  !! the elsewhere-stmt doesn't (it may have a construct name following the
  !! keywords).
  
  SUBROUTINE test_elsewhere(tlist, inext, ist)
    
    USE Statements
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Token), INTENT(IN) :: tlist(:)
    INTEGER, INTENT(IN) :: inext
    INTEGER, INTENT(OUT) :: ist
    
    !***************************************************************************
    
    IF (SIZE(tlist) >= inext) THEN
      IF (IsToken(tlist(inext), ittDelimiter, scck_'(')) THEN
        ist = istMaskedElsewhere
        RETURN
      END IF
    END IF
    ist = istElsewhere
    
  END SUBROUTINE test_elsewhere
  
  
  !*****************************************************************************
  !!
  !> Decide between a computed-goto-stmt and a goto-stmt.
  !! 
  !! @param[in]     tlist             The sequence of tokens, with names and 
  !! keywords upper cased.
  !!
  !! @param[in]     inext             Index of the token following the 
  !! keywords.
  !!
  !! @param[out]    ist               Either istGoto or istComputedGoto.
  !!
  !! The computed-goto-stmt has a set of parentheses following the keywords.
  
  SUBROUTINE test_goto(tlist, inext, ist)
    
    USE Statements
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Token), INTENT(IN) :: tlist(:)
    INTEGER, INTENT(IN) :: inext
    INTEGER, INTENT(OUT) :: ist
    
    !***************************************************************************
    
    IF (SIZE(tlist) >= inext) THEN
      IF (IsToken(tlist(inext), ittDelimiter, scck_'(')) THEN
        ist = istComputedGoto
        RETURN
      END IF
    END IF
    ist = istGoto
    
  END SUBROUTINE test_goto
  
  
  !*****************************************************************************
  !!
  !> Decide between a type declaration or a derived type component definition.
  !!
  !! @param[in]     stack             Current syntax part stack.
  !!
  !! @param[out]    ist               Either istDataComponentDef or 
  !! istTypeDeclaration.
  !!
  !! Derived type data component definitions can only occur inside a 
  !! derived type definition.
  
  SUBROUTINE test_dtcomponent(stack, ist)
    
    USE SyntaxParts
    USE Statements
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    INTEGER, INTENT(IN) :: stack(:)
    INTEGER, INTENT(OUT) :: ist
    
    !***************************************************************************
    
    IF (ANY(stack == iptDerivedTypeDef)) THEN
      ist = istDataComponentDef
    ELSE
      ist = istTypeDeclaration
    END IF
    
  END SUBROUTINE test_dtcomponent
  
  
  !*****************************************************************************
  !!
  !> Test a statement that looks like a type declaration statement to see
  !! if it is a /type-declaration-stmt/, a /data-component-def-stmt/ or a
  !! /type-param-def-stmt/.
  !!
  !! @param[in]     tlist             The sequence of tokens, with names and 
  !! keywords upper cased.
  !!
  !! @param[in]     stack             Current syntax part stack.
  !!
  !! @param[out]    ist               Either istDataComponentDef, 
  !! istTypeDeclaration or istTypeParamDef.
  !!
  !! Type parameter definitions and data component definitions can only 
  !! occur inside a derived type.  Type parameter definitions have the 
  !! KIND or LEN keywords in the list of attributes.
  !!
  !! This procedure assumes that the first keyword is INTEGER, and the 
  !! possibility of the statement being a FUNCTION statement has already 
  !! been eliminated.
  
  SUBROUTINE test_dtcomponent_typeparam(tlist, stack, ist)
    
    USE MatchUtils
    USE Statements
    USE SyntaxParts
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Token), INTENT(IN) :: tlist(:)
    INTEGER, INTENT(IN) :: stack(:)
    INTEGER, INTENT(OUT) :: ist
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: it             ! Current token index.
    
    !***************************************************************************
    
    ! If we are not in a derived type def, then we are not dealing with 
    ! type parameters or components.
    IF (ANY(stack == iptDerivedTypeDef)) THEN
      ! Locate the type parameter nature (KIND or LEN).  We need to skip 
      ! over a potential type parameter kind specification immediately 
      ! after the INTEGER keyword.  Indicate problems by setting it to zero.
      it = 2
      IF (SIZE(tlist) > 4) THEN
        IF (IsToken(tlist(it), ittDelimiter, scck_'(')) THEN
          ! Looks like we have a kind specification.  Skip over it.
          it = FindClosingParens(tlist, it)
          IF ((it /= 0) .AND. (SIZE(tlist) > it + 1)) THEN
            it = it + 1
          ELSE
            it = 0
          END IF
        END IF
        
        IF (it /= 0) THEN
          IF (IsToken(tlist(it), ittComma, scck_',')) THEN
            it = it + 1
          END IF
        END IF
      END IF
      
      IF (it /= 0) THEN
        ! it guaranteed by above to be in bounds for tlist.
        IF ( IsKeyword(tlist(it), scck_'KIND')  &
            .OR. IsKeyword(tlist(it), scck_'LEN') ) THEN
          ! Looks like a type parameter.
          ist = istTypeParamDef
          RETURN
        END IF
      END IF
      ! A bungled type parameter statement results in us calling the thing 
      ! a data component (it starts with INTEGER, so that's fair enough).
      ist = istDataComponentDef
    ELSE
      ! Not in a type - looks like a type declaration (function statements 
      ! have already been excluded prior to this procedure).
      ist = istTypeDeclaration
    END IF
    
  END SUBROUTINE test_dtcomponent_typeparam
  
  
  !*****************************************************************************
  !!
  !> Get the depth of a part in a stack.
  !!
  !! @param[in]     stack             The syntax part stack to search.
  !!
  !! @param[in]     ipt               The index of the part to search for.
  !! This cannot be iptProgram as that is implicit in the stack.
  !!
  !! @returns The deepest depth of the part if it is in the stack, zero if
  !! the part wasn't found.
  
  PURE FUNCTION part_depth(stack, ipt) RESULT(depth)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    INTEGER, INTENT(IN) :: ipt
    INTEGER, INTENT(IN) :: stack(:)
    
    ! Function result
    INTEGER :: depth
    
    !***************************************************************************
    
    DO depth = SIZE(stack), 1, -1
      IF (stack(depth) == ipt) RETURN
    END DO
    
    depth = 0   ! Not found.
    
  END FUNCTION part_depth
  
  
  !*****************************************************************************
  !!
  !> Test the top of a syntax part stack.
  !!
  !! @param[in]     stack             The syntax part stack to test.
  !!
  !! @param[in]     ipt               The part to check.
  !!
  !! @returns True if the top (most nested part) of the stack is @a ipt, 
  !! false otherwise.
  
  FUNCTION stack_top_match(stack, ipt) RESULT(b)
    
    USE SyntaxParts
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    INTEGER, INTENT(IN) :: stack(:)
    INTEGER, INTENT(IN) :: ipt
    
    ! Function result
    LOGICAL :: b
    
    !***************************************************************************
    
    IF (SIZE(stack) > 0) THEN
      b = stack(SIZE(stack)) == ipt
    ELSE
      ! iptProgram implicit in an empty stack.
      b = ipt == iptProgram
    END IF
    
  END FUNCTION stack_top_match
  
END MODULE Classifier
