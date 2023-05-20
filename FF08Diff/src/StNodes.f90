! $Id: StNodes.f90 2915 2019-04-17 22:10:04Z ian $
! ff08 source code copyright 2012 M.E.G.M.S.  See LICENCE.txt for licence.
!> @file
!! Defines the StNodes module


!*******************************************************************************
!!
!> Provides the StNode type and associated procedures.
!!
!! A StNode is a Statement Tree Node - a type that represents a logical 
!! grouping of statements in the program in a tree like structure.  The 
!! root of the tree represents the entire program, underneath which sits the 
!! program units (modules, main program, external subprograms, etc).  At the 
!! lowest level are individual statements, which are grouped up into 
!! specification parts, execution parts and the various syntax constructs.
!!
!! The StNode structure more or less follows the structure of the "parts" 
!! within the standard (syntax rules that represent a particular sequence 
!! of statements.  Consequently, currently each type of syntax node represents 
!! one particular syntax part (a unique ipt* constant), though that wasn't 
!! always the intention.  Just as there are exceptions in the representation 
!! of ipt* constants to the standard there are a few exceptions in how 
!! StNode's map to the standard.
!!
!! Each StNode has a BuildTree binding which lets the StNode either incorporate 
!! a particular statement at its level of the tree, or perhaps create and pass 
!! the statement to a child node, or perhaps pass the statement back to its 
!! parent node (which indicates that the child node is "complete" or at least 
!! doesn't recognise the statement that it has been given).
!!
!! The process of calling BuildTree for each statement within a source file is 
!! known as the BuildTree pass - see the BuildTreePass module for more 
!! information.
!!
!! Each StNode has a parent component.  When new child StNode's are created 
!! this component needs to be explicitly set to the parent node of the 
!! new child node.

MODULE StNodes
  
  USE LabelsUtils
  
  IMPLICIT NONE
  
  PRIVATE
  
  !-----------------------------------------------------------------------------
  
  PUBLIC :: AddNode
  PUBLIC :: BuildTree
  PUBLIC :: GetParts
  PUBLIC :: GetPartName
  PUBLIC :: TerminateTree
  
  !-----------------------------------------------------------------------------
  
  !> A node in the statement tree.
  !!
  !! Each node contains the information for the statement or statements that 
  !! make up the node.
  !!
  !! Each node stores a pointer to its parent node.
  !!
  !! Vague suspicion that we should have more stuff around statement 
  !! labels going on with this type - perhaps a GetLabel or GetLabels 
  !! binding.
  TYPE, ABSTRACT, PUBLIC :: StNode
    !> The parent statement tree node of this node.
    !!
    !! NULL for the top level node in the tree.
    CLASS(StNode), POINTER :: parent => NULL()
  CONTAINS
    !> Called for all statements to build the statement tree.
    PROCEDURE(stnode_BuildTree), DEFERRED :: BuildTree
    
    !> Check that a successfully constructed tree node complies with the 
    !! rules of the language relevant to the node.
    PROCEDURE :: Check => stnode_Check
    
    !> Return the syntax part stack associated with this node.
    !!
    !! This part stack is provided to Classify to enable classification of 
    !! statements that depend on context.
    !!
    !! Note that some statements (notably statement functions definitions) 
    !! require information beyond the context to be correctly classified.
    PROCEDURE(stnode_GetPart), DEFERRED :: GetPart
    
    !> Check if a syntax node is looking for a particular label that 
    !! terminates its scope.  This is used for the /do-construct/ that 
    !! follows a /label-do-stmt/.
    PROCEDURE :: CheckLabel => stnode_CheckLabel
    
    !> Called when a parent StNode decides that its children need 
    !! to stop growing, perhaps due to a statement match.
    PROCEDURE :: Terminate => stnode_Terminate
    
    !> Debugging routine that dumps the statement tree to a logical 
    !! unit connected for formatted output in a particular format.
    PROCEDURE(stnode_DumpTree), DEFERRED :: DumpTree
    
    !> Called to initiate traversal for a query - invoke the ExecuteNode 
    !! binding of the visitor on this StNode, the ExecuteStmt binding on 
    !! any stored statements and forward the Query call to any child StNodes; 
    !! and do all of that in source order.  Please.
    PROCEDURE(stnode_Query), DEFERRED :: Query
    
    !> Called to initiate traversal for a modify - invoke the ExecuteNode 
    !! binding of the visitor on this StNode, the ExecuteStmt binding on 
    !! any stored statements and forward the Modify call to any child StNodes; 
    !! and do all of that in source order.
    PROCEDURE(stnode_Modify), DEFERRED :: Modify
    
    ! @todo Instead of the following two bindings, perhaps 
    ! we should have had a `GetFirst` binding that returned the 
    ! statement data for the first statement in the node, then 
    ! we could have queried that statement data for location, 
    ! label, whatever.  But that would require each node to have 
    ! at least one statement, which is not a current requirement.
    
    !> Returns an indicative location for the start of the node in source.
    PROCEDURE(stnode_GetLocation), DEFERRED :: GetLocation
    
    !> Returns the statement label associated with the first statement in 
    !! the node.
    PROCEDURE(stnode_GetLabel), DEFERRED :: GetLabel
  END TYPE StNode
  
  !> A visitor to a node in the statement tree for a query operation.
  TYPE, ABSTRACT, PUBLIC :: StNodeQueryVisitor
  CONTAINS
    !> Nodes shoud invoke this when visited 
    PROCEDURE(stnodequeryvisitor_ExecuteNode), DEFERRED :: ExecuteNode
    PROCEDURE(stnodequeryvisitor_ExecuteStmt), DEFERRED :: ExecuteStmt
  END TYPE StNodeQueryVisitor
  
  !> A visitor to a node in the statement tree for a modify operation.
  TYPE, ABSTRACT, PUBLIC :: StNodeModifyVisitor
  CONTAINS
    !> Nodes shoud invoke this when visited 
    PROCEDURE(stnodemodifyvisitor_ExecuteNode), DEFERRED :: ExecuteNode
    PROCEDURE(stnodemodifyvisitor_ExecuteStmt), DEFERRED :: ExecuteStmt
  END TYPE StNodeModifyVisitor
  
  !> Stores a label, or not.
  !!
  !! (Originally we stored labels as an integer, a value of zero indicating 
  !! that the label wasn't present.  But then we wanted to store the source 
  !! location of the label as well, so then we created the Label type 
  !! which stored the value of the label and its location as components.  
  !! That type could be used as an allocatable component or allocatable 
  !! dummy argument, where unallocated indicated "label not present".  
  !! But the need to use a subroutine call was not convenient given the 
  !! existing infrastucture, so this type was born - allowing us to 
  !! have the concept of "label not present/label present with this value at 
  !! this source location" as a function result.  Some further evolution 
  !! is probably required.)
  TYPE, PUBLIC :: LabelWrapper
    !> Not allocated if the label was not present.
    TYPE(Label), ALLOCATABLE :: item
  END TYPE LabelWrapper
  
  !> Deferred type-bound-procedures interfaces for StNode
  ABSTRACT INTERFACE
    !> Interface for StNode%BuildTree - incorporate the current statement 
    !! into the statement tree.
    !!
    !! @param[in,out] st              The statement tree node.
    !!
    !! @param[in,out] stmt            The current statement.  Note that this 
    !! is INTENT(INOUT) so that Move can be called to relocate statement 
    !! information to child components of StNode.  @a stmt will not be used 
    !! by client code after the Process call.
    !! 
    !! @param[out]    new_tip         The new tip of the statement tree after 
    !! the statement has been processed.  This could be the same as @a st, 
    !! or a child node of @a st, or even the parent or sibling of @a st. 
    !!
    !! @param[out]    err_list        List of errors.  Errors should 
    !! mainly be concerned with structural problems with the source - 
    !! unmatched statement block end statements, incorrectly positioned 
    !! statements etc.
    !!
    !! The procedures that implement this binding nearly always need to be 
    !! recursive.
    !!
    !! This binding is how the current statement gets passed to the top 
    !! (current) statement tree node.  The statement tree node may in turn 
    !! decide to create leaf nodes for further processing of the statement, or 
    !! to pass the statement up to its parent node.
    SUBROUTINE stnode_BuildTree(st, stmt, new_tip, err_list)
      USE Errors
      USE StatementData
      IMPORT :: StNode
      IMPLICIT NONE
      !-------------------------------------------------------------------------
      CLASS(StNode), INTENT(INOUT), TARGET :: st
      TYPE(StData), INTENT(INOUT) :: stmt
      CLASS(StNode), INTENT(OUT), POINTER :: new_tip
      TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    END SUBROUTINE stnode_BuildTree
    
    !> Interface for StNode%GetPart - get the part associated with the 
    !! current statement tree node.
    !!
    !! @param[in]     st              The statement tree node being queried.
    !!
    !! @returns The part associated with the node.  This is used by the 
    !! Classify procedure to classify statements the type of which depends 
    !! on context (for example an END statement can end five different 
    !! types of program unit).
    !!
    !! Not all statements can be disambiguated by context, but most can.
    FUNCTION stnode_GetPart(st) RESULT(part)
      IMPORT :: StNode
      IMPLICIT NONE
      !-------------------------------------------------------------------------
      CLASS(StNode), INTENT(IN) :: st
      INTEGER :: part
    END FUNCTION stnode_GetPart
    
    !> Interface for StNode%DumpTree - print debugging information about 
    !! the statement tree.
    !!
    !! @param[in]     st              The statement tree node.
    !!
    !! @param[in]     unit            The unit to dump to.
    !!
    !! @param[in]     pfx             Prefix to write at the start 
    !! of each record (for indentation).
    !!
    !! Prints the chain of context associated with the node and its parents.
    !!
    !! Nodes that have child nodes should override this binding and also pass 
    !! the call onto their child nodes.
    SUBROUTINE stnode_DumpTree(st, unit, pfx)
      IMPORT :: StNode
      IMPLICIT NONE
      !-------------------------------------------------------------------------
      ! We gave this the TARGET attribute out of caution - expect that it 
      ! is not going to be needed.
      CLASS(StNode), INTENT(IN), TARGET :: st
      INTEGER, INTENT(IN) :: unit
      CHARACTER(*), INTENT(IN) :: pfx
    END SUBROUTINE stnode_DumpTree
    
    !> Interface of StNode%Query - called during a visitor tree traversal 
    !! operation.
    !!
    !! @param[in]     st              The statement tree node being 
    !! traversed.
    !!
    !! @param[in,out] visitor         The object that will execute actions 
    !! during the traversal.
    SUBROUTINE stnode_Query(st, visitor)
      IMPORT :: StNode
      IMPORT :: StNodeQueryVisitor
      !-------------------------------------------------------------------------
      ! This has the target attribute because there is a potential that 
      ! a Query might navigate up and down the tree.
      CLASS(StNode), INTENT(IN), TARGET :: st
      CLASS(StNodeQueryVisitor), INTENT(INOUT) :: visitor
    END SUBROUTINE stnode_Query
    
    !> Interface of StNode%Modify - called during a visitor tree traversal 
    !! operation.
    !!
    !! @param[in,out] st              The statement tree node being 
    !! traversed.
    !!
    !! @param[in,out] visitor         The object that will execute actions 
    !! during the traversal.
    SUBROUTINE stnode_Modify(st, visitor)
      IMPORT :: StNode
      IMPORT :: StNodeModifyVisitor
      !-------------------------------------------------------------------------
      ! This has the target attribute because there is a potential that 
      ! a Modify might navigate up and down the tree.
      CLASS(StNode), INTENT(INOUT), TARGET :: st
      CLASS(StNodeModifyVisitor), INTENT(INOUT) :: visitor
    END SUBROUTINE stnode_Modify
    
    !> Interface for StNode%GetLocation - get the source location of the 
    !! start of the node.
    !!
    !! @param[in]     st              The node being queried.
    !!
    !! @returns A location relevant to the start of the node.
    FUNCTION stnode_GetLocation(st)
      USE SourceLocations
      IMPORT :: StNode
      !-------------------------------------------------------------------------
      CLASS(StNode), INTENT(IN) :: st
      TYPE(SourceLocation) :: stnode_GetLocation
    END FUNCTION stnode_GetLocation
    
    !> Interface for StdNode%GetLabel - get the label of the first statement 
    !! in the node.
    !!
    !! @param[in]     st              The node being queried.
    !!
    !! @returns The label of the first statement in the node.  If that 
    !! statement has no label, returns zero.
    FUNCTION stnode_GetLabel(st)
      IMPORT :: StNode
      IMPORT :: LabelWrapper
      !-------------------------------------------------------------------------
      CLASS(StNode), INTENT(IN) :: st
      TYPE(LabelWrapper) :: stnode_GetLabel
    END FUNCTION stnode_GetLabel
    
    !> Interface for StNodeQueryVisitor%Execute - execute the action of the 
    !! visitor against a particular statement tree node.
    !!
    !! @param[in,out] visitor         The object executing the action.
    !!
    !! @param[in]     st              The tree node being traversed.
    SUBROUTINE stnodequeryvisitor_ExecuteNode(visitor, st)
      IMPORT :: StNodeQueryVisitor
      IMPORT :: StNode
      IMPLICIT NONE
      !-------------------------------------------------------------------------
      CLASS(StNodeQueryVisitor), INTENT(INOUT) :: visitor
      ! Target so the execute action can navigate up and down the tree.
      CLASS(StNode), INTENT(IN), TARGET :: st
    END SUBROUTINE stnodequeryvisitor_ExecuteNode
    
    !> Interface for StNodeQueryVisitor%Execute - execute the action of the 
    !! visitor against a particular statement in the tree.
    !!
    !! @param[in,out] visitor         The object executing the action.
    !!
    !! @param[in]     parent          Pointer to the node that owns @a stmt.
    !!
    !! @param[in]     stmt            The statement being traversed.
    SUBROUTINE stnodequeryvisitor_ExecuteStmt(visitor, parent, stmt)
      USE StatementData
      IMPORT :: StNodeQueryVisitor
      IMPORT :: StNode
      IMPLICIT NONE
      !-------------------------------------------------------------------------
      CLASS(StNodeQueryVisitor), INTENT(INOUT) :: visitor
      ! Target so the execute action can navigate up and down the tree.
      CLASS(StNode), INTENT(IN), TARGET :: parent
      TYPE(StData), INTENT(IN) :: stmt
    END SUBROUTINE stnodequeryvisitor_ExecuteStmt
    
    !> Interface for StNodeQueryVisitor%Execute - execute the action of the 
    !! visitor against a particular statement tree node.
    !!
    !! @param[in,out] visitor         The object executing the action.
    !!
    !! @param[in]     st              The tree node being traversed.
    SUBROUTINE stnodemodifyvisitor_ExecuteNode(visitor, st)
      IMPORT :: StNodeModifyVisitor
      IMPORT :: StNode
      IMPLICIT NONE
      !-------------------------------------------------------------------------
      CLASS(StNodeModifyVisitor), INTENT(INOUT) :: visitor
      ! Target so the execute action can navigate up and down the tree.
      CLASS(StNode), INTENT(INOUT), TARGET :: st
    END SUBROUTINE stnodemodifyvisitor_ExecuteNode
    
    !> Interface for StNodeQueryVisitor%Execute - execute the action of the 
    !! visitor against a particular statement in the tree.
    !!
    !! @param[in,out] visitor         The object executing the action.
    !!
    !! @param[in]     parent          Pointer to the node that owns @a stmt.
    !!
    !! @param[in]     stmt            The statement being traversed.
    SUBROUTINE stnodemodifyvisitor_ExecuteStmt(visitor, parent, stmt)
      USE StatementData
      IMPORT :: StNodeModifyVisitor
      IMPORT :: StNode
      IMPLICIT NONE
      !-------------------------------------------------------------------------
      CLASS(StNodeModifyVisitor), INTENT(INOUT) :: visitor
      ! Target so the execute action can navigate up and down the tree.
      CLASS(StNode), INTENT(INOUT), TARGET :: parent
      TYPE(StData), INTENT(INOUT) :: stmt
    END SUBROUTINE stnodemodifyvisitor_ExecuteStmt
  END INTERFACE
  
  !-----------------------------------------------------------------------------
  
  !> A list (vector) of nodes.
  !!
  !! Provided here because many statement tree nodes include child nodes.
  !!
  !! Some convenience bindings are provided that forward the call to the 
  !! relevant binding of the item.
  !!
  !! It isn't a list, it is the type of an element that can be in an array.  
  !! So rename this to be StNodeElement.  One day, when it is raining.
  TYPE, PUBLIC :: StNodeList
    !> An item in the list of nodes.
    CLASS(StNode), ALLOCATABLE :: item
  CONTAINS
    !> Formward to the Check binding of the item component.
    PROCEDURE :: Check => stnl_Check
    !> Forward to the Query binding of the item component.
    PROCEDURE :: Query => stnl_Query
    !> Forward to the Modify binding of the item component.
    PROCEDURE :: Modify => stnl_Modify
    !> Forward to the GetLocation binding of the item component.
    PROCEDURE :: GetLocation => stnl_GetLocation
  END TYPE StNodeList
  
  !-----------------------------------------------------------------------------
  
  !> Add a node to a list of nodes.
  INTERFACE AddNode
    MODULE PROCEDURE AddNode_
  END INTERFACE AddNode
  
  !> Get the syntax parts for the statement tree.
  INTERFACE GetParts
    MODULE PROCEDURE GetParts_
  END INTERFACE GetParts
  
  !> Get the syntax part name for a statement tree node.
  INTERFACE GetPartName
    MODULE PROCEDURE GetPartName_
  END INTERFACE GetPartName
  
  !> Incorporate a statement into the statement tree.
  INTERFACE BuildTree
    MODULE PROCEDURE BuildTree_
  END INTERFACE BuildTree
  
  !-----------------------------------------------------------------------------
  ! Format specifications
  
  !> Format specification for redering a part name in DumpTree.
  !!
  !! The first 'A' is for the prefix (blanks for indentation).
  CHARACTER(*), PARAMETER, PUBLIC :: fmt_partname = "(A,'+ ',A)"
  
  !> Format specification for redering a statement name in DumpTree.
  !!
  !! The first 'A' is for the prefix (blanks for indentation).
  CHARACTER(*), PARAMETER, PUBLIC :: fmt_stmtname = "(A,'- ',A)"
  
CONTAINS
  
  !****************************************************************************
  !!
  !> Implementation of StNode%Check - check that the node complies with 
  !! the rules of the language.
  !!
  !! @param[in]     st                The statement tree node.
  !!
  !! @param[out]    err_list          List of errors
  !!
  !! The default implementation does nothing.
  
  SUBROUTINE stnode_Check(st, err_list)
    
    USE Errors
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(StNode), INTENT(IN) :: st
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !***************************************************************************
    
    ALLOCATE(err_list(0))
    
  END SUBROUTINE stnode_Check
  
  
  !*****************************************************************************
  !!
  !> Check the label associated with a statement tree node.
  !!
  !! @param[in]     st                The statement tree node.
  !!
  !! @param[out]    statement_label   The label to check.
  !!
  !! @returns The label check status.  This implementation, provided as a 
  !! default, returns .FALSE.
  
  FUNCTION stnode_CheckLabel(st, statement_label)
    
    USE LabelsUtils
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(StNode), INTENT(IN), TARGET :: st
    TYPE(Label), INTENT(IN) :: statement_label
    
    ! Function result.
    LOGICAL :: stnode_CheckLabel
    
    !***************************************************************************
    
    stnode_CheckLabel = .FALSE.
    
  END FUNCTION stnode_CheckLabel
  
  
  !*****************************************************************************
  !!
  !> Implementation of StNode%Terminate - called when a parent node decides 
  !! that it has finished.
  !!
  !! @param[in,out] st                The statement tree node being 
  !! terminated.
  !!
  !! @paramin]      eof               The statement tree node was terminated 
  !! by end of file.  @a stmt is not defined.
  !!
  !! @param[in]     stmt              The statement that was incorporated into 
  !! the tree and triggered termination.  Not defined if @a eof is true.
  !!
  !! @param[out]    err_list          List of errors.
  
  SUBROUTINE stnode_Terminate(st, eof, stmt, err_list)
    
    USE CharUtils
    USE Errors
    USE ErrorCodes
    USE StatementData
    USE SyntaxParts
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(StNode), INTENT(INOUT), TARGET :: st
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
    
    IF (eof) THEN
      CALL Add( err_list,  &
          CODE=errUnterminatedBlock,  &
          COMPONENT=comp,  &
          MSG='An unterminated block exists for a /'  &
              // GetPartName(st) // '/.' )
    ELSE
      CALL Add( err_list,  &
          CODE=errUnterminatedBlock,  &
          COMPONENT=comp, LOCATION=QueryLocation(stmt%tlist(1)),  &
          MSG='An unterminated block exists for a /'  &
              // GetPartName(st) // '/.' )
    END IF
    
  END SUBROUTINE stnode_Terminate
  
  
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! Helper procedures related to StNodes.
  
  
  !****************************************************************************
  !!
  !> Implementation of SingleStNode%Check - check that the node complies with 
  !! the rules of the language.
  !!
  !! @param[in]     st                The statement tree node element.
  !!
  !! @param[out]    err_list          List of errors
  !!
  !! The default implementation does nothing.
  
  SUBROUTINE stnl_Check(st, err_list)
    
    USE Errors
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(StNodeList), INTENT(IN) :: st
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !***************************************************************************
    
    CALL st%item%Check(err_list)
    
  END SUBROUTINE stnl_Check
  
  
  !*****************************************************************************
  !!
  !> Implementation of StNodeList%Query - forward the Query call onto the 
  !! item component.
  !!
  !! @param[in]     st                The node being traversed.
  !!
  !! @param[in,out] visitor           The object executing the actions during 
  !! the traversal.
  
  SUBROUTINE stnl_Query(st, visitor)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(StNodeList), INTENT(IN), TARGET :: st
    CLASS(StNodeQueryVisitor), INTENT(INOUT) :: visitor
    
    !***************************************************************************
    
    CALL st%item%Query(visitor)
    
  END SUBROUTINE stnl_Query
  
  
  !*****************************************************************************
  !!
  !> Implementation of StNodeList%Modify - forward the Modify call onto the 
  !! item component.
  !!
  !! @param[in]     st                The node being traversed.
  !!
  !! @param[in,out] visitor           The object executing the actions during 
  !! the traversal.
  
  SUBROUTINE stnl_Modify(st, visitor)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(StNodeList), INTENT(INOUT), TARGET :: st
    CLASS(StNodeModifyVisitor), INTENT(INOUT) :: visitor
    
    !***************************************************************************
    
    CALL st%item%Modify(visitor)
    
  END SUBROUTINE stnl_Modify
  
  
  !*****************************************************************************
  !!
  !> Implementation of StNodeList%GetLocation - forward the GetLocation 
  !! reference onto the item component.
  !!
  !! @param[in]     st                The node being traversed.
  !!
  !! @returns The location of the contained node.
  
  FUNCTION stnl_GetLocation(st)
    
    USE SourceLocations
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(StNodeList), INTENT(IN), TARGET :: st
    
    ! Function result.
    TYPE(SourceLocation) :: stnl_GetLocation
    
    !***************************************************************************
    
    stnl_GetLocation = st%item%GetLocation()
    
  END FUNCTION stnl_GetLocation
  
  
  !*****************************************************************************
  !!
  !> Add a StNode to a StNodeList.
  !!
  !! @param[in,out] list              The list (vector) of StNodeList.
  !!
  !! Client code needs to allocate and define the added element.  So this 
  !! doesn't really "Add" - it just "Grows".  Oh well.
  
  SUBROUTINE AddNode_(list)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(StNodeList), INTENT(INOUT), ALLOCATABLE, TARGET :: list(:)
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Temporary for growing list of nodes.  The target attribute is requied 
    ! so that pointers maintain their validity over the move.
    TYPE(StNodeList), ALLOCATABLE, TARGET :: tmp(:)
    
    INTEGER :: i              ! Node index.
    
    !***************************************************************************
    
    IF (ALLOCATED(list)) THEN
      ALLOCATE(tmp(SIZE(list)+1))
      DO i = 1, SIZE(list)
        ! The move alloc means that pointers to list(i)%item continue to point 
        ! to tmp(i)%item.
        CALL MOVE_ALLOC(list(i)%item, tmp(i)%item)
      END DO
    ELSE
      ALLOCATE(tmp(1))
    END IF
    
    CALL MOVE_ALLOC(tmp, list)
    
  END SUBROUTINE AddNode_
  
  
  !*****************************************************************************
  !!
  !> Get the part stack from a given stnode item back up to the base.
  !!
  !! @param[in]     st                The statement tree node to start the 
  !! query.
  !!
  !! @returns The part stack for the corresponding statement tree node.  
  !! The node indicated by @a st is placed at the end of the stack - items 
  !! towards index one represent the parts for syntax constructs that wrap 
  !! @a st.
  
  RECURSIVE FUNCTION GetParts_(st) RESULT(part_stack)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(StNode), INTENT(IN), TARGET :: st
    
    ! Function result
    INTEGER, ALLOCATABLE :: part_stack(:)
    
    !***************************************************************************
    
    IF (ASSOCIATED(st%parent)) THEN
      part_stack = [GetParts(st%parent), st%GetPart()]
    ELSE
      part_stack = [st%GetPart()]
    END IF
    
  END FUNCTION GetParts_
  
  
  !*****************************************************************************
  !!
  !> Gets the part name associated with the StNode.
  !!
  !! @param[in]     st                The statement tree node of interest.
  !!
  !! @returns The part name associated with the node.
  
  FUNCTION GetPartName_(st) RESULT(name)
    
    USE SyntaxParts
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(StNode), INTENT(IN) :: st
    
    ! Function result
    CHARACTER(:), ALLOCATABLE :: name
    
    !***************************************************************************
    
    name = GetPartName(st%GetPart())
    
  END FUNCTION GetPartName_
  
  
  !*****************************************************************************
  !!
  !> Wrapper for BuildTree that also carries out label matching.
  !!
  !! @param[in,out] tip               The current tip of the statement tree.
  !! When the procedure finishes executing this is updated to represent the 
  !! new tip of the statement tree after the given statement has been 
  !! incorporated.
  !!
  !! @param[in]     ist               The classification of the current 
  !! statement.
  !!
  !! @param[in]     statement_label   The label for the current statement.
  !!
  !! @param[in]     tlist             The token sequence for the current 
  !! statement (excluding label).
  !!
  !! @param[out]    err_list          List of errors.
  
  SUBROUTINE BuildTree_(tip, ist, statement_label, tlist, err_list)
    
    USE CharUtils
    USE Errors
    USE ErrorCodes
    USE ErrorLevels
    USE LabelsUtils
    USE Statements
    USE StatementData
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(StNode), INTENT(INOUT), POINTER :: tip
    INTEGER, INTENT(IN) :: ist
    TYPE(Label), INTENT(IN), ALLOCATABLE :: statement_label
    TYPE(Token), INTENT(IN) :: tlist(:)
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! The statment data provided in the arguments packaged up into a single 
    ! object.  This is partly due to code legacy.
    TYPE(StData) :: stmt
    
    ! Tip after stmt has been incorporated into the tree.
    CLASS(StNode), POINTER :: new_tip
    
    ! Error list for procedure calls.
    TYPE(Error), ALLOCATABLE :: sub_err_list(:)
    
    !***************************************************************************
    
    ! ifort bug - allocatable component in structure constructor.
    if (allocated(statement_label)) then
      stmt = StData(ist, statement_label, tlist)
    else
      stmt = StData(ist, null(), tlist)
    end if
    
    ! Incorporate stmt into the tree, initially under the direction of the 
    ! current tip.  Note this allocates err_list and also moves information 
    ! out of stmt.
    CALL tip%BuildTree(stmt, new_tip, err_list)
    IF (Fatal(err_list)) RETURN
    
    ! Update the tip.
    tip => new_tip
    
    ! If there is no new tip, then checking labels is irrelevant.  This might 
    ! happen when this routine is called from test code.
    IF (.NOT. ASSOCIATED(tip)) RETURN
    
    ! If the statement had no label then it cannot terminate a labelled-block.
    IF (.NOT. ALLOCATED(statement_label)) RETURN
    
    ! We might need the packaged statement data again.  Build tree above 
    ! has destroyed it, so repackage.  Clumsy, but it is what it is.
    stmt = StData(ist, statement_label, tlist)
    
    ! Check to see if the statement label terminates a labelled-block.  Do 
    ! this by working our way back up the tree, from tip towards the ultimate 
    ! parent.
    DO WHILE (ASSOCIATED(new_tip))
      IF (new_tip%CheckLabel(statement_label)) THEN
        ! We have a match.  Now unwind the tip back to this matching node.
        DO WHILE (.NOT. ASSOCIATED(tip, new_tip))
          CALL tip%Terminate(.FALSE., stmt, sub_err_list)
          CALL Add(err_list, sub_err_list)
          tip => tip%parent
        END DO
        
        ! Here new_tip and tip are the same and point to the node that 
        ! matched.  Terminate that node.
        CALL tip%Terminate(.FALSE., stmt, sub_err_list)
        CALL Add(err_list, sub_err_list)
        tip => tip%parent
      END IF
      new_tip => new_tip%parent
    END DO
    
  END SUBROUTINE BuildTree_
  
  
  !*****************************************************************************
  !!
  !> Check a label against the statement tree, searching up the hierarchy 
  !! until we find the highest match.
  !!
  !! @param[in]     st                The statement tree node.
  !!
  !! @param[out]    statement_label   The label to check.
  !!
  !! @param[out]    match_ptr         If a label match is found, then 
  !! this points to the matching StNode.  Otherwise not associated.
  
  RECURSIVE SUBROUTINE CheckLabel(st, statement_label, match_ptr)
    
    USE LabelsUtils
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(StNode), INTENT(IN), TARGET :: st
    TYPE(Label), INTENT(IN) :: statement_label
    CLASS(StNode), INTENT(OUT), POINTER :: match_ptr
    
    !***************************************************************************
    
    IF (ASSOCIATED(st%parent)) THEN
      CALL CheckLabel(st%parent, statement_label, match_ptr)
    ELSE IF (st%CheckLabel(statement_label)) THEN
      match_ptr => st
    ELSE
      match_ptr => NULL()
    END IF
    
  END SUBROUTINE CheckLabel
  
  
  !*****************************************************************************
  !!
  !> Terminate the tree up to and including the given node.
  !!
  !! @param[in]     base              The node to terminate up to.
  !!
  !! @param[in]     current_tip       The current node.
  !!
  !! @param[out]    err_list          List of errors.
  
  SUBROUTINE TerminateTree(base, current_tip, err_list)
    
    USE Errors
    USE StatementData
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(StNode), INTENT(IN), TARGET :: base
    CLASS(StNode), INTENT(IN), POINTER :: current_tip
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Error list for child terminate calls.
    TYPE(Error), ALLOCATABLE :: sub_err_list(:)
    
    TYPE(StData) :: stmt      ! Dummy statement.
    
    ! Pointer for heading back up the tree.
    CLASS(StNode), POINTER :: tip
    
    !***************************************************************************
    
    ALLOCATE(err_list(0))
    
    !---------------------------------------------------------------------------
    
    tip => current_tip
    DO WHILE (.NOT. ASSOCIATED(tip, base))
      ! Because we pass in .TRUE. for the EOF argument, the actual statement 
      ! is ignored (it should have an empty tlist anyway).
      CALL tip%Terminate(.TRUE., stmt, sub_err_list)
      CALL Add(err_list, sub_err_list)
      
      tip => tip%parent
    END DO
    
    CALL tip%Terminate(.TRUE., stmt, sub_err_list)
    CALL Add(err_list, sub_err_list)
    
  END SUBROUTINE TerminateTree
  
END MODULE StNodes
