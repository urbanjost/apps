! $Id: ExecStNodeFactory.f90 1003 2013-03-18 04:31:00Z ian $
! ff08 source code copyright 2012 M.E.G.M.S.  See LICENCE.txt for licence.
!> @file
!! Defines the ExecStNodeFactoryMod


!*******************************************************************************
!!
!> External procedure to create statement tree nodes for executable 
!! statements and constructs.
!!
!! This exists as an external procedure to break a cyclic dependency loop 
!! between statement tree nodes (program units are used in interface blocks, 
!! which are used in specification sections which are used in block constructs, 
!! which are used in executable parts which are used in program units.
!!
!! @param[in]     ist                 The statement identifier that describes 
!! the type of statement tree node that needs to be created.
!!
!! @param[out]    node                Allocated to the appropriate executable 
!! statement tree node, or not allocated if the statement is not recognised 
!! as a statement that can belong in an /execution-part/.
!!
!! This procedure recognises all the statements that can be part of a 
!! /execution-part/.

SUBROUTINE ExecStNodeFactory(ist, node)
  
  ! Only clauses to make sure we don't pick up our own interface.
  USE AssociateConstructStNodes, ONLY: AssociateConstructStNode
  USE BlockConstructStNodes, ONLY: BlockConstructStNode
  USE CaseConstructStNodes, ONLY: CaseConstructStNode
  USE CriticalConstructStNodes, ONLY: CriticalConstructStNode
  USE DoConstructStNodes, ONLY: DoConstructStNode
  USE ForallConstructStNodes, ONLY: ForallConstructStNode
  USE IfConstructStNodes, ONLY: IfConstructStNode
  USE SelectTypeConstructStNodes, ONLY: SelectTypeConstructStNode
  USE SingleStNodes, ONLY: SingleStNode
  USE Statements
  USE StNodes
  USE WhereConstructStNodes, ONLY: WhereConstructStNode
  
  IMPLICIT NONE
  
  !-----------------------------------------------------------------------------
  ! Arguments
  
  INTEGER, INTENT(IN) :: ist
  CLASS(StNode), INTENT(OUT), ALLOCATABLE :: node
  
  !*****************************************************************************
  
  SELECT CASE (ist)
  CASE (istAssociate)
    ALLOCATE(AssociateConstructStNode :: node)
  CASE (istBlock)
    ALLOCATE(BlockConstructStNode:: node)
  CASE (istSelectCase)
    ALLOCATE(CaseConstructStNode:: node)
  CASE (istCritical)
    ALLOCATE(CriticalConstructStNode:: node)
  CASE (istNonlabelDo, istLabelDo)
    ALLOCATE(DoConstructStNode:: node)
  CASE (istForallConstruct)
    ALLOCATE(ForallConstructStNode:: node)
  CASE (istIfThen)
    ALLOCATE(IfConstructStNode:: node)
  CASE (istSelectType)
    ALLOCATE(SelectTypeConstructStNode:: node)
  CASE (istWhereConstruct)
    ALLOCATE(WhereConstructStNode:: node)
  CASE ( istFormat, istEntry, istData,  &
      istAllocate, istAssignment, istBackspace, istCall, istClose,  &
      istContinue, istCycle, istDeallocate, istEndfile, istErrorStop,  &
      istExit, istFlush, istForall, istGoto, istIf, istInquire,  &
      istLock, istNullify, istOpen, istPointerAssignment, istPrint,  &
      istRead, istReturn, istRewind, istStop, istSyncAll, istSyncImages,  &
      istSyncMemory, istUnlock, istWait, istWhere, istWrite,  &
      istArithmeticIf, istComputedGoto )
    ALLOCATE(SingleStNode:: node)
  END SELECT
  
END SUBROUTINE ExecStNodeFactory
