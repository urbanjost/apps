! $Id: DependencyUtils.f90 1005 2013-03-28 03:20:40Z ian $
! ff08 source code copyright 2012 M.E.G.M.S.  See LICENCE.txt for licence.
!> @file
!! Defines the DependencyUtils module.


!*******************************************************************************
!!
!> Routines for conducting dependency analyses.
!!
!! This is a general partitioning utility, adapted from a process flowsheet 
!! application.  When we talk about units below we are really referring to 
!! files.  Note that if file one defines a module that is used in file two, 
!! then file one "supplies" file two, i.e. conn_matrix(2,1) should be 
!! .TRUE..
!!
!! NOTE WELL: to allow filtering to work the way we want it to we typically 
!! supply the transpose of that arrangement, hence the order comes out 
!! backwards.  Confused yet?
!!
!! (The dependency analysis could also be conducted at a program unit level.)
!!
!! Connection matrices are interpreted with the sources (from unit) by columns 
!! and the destinations (to unit) by rows.

MODULE DependencyUtils
  
  USE IntegerLists
  
  IMPLICIT NONE
  
  PRIVATE
  
  !-----------------------------------------------------------------------------
  
  PUBLIC :: IntegerList       ! From IntegerLists
  
  PUBLIC :: GetPartitions
  
  !> Partition a connection matrix.
  INTERFACE GetPartitions
    MODULE PROCEDURE GetPartitions_
  END INTERFACE GetPartitions
  
CONTAINS
  
  !*****************************************************************************
  !!
  !> Partition a connection matrix.
  !!
  !! @param[in]     conn_matrix       The connection matrix that specifies 
  !! when "columns supply rows" (i.e. if conn_matrix(2,1) is .TRUE., then 
  !! unit one is upstream of unit two (column one supplies row two).  That 
  !! means in terms of ordering, unit one must be calculated before unit one.
  !!
  !! @param[in]     filter            Indices of the units of interest.  
  !! Units that are not in this list or that are not downstream of units 
  !! in this list are not included in the output in @a part.  If zero 
  !! size then all units are considered units of interest.
  !!
  !! @param[out]    part              The partitions, ordered based on the 
  !! dependencies (i.e. given the example for @a conn_matrix, a partition 
  !! for one will appear before a partition for two).  Partitions with 
  !! size greater than one indicate cycles (dependency loops) - the order 
  !! within such a partition is indeterminate.
  !!
  !! (You can transpose the meaning of @a conn_matrix, and then just reverse 
  !! the ordering of @a part.)
  
  SUBROUTINE GetPartitions_(conn_matrix, filter, part)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    LOGICAL, INTENT(IN) :: conn_matrix(:,:)
    INTEGER, INTENT(IN) :: filter(:)
    TYPE(IntegerList), INTENT(OUT), ALLOCATABLE :: part(:)
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! List of units not yet processed.
    INTEGER, ALLOCATABLE :: remainder(:)
    
    ! Work list.  As we follow the connections we add items to this list.  
    ! when we reach the end of a connection chain we then transfer the last 
    ! item across to the @a part output.  In the meantime, encountering a 
    ! unit that is already in the work list means that we've looped.
    TYPE(IntegerList), ALLOCATABLE :: work_list(:)
    
    INTEGER :: i              ! Utility index.
    INTEGER :: iu             ! Focus unit.
    
    ! Copy of the input matrix, updated as we go to remove followed 
    ! connections.
    LOGICAL :: matrix(SIZE(conn_matrix,1),SIZE(conn_matrix,2))
    
    !***************************************************************************
    
    ! To allow the algorithm to modify the connection matrix (so it can keep 
    ! track of which connections have been followed) we make a copy of the 
    ! input matrix.
    matrix = conn_matrix
    
    ! At the start, the units not yet processed is the complete list of units 
    ! of interest.  In the absence of a specification to the contrary, the 
    ! list of units of interest is all units.
    IF (SIZE(filter) == 0) THEN
      remainder = [(i, i = 1, SIZE(conn_matrix,1))]
    ELSE
      remainder = filter
    END IF
    
    ALLOCATE(work_list(0))
    ALLOCATE(part(0))
    
    ! Loop until we've run out of things to consider.
    outer_loop: DO WHILE ((SIZE(remainder) > 0) .OR. SIZE(work_list) > 0)
      ! Get the next unit to consider.  Prefer a unit from the work list.
      IF (SIZE(work_list) == 0) THEN
        ! Get a unit from the unprocessed list.
        iu = remainder(1)
      ELSE
        ! Get a unit index from the last partition in our work list.
        iu = work_list(SIZE(work_list))%items(1)
      END IF
      
      DO
        ! Have we seen this unit before?
        i = Find(work_list, iu)
        IF (i /= 0) THEN
          ! We've looped - make all the partitions in the loop one big 
          ! partition.
          CALL Collapse(work_list, i)
          ! Set all the connections for the units in that big partition to 
          ! be the same externally.  Consequently, if we end up anywhere 
          ! in this partition again then any unit connected to the partition 
          ! becomes a candidate for follow to pick.
          CALL block(matrix, work_list(SIZE(work_list))%items)
        ELSE
          ! No loop (yet) - append to our work list as its own little 
          ! partition and remove from the remainder list.
          ! ifort 12.0 (earlier?) bug work around:
          ! was list_one = [list_one, Partition([iu])]
          CALL Append(work_list, [iu])
          CALL Remove(remainder, [iu])
        END IF
        ! Move to the next unit in the flowsheet.
        iu = follow(matrix, iu)
        
        IF (iu == 0) THEN
          ! There was no following block.  Put the last partition at the 
          ! front of the output list.
          CALL Prepend(part, work_list(SIZE(work_list))%items)
          
          ! Break all connections to the removed partition.
          matrix(work_list(SIZE(work_list))%items,:) = .FALSE.
          
          ! Remove the last partition from the work list.
          work_list = work_list(:SIZE(work_list)-1)
          
          ! Go and get a new unit.
          CYCLE outer_loop
        END IF
      END DO
    END DO outer_loop
    
  END SUBROUTINE GetPartitions_
  
  
  !*****************************************************************************
  !!
  !> Alter a connection matrix to treat the given units as a single block.
  !!
  !! @param[in,out] matrix            The connection matrix, dimensions 
  !! arranged (to, from).
  !!
  !! @param[in]     indices           Indices of the units to block together.
  !!
  !! All the units in @a indices will have the same inputs and the same 
  !! outputs.  None of the units in block will be connected to each other.
  
  SUBROUTINE block(matrix, indices)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    LOGICAL, INTENT(INOUT) :: matrix(:,:)
    INTEGER, INTENT(IN) :: indices(:)
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: i              ! Utility index.
    
    !***************************************************************************
    
    ! Internally disconnect the block.
    DO i = 1, SIZE(indices)
      matrix(indices(i),indices) = .FALSE.
    END DO
    
    ! Make inputs and outputs the same for all units in the block.
    DO i = 1, SIZE(matrix, 1)
      IF (ANY(indices == i)) CYCLE
      matrix(i,indices) = ANY(matrix(i,indices))
      matrix(indices,i) = ANY(matrix(indices,i))
    END DO
    
  END SUBROUTINE block
  
  
  !*****************************************************************************
  !!
  !> Get a downstream unit for a given unit based on a connection matrix.
  !!
  !! @param[in]     matrix            The connection matrix.
  !!
  !! @param[in]     from              The source (from) unit.
  !!
  !! @returns A destination (to) unit for unit @a from, or zero if there 
  !! are no destination units.
  
  FUNCTION follow(matrix, from) RESULT(to)
    
    !---------------------------------------------------------------------------
    ! Local arguments
    
    LOGICAL, INTENT(IN) :: matrix(:,:)
    INTEGER, INTENT(IN) :: from
    
    ! Function result
    INTEGER :: to
    
    !***************************************************************************
    
    DO to = 1, SIZE(matrix, 1)
      IF (matrix(to,from)) RETURN
    END DO
    to = 0
    
  END FUNCTION follow
  
END MODULE DependencyUtils
