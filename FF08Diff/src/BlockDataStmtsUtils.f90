! $Id: BlockDataStmtsUtils.f90 2800 2019-03-22 18:13:05Z ian $
! ff08 source code copyright 2014 M.E.G.M.S.  See LICENCE.txt for licence.
!> @file
!! Defines the BlockDataStmtsUtils module.


!*******************************************************************************
!!
!> Procedure for extracting the block data name from a block data statement.

MODULE BlockDataStmtsUtils
  
  IMPLICIT NONE
  
  PRIVATE
  
  PUBLIC :: GetBlockDataName
  
  ! Get the block data unit's name.
  INTERFACE GetBlockDataName
    MODULE PROCEDURE GetBlockDataName_
  END INTERFACE GetBlockDataName
  
CONTAINS
  
  !*****************************************************************************
  !!
  !> Get the name of the block data unit from the /block-data-stmt/.
  !!
  !! @param[in]     tlist             The sequence of tokens that makes up the 
  !! /block-data-stmt/.
  !!
  !! @param[out]    name              The /block-data-name/ in the statement.  
  !! Not allocated if the name cannot be sensibly determined.  Zero length 
  !! if the block data statement had no name (the name is optional for 
  !! a block data program unit).
  
  SUBROUTINE GetBlockDataName_(tlist, name)
    
    USE CompilerKinds
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Token), INTENT(IN) :: tlist(:)
    CHARACTER(:), INTENT(OUT), ALLOCATABLE :: name
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    !***************************************************************************
    
    ! Bit of trickiness as the BLOCK and DATA keywords may be adjacent 
    ! without a separating blank.
    
    IF (IsToken(tlist(1), ittName, scck_'BLOCK')) THEN
      IF (SIZE(tlist) > 2) THEN
        IF (IsToken(tlist(2), ittName, scck_'DATA')) THEN
          IF (tlist(3) == ittName) THEN
            name = QueryValue(tlist(3))
          END IF
        END IF
      ELSE
        name = ''             ! Indicate an unnamed unit.
      END IF
    ELSE IF (IsToken(tlist(1), ittName, scck_'BLOCKDATA')) THEN
      IF (SIZE(tlist) > 1) THEN
        IF (tlist(3) == ittName) THEN
          name = QueryValue(tlist(3))
        END IF
      ELSE
        name = ''
      END IF
    END IF
    
  END SUBROUTINE GetBlockDataName_
  
END MODULE BlockDataStmtsUtils
