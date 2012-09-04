! Copyright (c) 2005-2006 Gerrie Thiart
!
! This is Free Software. For license terms, see the LICENSE file.

! This module implements some file handlig utilities [Origin: Package Ellipsoid]
MODULE FileHandlingUtilities
  IMPLICIT NONE    
! Interfaces
! Parameters
! Types
! Variables  

CONTAINS
                                          
! This function returns the lowest available FORTRAN unit number above 6  
  FUNCTION FindUnit() RESULT(unitNr)
    IMPLICIT NONE
    INTEGER :: unitNr
    LOGICAL :: isOpen
    unitNr = 6
    isOpen = .TRUE.
    DO WHILE (isOpen)
      unitNr = unitNr+1
      INQUIRE(UNIT = unitNr, OPENED = isOpen)
    END DO
  END FUNCTION FindUnit
                             
! This subroutine returns an unused file name as close as possible to the input file name  
  SUBROUTINE FindFile(fileName)
    USE StringHandlingUtilities, ONLY : AttachIntToString 
    IMPLICIT NONE
    CHARACTER(LEN = *), INTENT(INOUT) :: fileName
    CHARACTER(LEN = LEN(fileName)) :: tempFileName
    INTEGER :: counter = 0
    LOGICAL :: fileExists
    tempFileName = fileName
    DO WHILE (LEN_TRIM(tempFileName).LT.LEN(tempFileName))
      CALL AttachIntToString(counter, tempFileName, .FALSE.)
      INQUIRE(FILE = tempFileName, EXIST = fileExists)
      IF (.NOT.fileExists) THEN
        fileName = tempFileName
        EXIT
      ELSE
        tempFileName = fileName
        counter = counter+1
      END IF
    END DO
  END SUBROUTINE FindFile
                                                
END MODULE FileHandlingUtilities
