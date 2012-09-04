! Copyright (c) 2005-2006 Gerrie Thiart
!
! This is Free Software. For license terms, see the LICENSE file.

! This module implements some string handling utilities [Origin: Package Ellipsoid]
MODULE StringHandlingUtilities 
  USE KindControl, ONLY : realKind
  IMPLICIT NONE     
  PRIVATE RealToScientificString, RealToFixedString
! Interfaces     
  INTERFACE RealToString
    MODULE PROCEDURE RealToScientificString, RealToFixedString
  END INTERFACE RealToString        
! Parameters
! Types    
! Variables  

CONTAINS
                                          
! [INTERFACE RealToString] Convert a real number to the string equivalent, in scientific number notation
  FUNCTION RealToScientificString(numberVal) RESULT(numberString)
    IMPLICIT NONE
    REAL(KIND = realKind), INTENT(IN) :: numberVal
    CHARACTER(LEN = 11) :: numberString
    INTEGER :: expVal, intFrac
    REAL(KIND = realKind) :: realFrac
    IF (ABS(numberVal).LE.0.0) THEN
      numberString = ' 0.000E+000'
    ELSE
      IF (numberVal.GE.0.0) THEN
        numberString = '+'
      ELSE
        numberString = '-'
      END IF  
      expVal = FLOOR(LOG10(ABS(numberVal)))
      realFrac = 10.0**(LOG10(ABS(numberVal))-FLOAT(expVal))
      intFrac = NINT(1000.0*realFrac)
      IF (intFrac.GE.10000) THEN
        intFrac = intFrac/10
        expVal = expVal+1
      END IF
      CALL AttachIntToString(intFrac/1000, numberString, .FALSE.)
      numberString = TRIM(numberString)//'.'
      intFrac = MOD(intFrac, 1000)
      IF (intFrac.LT.10) THEN
        numberString(4:5) = '00'
      ELSE IF (intFrac.LT.100) THEN
        numberString(4:4) = '0'
      END IF
      CALL AttachIntToString(intFrac, numberString, .FALSE.)
      IF (expVal.GE.0) THEN
        numberString(7:8) = 'E+'
      ELSE
        numberString(7:8) = 'E-'
      END IF
      CALL AttachIntToString(ABS(expVal), numberString, .FALSE.)
      IF (ABS(expVal).LT.10) THEN
        numberString(11:11) = numberString(9:9)
        numberString(9:10) = '00'
      ELSE IF (ABS(expVal).LT.100) THEN
        numberString(10:11) = numberString(9:10)
        numberString(9:9) = '0'
      END IF
    END IF
  END FUNCTION RealToScientificString   
                                                                                          
! [INTERFACE RealToString] Convert a real number to the string equivalent    
  FUNCTION RealToFixedString(numberVal, nrDecimals) RESULT(numberString)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: nrDecimals
    REAL(KIND = realKind), INTENT(IN) :: numberVal
    CHARACTER(LEN = 11) :: numberString
    INTEGER :: intVal, exponent, iDecimal
    IF ((ABS(numberVal).GE.1.0e6).OR.(ABS(numberVal).LT.1.0e-8)) THEN
      numberString = RealToScientificString(numberVal)
    ELSE
      IF (numberVal.GE.0.0) THEN 
        numberString = '+'
      ELSE 
        numberString = '-'
      END IF
      exponent = MIN(nrDecimals, 7-FLOOR(LOG10(MAX(ABS(numberVal), EPSILON(0.0d0)))))
      intVal = NINT(ABS(numberVal*10.0**exponent))
      CALL AttachIntToString(intVal/10**exponent, numberString, .FALSE.)
      numberString = TRIM(numberString)//'.' 
      DO iDecimal = 1, exponent     
        intVal = MOD(intVal, 10**(exponent+1-iDecimal))
        CALL AttachIntToString(intVal/10**(exponent-iDecimal), numberString, .FALSE.) 
      END DO 
    END IF 
  END FUNCTION RealToFixedString

! Write an integer number to a specified FORTRAN unit without unneccessary leading spaces  
  RECURSIVE SUBROUTINE WriteInt(unitNr, intNumber)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: unitNr, intNumber
! Write a single digit
    IF (intNumber.LT.10) THEN
      WRITE(unitNr, FMT = '(i1)', ADVANCE = 'no') intNumber
! Write more than one digit
    ELSE
      CALL WriteInt(unitNr, intNumber/10)
      CALL WriteInt(unitNr, MOD(intNumber, 10))
    END IF
  END SUBROUTINE WriteInt
                                                                      
! Attach an integer number to a string, with a space inserted if required 
  RECURSIVE SUBROUTINE AttachIntToString(intNumber, string, insertSpace)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: intNumber
    CHARACTER(LEN = *), INTENT(INOUT) :: string
    LOGICAL, INTENT(IN) :: insertSpace
! Add a single digit to the string
    IF (intNumber.LT.10) THEN
      IF (insertSpace) THEN
        string = TRIM(string)//' '//ACHAR(IACHAR('0')+intNumber)
      ELSE
        string = TRIM(string)//ACHAR(IACHAR('0')+intNumber)
      END IF
! Add more than one digit to the string
    ELSE
      CALL AttachIntToString(intNumber/10, string, insertSpace)
      CALL AttachIntToString(MOD(intNumber, 10), string, .FALSE.)
    END IF
  END SUBROUTINE AttachIntToString   
                                                                       
END MODULE StringHandlingUtilities
