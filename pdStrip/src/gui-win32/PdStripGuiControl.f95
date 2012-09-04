! Copyright (c) 2005-2006 Gerrie Thiart
!
! This is Free Software. For license terms, see the LICENSE file.

! Data and methods to control the flow of PROGRAM "PdStrip"
MODULE PdStripGuiControl
  IMPLICIT NONE
! Interfaces
! Parameters
  INTEGER, PARAMETER :: shortStringLength = 24
  INTEGER, PARAMETER :: mediumStringLength = 32
  INTEGER, PARAMETER :: longStringLength = 128 
  INTEGER, PARAMETER :: maxPrevAppl = 10
  CHARACTER(LEN = shortStringLength), PARAMETER :: mainCaption = 'PDSTRIP GUI'
  CHARACTER(LEN = mediumStringLength), PARAMETER :: filePathCaption = 'PdStrip File path' 
  CHARACTER(LEN = shortStringLength), PARAMETER :: readyStatus = 'Ready to go ...'
  CHARACTER(LEN = shortStringLength), PARAMETER :: launchedStatus = 'PdStrip launched ...'   
  CHARACTER(LEN = shortStringLength), PARAMETER :: doneStatus = 'Done!'
  DOUBLE PRECISION, PARAMETER :: editBoxFontSize = 0.9D0           
! Types
! Variables
  CHARACTER(LEN = longStringLength) :: editBoxDescr, inputFileName = ' ', pathName = ' ', genFileName
  CHARACTER(LEN = shortStringLength) :: progStatus  
  CHARACTER(LEN = longStringLength), DIMENSION(1:maxPrevAppl) :: applList
  INTEGER :: mainWinControl, inputFileWinControl, messageWinControl = 1  
  INTEGER :: inputFileOk = 1, inputFileOpen = 0, inputFileClosed = 1        ! Change "inputFileOk = 1" to "inputFileOk = 0"
  INTEGER :: frameWidth, frameDepth, xInputFileWin = 0, yInputFileWin = 0  
  INTEGER :: nrPrevAppl, handleAppl
  INTEGER, DIMENSION(1:24) :: inputFileInfo = 0

CONTAINS

! Initialize the program
  SUBROUTINE InitializePdStrip
    USE CLRWIN
    USE StringHandlingUtilities, ONLY : AttachIntToString
    IMPLICIT NONE 
    INTEGER :: charWidth, charHeight
    frameWidth = (99*CLEARWIN_INFO@('screen_width'))/100
    frameDepth = (9*CLEARWIN_INFO@('screen_depth'))/10
    charWidth = CLEARWIN_INFO@('pixels_per_h_unit')
    charHeight = CLEARWIN_INFO@('pixels_per_v_unit')
    editBoxDescr = '%pv%'
    CALL AttachIntToString(INT(0.7*frameWidth/(editBoxFontSize*charWidth)), editBoxDescr, .FALSE.)
    editBoxDescr = TRIM(editBoxDescr)//'.'
    CALL AttachIntToString(INT(0.7*frameDepth/(editBoxFontSize*charHeight)), editBoxDescr, .FALSE.)
    progStatus = readyStatus      
  END SUBROUTINE InitializePdStrip
  
! Separate the pathname and general file name from the input file name
  SUBROUTINE GetPathName
    IMPLICIT NONE
    INTEGER :: iChar, iLastSlash, iLastPoint
    iLastSlash = 0; iLastpoint = 0
    DO iChar = 1, LEN(inputFileName)
      IF (inputFileName(iChar:iChar).EQ.'\') iLastSlash = iChar
      IF (inputFileName(iChar:iChar).EQ.'.') iLastPoint = iChar
    END DO
    IF (iLastSlash.EQ.0) THEN
      pathname = ' '
    ELSE
      pathName = inputFileName(1:iLastSlash)
    END IF
    IF(iLastPoint.GT.iLastSlash) genFileName = inputFileName(iLastSlash+1:iLastPoint-1)
  END SUBROUTINE GetPathName 

END MODULE PdStripGuiControl