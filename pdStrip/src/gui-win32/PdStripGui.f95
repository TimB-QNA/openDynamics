! Copyright (c) 2005-2006 Gerrie Thiart
!
! This is Free Software. For license terms, see the LICENSE file.

WINAPP

PROGRAM PdStripGui
  USE CLRWIN
  USE PdStripGuiControl, ONLY : InitializePdStrip, mainCaption, progStatus, mainWinControl, handleAppl, &
                                 inputFileOk, inputFileOpen, inputFileClosed, inputFileName, frameWidth, frameDepth
  IMPLICIT NONE
  EXTERNAL CommandsInfo$, AboutPdStripGui$, AboutPdStrip$, GetInputFile$, LoadNewInputFile$, CloseInputFile$, &
            InputErrorCheck$, Compute$, ClosePdStripGui$
  INTEGER :: mainWin, WINIO@
  CALL InitializePdStrip
  CALL GetApplicationList
  mainWin = WINIO@('%ww[no_border, maximize]%bg[white]%ca@%lw&', TRIM(mainCaption), mainWinControl)
  mainWin = WINIO@('%sc%cc&', CommandsInfo$, '+', ClosePdStripGui$, '+', CloseInputFile$, 'EXIT')
  mainWin = WINIO@('%pv%fr&', frameWidth, frameDepth)   
  mainWin = WINIO@('%mn[&Application[~&Load]]&', inputFileClosed, '+', GetInputFile$, LoadNewInputFile$) 
  mainWin = WINIO@('%mn[[~&Close, |]]&', inputFileOpen, CloseInputFile$)
  mainWin = WINIO@('%mn[[*]]&', handleAppl)  
  mainWin = WINIO@('%mn[[&Exit]]&', 'EXIT')
  mainWin = WINIO@('%mn[~&Check]&', inputFileOpen, InputErrorCheck$) 
  mainWin = WINIO@('%mn[~&Launch]&', inputFileOpen, Compute$) ! change "inputFileOpen" to "inputFileOpen"
  mainWin = WINIO@('%mn[&Help[&About PdStripGui]]&', AboutPdStripGui$)  
  mainWin = WINIO@('%mn[[&About PdStrip]]&', AboutPdStrip$)
  mainWin = WINIO@('')
  CALL UpdateApplList(.FALSE.)
END PROGRAM PdStripGui 
                      
! This subroutine initializes the list of most recently used applications
SUBROUTINE GetApplicationList
  USE PdStripGuiControl, ONLY : maxPrevAppl, nrPrevAppl, handleAppl, applList
  USE FileHandlingutilities, ONLY : FindUnit
  IMPLICIT NONE
  INTEGER :: unitIni, endOfList, iAppl
  LOGICAL :: listExists
! Read available application names
  INQUIRE(FILE = 'PdStripGui.ini', EXIST = listExists)
  unitIni = FindUnit()   
  nrPrevAppl = 0
  IF (listExists) THEN
    OPEN(unitIni, FILE = 'PdStripGui.ini', STATUS = 'old')
    DO WHILE (nrPrevAppl.LT.maxPrevAppl)
      READ(unitIni, *, IOSTAT = endOfList) applList(nrPrevAppl+1)
      IF (endOfList.EQ.0) THEN
        nrPrevAppl = nrPrevAppl+1
      ELSE
        EXIT
      END IF
    END DO 
  ELSE
    OPEN(unitIni, FILE = 'PdStripGui.ini', STATUS = 'new')
  END IF      
  CLOSE(unitIni)
! Fill up list with blanks
  DO iAppl = nrPrevAppl+1, maxPrevAppl
    applList(iAppl) = ' '
  END DO
END SUBROUTINE GetApplicationList
                          
! This subroutine updates the recently used application list
SUBROUTINE UpdateApplList(removeOld)
  USE CLRWIN
  USE PdStripGuiControl, ONLY : maxPrevAppl, nrPrevAppl, handleAppl, applList, inputFileClosed, inputFileName
  IMPLICIT NONE
  EXTERNAL LoadPrevInputFile$
  LOGICAL, INTENT(IN) :: removeOld  
  INTEGER :: iAppl, jAppl
! Remove the current application list
  IF (removeOld) THEN
    DO iAppl = nrPrevAppl, 1, -1
      CALL REMOVE_MENU_ITEM@(handleAppl)
      IF (MOD(iAppl-1, 5).EQ.0) CALL REMOVE_MENU_ITEM@(handleAppl)
    END DO
! Update the application list
    jAppl = maxPrevAppl
    DO iAppl = 1, nrPrevAppl
      IF (applList(iAppl).EQ.inputFileName) THEN
        jAppl=iAppl
        EXIT
      END IF
    END DO
    IF (jAppl.EQ.maxPrevAppl) nrPrevAppl = MIN(maxPrevAppl, nrPrevAppl+1)
    DO iAppl = MIN(jAppl, nrPrevAppl), 2, -1
      applList(iAppl) = applList(iAppl-1)
    END DO
    applList(1) = inputFileName
  END IF
! Set up the recently used file list
  DO iAppl = 1, nrPrevAppl
    IF (MOD(iAppl-1, 5).EQ.0) CALL ADD_MENU_ITEM@(handleAppl, CHAR(0), 0, 0, 'CONTINUE')
    CALL ADD_MENU_ITEM@(handleAppl, applList(iAppl), inputFileClosed, 0, LoadPrevInputFile$)
  END DO
END SUBROUTINE UpdateApplList

! Get the input file name (and from it the path name and general file name)
INTEGER FUNCTION GetInputFile$()
  USE PdStripGuiControl, ONLY : filePathCaption, inputFileName
  USE FileHandlingutilities, ONLY : FindUnit
  IMPLICIT NONE
  CALL GET_FILTERED_FILE@(filePathCaption, inputFileName, ' ', 'Input data', '*.inp', 1, 1)
  CALL WritePdStripIni
  GetInputFile$ = 1
END FUNCTION GetInputFile$

! Get path name and general file name and write them to the PdStrip initialization file
SUBROUTINE WritePdStripIni
  USE PdStripGuiControl, ONLY : pathName, inputFileName, genFileName, GetPathName
  USE FileHandlingutilities, ONLY : FindUnit
  IMPLICIT NONE
  INTEGER :: unitNo
  CALL GetPathName
  unitNo = Findunit()
  OPEN(unitNo, FILE = 'PdStrip.ini', STATUS = 'replace')
  WRITE(unitNo, '(a, /, a, /)') TRIM(pathName), TRIM(genFileName)
  CLOSE(unitNo)
END SUBROUTINE WritePdStripIni

! Do the computation
INTEGER FUNCTION Compute$()
!  USE CLRWIN      
  USE PdStripGuiControl, ONLY : progStatus, launchedStatus, doneStatus, pathName, inputFileName, inputFileInfo, inputFileOk, &
                                 messageWinControl 
  IMPLICIT NONE 
!  LOGICAL :: isOk
  INTEGER :: WINIO@, statusWin, commandFail
  IF (inputFileInfo(13).EQ.1) THEN
    CALL SaveInputFileChanges
!    IF (inputFileOk.EQ.0) THEN
!      CALL CheckInput(isOk)
!      IF (isOk) THEN
!        inputFileOk = 1
!        CALL WINDOW_UPDATE@(inputFileOk)
!      END IF
!    END IF
  END IF
  IF (inputFileOk.EQ.1) THEN
    commandFail = 0
    IF (messageWinControl.LT.0) THEN
      messageWinControl = 0
      CALL WINDOW_UPDATE@(messageWinControl)
    END IF
    statusWin = WINIO@('%ww[no_sysmenu, no_border, volatile]&')     
    statusWin = WINIO@('%sc%lw&', 'BEEP[ASTERISK]', messageWinControl)
    statusWin = WINIO@('%ob[thin-panelled]%dy%si*%fn[Arial]%tsPdStrip launched ...%cb&', 0.5d0, 1.6d0)
    statusWin = WINIO@(' ')
    CALL CISSUE('pdstrip.exe', commandFail)
    IF (messageWinControl.LT.0) THEN
      messageWinControl = 0
      CALL WINDOW_UPDATE@(messageWinControl)
    END IF
    statusWin = WINIO@('%ww[no_sysmenu, no_border, volatile, topmost]&')     
    statusWin = WINIO@('%sc%lw&', 'BEEP[ASTERISK]', messageWinControl)
    statusWin = WINIO@('%ob[thin-panelled]%dy%si*%fn[Arial]%tsPdStrip done!%cb&', 0.5d0, 1.6d0)
    statusWin = WINIO@(' ')
  END IF
  Compute$ = 1
END FUNCTION Compute$  
                            
! Display information about the main menu commands
INTEGER FUNCTION CommandsInfo$()
  USE PdStripGuiControl, ONLY : messageWinControl
  IMPLICIT NONE 
  INTEGER :: WINIO@, commandsInfoWin
  IF (messageWinControl.LT.0) THEN
    messageWinControl = 0
    CALL WINDOW_UPDATE@(messageWinControl)
  END IF
  commandsInfoWin = WINIO@('%ww[no_sysmenu, no_border, volatile]&')
  commandsInfoWin = WINIO@('%sc&', 'BEEP[ASTERISK]')
  commandsInfoWin = WINIO@('%ob[thin-panelled]%nl%si*%nl%fn[Arial]%tsPdStrip GUI Menu Commands&', 1.6d0)
  commandsInfoWin = WINIO@('%ts%2nlUse the %tc[blue]Application %tc[black]option to manage your input files.&', 1.2D0) 
  commandsInfoWin = WINIO@('%nlUse the %tc[blue]Check %tc[black]option to check your input file for errors.&')
  commandsInfoWin = WINIO@('%nlThe %tc[blue]Launch %tc[black]option launches PdStrip.exe.&')
  commandsInfoWin = WINIO@('%nlThe %tc[blue]Help %tc[black]option provides limited information.&')
  commandsInfoWin = WINIO@('%2nl%tc[red]Note that the %tc[blue]Check %tc[red]option is currently void.%cb&')
  commandsInfoWin = WINIO@('%lw', messageWinControl)
  CommandsInfo$ = 1
END FUNCTION CommandsInfo$      
                             
! Display information about PdStrip
INTEGER FUNCTION AboutPdStrip$()
  IMPLICIT NONE
  INTEGER :: WINIO@, RGB@, aboutWin
  aboutWin = WINIO@('%ww[no_sysmenu]%ca[ABOUT PdStrip]&') 
  aboutWin = WINIO@('%fn[Arial]%ts&', 1.5D0)
  aboutWin = WINIO@('%tc[red]%cnThe Public Domain Strip Code&')  
  aboutWin = WINIO@('%ts%2nl%tc%cnMarch 2006 Version%tc[black]&', 1.375D0, RGB@(0, 127, 0))  
  aboutWin = WINIO@('%2nl%ts%cnContact: %tc[blue]V Bertram&', 1.25D0)
  aboutWin = WINIO@('%it%ts%1nl%cnVolker.Bertram@ensieta.fr%tc[black]&', 1.0D0)  
  aboutWin = WINIO@('%sf%3nl%cn%`8bt[OK]')
  AboutPdStrip$=1
END FUNCTION AboutPdStrip$    
                             
! Display information about the PdStrip GUI
INTEGER FUNCTION AboutPdStripGui$()
  IMPLICIT NONE
  INTEGER :: WINIO@, RGB@, aboutWin
  aboutWin = WINIO@('%ww[no_sysmenu]%ca[ABOUT PdStripGui]&') 
  aboutWin = WINIO@('%fn[Arial]%ts&', 1.5D0)
  aboutWin = WINIO@('%tc[red]%cnThe PdStrip Graphical User Interface&')  
  aboutWin = WINIO@('%ts%2nl%tc%cnVersion1.01%tc[black]&', 1.375D0, RGB@(0, 127, 0))  
  aboutWin = WINIO@('%2nl%ts%cnContact: %tc[blue]GD Thiart&', 1.25D0)
  aboutWin = WINIO@('%it%ts%1nl%cnthiart@ma2.sun.ac.za%tc[black]&', 1.0D0)  
  aboutWin = WINIO@('%sf%3nl%cn%`8bt[OK]')
  AboutPdStripGui$=1
END FUNCTION AboutPdStripGui$
                       
! Open the input file edit box
INTEGER FUNCTION LoadNewInputFile$()  
  USE CLRWIN
  USE PdStripGuiControl, ONLY : inputFileName
  IMPLICIT NONE
  CALL OpenInputFile
  LoadNewInputFile$ = 1
END FUNCTION LoadNewInputFile$     
                              
! This is a call-back function to open an application on the recently-used list
INTEGER FUNCTION LoadPrevInputFile$()
  USE CLRWIN
  USE PdStripGuiControl, ONLY : inputFileName
  IMPLICIT NONE
  inputFileName = CLEARWIN_STRING@('CURRENT_MENU_ITEM') 
  CALL WritePdStripIni
  CALL OpenInputFile
  LoadPrevInputFile$ = 1
END FUNCTION LoadPrevInputFile$     
                       
! Open the input file edit box
SUBROUTINE OpenInputFile
  USE CLRWIN
  USE PdStripGuiControl, ONLY : inputFileName, inputFileOpen, inputFileClosed, inputFileWinControl, xInputFileWin, yInputFileWin, &
                                 editBoxFontSize, editBoxDescr, inputFileInfo, mainWinControl, messageWinControl
  IMPLICIT NONE
  EXTERNAL UpdateInputEditor$, CloseInputFile$, UpdateInputCheck$
  INTEGER :: WINIO@, inputFileWin, inputFileHandle
  LOGICAL :: inputFileExists
  INQUIRE(FILE = TRIM(inputFileName), EXIST = inputFileExists) 
  IF (inputFileExists) THEN
    CALL UpdateApplList(.TRUE.)
    inputFileWin = WINIO@('%ww[no_maxminbox, no_border]%`ca@%aw&', TRIM(inputFileName), mainWinControl)
    inputFileWin = WINIO@('%fn[Courier]%ts%hw%lw&', editBoxFontSize, inputFileHandle, inputFileWinControl)
    inputFileWin = WINIO@(TRIM(editBoxDescr)//'`eb[vscrollbar, hscrollbar, undo]&', '*', 0, inputFileInfo)
    inputFileWin = WINIO@('%sc&', UpdateInputEditor$)
    inputFileWin = WINIO@('%cc&', '+', UpdateInputEditor$, CloseInputFile$)
    inputFileWin = WINIO@('%pm[Undo     Ctrl-U, |]&', 'EDIT_UNDO')
    inputFileWin = WINIO@('%pm[Cut       Ctrl-X]&', 'CUT')
    inputFileWin = WINIO@('%pm[Copy     Ctrl-C]&', 'COPY')
    inputFileWin = WINIO@('%pm[Paste    Ctrl-V, |]&', 'PASTE')
    inputFileWin = WINIO@('%pm[&Save]&', '+', 'EDIT_FILE_SAVE', '*.*', inputFileName, UpdateInputCheck$)
    inputFileWin = WINIO@('%pm[&Exit]&', CloseInputFile$)
    inputFileWin = WINIO@(' ')
    CALL MOVE_WINDOW@(inputFileHandle, xInputFileWin, yInputFileWin)
    inputFileWin = OPEN_EDIT_FILE@(inputFileInfo, inputFileName)
  ELSE
    IF (messageWinControl.LT.0) THEN
      messageWinControl = 0
      inputFileWin =  WINDOW_UPDATE@(messageWinControl)
    END IF
    inputFileWin = WINIO@('%ww[no_sysmenu, no_border, volatile]&')     
    inputFileWin = WINIO@('%sc%lw&', 'BEEP[EXCLAMATION]', messageWinControl)
    inputFileWin = WINIO@('%ob[thin-panelled]%dy%si#%fn[Arial]%tsFile does not exist!%cb&', 0.5d0, 1.6d0)
    inputFileWin = WINIO@(' ')
  END IF
END SUBROUTINE OpenInputFile   
                              
! Update the input file edit box
INTEGER FUNCTION UpdateInputEditor$()
  USE PdStripGuiControl, ONLY : inputFileOpen, inputFileClosed
  IMPLICIT NONE 
  inputFileOpen = inputFileClosed
  CALL WINDOW_UPDATE@(inputFileOpen)
  inputFileClosed = 1-inputFileOpen
  CALL WINDOW_UPDATE@(inputFileClosed)
  UpdateInputEditor$ = 1
END FUNCTION UpdateInputEditor$        
                              
! Update the input file error check indicator
INTEGER FUNCTION UpdateInputCheck$()
  USE PdStripGuiControl, ONLY : inputFileOk
  IMPLICIT NONE 
!  IF (inputFileOk.EQ.1) THEN
!    inputFileOk = 0
    CALL WINDOW_UPDATE@(inputFileOk)
!  END IF
  UpdateInputCheck$ = 1
END FUNCTION UpdateInputCheck$   
                            
! Close the input file edit box
INTEGER FUNCTION CloseInputFile$()
  USE PdStripGuiControl, ONLY : inputFileWinControl, inputFileOk
  IMPLICIT NONE
  CALL SaveInputFileChanges
!  inputFileOk = 0 
!  CALL WINDOW_UPDATE@(inputFileOk)
  inputFileWinControl = 0
  CALL WINDOW_UPDATE@(inputFileWinControl)
  CloseInputFile$ = 1
END FUNCTION CloseInputFile$    
                            
! Save or discard the changes to the input data file
SUBROUTINE SaveInputFileChanges
  USE PdStripGuiControl, ONLY : inputFileName, mainCaption, inputFileInfo, inputFileOk
  IMPLICIT NONE 
  INTEGER :: WINIO@, saveChangesWin
  IF (inputFileInfo(13).EQ.1) THEN
    saveChangesWin = WINIO@('%ww[no_sysmenu, topmost]%ca@%bg[grey]&', TRIM(mainCaption))
    saveChangesWin = WINIO@('%cn%si!Save changes to input file?%2nl&')
!   IF (inputFileOk.EQ.0) THEN
      saveChangesWin = WINIO@('%cn%5^bt[Yes]   %5^bt[No]', '+', 'EDIT_FILE_SAVE', '*.*', inputFileName, 'EXIT', 'EXIT')
!   ELSE
!     saveChangesWin = WINIO@('%cn%5^bt[Yes]   %5^bt[No]', '+', 'EDIT_FILE_SAVE', '*.*', inputFileName, &
!                             '+', 'TOGGLE', inputFileOk, 'EXIT', 'EXIT')
!   END IF
  END IF
  inputFileInfo(13) = 0
  CALL WINDOW_UPDATE@(inputFileOk)
END SUBROUTINE SaveInputFileChanges                           
                              
! Run eror checks on the input data file
INTEGER FUNCTION InputErrorCheck$()
  USE PdStripGuiControl, ONLY : inputFileOk
  IMPLICIT NONE        
  LOGICAL :: isOk
  CALL CheckInput(isOk)
!  IF (isOk) THEN 
    inputFileOk = 1
!  ELSE
!    inputFileOk = 0
!  END IF
  CALL WINDOW_UPDATE@(inputFileOk)
  InputErrorCheck$ = 1
END FUNCTION InputErrorCheck$     
                              
! Update the input file edit box
SUBROUTINE CheckInput(isOk)
  IMPLICIT NONE 
  LOGICAL, INTENT(OUT) :: isOk
  isOk = .TRUE.
END SUBROUTINE CheckInput

! Exit the PdStrip GUI
INTEGER FUNCTION ClosePdStripGui$()
  USE PdStripGuiControl, ONLY : nrPrevAppl, applList, messageWinControl  
  USE FileHandlingutilities, ONLY : FindUnit
 ! EXTERNAL CloseAppl$
 ! INTEGER :: CloseAppl$
  INTEGER :: iAppl, unitIni
  unitIni = FindUnit()
  OPEN(unitIni, FILE = 'PdStripGui.ini', STATUS = 'replace')
  DO iAppl = 1, nrPrevAppl
    WRITE(unitIni, *) applList(iAppl)
  END DO
  CLOSE(unitIni)
  IF (messageWinControl.NE.0) THEN
    messageWinControl = 0
    CALL WINDOW_UPDATE@(messageWinControl)
  END IF
!  CloseSucc$ = CloseAppl$()
  ClosePdStripGui$ = 1
END FUNCTION ClosePdStripGui$
      
