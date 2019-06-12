#include <Constants.au3>
#include <FileConstants.au3>
;
;
; Script Function:
;   Opens saddlepoint, generate synthetic data, run regression.
; * parameters you might want to check when changing dim. of data.

 $APP_EXE = "SaddlePoint-Signature.exe"

; Prompt the user to run the script - use a Yes/No prompt with the flag parameter set at 4 (see the help file for more details)
Local $iAnswer = MsgBox(BitOR($MB_YESNO, $MB_SYSTEMMODAL), "Auto", "This script will run create Synthetic Time-to-event data on Saddlepoint. Do you want to run it?")

; Check the user's answer to the prompt (see the help file for MsgBox return values)
; If "No" was clicked (7) then exit the script
If $iAnswer = 7 Then
	MsgBox($MB_SYSTEMMODAL, "AutoIt", "OK.  Bye!")
	Exit
 EndIf

; * change array of numbers according to the names of your datafiles.
 Local $arr[5] = [10,20,30,40,50]

for $i = 0 to 4

;creat a new folder for each run.
DirCreate("C:\Users\Public\bin\SaddlePoint-Signature-v2.8.7\Test\sd"&$arr[$i])

 ; Run Saddlepoint
Run($APP_EXE)

; Wait for the saddlepoint to become active. T
WinWait("Run", "", 1)
Sleep(100)

;output directory: analysis_results change?  (y/n) y
Send("y{enter}")
Sleep(100)

;new output directory:
Send("sd"&$arr[$i]&"{enter}")
Sleep(100)

;output directory: (newone) change?
Send("n{enter}")
Sleep(100)

;MainMenu -1:data input and generation
Send("1{enter}")
Sleep(100)

;2.generate synthetic data
Send("2{enter}")
Sleep(100)

;1.time-to-event outcomes
Send("1{enter}")
Sleep(100)

; *nr of samples (N):
Send("100{enter}")
Sleep(100)

; nr of covariates(P)
Send($arr[$i]&"{enter}")
Sleep(100)

;*nr of active risks (excluding EOT):1
Send("2{enter}")
Sleep(100)

; nr of latent classes (L): 1
Send("1{enter}")
Sleep(100)

; 1: uncorrelated Gaussian covariates
Send("1{enter}")
Sleep(100)

; fraction of missing covariate values:0
Send("0{enter}")
Sleep(100)

; include EOT censoring?  (y/n)
Send("n{enter}")
Sleep(100)

; *nr of truly associated covariates:
Send("10{enter}")
Sleep(100)

; *assign beta risks
Send("0.1{enter}0.2{enter}0.3{enter}0.4{enter}0.5{enter}-0.1{enter}-0.2{enter}-0.3{enter}-0.4{enter}-0.5{enter}")
Sleep(100)

; *assign beta risks
Send("0.01{enter}0.02{enter}0.03{enter}0.04{enter}0.05{enter}-0.01{enter}-0.02{enter}-0.03{enter}-0.04{enter}-0.05{enter}")
Sleep(100)


;baserate shape, risk 1: 1: lambda(t)=C
Send("1{enter}")
Sleep(100)

;average_time(risk 1):4
Send("4{enter}")
Sleep(100)

;* baserate shape, risk 1: 1: lambda(t)=C
Send("1{enter}")
Sleep(100)

;* average_time(risk 2):100
Send("5{enter}")
Sleep(100)

;filename:
Send("SD"&$arr[$i]&"{enter}")
Sleep(100)

;~  generate correlograms?  (y/n)
Send("y{enter}")
Sleep(500)


FileCopy("C:\Users\Public\bin\SaddlePoint-Signature-v2.8.7\Test\SaddlePoint-Signature.ini", "C:\Users\Public\bin\SaddlePoint-Signature-v2.8.7\Test\sd"&$arr[$i]&"\")
FileCopy("C:\Users\Public\bin\SaddlePoint-Signature-v2.8.7\Test\SaddlePoint-Signature.exe", "C:\Users\Public\bin\SaddlePoint-Signature-v2.8.7\Test\sd"&$arr[$i]&"\")

DirMove("C:\Users\Public\bin\SaddlePoint-Signature-v2.8.7\Test\analysis_results", "C:\Users\Public\bin\SaddlePoint-Signature-v2.8.7\Test\analysis_results"&$arr[$i]&"\")
DirMove("C:\Users\Public\bin\SaddlePoint-Signature-v2.8.7\Test\analysis_results"&$arr[$i]&"\","C:\Users\Public\bin\SaddlePoint-Signature-v2.8.7\Test\sd"&$arr[$i]&"\")

;Sleep(500)


;MainMenu -1:data input and generation
Send("1{enter}")
Sleep(100)

;Data input and generation
Send("1{enter}")
Sleep(100)

; 1: time-to-event outcome data with sample identifiers
Send("1{enter}")
Sleep(100)

;filename:
Send("SD"&$arr[$i]&".dat"&"{enter}")
Sleep(100)

;save as Mosaic file?
Send("n{enter}")
Sleep(100)

;generate new file with auxiliary NA-covariates?
Send("n{enter}")
Sleep(500)

;run it for regression
Send("3{enter}")
Sleep(100)

;batch regression with cross-validation
Send("3{enter}")
Sleep(100)

;default settings?
Send("y{enter}")
Sleep(100)

;limit initial covariate set by univariate outcome association?  (y/n)
Send("n{enter}")
Sleep(100)

;nr of training sets (default=200)
Send("200{enter}")
Sleep(500)
Local $iAnswer = MsgBox(BitOR($MB_YESNO, $MB_SYSTEMMODAL), "Auto", "This script will run create Synthetic Time-to-event data on Saddlepoint. Do you want to run it?")

; Check the user's answer to the prompt (see the help file for MsgBox return values)
; If "No" was clicked (7) then exit the script
If $iAnswer = 7 Then
	MsgBox($MB_SYSTEMMODAL, "AutoIt", "OK.  Bye!")
	Exit
 EndIf

;_WaitForCmdText ("save as covariate mask?")
;save as covariate mask?  (y/n)
Sleep(100)

Send("y{enter}")
Sleep(100)

;save MRS (multivariate risk score formula)?  (y/n)
Send("y{enter}")
sleep(1000)

_WaitAndCloseWindow ("gnuplot graph", "" )

Sleep(100)

FileMove("C:\Users\Public\bin\SaddlePoint-Signature-v2.8.7\Test\SD"&$arr[$i]&".dat","C:\Users\Public\bin\SaddlePoint-Signature-v2.8.7\Test\sd"&$arr[$i]&"\")
FileMove("C:\Users\Public\bin\SaddlePoint-Signature-v2.8.7\Test\SD"&$arr[$i]&".names","C:\Users\Public\bin\SaddlePoint-Signature-v2.8.7\Test\sd"&$arr[$i]&"\")
Sleep(100)


Send("0{enter}")
Sleep(100)

Send("0{enter}")
Sleep(100)

WinWaitClose("C:\Users\Public\bin\SaddlePoint-Signature-v2.8.7\Test\SaddlePoint-Signature.exe")

next
; HELPER FUNCTIONS
Func _WaitAndCloseWindow ( $_Title, $_Text='' )
    WinWait  ( $_Title, $_Text )
    WinActivate ( $_Title, $_Text )
    WinWaitActive ( $_Title, $_Text )
	WinClose ( $_Title, $_Text )
 EndFunc ;==> _WaitAndCloseWindow ( )

Func _WaitAndActivateWindow ( $_Title, $_Text='' )
    WinWait  ( $_Title, $_Text )
    WinActivate ( $_Title, $_Text )
    WinWaitActive ( $_Title, $_Text )
EndFunc ;==> _WaitAndActivateWindow ( )

Func _CopyDosWindow ( $_Title, $_Text='' )
   Send ( "{ESC}!{SPACE}{DOWN 6}{ENTER}{DOWN 3}{ENTER}")
   Send ( "!{SPACE}{DOWN 6}{ENTER}{DOWN 1}{ENTER}" )
EndFunc ;==> _CopyDosWindow ( )

Func _WaitForCmdText ( $text )
   Opt("WinTitleMatchMode", 2) ;1=start, 2=subStr, 3=exact, 4=advanced, -1 to -4=Nocase
   _WaitAndActivateWindow ( $APP_EXE )
   While True
	  ; If we do not take care with this loop it can start to act on other windows!
	  ; So we wait for the right window to be active
	  ; and exit if any problems or window not there
	  _WaitAndActivateWindow ( $APP_EXE )
	  _CopyDosWindow (  $APP_EXE )
	  $last = StringRight(ClipGet ( ), 100)    ; Get last 50 chars from DOS to look for prompt in
	  If StringInStr ($last,$text)>0 Then ExitLoop
	  If @error Then ExitLoop
	  If ProcessExists("Run")==0 Then Exit   ; Exit whole script if process has been stopped
	  Sleep (1000)
   WEnd
EndFunc ;==> _WaitForCmdText