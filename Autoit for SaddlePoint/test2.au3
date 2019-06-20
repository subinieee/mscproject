
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

Local $arr[3] = [10,50,100]
Local $arr2[5] = [20,40,60,80,100]

for $i = 0 to 2

   ; * change array of numbers according to the names of your datafiles.
   ; Run Saddlepoint
   Run($APP_EXE)

   ; Wait for the saddlepoint to become active. T
   WinWait("Run", "", 1)
   Sleep(100)

   ;output directory: analysis_results change?  (y/n) n
   Send("n{enter}")
   Sleep(100)

   for $j = 0 to 4

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
	  Send($arr[$i])
	  Sleep(200)

	  ; nr of covariates(P)
	  Send($arr2[$j]&"{enter}")
	  Sleep(100)

	  ;*nr of active risks (excluding EOT):1
	  Send("1{enter}")
	  Sleep(100)

	  ; nr of latent classes (L): 1
	  Send("1{enter}")
	  Sleep(100)

	  ; 1: uncorrelated Gaussian covariates
	  Send("1{enter}")
	  Sleep(100)

	  ; fraction of missing covariate values:0
	  Send("0.05{enter}")
	  Sleep(100)

	  ; include EOT censoring?  (y/n)
	  Send("y{enter}")
	  Sleep(100)

	  ;3:block censoring
	  Send("3{enter}")
	  Sleep(100)

	  ;lower censoring time:
	  Send("1{enter}")
	  Sleep(100)

	  ;upper censoring time:
	  Send("12{enter}")
	  Sleep(300)

	  ; *nr of truly associated covariates:
	  Send("10{enter}")
	  Sleep(100)

	  ; *assign beta risks
	  Send("0.5{enter}0.5{enter}0.5{enter}0.5{enter}0.5{enter}-0.5{enter}-0.5{enter}-0.5{enter}-0.5{enter}-0.5{enter}")
	  Sleep(100)


	  ;baserate shape, risk 1: 1: lambda(t)=C
	  Send("1{enter}")
	  Sleep(100)

	  ;average_time(risk 1):4
	  Send("5{enter}")
	  Sleep(100)

	  ;filename:
	  Send("N"&$arr[$i]&"_P"&$arr2[$j]&"{enter}")
	  Sleep(100)

	  ;~  generate correlograms?  (y/n)
	  Send("n{enter}")

	  Sleep(500)

	  DirMove("C:\Users\Public\bin\SaddlePoint-Signature-v2.8.7\Test\analysis_results", "C:\Users\Public\bin\SaddlePoint-Signature-v2.8.7\Test\analysis_results"&"N"&$arr[$i]&"_P"&$arr2[$j])

	  Sleep(500)

   Next

   Sleep(500)

Next
