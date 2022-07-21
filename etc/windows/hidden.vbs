' Run argument programs or scripts without showing command prompt window.

' Example usage of this script for shortcuts:
' wscript.exe "hidden.vbs" "example.cmd" "argument"

Set oShell = CreateObject ("Wscript.Shell")
Set oArgs = CreateObject("System.Collections.ArrayList")

For Each oItem In Wscript.Arguments: oArgs.Add oItem: Next
oShell.Run """" & Join(oArgs.ToArray, """ """) & """", 0, False
