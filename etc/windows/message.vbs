' Show popup message
Set oArgs = WScript.Arguments
prompt = oArgs(0)
buttons = oArgs(1)
title = oArgs(2)
MsgBox prompt, buttons, title
