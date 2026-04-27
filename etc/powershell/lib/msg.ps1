# msg — output helpers (analog of etc/bash/lib/msg.bash).
# ANSI bold + color, gracefully falling back to plain text when stdout is
# redirected. Names follow the PowerShell Verb-Noun convention; `Failure`
# stands in for `Error` to avoid clashing with the built-in `Write-Error`.

if (-not [Console]::IsOutputRedirected) {
    $Script:MsgEsc   = [char]27
    $Script:MsgBold  = "$Script:MsgEsc[1m"
    $Script:MsgGreen = "$Script:MsgEsc[1;32m"
    $Script:MsgRed   = "$Script:MsgEsc[1;31m"
    $Script:MsgBlue  = "$Script:MsgEsc[1;34m"
    $Script:MsgReset = "$Script:MsgEsc[0m"
} else {
    $Script:MsgBold = ''; $Script:MsgGreen = ''; $Script:MsgRed = ''
    $Script:MsgBlue = ''; $Script:MsgReset = ''
}

function Write-Heading([string]$msg) {
    Write-Host ''
    Write-Host ('{0}==>{1} {2}{3}{4}' -f $Script:MsgBlue, $Script:MsgReset, $Script:MsgBold, $msg, $Script:MsgReset)
}
function Write-Arrow([string]$msg) {
    Write-Host ('  {0}->{1} {2}' -f $Script:MsgBlue, $Script:MsgReset, $msg)
}
function Write-Success([string]$msg) {
    Write-Host ('  {0}✔{1} {2}' -f $Script:MsgGreen, $Script:MsgReset, $msg)
}
function Write-Failure([string]$msg) {
    [Console]::Error.WriteLine('  {0}✖{1} {2}' -f $Script:MsgRed, $Script:MsgReset, $msg)
}
