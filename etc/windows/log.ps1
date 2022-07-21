# Log recording script
# It can be used to record the time of logon or logoff by setting it as
# logon/logoff script via group policy editor (gpedit.msc)
#
# Example parameters:
# $dirPath = C:\log
# $recordType = LOGON

Param(
    [Parameter(Position=0,
               HelpMessage="Directory path to save the log file",
               Mandatory=$true)]
    [String]$dirPath,
    [Parameter(Position=1,
               HelpMessage="Log record type",
               Mandatory=$true)]
    [String]$recordType
)

# Right pad record type name
$recordType = $recordType.PadRight(6)

# Getting dates
$currentDateTime = Get-Date
$longDateTime = $currentDateTime.ToString('G')

# Setting log filename and location to save the log file
$logFileName = "log.txt"
$logPath = Join-Path $dirPath $logFileName

# Record to log
$record = "$($longDateTime) $($recordType) :: $($env:UserName) - $($env:ComputerName)"

# Append the record to the log
$record | Out-File $logPath -Encoding default -Append
